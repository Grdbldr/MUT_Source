"""Focus areas: named subregions of the regional model with their own high-resolution (~1 m) ground surface.

A focus area is defined (outline saved, ``status: defined``) and accepted by the user before anything large is
downloaded; ``build_focus`` then fills its grid from local fine DEMs and the enabled ``fine`` providers.
"""

from __future__ import annotations

import json
import tempfile
from dataclasses import asdict, replace
from pathlib import Path
from typing import Callable

import numpy as np

from .boundary import boundary_area_km2, boundary_bounds, snap_out, write_boundary
from .geo import LARGE_FOCUS_CELLS, MAX_FOCUS_RES_M, Grid, check_grid, grid_advice
from .net import prepare_network
from .pipeline import Fetcher, PipelineResult, output_stats
from .project import FocusArea, Project
from .providers.local import LocalFiles
from .raster import fill_small_gaps, hillshade, polygon_mask, read_array, warp_to_grid, write_array
from .report import update_focus_section, write_focus_report

FINE_GAP_FILL_M = 50.0
FOCUS_RES_STEP_M = 0.25

__all__ = [
    "FocusArea",
    "build_focus",
    "define_focus",
    "focus_grid",
    "focus_outputs",
    "grid_summary",
]


def focus_outputs(project: Project, focus: FocusArea) -> dict[str, Path]:
    folder = project.focus_dir(focus)
    stem = f"ground_top_{focus.resolution_m:g}m"
    return {
        "boundary": folder / "focus_boundary.geojson",
        "ground_top": folder / f"{stem}.tif",
        "hillshade": folder / f"{stem}_hillshade.tif",
        "water_surface": folder / f"{stem}_water_surface.tif",
        "lake_depth": folder / f"lake_depth_{focus.resolution_m:g}m.tif",
        "lakes": folder / "lakes.geojson",
        "sources": folder / "sources.json",
        "report": folder / f"{focus.slug}_focus_report.md",
    }


def focus_grid(project: Project, focus: FocusArea) -> Grid:
    """Outline extent plus ``margin_m``, snapped outward to regional cells, at ``resolution_m``."""
    path = Path(focus.boundary_file)
    if not focus.boundary_file or not path.exists():
        raise ValueError(f"Focus area outline not found: '{focus.boundary_file}'")
    xmin, ymin, xmax, ymax = boundary_bounds(path, project.epsg)
    pad = focus.margin_m
    snap = project.options.coarse_resolution_m
    return Grid(
        project.epsg,
        snap_out(xmin - pad, snap, False),
        snap_out(ymin - pad, snap, False),
        snap_out(xmax + pad, snap, True),
        snap_out(ymax + pad, snap, True),
        focus.resolution_m,
    )


def grid_summary(project: Project, focus: FocusArea) -> dict:
    """Area, grid size and approximate output size of a focus area, with a warning when it is very large.

    ``level`` is "" (fine), "large" (above ``LARGE_FOCUS_CELLS``) or "too_large" (above ``MAX_GRID_CELLS``).
    """
    grid = focus_grid(project, focus)
    cells = grid.cells
    level, warning = grid_advice(grid, LARGE_FOCUS_CELLS, FOCUS_RES_STEP_M, MAX_FOCUS_RES_M, "focus")
    summary = {
        "area_km2": boundary_area_km2(Path(focus.boundary_file), project.epsg),
        "width": grid.width,
        "height": grid.height,
        "cells": cells,
        "size_mb": cells * 4 / 1e6,
        "bounds": grid.bounds,
        "level": level,
        "warning": warning,
    }
    return summary


def preview_summary(project: Project, focus: FocusArea, geometry: dict) -> dict:
    """``grid_summary`` for an outline that has not been saved yet."""
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "outline.geojson"
        write_boundary(geometry, path, {})
        return grid_summary(project, replace(focus, boundary_file=str(path)))


def describe(summary: dict, res: float) -> str:
    return (
        f"{summary['area_km2']:,.2f} km2 outline; {res:g} m grid {summary['width']} x {summary['height']} "
        f"({summary['cells'] / 1e6:,.1f} million cells, about {summary['size_mb']:,.0f} MB)"
    )


def define_focus(project: Project, focus: FocusArea, geometry: dict, citation: str = "") -> FocusArea:
    """Save the accepted outline, add ``focus`` to the project (replacing one with the same name) and save it."""
    path = focus_outputs(project, focus)["boundary"]
    props = {
        "name": focus.name,
        "mode": focus.mode,
        "source": focus.source,
        "feature": focus.feature_name,
        "delineation": focus.delineation,
        "citation": citation,
    }
    write_boundary(geometry, path, props)
    focus.boundary_file = str(path)
    focus.status = "defined"
    project.set_focus(focus)
    project.save()
    update_focus_section(project)
    return focus


def _compare_with_regional(project: Project, focus: FocusArea, grid: Grid, dem: Path, is_canceled=None) -> dict | None:
    """Focus DEM averaged to regional cells minus the regional ground surface (a vertical-datum check).

    Water surfaces are compared where both exist, so independent lake-bottom estimates do not mask a datum offset.
    """
    regional = project.surfaces_dir / "ground_top_water_surface.tif"
    if not regional.exists() or not dem.exists():
        regional = project.surfaces_dir / "ground_top.tif"
    if not regional.exists():
        return None
    coarse = Grid(grid.epsg, grid.xmin, grid.ymin, grid.xmax, grid.ymax, project.options.coarse_resolution_m)
    work = project.downloads_dir / "focus_check"
    fine_c = read_array(warp_to_grid([str(dem)], coarse, work / f"{focus.slug}_fine.tif", resample="average", is_canceled=is_canceled))
    reg_c = read_array(warp_to_grid([str(regional)], coarse, work / f"{focus.slug}_regional.tif", resample="near", is_canceled=is_canceled))
    valid = np.isfinite(fine_c) & np.isfinite(reg_c)
    if not valid.any():
        return None
    diff = fine_c[valid] - reg_c[valid]
    return {"mean": float(diff.mean()), "std": float(diff.std()), "cells": int(valid.sum())}


def build_focus(
    project: Project,
    focus: FocusArea,
    api_keys: dict | None = None,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
    is_canceled: Callable[[], bool] | None = None,
    refresh: bool = False,
    allow_large_grid: bool = False,
) -> PipelineResult:
    """Fill the focus grid from local fine DEMs and the enabled fine providers; write outputs and report.

    Raises ``GridTooLargeError`` (before any download) above ``MAX_GRID_CELLS`` unless ``allow_large_grid``.
    """
    grid = focus_grid(project, focus)
    log(f"Focus area '{focus.name}': grid {grid.width} x {grid.height} at {grid.res:g} m ({grid.cells / 1e6:,.1f} million cells)")
    size_warning = check_grid(grid, LARGE_FOCUS_CELLS, FOCUS_RES_STEP_M, MAX_FOCUS_RES_M, "focus", allow_large_grid)
    prepare_network()
    fetcher = Fetcher(project, api_keys, log, progress, is_canceled, refresh)
    result = fetcher.result
    out = focus_outputs(project, focus)
    project.downloads_dir.mkdir(parents=True, exist_ok=True)
    if size_warning:
        fetcher.warn(size_warning)
    result.boundary_files = [Path(focus.boundary_file)]

    fine_local, _ = fetcher.ground_dems()
    local = [LocalFiles("fine", "ground_dem", fine_local, project.lat)] if fine_local else []
    fetcher.progress(5)
    dem = fetcher.accumulate("fine", fetcher.chain("fine", local), grid)
    fetcher.progress(80)

    if dem is None:
        fetcher.warn("No high-resolution DEM covers this focus area.")
        result.suggestions.append(
            f"Put a lidar DEM (<= {project.options.fine_threshold_m:g} m pixels) in the data folder, mark it "
            "'ground_dem' and build the focus area again."
        )
    else:
        dem = fill_small_gaps(dem, grid, max(1, int(FINE_GAP_FILL_M / grid.res)), fetcher.is_canceled)
        regional_lakes = (project.surfaces_dir / "lake_depth.tif", project.surfaces_dir / "lakes.geojson")
        dem, lake_outputs = fetcher.lakes(dem, grid, out, "ground_top_focus", regional_lakes)
        result.outputs["ground_top_focus"] = write_array(out["ground_top"], dem, grid, f"ground surface, {focus.name} (m)")
        result.outputs.update(lake_outputs)
        result.hillshades["ground_top_focus"] = hillshade(out["ground_top"], out["hillshade"], fetcher.is_canceled)
        mask = polygon_mask(Path(focus.boundary_file), grid)
        if mask.any():
            covered = float(100.0 * np.isfinite(dem[mask]).mean())
            result.stats["inside_outline_coverage_percent"] = {"value": covered}
            if covered < 99.0:
                fetcher.warn(f"The high-resolution DEM covers only {covered:.1f}% of the focus outline.")
        compare = out["water_surface"] if "ground_top_focus_water_surface" in result.outputs else out["ground_top"]
        diff = _compare_with_regional(project, focus, grid, compare, fetcher.is_canceled)
        if diff:
            result.stats["focus_minus_regional"] = diff
            log(f"Focus minus regional DEM: mean {diff['mean']:+.2f} m, std {diff['std']:.2f} m")
    output_stats(result)
    fetcher.progress(90)

    result.ok = dem is not None
    focus.status = "built" if result.ok else "defined"
    out["sources"].write_text(
        json.dumps(
            {
                "focus_area": asdict(focus),
                "grid": grid.signature(),
                "outputs": {k: str(v) for k, v in result.outputs.items()},
                "hillshades": {k: str(v) for k, v in result.hillshades.items()},
                "lakes_file": str(result.lakes_file) if result.lakes_file else None,
                "sources": [asdict(r) for r in result.records],
                "stats": result.stats,
                "warnings": result.warnings,
                "suggestions": result.suggestions,
            },
            indent=2,
        ),
        encoding="utf-8",
    )
    result.sources_path = out["sources"]
    result.report_path = write_focus_report(project, focus, result, grid, out["report"])
    project.set_focus(focus)
    project.save()
    update_focus_section(project)
    fetcher.progress(100)
    log("Done." if result.ok else "Finished without a high-resolution DEM; see the report.")
    return result
