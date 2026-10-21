"""Build the surficial-layer top (ground) and bottom (bedrock) surfaces for a project."""

from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np

from .geo import LARGE_GRID_CELLS, MAX_REGIONAL_RES_M, Grid, check_grid
from .net import prepare_network
from .project import Project
from .providers import PROVIDERS, FetchContext, Provider, SourceRecord, default_provider_ids
from .providers.local import LocalFiles, raster_pixel_m
from .bathymetry import apply_bathymetry, lake_stats, write_lakes
from .boundary import boundary_area_km2, contains_point
from .climate import retrieve_climate
from .dams import reservoir_dams
from .flows import retrieve_flows
from .raster import fill_small_gaps, hillshade, polygon_mask, read_array, write_array
from .report import write_report

DEPTH_GAP_FILL_PX = 100


@dataclass
class PipelineResult:
    ok: bool = False
    outputs: dict[str, Path] = field(default_factory=dict)
    hillshades: dict[str, Path] = field(default_factory=dict)
    records: list[SourceRecord] = field(default_factory=list)
    suggestions: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    stats: dict[str, dict] = field(default_factory=dict)
    boundary_files: list[Path] = field(default_factory=list)
    lakes_file: Path | None = None
    report_path: Path | None = None
    sources_path: Path | None = None


class Fetcher:
    """Fills grids from local files and enabled download providers, recording each source used."""

    def __init__(self, project, api_keys=None, log=print, progress=None, is_canceled=None, refresh=False) -> None:
        self.project: Project = project
        self.api_keys = api_keys or {}
        self.log = log
        self.progress = progress or (lambda pct: None)
        self.is_canceled = is_canceled or (lambda: False)
        self.refresh = refresh
        self.result = PipelineResult()
        enabled = project.providers or default_provider_ids(project, self.api_keys)
        self.enabled = [p for p in PROVIDERS if p.id in enabled]

    def warn(self, msg: str) -> None:
        self.result.warnings.append(msg)
        self.log(f"WARNING: {msg}")

    def context(self, grid: Grid) -> FetchContext:
        return FetchContext(
            grid=grid,
            out_dir=self.project.downloads_dir,
            api_keys=self.api_keys,
            log=self.log,
            is_canceled=self.is_canceled,
            refresh=self.refresh,
        )

    def chain(self, kind: str, local: list[Provider]) -> list[Provider]:
        return local + [p for p in self.enabled if p.kind == kind]

    def accumulate(self, kind: str, providers: list[Provider], grid: Grid, convert=None) -> np.ndarray | None:
        """Fill ``grid`` from each provider in turn; later providers only fill cells still empty."""
        ctx = self.context(grid)
        bbox = grid.geo_bbox()
        merged: np.ndarray | None = None
        for provider in providers:
            if self.is_canceled():
                raise InterruptedError("Run canceled")
            if not provider.covers(bbox):
                continue
            if provider.missing_key(self.api_keys):
                self.log(f"  {provider.name}: skipped (needs a {provider.requires_key} API key)")
                continue
            self.log(f"{kind}: trying {provider.name}")
            try:
                record = provider.fetch(bbox, ctx)
            except InterruptedError:
                raise
            except Exception as exc:  # noqa: BLE001 (one failed service must not stop the run)
                self.warn(f"{provider.name} failed: {exc}")
                continue
            if record is None:
                self.log(f"  {provider.name}: no data for this area")
                continue
            arr = read_array(Path(record.path))
            if convert is not None:
                arr = convert(record, arr)
            if not np.isfinite(arr).any():
                self.log(f"  {provider.name}: returned only nodata here")
                continue
            if merged is None:
                merged = arr
            else:
                gap = ~np.isfinite(merged)
                merged[gap] = arr[gap]
            filled = 100.0 * np.isfinite(merged).mean()
            record.notes = (record.notes + "; " if record.notes else "") + f"model area filled to {filled:.1f}%"
            self.result.records.append(record)
            self.log(f"  {provider.name}: ok, area filled to {filled:.1f}%")
            if filled >= 99.99:
                break
        self.result.suggestions.extend(ctx.suggestions)
        return merged

    def lakes(
        self,
        dem: np.ndarray,
        grid: Grid,
        paths: dict[str, Path],
        prefix: str,
        regional: tuple[Path, Path] | None = None,
    ) -> tuple[np.ndarray, dict[str, Path]]:
        """Lower lake water surfaces to lake bottoms (project option ``lake_bathymetry``).

        ``paths`` gives ``water_surface``, ``lake_depth`` and ``lakes`` files; ``regional`` (lake depth, lakes file)
        supplies lakes that extend beyond ``grid``. Returns the corrected DEM and the extra outputs (keys prefixed with
        ``prefix``) to add after the ground surface.
        """
        for key in ("water_surface", "lake_depth", "lakes"):
            paths[key].unlink(missing_ok=True)
        if not self.project.options.lake_bathymetry:
            self.log("Lakes: bathymetry correction is off; lakes keep the DEM water surface")
            return dem, {}
        try:
            bathy = apply_bathymetry(self.project, dem, grid, self.log, self.refresh, self.is_canceled, regional)
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001 (lakes then keep their water surface)
            self.warn(f"Lake bathymetry failed, lakes keep the DEM water surface: {exc}")
            self.result.stats["bathymetry"] = {"error": str(exc)}
            return dem, {}
        if bathy is None or not bathy.applied:
            notes = sorted({l.note for l in bathy.lakes if l.note}) if bathy else []
            self.log("Lakes: no lake corrected" + (f" ({'; '.join(notes)})" if notes else ""))
            if bathy:
                self.result.stats["bathymetry"] = lake_stats(bathy)
            return dem, {}
        outputs = {
            f"{prefix}_water_surface": write_array(paths["water_surface"], dem, grid, "ground surface with lake water surfaces (m)"),
            "lake_depth": write_array(paths["lake_depth"], bathy.depth, grid, "lake depth below the water surface (m)"),
        }
        self.result.lakes_file = write_lakes(bathy.lakes, paths["lakes"])
        self.result.records.extend(bathy.records)
        self.result.stats["bathymetry"] = lake_stats(bathy)
        counts = bathy.counts()
        detail = ", ".join(f"{n} {m}" for m, n in sorted(counts.items()))
        self.log(f"Lakes: BATHYMETRY APPLIED to {len(bathy.applied)} lake(s) ({detail}); see the report")
        if counts.get("estimated"):
            self.log("Lakes: estimated bottoms are not surveys; replace them with measured bathymetry when available")
        self.dams(grid, bathy.applied)
        return bathy.surface, outputs

    def dams(self, grid: Grid, lakes: list) -> None:
        """Dams on the corrected lakes, with measured and target reservoir levels, for the report."""
        try:
            found = reservoir_dams(self.project, grid, lakes, self.log, self.refresh)
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001 (the report then says the lookup failed)
            self.warn(f"Dam and reservoir level lookup failed: {exc}")
            self.result.stats["dams"] = {"error": str(exc)}
            return
        for text in found.warnings:
            self.warn(text)
        self.result.stats["dams"] = found.stats()
        self.result.records.extend(found.records)

    def climate(self, grid: Grid) -> None:
        """Domain-mean precip, temperature and PET for the regional area (Daymet → Open-Meteo)."""
        if not self.project.options.climate_retrieval:
            self.log("Climate: retrieval is off (climate_retrieval: false)")
            return
        try:
            found = retrieve_climate(self.project, grid.geo_bbox(), self.log, self.refresh, self.is_canceled)
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001
            self.warn(f"Climate retrieval failed: {exc}")
            self.result.stats["climate"] = {"error": str(exc)}
            return
        for text in found.warnings:
            self.warn(text)
        self.result.stats["climate"] = found.stats()
        self.result.records.extend(found.records)

    def flows(self) -> None:
        """Discharge gauges near level-1 outlets (ECCC → USGS)."""
        if not self.project.options.climate_retrieval:
            return
        try:
            found = retrieve_flows(self.project, self.log, self.refresh, self.is_canceled)
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001
            self.warn(f"River-flow retrieval failed: {exc}")
            self.result.stats["flows"] = {"error": str(exc)}
            return
        for text in found.warnings:
            self.warn(text)
        self.result.stats["flows"] = found.stats()
        self.result.records.extend(found.records)

    def ground_dems(self) -> tuple[list[Path], list[Path]]:
        """Local ground DEMs split into (fine, coarse) by pixel size."""
        project = self.project
        ground = project.files_with_role("ground_dem")
        fine = [p for p in ground if raster_pixel_m(p, project.lat) <= project.options.fine_threshold_m]
        return fine, [p for p in ground if p not in fine]


class _Runner(Fetcher):
    def run(self, allow_large_grid: bool = False) -> PipelineResult:
        project = self.project
        regional = project.regional_grid()
        self.log(f"Project '{project.name}', CRS EPSG:{project.epsg}")
        self.log(f"Regional grid {regional.width} x {regional.height} at {regional.res:g} m ({regional.cells / 1e6:,.1f} million cells)")
        size_warning = check_grid(regional, LARGE_GRID_CELLS, 1.0, MAX_REGIONAL_RES_M, "regional", allow_large_grid)
        prepare_network()
        surfaces = project.surfaces_dir
        surfaces.mkdir(parents=True, exist_ok=True)
        project.downloads_dir.mkdir(parents=True, exist_ok=True)
        if size_warning:
            self.warn(size_warning)
        self.result.boundary_files = project.files_with_role("boundary")
        if project.has_boundary:
            boundary = Path(project.boundary_file)
            self.result.boundary_files.insert(0, boundary)
            area = boundary_area_km2(boundary, project.epsg)
            self.result.stats["boundary"] = {"area_km2": area}
            self.log(f"Model boundary: {project.boundary_name or boundary.name} ({area:,.1f} km2, {project.boundary_mode})")
            if not contains_point(boundary, project.lat, project.lon):
                self.warn("The project location lies outside the model boundary.")

        fine_local, coarse_local = self.ground_dems()

        self.progress(5)
        # Warp order: fine local DEMs last, so where both exist the fine DEM (averaged to the regional cell) wins.
        ground = coarse_local + fine_local
        local = [LocalFiles("coarse", "ground_dem", ground, project.lat)] if ground else []
        top = self.accumulate("coarse", self.chain("coarse", local), regional)
        self.progress(50)

        if top is None:
            self.warn("No ground-surface data found for this location; nothing written.")
            return self.finish(regional)

        top = fill_small_gaps(top, regional, 3, self.is_canceled)
        top, lake_outputs = self.lakes(
            top,
            regional,
            {
                "water_surface": surfaces / "ground_top_water_surface.tif",
                "lake_depth": surfaces / "lake_depth.tif",
                "lakes": surfaces / "lakes.geojson",
            },
            "ground_top",
        )
        self.result.outputs["ground_top"] = write_array(surfaces / "ground_top.tif", top, regional, "ground surface (m)")
        self.result.outputs.update(lake_outputs)
        self.progress(70)

        bottom = self.bedrock(top, regional)
        self.progress(90)
        if bottom is not None:
            self.result.outputs["bedrock_bottom"] = write_array(surfaces / "bedrock_bottom.tif", bottom, regional, "bedrock surface (m)")
            thick = top - bottom
            self.result.outputs["surficial_thickness"] = write_array(
                surfaces / "surficial_thickness.tif", thick, regional, "surficial layer thickness (m)"
            )
        else:
            self.warn("No bedrock surface found; bedrock_bottom.tif and surficial_thickness.tif were not written.")

        self.progress(92)
        self.climate(regional)
        self.progress(96)
        self.flows()

        self.result.ok = bottom is not None
        return self.finish(regional)

    def bedrock(self, top: np.ndarray, regional: Grid) -> np.ndarray | None:
        project = self.project
        local: list[Provider] = []
        for role in ("bedrock_elevation", "bedrock_depth_m", "bedrock_depth_cm"):
            paths = project.files_with_role(role)
            if paths:
                local.append(LocalFiles("bedrock", role, paths, project.lat))

        def to_elevation(record: SourceRecord, arr: np.ndarray) -> np.ndarray:
            if record.quantity == "elevation":
                return arr
            depth = arr / 100.0 if record.quantity == "depth_cm" else arr
            depth = fill_small_gaps(depth, regional, DEPTH_GAP_FILL_PX, self.is_canceled)
            return top - depth

        bottom = self.accumulate("bedrock", self.chain("bedrock", local), regional, convert=to_elevation)
        if bottom is None:
            return None
        bottom = fill_small_gaps(bottom, regional, DEPTH_GAP_FILL_PX, self.is_canceled)
        limit = top - project.options.min_thickness_m
        clipped = np.isfinite(bottom) & np.isfinite(limit) & (bottom > limit)
        if clipped.any():
            pct = 100.0 * clipped.sum() / np.isfinite(top).sum()
            self.log(f"Bedrock lowered to {project.options.min_thickness_m:g} m below ground in {clipped.sum()} cells ({pct:.1f}%)")
            self.result.stats["bedrock_clipped_cells"] = {"count": int(clipped.sum()), "percent": float(pct)}
        bottom = np.where(clipped, limit, bottom)
        bottom[~np.isfinite(top)] = np.nan
        return bottom

    def finish(self, regional: Grid) -> PipelineResult:
        project = self.project
        if "ground_top" in self.result.outputs:
            src = self.result.outputs["ground_top"]
            self.result.hillshades["ground_top"] = hillshade(src, src.with_name(src.stem + "_hillshade.tif"), self.is_canceled)
        output_stats(self.result)
        if project.has_boundary:
            mask = polygon_mask(Path(project.boundary_file), regional)
            inside: dict[str, dict] = {}
            for key in ("ground_top", "bedrock_bottom", "surficial_thickness"):
                if key in self.result.outputs:
                    vals = read_array(self.result.outputs[key])[mask]
                    vals = vals[np.isfinite(vals)]
                    if vals.size:
                        inside[key] = {"min": float(vals.min()), "max": float(vals.max()), "mean": float(vals.mean())}
            self.result.stats["inside_boundary"] = inside
        sources = {
            "project": project.to_dict(),
            "regional_grid": regional.signature(),
            "outputs": {k: str(v) for k, v in self.result.outputs.items()},
            "hillshades": {k: str(v) for k, v in self.result.hillshades.items()},
            "lakes_file": str(self.result.lakes_file) if self.result.lakes_file else None,
            "sources": [asdict(r) for r in self.result.records],
            "stats": self.result.stats,
            "warnings": self.result.warnings,
            "suggestions": self.result.suggestions,
        }
        self.result.sources_path = project.surfaces_dir / "sources.json"
        self.result.sources_path.parent.mkdir(parents=True, exist_ok=True)
        self.result.sources_path.write_text(json.dumps(sources, indent=2), encoding="utf-8")
        self.result.report_path = write_report(project, self.result, regional)
        self.progress(100)
        self.log("Done." if self.result.ok else "Finished with missing outputs; see the report.")
        return self.result


def load_result(project: Project) -> PipelineResult:
    """Regional outputs of an earlier run, read back from ``sources.json``."""
    result = PipelineResult()
    path = project.surfaces_dir / "sources.json"
    if not path.exists():
        raise FileNotFoundError(f"No regional run found ({path} is missing); run the regional model first.")
    data = json.loads(path.read_text(encoding="utf-8"))

    def local(entries: dict) -> dict[str, Path]:
        # Resolved by name so a moved or copied project folder still works.
        found = {k: project.surfaces_dir / Path(v).name for k, v in entries.items() if k != "ground_top_site"}
        return {k: p for k, p in found.items() if p.exists()}

    result.outputs = local(data.get("outputs", {}))
    result.hillshades = local(data.get("hillshades", {}))
    result.stats = data.get("stats", {})
    if data.get("lakes_file"):
        lakes = project.surfaces_dir / Path(data["lakes_file"]).name
        result.lakes_file = lakes if lakes.exists() else None
    result.boundary_files = project.files_with_role("boundary")
    if project.has_boundary:
        result.boundary_files.insert(0, Path(project.boundary_file))
    result.sources_path = path
    result.ok = "ground_top" in result.outputs
    return result


def output_stats(result: PipelineResult) -> None:
    for key, path in result.outputs.items():
        arr = read_array(path)
        finite = arr[np.isfinite(arr)]
        if finite.size:
            result.stats[key] = {
                "min": float(finite.min()),
                "max": float(finite.max()),
                "mean": float(finite.mean()),
                "coverage_percent": float(100.0 * finite.size / arr.size),
            }


def run_pipeline(
    project: Project,
    api_keys: dict | None = None,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
    is_canceled: Callable[[], bool] | None = None,
    refresh: bool = False,
    allow_large_grid: bool = False,
) -> PipelineResult:
    """Fetch or reuse data and write the surface GeoTIFFs, sources.json and the Markdown report.

    Raises ``GridTooLargeError`` (before any download) when the regional grid exceeds ``MAX_GRID_CELLS`` unless
    ``allow_large_grid``; ``InterruptedError`` when ``is_canceled()`` becomes true.
    """
    return _Runner(project, api_keys, log, progress, is_canceled, refresh).run(allow_large_grid)
