"""SWF observation points at the deepest bathymetry cell of each corrected lake."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr, osr

from .bathymetry import _label_lakes
from .geo import Grid
from .raster import read_array

INSTRUCTIONS_NAME = "SWF_LakeLevelObservationPoints.instructions"
# Matches GeneralRoutines.f90 MAX_OBS
MAX_OBS = 1000


@dataclass(frozen=True)
class LakeObsPoint:
    name: str
    x: float
    y: float
    z: float
    max_depth_m: float = 0.0
    method: str = ""


def uniquify_names(names: list[str]) -> list[str]:
    """First occurrence keeps the name; later duplicates become ``Name (2)``, ``Name (3)``, …"""
    seen: dict[str, int] = {}
    out: list[str] = []
    for raw in names:
        base = raw if raw else "(unnamed)"
        n = seen.get(base, 0) + 1
        seen[base] = n
        out.append(base if n == 1 else f"{base} ({n})")
    return out


def deepest_cell(
    depth: np.ndarray,
    mask: np.ndarray,
    grid: Grid,
    water_level_m: float,
) -> tuple[float, float, float, float] | None:
    """Return ``(x, y, z_bottom, depth_m)`` at the deepest finite cell under ``mask``, or None."""
    d = np.where(mask, depth, np.nan)
    if not np.isfinite(d).any():
        return None
    r, c = np.unravel_index(int(np.nanargmax(d)), d.shape)
    depth_m = float(d[r, c])
    if not np.isfinite(depth_m) or depth_m <= 0:
        return None
    x = grid.xmin + (c + 0.5) * grid.res
    y = grid.ymax - (r + 0.5) * grid.res
    z = float(water_level_m) - depth_m
    return x, y, z, depth_m


def instructions_text(points: list[LakeObsPoint]) -> str:
    """MUT include body: SWF ``observation point`` entries (inline names support spaces)."""
    lines = [
        "! SWF lake-level observation points (deepest bathymetry cell)",
        f"! {len(points)} lake(s)",
        "active domain",
        "swf",
    ]
    for p in points:
        lines += [
            "    observation point",
            f"    {p.name}",
            f"    {p.x:.6f} {p.y:.6f} {p.z:.6f}",
        ]
    lines.append("")
    return "\n".join(lines)


def write_swf_lake_level_obs_instructions(path: Path, points: list[LakeObsPoint]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(instructions_text(points), encoding="utf-8")
    return path


def grid_from_geotiff(path: Path, epsg: int | None = None) -> Grid:
    """Build a ``Grid`` from a GeoTIFF geotransform; ``epsg`` overrides the file CRS when set."""
    ds = gdal.Open(str(path))
    if ds is None:
        raise FileNotFoundError(f"Cannot open {path}")
    gt = ds.GetGeoTransform()
    width, height = ds.RasterXSize, ds.RasterYSize
    xmin, res, _, ymax, _, yres = gt
    if abs(res + yres) > 1e-9 * max(abs(res), 1.0) and abs(res) != abs(yres):
        # Allow north-up rasters with yres = -res
        pass
    cell = abs(res)
    ymin = ymax + yres * height
    code = epsg
    if code is None:
        srs = ds.GetSpatialRef()
        if srs is not None:
            srs.AutoIdentifyEPSG()
            auth = srs.GetAuthorityCode(None)
            if auth:
                code = int(auth)
    if not code:
        raise ValueError(f"No EPSG code for {path}; pass epsg= explicitly")
    return Grid(int(code), float(xmin), float(ymin), float(xmin + cell * width), float(ymax), float(cell))


def _load_applied_lakes(path: Path, epsg: int) -> list[tuple[dict, ogr.Geometry]]:
    """Lakes with determined bathymetry from ``lakes.geojson``, geometries in ``epsg``."""
    data = json.loads(path.read_text(encoding="utf-8"))
    src = osr.SpatialReference()
    src.ImportFromEPSG(4326)
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    to_grid = osr.CoordinateTransformation(src, dst)
    out: list[tuple[dict, ogr.Geometry]] = []
    for feat in data.get("features") or []:
        props = feat.get("properties") or {}
        if props.get("method") == "skipped":
            continue
        try:
            max_d = float(props.get("max_depth_m") or 0.0)
        except (TypeError, ValueError):
            continue
        if not np.isfinite(max_d) or max_d <= 0:
            continue
        wl = props.get("water_level_m")
        if wl is None:
            continue
        try:
            float(wl)
        except (TypeError, ValueError):
            continue
        geom_json = feat.get("geometry")
        if not geom_json:
            continue
        geom = ogr.CreateGeometryFromJson(json.dumps(geom_json))
        if geom is None or geom.IsEmpty():
            continue
        geom = geom.Clone()
        geom.Transform(to_grid)
        if not geom.IsValid():
            geom = geom.MakeValid()
        out.append((props, geom))
    return out


def find_lake_level_obs_points(
    lakes_path: Path,
    depth_path: Path,
    *,
    epsg: int | None = None,
) -> tuple[list[LakeObsPoint], list[str]]:
    """Deepest-cell SWF observation points for bathymetry-corrected lakes.

    Returns ``(points, warnings)``. Empty points when inputs are missing or no lake qualifies.
    """
    warnings: list[str] = []
    if not lakes_path.is_file():
        warnings.append(f"Lake observation points skipped: missing {lakes_path.name}")
        return [], warnings
    if not depth_path.is_file():
        warnings.append(f"Lake observation points skipped: missing {depth_path.name}")
        return [], warnings

    grid = grid_from_geotiff(depth_path, epsg=epsg)
    lakes = _load_applied_lakes(lakes_path, grid.epsg)
    if not lakes:
        warnings.append("Lake observation points: no lakes with determined bathymetry")
        return [], warnings

    depth = read_array(depth_path)
    if depth.shape != (grid.height, grid.width):
        warnings.append(
            f"Lake observation points skipped: depth raster shape {depth.shape} "
            f"!= grid ({grid.height}, {grid.width})"
        )
        return [], warnings

    labels = _label_lakes(lakes, grid)
    raw_names = [str(props.get("name") or "(unnamed)") for props, _ in lakes]
    names = uniquify_names(raw_names)
    points: list[LakeObsPoint] = []
    for i, ((props, _), name) in enumerate(zip(lakes, names), start=1):
        mask = labels == i
        if not mask.any():
            continue
        wl = float(props["water_level_m"])
        hit = deepest_cell(depth, mask, grid, wl)
        if hit is None:
            continue
        x, y, z, dmax = hit
        points.append(
            LakeObsPoint(
                name=name,
                x=x,
                y=y,
                z=z,
                max_depth_m=dmax,
                method=str(props.get("method") or ""),
            )
        )

    if len(points) > MAX_OBS:
        warnings.append(
            f"Lake observation points capped at {MAX_OBS} (had {len(points)}; MUT MAX_OBS limit)"
        )
        points = points[:MAX_OBS]
    if not points:
        warnings.append("Lake observation points: no deepest cell found for any corrected lake")
    return points, warnings
