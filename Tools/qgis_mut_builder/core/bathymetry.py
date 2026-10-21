"""Lake bathymetry: lower the flat water surface of lakes in a DEM to lake-bottom elevations.

Lidar DTMs (and DEMs built from them, such as NRCan HRDEM and MRDEM) show lakes as a flat water surface. For each
OpenStreetMap lake, pond or reservoir in the grid the water level is read from the DEM, and the bottom is the water
level minus the depth. Depth comes, cell by cell, from local bathymetry rasters, then measured surveys (Ontario MNRF
depth contours and points), then an estimate from the lake shape and the slope of the surrounding land. Every lake is
reported with the method used so estimated bottoms are never mistaken for surveys.
"""

from __future__ import annotations

import hashlib
import json
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import gdal, ogr, osr
from scipy import interpolate, ndimage

from . import boundary as bnd
from .geo import GeoBBox, Grid
from .net import session
from .providers.base import SourceRecord
from .raster import _wkt, read_array, warp_to_grid

OVERPASS_URLS = (
    bnd.OVERPASS,
    "https://overpass.private.coffee/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
)
MNRF = "https://ws.lioservices.lrc.gov.on.ca/arcgis2/rest/services/LIO_OPEN_DATA/LIO_Open01/MapServer"
MNRF_LINES, MNRF_POINTS = 30, 27
MNRF_COVERAGE = GeoBBox(-95.2, 41.6, -74.3, 56.9)
SKIP_WATER = {"river", "stream", "canal", "ditch", "drain", "wastewater", "lock", "rapids", "fountain", "pool", "fish_pass"}
MIN_MEASURED_SAMPLES = 10
MIN_LAKE_CELLS = 4
FLAT_TOLERANCE_M = 0.05
EST_MIN_DEPTH_M = 0.5
EST_MAX_DEPTH_M = 60.0
EST_MAX_RING_M = 2000.0

CITATIONS = {
    "mnrf": (
        "Ontario Ministry of Natural Resources, Bathymetry Line and Bathymetry Point (Land Information Ontario), "
        "Open Government Licence - Ontario. https://geohub.lio.gov.on.ca/datasets/mnrf::bathymetry-line"
    ),
    "estimated": (
        "Depth from distance to shore (Hollister, J.W., Milstead, W.B., 2010, Lake and Reservoir Management 26, "
        "194-199) with the maximum depth predicted from the surrounding land slope (Hollister, J.W., Milstead, W.B., "
        "Urrutia, M.A., 2011, PLoS ONE 6(9), e25764), the method used for GLOBathy (Khazaei et al., 2022, "
        "Scientific Data 9, 36)."
    ),
}


@dataclass
class Lake:
    name: str
    osm_id: str
    water: str
    area_ha: float
    water_level_m: float | None = None
    flat_percent: float = 0.0
    method: str = "skipped"
    max_depth_m: float = 0.0
    mean_depth_m: float = 0.0
    samples: int = 0
    note: str = ""
    geometry: dict = field(default_factory=dict, repr=False)


@dataclass
class BathymetryResult:
    surface: np.ndarray
    depth: np.ndarray
    lakes: list[Lake]
    records: list[SourceRecord]
    warnings: list[str] = field(default_factory=list)

    @property
    def applied(self) -> list[Lake]:
        return [lake for lake in self.lakes if lake.method != "skipped"]

    def counts(self) -> dict[str, int]:
        out: dict[str, int] = {}
        for lake in self.applied:
            for method in lake.method.split("+"):
                out[method] = out.get(method, 0) + 1
        return out


# --- lake outlines -----------------------------------------------------------------------------------------------


def _ring(points: list[dict]) -> ogr.Geometry:
    ring = ogr.Geometry(ogr.wkbLinearRing)
    for p in points:
        ring.AddPoint_2D(p["lon"], p["lat"])
    ring.CloseRings()
    return ring


def _relation_polygon(element: dict) -> ogr.Geometry | None:
    lines = {"outer": ogr.Geometry(ogr.wkbMultiLineString), "inner": ogr.Geometry(ogr.wkbMultiLineString)}
    for member in element.get("members", []):
        if member.get("type") != "way" or not member.get("geometry"):
            continue
        line = ogr.Geometry(ogr.wkbLineString)
        for p in member["geometry"]:
            line.AddPoint_2D(p["lon"], p["lat"])
        lines["inner" if member.get("role") == "inner" else "outer"].AddGeometry(line)
    outer = lines["outer"].Polygonize() if lines["outer"].GetGeometryCount() else None
    if outer is None or outer.IsEmpty():
        return None
    geom = outer.UnaryUnion()
    if lines["inner"].GetGeometryCount():
        inner = lines["inner"].Polygonize()
        if inner is not None and not inner.IsEmpty():
            geom = geom.Difference(inner.UnaryUnion())
    return geom


def overpass_area(bbox: GeoBBox) -> str:
    return f"({bbox.south},{bbox.west},{bbox.north},{bbox.east})"


def _overpass(bbox: GeoBBox) -> dict:
    area = overpass_area(bbox)
    return overpass_json(
        "[out:json][timeout:240];("
        f'way["natural"="water"]{area};relation["natural"="water"]{area};'
        f'way["landuse"="reservoir"]{area};relation["landuse"="reservoir"]{area};'
        ");out geom;",
        "lake",
    )


def overpass_json(query: str, what: str, read_timeout: float = 240, attempts: int = 2) -> dict:
    """Run an Overpass query, trying each server (``attempts`` rounds when they fail quickly)."""
    errors: list[str] = []
    start = time.monotonic()
    for attempt in range(attempts):
        for url in OVERPASS_URLS:
            try:
                resp = session().post(url, data={"data": query}, timeout=(20, read_timeout))
                resp.raise_for_status()
                return resp.json()
            except Exception as exc:  # noqa: BLE001 (try the next Overpass server)
                errors.append(f"{url.split('/')[2]}: {exc}")
        if attempt < attempts - 1:
            if time.monotonic() - start > 120:
                break
            time.sleep(20)
    raise RuntimeError(f"OpenStreetMap {what} query failed (all servers busy, try again later): " + "; ".join(errors[-3:]))


def osm_lakes(bbox: GeoBBox, cache_dir: Path, refresh: bool, log: Callable[[str], None]) -> list[tuple[str, str, str, ogr.Geometry]]:
    """(osm id, name, water type, EPSG:4326 polygon) for lakes, ponds and reservoirs in ``bbox``."""
    key = hashlib.sha1(json.dumps([round(v, 4) for v in (bbox.west, bbox.south, bbox.east, bbox.north)]).encode()).hexdigest()[:12]
    cache = cache_dir / f"osm_lakes_{key}.json"
    if cache.exists() and not refresh:
        data = json.loads(cache.read_text(encoding="utf-8"))
    else:
        log("Lakes: querying OpenStreetMap water bodies ...")
        data = _overpass(bbox)
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(data), encoding="utf-8")
    lakes = []
    for element in data.get("elements", []):
        tags = element.get("tags", {})
        water = tags.get("water", tags.get("landuse", ""))
        if water in SKIP_WATER:
            continue
        if element["type"] == "way":
            pts = element.get("geometry") or []
            if len(pts) < 4 or (pts[0]["lat"], pts[0]["lon"]) != (pts[-1]["lat"], pts[-1]["lon"]):
                continue
            geom = ogr.Geometry(ogr.wkbPolygon)
            geom.AddGeometry(_ring(pts))
        else:
            try:
                geom = _relation_polygon(element)
            except RuntimeError:
                geom = None
        if geom is None or geom.IsEmpty():
            continue
        if not geom.IsValid():
            geom = geom.MakeValid()
        lakes.append((f"{element['type']}/{element['id']}", tags.get("name", ""), water or "water", geom))
    return lakes


# --- measured depths ---------------------------------------------------------------------------------------------


def _mnrf_query(layer: int, params: dict) -> dict:
    resp = session().post(f"{MNRF}/{layer}/query", data={"f": "json", **params}, timeout=180)
    resp.raise_for_status()
    data = resp.json()
    if "error" in data:
        raise RuntimeError(f"Ontario bathymetry service: {data['error'].get('message', data['error'])}")
    return data


def _mnrf_layer(layer: int, bbox: GeoBBox) -> list[dict]:
    """All features of ``layer`` in ``bbox``; the service fails on large responses, so ids are fetched in batches."""
    ids = _mnrf_query(layer, {
        "geometry": f"{bbox.west},{bbox.south},{bbox.east},{bbox.north}",
        "geometryType": "esriGeometryEnvelope",
        "inSR": 4326,
        "spatialRel": "esriSpatialRelIntersects",
        "returnIdsOnly": "true",
    }).get("objectIds") or []
    features: list[dict] = []
    pending = [sorted(ids)[i : i + 200] for i in range(0, len(ids), 200)]
    while pending:
        batch = pending.pop(0)
        try:
            data = _mnrf_query(layer, {
                "objectIds": ",".join(map(str, batch)),
                "outFields": "DEPTH",
                "outSR": 4326,
                "returnGeometry": "true",
            })
        except RuntimeError:
            if len(batch) == 1:
                continue
            half = len(batch) // 2
            pending[:0] = [batch[:half], batch[half:]]
            continue
        features += data.get("features", [])
    return features


def mnrf_samples(bbox: GeoBBox, epsg: int, step_m: float, cache_dir: Path, refresh: bool, log) -> np.ndarray:
    """Depth samples (x, y, depth below water in m, positive) from Ontario MNRF contours and points in ``bbox``."""
    if not MNRF_COVERAGE.intersects(bbox):
        return np.empty((0, 3))
    key = hashlib.sha1(json.dumps([round(v, 4) for v in (bbox.west, bbox.south, bbox.east, bbox.north)]).encode()).hexdigest()[:12]
    cache = cache_dir / f"mnrf_bathymetry_{key}.json"
    if cache.exists() and not refresh:
        data = json.loads(cache.read_text(encoding="utf-8"))
    else:
        log("Lakes: querying Ontario MNRF bathymetry ...")
        data = {"lines": _mnrf_layer(MNRF_LINES, bbox), "points": _mnrf_layer(MNRF_POINTS, bbox)}
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(data), encoding="utf-8")
    src = osr.SpatialReference()
    src.ImportFromEPSG(4326)
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    to_grid = osr.CoordinateTransformation(src, dst)
    rows: list[tuple[float, float, float]] = []
    for feat in data["lines"]:
        depth = feat["attributes"].get("DEPTH")
        if depth is None:
            continue
        for path in feat.get("geometry", {}).get("paths", []):
            line = ogr.Geometry(ogr.wkbLineString)
            for lon, lat in path:
                line.AddPoint_2D(lon, lat)
            line.Transform(to_grid)
            line.Segmentize(step_m)
            rows += [(x, y, abs(depth)) for x, y in line.GetPoints()]
    for feat in data["points"]:
        depth = feat["attributes"].get("DEPTH")
        g = feat.get("geometry") or {}
        if depth is None or "x" not in g:
            continue
        x, y, _ = to_grid.TransformPoint(g["x"], g["y"])
        rows.append((x, y, abs(depth)))
    return np.asarray(rows, dtype="float64").reshape(-1, 3)


# --- per-lake depth ----------------------------------------------------------------------------------------------


def _label_lakes(lakes: list, grid: Grid) -> np.ndarray:
    """Raster of 1-based lake indices (cell centres inside the polygon), larger lakes drawn last so they win."""
    mem = gdal.GetDriverByName("MEM").Create("", grid.width, grid.height, 1, gdal.GDT_Int32)
    mem.SetGeoTransform((grid.xmin, grid.res, 0.0, grid.ymax, 0.0, -grid.res))
    mem.SetProjection(_wkt(grid.epsg))
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(grid.epsg)
    vec = ogr.GetDriverByName("MEM").CreateDataSource("lakes")
    layer = vec.CreateLayer("lakes", srs, ogr.wkbMultiPolygon)
    layer.CreateField(ogr.FieldDefn("id", ogr.OFTInteger))
    order = sorted(range(len(lakes)), key=lambda i: lakes[i][1].GetArea())
    for i in order:
        feat = ogr.Feature(layer.GetLayerDefn())
        feat.SetField("id", i + 1)
        feat.SetGeometry(lakes[i][1])
        layer.CreateFeature(feat)
    gdal.RasterizeLayer(mem, [1], layer, options=["ATTRIBUTE=id"])
    return mem.GetRasterBand(1).ReadAsArray()


def _measured_depth(mask: np.ndarray, x0: float, y0: float, res: float, samples: np.ndarray) -> np.ndarray:
    """Depth over ``mask`` interpolated (linear, then nearest) from samples plus zero depth along the shoreline."""
    shore = mask & ~ndimage.binary_erosion(mask)
    rr, cc = np.nonzero(shore)
    shore_pts = np.column_stack([x0 + (cc + 0.5) * res, y0 - (rr + 0.5) * res, np.zeros(rr.size)])
    pts = np.vstack([samples, shore_pts])
    rows, cols = np.nonzero(mask)
    xi = np.column_stack([x0 + (cols + 0.5) * res, y0 - (rows + 0.5) * res])
    vals = interpolate.griddata(pts[:, :2], pts[:, 2], xi, method="linear")
    gap = ~np.isfinite(vals)
    if gap.any():
        vals[gap] = interpolate.griddata(pts[:, :2], pts[:, 2], xi[gap], method="nearest")
    out = np.full(mask.shape, np.nan)
    out[rows, cols] = vals
    return out


def _land_slope(dem: np.ndarray, labels: np.ndarray, idx: int, sl: tuple, grid: Grid, ring_m: float) -> float:
    """Median land slope within ``ring_m`` of lake ``idx``, other lakes excluded, sampled at about 10 m."""
    step = max(1, int(round(10.0 / grid.res)))
    pad = int(np.ceil(ring_m / grid.res)) + 2
    r0, r1 = max(sl[0].start - pad, 0), min(sl[0].stop + pad, grid.height)
    c0, c1 = max(sl[1].start - pad, 0), min(sl[1].stop + pad, grid.width)
    lab = labels[r0:r1:step, c0:c1:step]
    sub = dem[r0:r1:step, c0:c1:step]
    res = grid.res * step
    mask = lab == idx
    if not mask.any() or not np.isfinite(sub).any():
        return 0.0
    outside = ndimage.distance_transform_edt(~mask) * res
    ring = (lab == 0) & (outside <= ring_m) & np.isfinite(sub)
    gy, gx = np.gradient(np.where(np.isfinite(sub), sub, np.nanmedian(sub)), res)
    slopes = np.hypot(gx, gy)[ring]
    return float(np.median(slopes)) if slopes.size else 0.0


def _estimated_depth(mask: np.ndarray, res: float, slope: float) -> tuple[np.ndarray, float, float]:
    """Distance-to-shore bowl with Dmax = median land slope around the lake x the largest distance to shore."""
    dist = ndimage.distance_transform_edt(mask) * res
    far = float(dist.max())
    dmax = float(np.clip(slope * far, EST_MIN_DEPTH_M, EST_MAX_DEPTH_M))
    return np.where(mask, dmax * dist / far if far > 0 else 0.0, np.nan), dmax, far


def _local_depth(project, grid: Grid, work: Path, is_canceled=None) -> np.ndarray | None:
    """Depth below water from local ``bathymetry_depth_m`` rasters; ``bathymetry_elevation`` is handled by the caller."""
    paths = project.files_with_role("bathymetry_depth_m")
    if not paths:
        return None
    return np.abs(read_array(warp_to_grid([str(p) for p in paths], grid, work / f"bathymetry_depth_{grid.res:g}m.tif", is_canceled=is_canceled)))


def _local_elevation(project, grid: Grid, work: Path, is_canceled=None) -> np.ndarray | None:
    paths = project.files_with_role("bathymetry_elevation")
    if not paths:
        return None
    return read_array(warp_to_grid([str(p) for p in paths], grid, work / f"bathymetry_elevation_{grid.res:g}m.tif", is_canceled=is_canceled))


def apply_bathymetry(
    project,
    dem: np.ndarray,
    grid: Grid,
    log: Callable[[str], None] = print,
    refresh: bool = False,
    is_canceled: Callable[[], bool] = lambda: False,
    regional: tuple[Path, Path] | None = None,
) -> BathymetryResult | None:
    """Lower lake water surfaces in ``dem`` to lake-bottom elevations; None when no lake is large enough.

    ``regional`` (lake depth raster, lakes GeoJSON) supplies the depth of lakes that extend beyond ``grid``.
    """
    opts = project.options
    work = project.downloads_dir
    bbox = grid.geo_bbox()
    found = osm_lakes(bbox, work, refresh, log)
    if is_canceled():
        raise InterruptedError("Run canceled")
    src = osr.SpatialReference()
    src.ImportFromEPSG(4326)
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(grid.epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    to_grid = osr.CoordinateTransformation(src, dst)
    min_area = max(opts.lake_min_area_ha * 1e4, MIN_LAKE_CELLS * grid.res**2)
    lakes: list[tuple[Lake, ogr.Geometry]] = []
    for osm_id, name, water, geom in found:
        proj = geom.Clone()
        proj.Transform(to_grid)
        if proj.GetArea() < min_area:
            continue
        info = Lake(name or "(unnamed)", osm_id, water, proj.GetArea() / 1e4, geometry=json.loads(geom.ExportToJson()))
        lakes.append((info, proj))
    log(f"Lakes: {len(lakes)} of {len(found)} OpenStreetMap water bodies are at least {min_area / 1e4:g} ha")
    if not lakes:
        return None

    labels = _label_lakes(lakes, grid)
    samples = mnrf_samples(bbox, grid.epsg, max(grid.res, 5.0), work, refresh, log)
    local_depth = _local_depth(project, grid, work, is_canceled)
    local_elev = _local_elevation(project, grid, work, is_canceled)
    surface = dem.copy()
    depth_out = np.full(dem.shape, np.nan)
    records: list[SourceRecord] = []
    used = set()
    if samples.size:
        col = np.floor((samples[:, 0] - grid.xmin) / grid.res).astype(int)
        row = np.floor((grid.ymax - samples[:, 1]) / grid.res).astype(int)
        ok = (row >= 0) & (row < grid.height) & (col >= 0) & (col < grid.width)
        sample_lake = np.zeros(len(samples), dtype=int)
        sample_lake[ok] = labels[row[ok], col[ok]]
    else:
        sample_lake = np.zeros(0, dtype=int)

    reg_depth, reg_lakes = None, {}
    if regional is not None and all(Path(p).exists() for p in regional):
        features = json.loads(Path(regional[1]).read_text(encoding="utf-8")).get("features", [])
        reg_lakes = {
            f["properties"]["osm_id"]: f["properties"]
            for f in features
            if f["properties"].get("method", "skipped") != "skipped"
        }
        if reg_lakes:
            key = hashlib.sha1(repr((grid.bounds, grid.res)).encode()).hexdigest()[:12]
            reg_depth = read_array(warp_to_grid([str(regional[0])], grid, work / f"regional_lake_depth_{key}.tif", is_canceled=is_canceled))
    extent = ogr.CreateGeometryFromWkt(
        f"POLYGON(({grid.xmin} {grid.ymin},{grid.xmax} {grid.ymin},{grid.xmax} {grid.ymax},"
        f"{grid.xmin} {grid.ymax},{grid.xmin} {grid.ymin}))"
    )
    objects = ndimage.find_objects(labels)
    for idx, (info, _proj) in enumerate(lakes, start=1):
        if is_canceled():
            raise InterruptedError("Run canceled")
        sl = objects[idx - 1] if idx - 1 < len(objects) else None
        if sl is None:
            info.note = "smaller than one cell at this resolution"
            continue
        r0, r1 = max(sl[0].start - 2, 0), min(sl[0].stop + 2, grid.height)
        c0, c1 = max(sl[1].start - 2, 0), min(sl[1].stop + 2, grid.width)
        win = (slice(r0, r1), slice(c0, c1))
        mask = labels[win] == idx
        if mask.sum() < MIN_LAKE_CELLS:
            info.note = "fewer than 4 cells inside the outline"
            continue
        sub = dem[win]
        inner = mask & ndimage.binary_erosion(mask)
        vals = sub[inner if inner.sum() >= MIN_LAKE_CELLS else mask]
        vals = vals[np.isfinite(vals)]
        if not vals.size:
            info.note = "no DEM values inside the lake"
            continue
        level = float(np.median(vals))
        info.water_level_m = level
        info.flat_percent = float(100.0 * (np.abs(vals - level) <= FLAT_TOLERANCE_M).mean())

        depth = np.full(mask.shape, np.nan)
        methods: list[str] = []
        if local_depth is not None:
            d = np.where(mask, local_depth[win], np.nan)
            if np.isfinite(d).any():
                depth = d
                methods.append("local")
        if local_elev is not None:
            e = np.where(mask & ~np.isfinite(depth), level - local_elev[win], np.nan)
            if np.isfinite(e).any():
                depth = np.where(np.isfinite(depth), depth, np.clip(e, 0, None))
                if "local" not in methods:
                    methods.append("local")
        mine = samples[sample_lake == idx] if samples.size else samples
        if (~np.isfinite(depth) & mask).any() and len(mine) >= MIN_MEASURED_SAMPLES:
            x0, y0 = grid.xmin + c0 * grid.res, grid.ymax - r0 * grid.res
            meas = _measured_depth(mask, x0, y0, grid.res, mine[:, :])
            depth = np.where(np.isfinite(depth), depth, meas)
            methods.append("measured")
            info.samples = int(len(mine))
            used.add("mnrf")
        clipped = not _proj.Within(extent)
        reg = reg_lakes.get(info.osm_id) if clipped and reg_depth is not None else None
        if reg and (~np.isfinite(depth) & mask).any():
            d = np.where(mask & ~np.isfinite(depth), reg_depth[win], np.nan)
            if np.isfinite(d).any():
                depth = np.where(np.isfinite(depth), depth, d)
                methods += [m for m in reg["method"].split("+") if m not in methods]
                used.update({"measured": "mnrf"}.get(m, m) for m in reg["method"].split("+"))
                info.note = f"regional lake depth (the lake extends beyond this grid); {reg.get('note') or reg['method']}"
                depth = np.where(mask & ~np.isfinite(depth), 0.0, depth)
        if clipped and (~np.isfinite(depth) & mask).any():
            info.note = "extends beyond the grid: no estimate (the estimate needs the whole shoreline)"
        elif (~np.isfinite(depth) & mask).any():
            far = float(ndimage.distance_transform_edt(mask).max() * grid.res)
            ring_m = min(max(far, 3 * grid.res), EST_MAX_RING_M)
            slope = _land_slope(dem, labels, idx, sl, grid, ring_m)
            est, dmax, far = _estimated_depth(mask, grid.res, slope)
            depth = np.where(np.isfinite(depth), depth, est)
            methods.append("estimated")
            info.note = (
                f"estimated: max depth {dmax:.1f} m = land slope {100 * slope:.2f}% (within {ring_m:.0f} m) x "
                f"{far:.0f} m to shore"
            )
            used.add("estimated")
        if not methods:
            continue
        depth = np.where(mask, np.clip(depth, 0, None), np.nan)
        bottom = level - depth
        cur = surface[win]
        new = np.where(mask & np.isfinite(bottom), np.where(np.isfinite(cur), np.minimum(cur, bottom), bottom), cur)
        surface[win] = new
        depth_out[win] = np.where(mask, level - new, depth_out[win])
        d = depth[mask]
        info.method = "+".join(methods)
        info.max_depth_m = float(np.nanmax(d)) if np.isfinite(d).any() else 0.0
        info.mean_depth_m = float(np.nanmean(d)) if np.isfinite(d).any() else 0.0

    result_lakes = [info for info, _ in lakes]
    if "mnrf" in used:
        records.append(
            SourceRecord(
                role="bathymetry",
                provider="mnrf_bathymetry",
                title="Ontario MNRF lake bathymetry (measured)",
                path=str(work),
                quantity="depth below water surface",
                vertical_accuracy="survey dependent; many surveys are historic (1950s-1990s) fish-habitat transects",
                vertical_datum="depth below the water level at the time of the survey",
                licence="Open Government Licence - Ontario",
                citation=CITATIONS["mnrf"],
                urls=[f"{MNRF}/{MNRF_LINES}", f"{MNRF}/{MNRF_POINTS}"],
                notes=f"{sum(1 for l in result_lakes if 'measured' in l.method)} lake(s)",
            )
        )
    if "estimated" in used:
        records.append(
            SourceRecord(
                role="bathymetry",
                provider="estimated_bathymetry",
                title="Estimated lake bathymetry (not measured)",
                path="",
                quantity="depth below water surface",
                vertical_accuracy="not measured; estimated from lake shape and surrounding slope, depth errors of tens of percent or more",
                vertical_datum="depth below the DEM water surface",
                licence="derived",
                citation=CITATIONS["estimated"],
                notes=f"{sum(1 for l in result_lakes if 'estimated' in l.method)} lake(s)",
            )
        )
    records.append(
        SourceRecord(
            role="bathymetry",
            provider="osm_lakes",
            title="Lake outlines (OpenStreetMap)",
            path="",
            quantity="lake polygons",
            licence="ODbL",
            citation=bnd.CITATIONS["osm"],
            notes=f"{len(result_lakes)} water bodies of at least {min_area / 1e4:g} ha",
        )
    )
    return BathymetryResult(surface, depth_out, result_lakes, records)


def write_lakes(lakes: list[Lake], path: Path) -> Path:
    """GeoJSON (EPSG:4326) of the lakes with their water level, method and depths."""
    features = []
    for lake in lakes:
        props = {k: v for k, v in asdict(lake).items() if k != "geometry"}
        features.append({"type": "Feature", "properties": props, "geometry": lake.geometry})
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps({"type": "FeatureCollection", "name": path.stem, "features": features}, indent=2),
        encoding="utf-8",
    )
    return path


def lake_stats(result: BathymetryResult) -> dict:
    return {
        "lakes": [{k: v for k, v in asdict(l).items() if k != "geometry"} for l in result.lakes],
        "counts": result.counts(),
    }
