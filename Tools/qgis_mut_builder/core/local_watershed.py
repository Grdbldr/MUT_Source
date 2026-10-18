"""Find a named feature inside the regional model and delineate the local watershed draining through it.

Delineation from the regional ground surface uses GRASS ``r.watershed`` (D8 flow accumulation and drainage
direction, cached per DEM) and ``r.water.outlet``, run through ``qgis_process`` so it works both inside QGIS and
headless. The outlet of a polygon or line feature is the cell with the largest flow accumulation inside it, i.e.
where water leaves the feature. Global Watersheds (MERIT-Hydro) is the online alternative.
"""

from __future__ import annotations

import json
import os
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import gdal, ogr, osr

from . import boundary as bnd
from .geo import GeoBBox, Grid, lonlat_to_projected, transformer
from .net import session
from .raster import cache_ok, polygon_mask, read_array, write_cache_meta

POINT_SNAP_M = 300.0
QGIS_PROCESS_NAMES = ("qgis_process-qgis-ltr.bat", "qgis_process-qgis.bat")
DEFAULT_QGIS_BIN = r"C:\Program Files\QGIS 3.44.15\bin"


@dataclass
class FeatureCandidate:
    label: str
    category: str
    kind: str
    lat: float
    lon: float
    geometry: dict
    ref: dict = field(default_factory=dict)

    @property
    def geometry_type(self) -> str:
        return self.geometry.get("type", "Point")

    @property
    def display(self) -> str:
        shape = {"Polygon": "area", "MultiPolygon": "area", "LineString": "line", "MultiLineString": "line"}.get(
            self.geometry_type, "point"
        )
        return f"{self.label} [{self.category}={self.kind}, {shape}]"


GENERIC_WORDS = {
    "marsh", "swamp", "bog", "fen", "wetland", "wetlands", "lake", "pond", "reservoir", "river", "creek", "brook",
    "stream", "park", "area", "conservation", "reserve", "wildlife", "management", "forest", "the", "of",
}  # fmt: skip
SKIP_CATEGORIES = {"highway", "railway", "building", "shop", "amenity", "office"}


def _rank(f: FeatureCandidate) -> int:
    return {"Polygon": 0, "MultiPolygon": 0, "LineString": 1, "MultiLineString": 1}.get(f.geometry_type, 2)


def search_features(name: str, region: GeoBBox, limit: int = 15) -> list[FeatureCandidate]:
    """OpenStreetMap features named like ``name`` inside the regional extent, areas first.

    Nominatim needs every word to match, so "Luther Marsh" misses "Luther Marsh Wildlife Management Area";
    when the full name finds no area the distinctive words alone are searched as well.
    """
    found = _nominatim(name, region, limit)
    core = [w for w in name.split() if w.lower() not in GENERIC_WORDS]
    if core and len(core) < len(name.split()) and not any(_rank(f) == 0 for f in found):
        seen = {(f.ref.get("osm_type"), f.ref.get("osm_id")) for f in found}
        for f in _nominatim(" ".join(core), region, limit):
            if (f.ref.get("osm_type"), f.ref.get("osm_id")) not in seen:
                found.append(f)
    words = [w.lower() for w in name.split()]
    return sorted(found, key=lambda f: (_rank(f), not all(w in f.label.lower() for w in words)))


def _nominatim(name: str, region: GeoBBox, limit: int) -> list[FeatureCandidate]:
    params = {
        "q": name,
        "format": "jsonv2",
        "limit": limit,
        "accept-language": "en",
        "viewbox": f"{region.west},{region.north},{region.east},{region.south}",
        "bounded": 1,
        "polygon_geojson": 1,
        "polygon_threshold": 0.00005,
    }
    resp = session().get(f"{bnd.NOMINATIM}/search", params=params, timeout=60)
    resp.raise_for_status()
    found = []
    for item in resp.json():
        if item.get("category") in SKIP_CATEGORIES:
            continue
        geom = item.get("geojson") or {"type": "Point", "coordinates": [float(item["lon"]), float(item["lat"])]}
        found.append(
            FeatureCandidate(
                label=item.get("display_name", name),
                category=item.get("category", "?"),
                kind=item.get("type", "?"),
                lat=float(item["lat"]),
                lon=float(item["lon"]),
                geometry=geom,
                ref={"osm_type": item.get("osm_type"), "osm_id": item.get("osm_id"), "name": item.get("name") or name},
            )
        )
    return found


def qgis_process() -> Path | None:
    roots = [os.environ.get("QGIS_BIN", "")]
    if os.environ.get("OSGEO4W_ROOT"):
        roots.append(str(Path(os.environ["OSGEO4W_ROOT"]) / "bin"))
    roots.append(DEFAULT_QGIS_BIN)
    for root in filter(None, roots):
        for name in QGIS_PROCESS_NAMES:
            path = Path(root) / name
            if path.exists():
                return path
    return None


def grass_available() -> bool:
    exe = qgis_process()
    return exe is not None and (exe.parent.parent / "apps" / "grass").is_dir()


def _run(algorithm: str, params: dict, log: Callable[[str], None]) -> None:
    exe = qgis_process()
    if exe is None:
        raise RuntimeError("qgis_process was not found; set QGIS_BIN to the QGIS bin folder")
    args = [str(exe), "run", algorithm, "--"] + [f"{k}={v}" for k, v in params.items()]
    flags = getattr(subprocess, "CREATE_NO_WINDOW", 0)
    proc = subprocess.run(args, capture_output=True, text=True, timeout=3600, creationflags=flags)
    if proc.returncode != 0:
        tail = "\n".join((proc.stderr or proc.stdout or "").strip().splitlines()[-8:])
        raise RuntimeError(f"{algorithm} failed (exit {proc.returncode}): {tail}")
    log(f"  {algorithm}: ok")


def _grid_of(path: Path) -> Grid:
    ds = gdal.Open(str(path))
    gt = ds.GetGeoTransform()
    srs = osr.SpatialReference(wkt=ds.GetProjection())
    epsg = int(srs.GetAuthorityCode(None) or 0)
    return Grid(epsg, gt[0], gt[3] + gt[5] * ds.RasterYSize, gt[0] + gt[1] * ds.RasterXSize, gt[3], gt[1])


def hydro_rasters(project, log: Callable[[str], None] = print) -> tuple[Path, Path]:
    """(accumulation, drainage) rasters from the regional ground surface; cached until the DEM changes."""
    dem = project.surfaces_dir / "ground_top.tif"
    if not dem.exists():
        raise RuntimeError("Run the regional model first: 1_data/surfaces/ground_top.tif is missing.")
    folder = project.focus_root / "_hydro"
    folder.mkdir(parents=True, exist_ok=True)
    acc, drain = folder / "accumulation.tif", folder / "drainage.tif"
    signature = {"dem": str(dem), "mtime": dem.stat().st_mtime, "size": dem.stat().st_size, "flow": "D8"}
    if cache_ok(acc, signature, False) and drain.exists():
        return acc, drain
    log("Computing flow accumulation and drainage directions on the regional ground surface (GRASS r.watershed) ...")
    _run(
        "grass:r.watershed",
        {"elevation": dem, "-s": 1, "-a": 1, "memory": 2000, "accumulation": acc, "drainage": drain},
        log,
    )
    write_cache_meta(acc, signature)
    return acc, drain


def _geometry_file(geometry: dict, folder: Path, name: str) -> Path:
    return bnd.write_boundary(geometry, folder / f"{name}.geojson", {"name": name})


def feature_outlet(project, geometry: dict, log: Callable[[str], None] = print) -> tuple[float, float, float, float]:
    """(x, y, lat, lon) of the largest-accumulation cell in a polygon/line feature, or near a point feature."""
    acc_path, _drain = hydro_rasters(project, log)
    grid = _grid_of(acc_path)
    acc = np.abs(read_array(acc_path))
    if geometry.get("type") == "Point":
        lon, lat = geometry["coordinates"][:2]
        x, y = lonlat_to_projected(grid.epsg, lon, lat)
        r = max(1, int(POINT_SNAP_M / grid.res))
        col, row = int((x - grid.xmin) / grid.res), int((grid.ymax - y) / grid.res)
        mask = np.zeros(acc.shape, dtype=bool)
        mask[max(row - r, 0) : row + r + 1, max(col - r, 0) : col + r + 1] = True
    else:
        path = _geometry_file(geometry, project.focus_root / "_hydro", "feature")
        mask = polygon_mask(path, grid, all_touched=False)
        if not mask.any():
            mask = polygon_mask(path, grid, all_touched=True)
    values = np.where(mask & np.isfinite(acc), acc, -1.0)
    if values.max() < 0:
        raise RuntimeError("The feature lies outside the regional ground surface.")
    row, col = np.unravel_index(int(np.argmax(values)), values.shape)
    x = grid.xmin + (col + 0.5) * grid.res
    y = grid.ymax - (row + 0.5) * grid.res
    lon, lat, _ = transformer(grid.epsg, 4326).TransformPoint(x, y)
    log(f"Outlet: {lat:.5f}, {lon:.5f} (upstream area {values[row, col] * grid.res**2 / 1e6:,.2f} km2)")
    return x, y, lat, lon


def delineate_regional(project, x: float, y: float, log: Callable[[str], None] = print) -> dict:
    """Watershed draining to (x, y) (project CRS) on the regional ground surface, as EPSG:4326 GeoJSON."""
    _acc, drain = hydro_rasters(project, log)
    basin = project.focus_root / "_hydro" / "basin.tif"
    basin.unlink(missing_ok=True)
    _run("grass:r.water.outlet", {"input": drain, "coordinates": f"{x},{y}", "output": basin}, log)
    ds = gdal.Open(str(basin))
    band = ds.GetRasterBand(1)
    data = band.ReadAsArray()
    nodata = band.GetNoDataValue()
    inside = data > 0
    if nodata is not None:
        inside &= data != nodata
    if not inside.any():
        raise RuntimeError("r.water.outlet produced an empty watershed")
    if inside[0, :].any() or inside[-1, :].any() or inside[:, 0].any() or inside[:, -1].any():
        log("WARNING: the watershed reaches the edge of the regional grid and may extend beyond the regional model.")
    mem = gdal.GetDriverByName("MEM").Create("", ds.RasterXSize, ds.RasterYSize, 1, gdal.GDT_Byte)
    mem.SetGeoTransform(ds.GetGeoTransform())
    mem.SetProjection(ds.GetProjection())
    mem.GetRasterBand(1).WriteArray(inside.astype("uint8"))
    srs = osr.SpatialReference(wkt=ds.GetProjection())
    vds = ogr.GetDriverByName("MEM").CreateDataSource("basin")
    layer = vds.CreateLayer("basin", srs=srs, geom_type=ogr.wkbPolygon)
    layer.CreateField(ogr.FieldDefn("v", ogr.OFTInteger))
    gdal.Polygonize(mem.GetRasterBand(1), mem.GetRasterBand(1), layer, 0)
    best = max((f.GetGeometryRef().Clone() for f in layer), key=lambda g: g.GetArea())
    wgs = osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    wgs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    best.AssignSpatialReference(srs)
    log(f"Local watershed: {best.GetArea() / 1e6:,.2f} km2")
    best.TransformTo(wgs)
    return json.loads(best.ExportToJson())


def delineate_global(lat: float, lon: float) -> dict:
    return bnd.delineate(lat, lon).geometry


def feature_area_km2(geometry: dict, epsg: int) -> float:
    geom = ogr.CreateGeometryFromJson(json.dumps(geometry))
    if geom is None or ogr.GT_Flatten(geom.GetGeometryType()) not in (ogr.wkbPolygon, ogr.wkbMultiPolygon):
        return 0.0
    wgs = osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    wgs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    geom.AssignSpatialReference(wgs)
    geom.TransformTo(dst)
    return geom.GetArea() / 1e6


@dataclass
class FocusOutline:
    geometry: dict
    source: str
    citation: str
    outlet_lat: float | None = None
    outlet_lon: float | None = None


POINT_FEATURE_HALF_M = 500.0


def outline_from_feature(
    project, feature: FeatureCandidate, delineation: str, log: Callable[[str], None] = print
) -> FocusOutline:
    """Focus outline for a found feature: the feature itself, or its local watershed (plus the feature)."""
    osm = bnd.CITATIONS["osm"]
    if delineation == "none":
        geometry = feature.geometry if is_areal(feature.geometry) else point_box(feature.lat, feature.lon, POINT_FEATURE_HALF_M, project.epsg)
        return FocusOutline(geometry, "OpenStreetMap feature", osm)
    if delineation == "regional_dem" or grass_available():
        x, y, lat, lon = feature_outlet(project, feature.geometry, log)
    else:
        x = y = None
        lat, lon = feature.lat, feature.lon
        log("GRASS is not available; using the feature's centre point as the outlet.")
    if delineation == "regional_dem":
        watershed = delineate_regional(project, x, y, log)
        citation = f"{osm} Local watershed delineated from the regional ground surface with GRASS r.watershed / r.water.outlet."
        source = "Regional ground surface (GRASS)"
    else:
        log(f"Delineating with Global Watersheds at {lat:.5f}, {lon:.5f} ...")
        watershed = delineate_global(lat, lon)
        citation = f"{bnd.CITATIONS['mghydro']} {osm}"
        source = "Global Watersheds"
    geometry = with_feature(watershed, feature.geometry)
    log(f"Focus outline: {feature_area_km2(geometry, project.epsg):,.2f} km2 (watershed plus the feature)")
    return FocusOutline(geometry, source, citation, lat, lon)


def with_feature(watershed: dict, feature: dict) -> dict:
    """Union of a watershed and an areal feature, so a feature straddling a divide stays fully inside."""
    if not is_areal(feature):
        return watershed
    union = ogr.CreateGeometryFromJson(json.dumps(watershed)).Union(ogr.CreateGeometryFromJson(json.dumps(feature)))
    return json.loads(union.ExportToJson())


def is_areal(geometry: dict) -> bool:
    return geometry.get("type") in ("Polygon", "MultiPolygon")


def point_box(lat: float, lon: float, half_m: float, epsg: int) -> dict:
    """Square polygon (EPSG:4326) of half-width ``half_m`` around a point; used for point features."""
    x, y = lonlat_to_projected(epsg, lon, lat)
    return bnd.projected_rectangle_geometry(epsg, (x - half_m, y - half_m, x + half_m, y + half_m))
