"""Scan an existing-data folder and suggest a role for each raster or vector file."""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from osgeo import gdal, ogr, osr

from .geo import GeoBBox, pixel_size_m

RASTER_EXT = {".tif", ".tiff", ".asc", ".img", ".vrt", ".flt", ".bil", ".adf", ".nc", ".grd", ".xyz", ".gpkg"}
VECTOR_EXT = {".shp", ".gpkg", ".geojson", ".json", ".kml", ".kmz", ".gml", ".tab"}
SKIP_DIRS = {"downloads", "__pycache__", ".git"}
# Builder outputs; scanning them would feed an earlier run back in as "existing data".
OUTPUT_DIRS = (("1_data", "surfaces"), ("1_data", "boundary"), ("1_data", "focus"))

_GROUND = re.compile(r"(dem|dtm|elev|lidar|srtm|topo|ground|terrain|nasadem|hrdem|ahn|copernicus|glo.?30)")
_SURFACE_MODEL = re.compile(r"(^|[^a-z])dsm([^a-z]|$)")
_BEDROCK = re.compile(r"(bedrock|bdrk|basement|rockhead|top.?of.?rock|dtb|bdticm|depth.?to.?rock|overburden|regolith|sediment.?thick)")
_DEPTH = re.compile(r"(depth|dtb|thick|bdticm|overburden|isopach|regolith)")
_CM = re.compile(r"(^|[^a-z])cm([^a-z]|$)|bdticm")
_BATHYMETRY = re.compile(r"(bathy|lake.?bottom|lake.?bed|sounding)")
_BOUNDARY = re.compile(r"(bound|domain|extent|outline|aoi|model.?area|study.?area|catchment|watershed)")


@dataclass
class InventoryItem:
    path: Path
    kind: str
    crs: str = ""
    pixel_m: float | None = None
    vmin: float | None = None
    vmax: float | None = None
    bbox: GeoBBox | None = None
    suggested_role: str = "ignore"
    note: str = ""

    @property
    def summary(self) -> str:
        parts = [self.kind]
        if self.crs:
            parts.append(self.crs)
        if self.pixel_m is not None:
            parts.append(f"{self.pixel_m:.3g} m pixels")
        if self.vmin is not None and self.vmax is not None:
            parts.append(f"values {self.vmin:.4g} to {self.vmax:.4g}")
        if self.note:
            parts.append(self.note)
        return ", ".join(parts)


def _crs_label(srs: osr.SpatialReference | None) -> str:
    if srs is None:
        return ""
    srs.AutoIdentifyEPSG()
    code = srs.GetAuthorityCode(None)
    name = srs.GetAuthorityName(None)
    return f"{name}:{code}" if code and name else (srs.GetName() or "")


def _geo_bbox(srs: osr.SpatialReference | None, xmin: float, ymin: float, xmax: float, ymax: float) -> GeoBBox | None:
    if srs is None:
        return None
    wgs = osr.SpatialReference()
    wgs.ImportFromEPSG(4326)
    wgs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    src = srs.Clone()
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    try:
        ct = osr.CoordinateTransformation(src, wgs)
        lons, lats = [], []
        for x in (xmin, (xmin + xmax) / 2, xmax):
            for y in (ymin, (ymin + ymax) / 2, ymax):
                lon, lat, _ = ct.TransformPoint(x, y)
                lons.append(lon)
                lats.append(lat)
        return GeoBBox(min(lons), min(lats), max(lons), max(lats))
    except (RuntimeError, TypeError):
        return None


def _describe_raster(path: Path, centre_lat: float | None) -> InventoryItem | None:
    try:
        ds = gdal.Open(str(path))
    except RuntimeError:
        return None
    if ds is None or ds.RasterCount == 0:
        return None
    gt = ds.GetGeoTransform()
    srs = ds.GetSpatialRef()
    item = InventoryItem(path=path, kind="raster", crs=_crs_label(srs))
    item.pixel_m = pixel_size_m(gt, srs, centre_lat)
    xmin, ymax = gt[0], gt[3]
    xmax = xmin + gt[1] * ds.RasterXSize
    ymin = ymax + gt[5] * ds.RasterYSize
    item.bbox = _geo_bbox(srs, xmin, ymin, xmax, ymax)
    try:
        vmin, vmax = ds.GetRasterBand(1).ComputeRasterMinMax(True)
        item.vmin, item.vmax = float(vmin), float(vmax)
    except RuntimeError:
        pass
    if srs is None:
        item.note = "no CRS (assumed to be the project CRS)"
    return item


def _describe_vector(path: Path) -> InventoryItem | None:
    try:
        ds = gdal.OpenEx(str(path), gdal.OF_VECTOR)
    except RuntimeError:
        return None
    if ds is None or ds.GetLayerCount() == 0:
        return None
    layer = ds.GetLayer(0)
    srs = layer.GetSpatialRef()
    item = InventoryItem(path=path, kind="vector", crs=_crs_label(srs))
    try:
        xmin, xmax, ymin, ymax = layer.GetExtent()
        item.bbox = _geo_bbox(srs, xmin, ymin, xmax, ymax)
    except RuntimeError:
        pass
    geom = ogr.GeometryTypeToName(layer.GetGeomType())
    item.note = f"{layer.GetFeatureCount()} {geom.lower()} features"
    return item


def suggest_role(item: InventoryItem) -> str:
    name = item.path.stem.lower()
    if item.kind == "vector":
        if _BOUNDARY.search(name) and "polygon" in item.note:
            return "boundary"
        return "ignore"
    if _BEDROCK.search(name):
        if _DEPTH.search(name):
            if _CM.search(name) or (item.vmax is not None and item.vmax > 2000):
                return "bedrock_depth_cm"
            return "bedrock_depth_m"
        return "bedrock_elevation"
    if _BATHYMETRY.search(name):
        return "bathymetry_depth_m" if _DEPTH.search(name) else "bathymetry_elevation"
    if _SURFACE_MODEL.search(name):
        return "ignore"
    if _GROUND.search(name):
        return "ground_dem"
    return "ignore"


def _under_output(dirs: list[str]) -> bool:
    return any(
        tuple(dirs[i : i + len(out)]) == out for out in OUTPUT_DIRS for i in range(len(dirs) - len(out) + 1)
    )


def scan_folder(folder: Path, centre_lat: float | None = None, region: GeoBBox | None = None) -> list[InventoryItem]:
    """Describe every raster and vector file under ``folder`` and suggest a role.

    Files whose footprint misses ``region`` are listed but suggested as ``ignore``.
    """
    folder = Path(folder)
    if not folder.is_dir():
        return []
    gdal.UseExceptions()
    items: list[InventoryItem] = []
    for path in sorted(folder.rglob("*")):
        dirs = [part.lower() for part in path.relative_to(folder).parts[:-1]]
        if not path.is_file() or any(d in SKIP_DIRS for d in dirs) or _under_output(dirs):
            continue
        ext = path.suffix.lower()
        if path.name.lower().endswith(".aux.xml") or ext in {".ovr", ".part"}:
            continue
        item = None
        if ext in RASTER_EXT:
            item = _describe_raster(path, centre_lat)
        if item is None and ext in VECTOR_EXT:
            item = _describe_vector(path)
        if item is None:
            continue
        item.suggested_role = suggest_role(item)
        if region is not None and item.bbox is not None and not item.bbox.intersects(region):
            item.suggested_role = "ignore"
            item.note = (item.note + "; " if item.note else "") + "outside the regional area"
        items.append(item)
    return items
