"""User-supplied rasters from the existing-data folder; these take priority over downloads."""

from __future__ import annotations

from pathlib import Path

import numpy as np
from osgeo import gdal

from ..geo import pixel_size_m
from ..raster import read_array
from .base import FetchContext, Provider, SourceRecord

QUANTITY = {
    "ground_dem": "elevation",
    "bedrock_elevation": "elevation",
    "bedrock_depth_m": "depth_m",
    "bedrock_depth_cm": "depth_cm",
}


def raster_pixel_m(path: Path, centre_lat: float) -> float:
    ds = gdal.Open(str(path))
    return pixel_size_m(ds.GetGeoTransform(), ds.GetSpatialRef(), centre_lat)


def _with_crs(path: Path, epsg: int, work_dir: Path) -> str:
    """Return a GDAL source name for ``path``; files without a CRS get the project CRS via a VRT."""
    ds = gdal.Open(str(path))
    if ds.GetSpatialRef() is not None:
        return str(path)
    work_dir.mkdir(parents=True, exist_ok=True)
    vrt = work_dir / f"{path.stem}_crs{epsg}.vrt"
    gdal.Translate(str(vrt), ds, options=gdal.TranslateOptions(format="VRT", outputSRS=f"EPSG:{epsg}"))
    return str(vrt)


class LocalFiles(Provider):
    resample = "average"

    def __init__(self, kind: str, role: str, paths: list[Path], centre_lat: float = 0.0) -> None:
        self.kind = kind
        self.role = role
        self.paths = paths
        self.centre_lat = centre_lat
        self.id = f"local_{role}"
        self.name = f"Local {role.replace('_', ' ')}"
        self.quantity = QUANTITY[role]
        self.vertical_accuracy = "as supplied by the user"
        self.vertical_datum = "as supplied by the user"
        self.licence = "project data"

    def fetch(self, bbox, ctx: FetchContext) -> SourceRecord | None:
        sources = [_with_crs(p, ctx.grid.epsg, ctx.out_dir / "local_crs") for p in self.paths]
        resample = "average" if self.kind == "coarse" else "bilinear"
        mtimes = {str(p): p.stat().st_mtime for p in self.paths}
        path = self.warp_cached(ctx, sources, resample=resample, extra=mtimes)
        if not np.isfinite(read_array(path)).any():
            ctx.log(f"  {self.name}: files do not overlap the model area")
            return None
        res = min(raster_pixel_m(p, self.centre_lat) for p in self.paths)
        record = self.record(path, [str(p) for p in self.paths], f"{len(self.paths)} local file(s)")
        record.resolution_m = round(res, 3)
        return record
