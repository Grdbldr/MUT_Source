"""USGS 3DEP 1 m DEM tiles found through The National Map (TNM) Access API."""

from __future__ import annotations

from ..geo import GeoBBox
from ..net import session
from .base import FetchContext, Provider, SourceRecord

TNM_API = "https://tnmaccess.nationalmap.gov/api/v1/products"
DATASET = "Digital Elevation Model (DEM) 1 meter"
MAX_TILES = 60


class Usgs3DEP1m(Provider):
    id = "usgs_3dep_1m"
    name = "USGS 3DEP 1 m DEM"
    kind = "fine"
    resolution_m = 1.0
    vertical_accuracy = "about 0.1 m RMSE (lidar quality level 2 or better)"
    vertical_datum = "NAVD88 (GEOID18 in recent projects)"
    licence = "Public domain (USGS)"
    citation = "U.S. Geological Survey, 3D Elevation Program 1-Meter Resolution Digital Elevation Model. https://www.usgs.gov/3d-elevation-program"
    coverage = (
        GeoBBox(-125.0, 24.0, -66.0, 50.0),
        GeoBBox(-180.0, 51.0, -129.0, 72.0),
        GeoBBox(-161.0, 18.5, -154.5, 22.5),
        GeoBBox(-68.0, 17.5, -64.5, 18.6),
    )

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        params = {
            "datasets": DATASET,
            "bbox": f"{bbox.west},{bbox.south},{bbox.east},{bbox.north}",
            "prodFormats": "GeoTIFF",
            "max": MAX_TILES,
            "outputFormat": "JSON",
        }
        resp = session().get(TNM_API, params=params, timeout=120)
        resp.raise_for_status()
        items = [i for i in resp.json().get("items", []) if i.get("downloadURL")]
        if not items:
            return None
        # Oldest first so the most recent lidar project ends up on top of the mosaic.
        items.sort(key=lambda i: i.get("publicationDate") or "")
        urls = [i["downloadURL"] for i in items]
        path = self.warp_cached(ctx, [f"/vsicurl/{u}" for u in urls])
        projects = sorted({i.get("title", "").split(" x")[0] for i in items if i.get("title")})
        return self.record(path, urls, f"{len(urls)} tile(s); " + "; ".join(projects[:5]))
