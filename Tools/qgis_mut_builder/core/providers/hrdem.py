"""NRCan High Resolution DEM (HRDEM, lidar 1-2 m) found through the CCMEO datacube STAC API."""

from __future__ import annotations

from ..geo import GeoBBox
from ..net import session
from .base import FetchContext, Provider, SourceRecord

STAC_SEARCH = "https://datacube.services.geo.ca/stac/api/search"
COLLECTION = "hrdem-lidar"
MAX_ITEMS = 40


class NrcanHRDEM(Provider):
    id = "nrcan_hrdem"
    name = "NRCan HRDEM lidar DTM (1-2 m)"
    kind = "fine"
    resolution_m = 1.0
    vertical_accuracy = "about 0.1-0.3 m (lidar)"
    vertical_datum = "CGVD2013"
    licence = "Open Government Licence - Canada"
    citation = "Natural Resources Canada, High Resolution Digital Elevation Model (HRDEM) - CanElevation Series. https://open.canada.ca/data/en/dataset/957782bf-847c-4644-a757-e383c0057995"
    coverage = (GeoBBox(-141.0, 41.6, -52.0, 84.0),)

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        body = {"collections": [COLLECTION], "bbox": bbox.as_list(), "limit": MAX_ITEMS}
        resp = session().post(STAC_SEARCH, json=body, timeout=120)
        resp.raise_for_status()
        features = resp.json().get("features", [])
        items = [f for f in features if "dtm" in f.get("assets", {})]
        if not items:
            return None
        # Oldest first so the most recent acquisition ends up on top of the mosaic.
        items.sort(key=lambda f: f.get("properties", {}).get("datetime") or f.get("properties", {}).get("end_datetime") or "")
        urls = [f["assets"]["dtm"]["href"] for f in items]
        path = self.warp_cached(ctx, [f"/vsicurl/{u}" for u in urls])
        return self.record(path, urls, f"{len(urls)} project(s): " + ", ".join(f["id"] for f in items[:5]))
