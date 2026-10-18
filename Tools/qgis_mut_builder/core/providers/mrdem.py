"""NRCan Medium Resolution DEM (MRDEM-30) bare-earth DTM, one national cloud-optimized GeoTIFF (no key)."""

from __future__ import annotations

from ..geo import GeoBBox
from .base import FetchContext, Provider, SourceRecord

DTM_URL = "https://canelevation-dem.s3.ca-central-1.amazonaws.com/mrdem-30/mrdem-30-dtm.tif"


class NrcanMRDEM30(Provider):
    id = "nrcan_mrdem30"
    name = "NRCan MRDEM-30 DTM"
    kind = "coarse"
    resolution_m = 30.0
    vertical_accuracy = "bare earth; built from HRDEM lidar where available, otherwise from regional DEMs (a few metres)"
    vertical_datum = "CGVD2013"
    licence = "Open Government Licence - Canada"
    citation = (
        "Natural Resources Canada, Medium Resolution Digital Elevation Model (MRDEM-30) - CanElevation Series. "
        "https://open.canada.ca/data/en/dataset/18752265-bda3-498c-a4ba-9dfe68cb98da"
    )
    coverage = (GeoBBox(-141.0, 41.6, -52.0, 84.0),)

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        path = self.warp_cached(ctx, [f"/vsicurl/{DTM_URL}"])
        return self.record(path, [DTM_URL], "bare-earth DTM; outside Canada left as nodata")
