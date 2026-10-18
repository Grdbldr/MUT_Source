"""Global absolute depth to bedrock (Shangguan et al., 2017), read from the ISRIC file server."""

from __future__ import annotations

from ..geo import GeoBBox
from .base import FetchContext, Provider, SourceRecord

BDTICM_URL = "https://files.isric.org/soilgrids/former/2017-03-10/data/BDTICM_M_250m_ll.tif"


class ShangguanDepthToBedrock(Provider):
    id = "shangguan2017_dtb"
    name = "Global depth to bedrock, 250 m (Shangguan et al., 2017)"
    kind = "bedrock"
    quantity = "depth_cm"
    resolution_m = 250.0
    vertical_accuracy = (
        "Low: cross-validated RMSE of several to tens of metres; deep sedimentary basins are "
        "underestimated. Replace with borehole-derived surfaces when available."
    )
    vertical_datum = "depth below ground surface"
    licence = "CC BY 4.0 (ISRIC - World Soil Information)"
    citation = (
        "Shangguan, W., Hengl, T., Mendes de Jesus, J., Yuan, H., Dai, Y. (2017). Mapping the global depth "
        "to bedrock for land surface modeling. Journal of Advances in Modeling Earth Systems 9, 65-88. "
        "https://doi.org/10.1002/2016MS000686"
    )
    coverage = (GeoBBox(-180.0, -56.0, 180.0, 84.0),)

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        ctx.log(f"  {self.name}: reading a window of the global file (row-striped, may take a few minutes) ...")
        path = self.warp_cached(ctx, [f"/vsicurl/{BDTICM_URL}"], src_nodata=-32768)
        return self.record(path, [BDTICM_URL], "absolute depth to bedrock in cm (BDTICM)")
