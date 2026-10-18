"""Copernicus GLO-30 DEM read directly from the public AWS Open Data bucket (no key)."""

from __future__ import annotations

import math

from ..geo import GeoBBox
from ..net import session
from .base import FetchContext, Provider, SourceRecord

BUCKET = "https://copernicus-dem-30m.s3.amazonaws.com"


def tile_name(lat: int, lon: int) -> str:
    ns = "N" if lat >= 0 else "S"
    ew = "E" if lon >= 0 else "W"
    return f"Copernicus_DSM_COG_10_{ns}{abs(lat):02d}_00_{ew}{abs(lon):03d}_00_DEM"


def tile_url(lat: int, lon: int) -> str:
    name = tile_name(lat, lon)
    return f"{BUCKET}/{name}/{name}.tif"


class CopernicusGLO30(Provider):
    id = "copernicus_glo30"
    name = "Copernicus GLO-30 DEM"
    kind = "coarse"
    resolution_m = 30.0
    vertical_accuracy = (
        "< 4 m absolute (LE90, product specification); surface model, so heights include "
        "buildings and forest canopy (typically 1-20 m above bare ground)"
    )
    vertical_datum = "EGM2008 geoid"
    licence = "Copernicus DEM licence (free, attribution required)"
    # Bilinear onto a 30 m UTM grid leaves moire striping in hillshades (source is 1 arc-second).
    resample = "average"
    citation = (
        "Copernicus DEM GLO-30, European Space Agency (2021), produced using Copernicus WorldDEM-30 "
        "(c) DLR e.V. 2010-2014 and (c) Airbus Defence and Space GmbH 2014-2018. "
        "https://doi.org/10.5270/ESA-c5d3d65"
    )

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        urls: list[str] = []
        missing = 0
        for lat in range(math.floor(bbox.south), math.floor(bbox.north) + 1):
            for lon in range(math.floor(bbox.west), math.floor(bbox.east) + 1):
                ctx.check_canceled()
                url = tile_url(lat, lon)
                status = session().head(url, timeout=60).status_code
                if status == 200:
                    urls.append(url)
                elif status in (403, 404):
                    missing += 1
                else:
                    raise RuntimeError(f"{self.name}: HTTP {status} for {url}")
        if not urls:
            ctx.log(f"  {self.name}: no tiles cover the area (open ocean?)")
            return None
        notes = f"{len(urls)} tile(s)"
        if missing:
            notes += f"; {missing} tile(s) absent (ocean, left as nodata)"
        path = self.warp_cached(ctx, [f"/vsicurl/{u}" for u in urls])
        return self.record(path, urls, notes)
