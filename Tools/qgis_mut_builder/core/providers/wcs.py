"""National lidar DTMs served through OGC WCS 2.0 (requested in tiles, then mosaicked)."""

from __future__ import annotations

import math
import re
from dataclasses import dataclass

from ..geo import GeoBBox, transformer
from ..net import download, session
from .base import FetchContext, Provider, SourceRecord

TILE_PX = 2000


@dataclass(frozen=True)
class WcsService:
    id: str
    name: str
    url: str
    coverage_id: str
    epsg: int
    axes: tuple[str, str]
    native_res_m: float
    coverage: GeoBBox
    vertical_accuracy: str
    vertical_datum: str
    licence: str
    citation: str
    coverage_pattern: bool = False


WCS_SERVICES: tuple[WcsService, ...] = (
    WcsService(
        id="nl_ahn_dtm",
        name="Netherlands AHN DTM 0.5 m (PDOK)",
        url="https://service.pdok.nl/rws/ahn/wcs/v1_0",
        coverage_id="dtm_05m",
        epsg=28992,
        axes=("x", "y"),
        native_res_m=0.5,
        coverage=GeoBBox(3.2, 50.7, 7.3, 53.6),
        vertical_accuracy="about 0.05-0.15 m",
        vertical_datum="NAP",
        licence="CC0 (Rijkswaterstaat / Het Waterschapshuis)",
        citation="Actueel Hoogtebestand Nederland (AHN), PDOK WCS. https://www.ahn.nl",
    ),
    WcsService(
        id="uk_ea_lidar_dtm",
        name="England EA lidar composite DTM 1 m",
        url="https://environment.data.gov.uk/spatialdata/lidar-composite-digital-terrain-model-dtm-1m/wcs",
        coverage_id=r"Lidar_Composite_Elevation_DTM_1m$",
        coverage_pattern=True,
        epsg=27700,
        axes=("E", "N"),
        native_res_m=1.0,
        coverage=GeoBBox(-6.5, 49.8, 2.0, 55.9),
        vertical_accuracy="about 0.15 m RMSE",
        vertical_datum="Ordnance Datum Newlyn",
        licence="Open Government Licence v3.0 (Environment Agency)",
        citation="Environment Agency, LIDAR Composite DTM 1 m. https://environment.data.gov.uk/survey",
    ),
)


class WcsLidarProvider(Provider):
    kind = "fine"

    def __init__(self, service: WcsService) -> None:
        self.service = service
        self.id = service.id
        self.name = service.name
        self.resolution_m = service.native_res_m
        self.vertical_accuracy = service.vertical_accuracy
        self.vertical_datum = service.vertical_datum
        self.licence = service.licence
        self.citation = service.citation
        self.coverage = (service.coverage,)

    def _coverage_id(self) -> str:
        svc = self.service
        if not svc.coverage_pattern:
            return svc.coverage_id
        resp = session().get(svc.url, params={"service": "WCS", "request": "GetCapabilities", "version": "2.0.1"}, timeout=120)
        resp.raise_for_status()
        ids = re.findall(r"<wcs:CoverageId>([^<]+)</wcs:CoverageId>", resp.text)
        for cid in ids:
            if re.search(svc.coverage_id, cid):
                return cid
        raise RuntimeError(f"{svc.name}: no coverage matching '{svc.coverage_id}' in {ids}")

    def _native_bounds(self, ctx: FetchContext) -> tuple[float, float, float, float]:
        grid = ctx.grid
        ct = transformer(grid.epsg, self.service.epsg)
        xs, ys = [], []
        for i in range(11):
            f = i / 10
            for x, y in (
                (grid.xmin + f * (grid.xmax - grid.xmin), grid.ymin),
                (grid.xmin + f * (grid.xmax - grid.xmin), grid.ymax),
                (grid.xmin, grid.ymin + f * (grid.ymax - grid.ymin)),
                (grid.xmax, grid.ymin + f * (grid.ymax - grid.ymin)),
            ):
                nx, ny, _ = ct.TransformPoint(x, y)
                xs.append(nx)
                ys.append(ny)
        pad = 4 * self.service.native_res_m
        return min(xs) - pad, min(ys) - pad, max(xs) + pad, max(ys) + pad

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        svc = self.service
        cid = self._coverage_id()
        xmin, ymin, xmax, ymax = self._native_bounds(ctx)
        step = TILE_PX * svc.native_res_m
        nx = max(1, math.ceil((xmax - xmin) / step))
        ny = max(1, math.ceil((ymax - ymin) / step))
        tile_dir = ctx.out_dir / svc.id
        tiles: list[str] = []
        ax, ay = svc.axes
        ctx.log(f"  {svc.name}: requesting {nx * ny} WCS tile(s) ...")
        for j in range(ny):
            for i in range(nx):
                ctx.check_canceled()
                x0, y0 = xmin + i * step, ymin + j * step
                x1, y1 = min(x0 + step, xmax), min(y0 + step, ymax)
                dest = tile_dir / f"{cid[-24:]}_{int(x0)}_{int(y0)}_{int(step)}.tif".replace(":", "_")
                if not dest.exists() or ctx.refresh:
                    params = [
                        ("service", "WCS"),
                        ("version", "2.0.1"),
                        ("request", "GetCoverage"),
                        ("coverageId", cid),
                        ("format", "image/tiff"),
                        ("subset", f"{ax}({x0:.2f},{x1:.2f})"),
                        ("subset", f"{ay}({y0:.2f},{y1:.2f})"),
                    ]
                    download(svc.url, dest, params=params, timeout=600, is_canceled=ctx.is_canceled)
                tiles.append(str(dest))
        path = self.warp_cached(ctx, tiles, src_srs=f"EPSG:{svc.epsg}", resample="average" if svc.native_res_m < ctx.grid.res else "bilinear")
        return self.record(path, [svc.url], f"coverage {cid}; {len(tiles)} WCS tile(s)")
