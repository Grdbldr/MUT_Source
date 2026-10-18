"""Provider interface and the provenance record written to sources.json."""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Callable

from ..geo import GeoBBox, Grid
from ..raster import cache_ok, clear_cache_meta, warp_to_grid, write_cache_meta

WORLD = GeoBBox(-180.0, -90.0, 180.0, 90.0)


@dataclass
class SourceRecord:
    role: str
    provider: str
    title: str
    path: str
    quantity: str = "elevation"
    resolution_m: float | None = None
    vertical_accuracy: str = ""
    vertical_datum: str = ""
    licence: str = ""
    citation: str = ""
    urls: list[str] = field(default_factory=list)
    retrieved: str = field(default_factory=lambda: datetime.now(timezone.utc).isoformat(timespec="seconds"))
    notes: str = ""


@dataclass
class FetchContext:
    grid: Grid
    out_dir: Path
    api_keys: dict = field(default_factory=dict)
    log: Callable[[str], None] = print
    is_canceled: Callable[[], bool] = lambda: False
    refresh: bool = False
    suggestions: list[str] = field(default_factory=list)

    def check_canceled(self) -> None:
        if self.is_canceled():
            raise InterruptedError("Run canceled")


class Provider:
    id = ""
    name = ""
    kind = ""
    quantity = "elevation"
    resolution_m: float | None = None
    vertical_accuracy = ""
    vertical_datum = ""
    licence = ""
    citation = ""
    requires_key: str | None = None
    coverage: tuple[GeoBBox, ...] = (WORLD,)
    resample = "bilinear"

    def covers(self, bbox: GeoBBox) -> bool:
        return any(region.intersects(bbox) for region in self.coverage)

    def missing_key(self, api_keys: dict) -> bool:
        return bool(self.requires_key) and not api_keys.get(self.requires_key)

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        """Return a record whose ``path`` is a GeoTIFF on ``ctx.grid``, or None if nothing covers ``bbox``."""
        raise NotImplementedError

    def output_path(self, ctx: FetchContext) -> Path:
        return ctx.out_dir / f"{self.id}_{self.kind}_{ctx.grid.res:g}m.tif"

    def record(self, path: Path, urls: list[str], notes: str = "") -> SourceRecord:
        return SourceRecord(
            role=self.kind,
            provider=self.id,
            title=self.name,
            path=str(path),
            quantity=self.quantity,
            resolution_m=self.resolution_m,
            vertical_accuracy=self.vertical_accuracy,
            vertical_datum=self.vertical_datum,
            licence=self.licence,
            citation=self.citation,
            urls=urls,
            notes=notes,
        )

    def warp_cached(
        self,
        ctx: FetchContext,
        sources: list[str],
        src_nodata: float | None = None,
        src_srs: str | None = None,
        resample: str | None = None,
        extra: dict | None = None,
    ) -> Path:
        """Warp ``sources`` onto the context grid, reusing an earlier result for the same grid and inputs."""
        dst = self.output_path(ctx)
        method = resample or self.resample
        signature = {"grid": ctx.grid.signature(), "sources": sorted(sources), "resample": method, "extra": extra or {}}
        if cache_ok(dst, signature, ctx.refresh):
            ctx.log(f"  {self.name}: using cached {dst.name}")
            return dst
        ctx.log(f"  {self.name}: reading {len(sources)} source file(s) onto the {ctx.grid.res:g} m grid ...")
        clear_cache_meta(dst)
        warp_to_grid(sources, ctx.grid, dst, method, src_nodata=src_nodata, src_srs=src_srs, is_canceled=ctx.is_canceled)
        write_cache_meta(dst, signature)
        return dst
