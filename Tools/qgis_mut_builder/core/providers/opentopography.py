"""OpenTopography: global DEM API (fallback, needs a free API key) and lidar catalogue search."""

from __future__ import annotations

from ..geo import GeoBBox
from ..net import download, session
from .base import FetchContext, Provider, SourceRecord

API = "https://portal.opentopography.org/API"


class OpenTopographyGlobalDEM(Provider):
    id = "opentopography_globaldem"
    name = "OpenTopography global DEM (NASADEM, SRTM GL1)"
    kind = "coarse"
    resolution_m = 30.0
    vertical_accuracy = "about 5-10 m (SRTM-derived, LE90)"
    vertical_datum = "EGM96 geoid"
    licence = "Public domain (NASA/USGS); OpenTopography API terms apply"
    citation = "NASA JPL (2020). NASADEM Merged DEM Global 1 arc second V001. https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_HGT.001"
    requires_key = "opentopography"
    coverage = (GeoBBox(-180.0, -56.0, 180.0, 60.0),)
    dem_types = ("NASADEM", "SRTMGL1")

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        key = ctx.api_keys.get(self.requires_key, "")
        if not key:
            ctx.log(f"  {self.name}: skipped (no OpenTopography API key)")
            return None
        pad = 0.01
        params = {
            "south": bbox.south - pad,
            "north": bbox.north + pad,
            "west": bbox.west - pad,
            "east": bbox.east + pad,
            "outputFormat": "GTiff",
            "API_Key": key,
        }
        for dem in self.dem_types:
            ctx.check_canceled()
            raw = ctx.out_dir / f"{self.id}_{dem}_raw.tif"
            try:
                ctx.log(f"  {self.name}: requesting {dem} ...")
                download(f"{API}/globaldem", raw, params={**params, "demtype": dem}, timeout=600, is_canceled=ctx.is_canceled)
            except InterruptedError:
                raise
            except Exception as exc:  # noqa: BLE001 (any HTTP/server failure means try the next DEM)
                ctx.log(f"  {self.name}: {dem} failed ({exc})")
                continue
            path = self.warp_cached(ctx, [str(raw)])
            return self.record(path, [f"{API}/globaldem?demtype={dem}"], f"demtype={dem}")
        return None


class OpenTopographyLidarCatalog(Provider):
    """Lists lidar datasets covering the site; most are point clouds that need manual processing."""

    id = "opentopography_lidar"
    name = "OpenTopography lidar catalogue (search only)"
    kind = "fine"
    resolution_m = 1.0
    vertical_accuracy = "typically 0.1-0.3 m (lidar)"

    def fetch(self, bbox: GeoBBox, ctx: FetchContext) -> SourceRecord | None:
        params = {
            "productFormat": "PointCloud",
            "minx": bbox.west,
            "miny": bbox.south,
            "maxx": bbox.east,
            "maxy": bbox.north,
            "detail": "false",
            "outputFormat": "json",
            "include_federated": "true",
        }
        try:
            resp = session().get(f"{API}/otCatalog", params=params, timeout=120)
            resp.raise_for_status()
            datasets = resp.json().get("Datasets", [])
        except Exception as exc:  # noqa: BLE001 (catalogue is advisory only)
            ctx.log(f"  {self.name}: search failed ({exc})")
            return None
        for entry in datasets[:25]:
            ds = entry.get("Dataset", {})
            ctx.suggestions.append(f"OpenTopography lidar: {ds.get('name', '?')} - {ds.get('url', '')}")
        ctx.log(f"  {self.name}: {len(datasets)} lidar dataset(s) listed in the report for manual download")
        return None
