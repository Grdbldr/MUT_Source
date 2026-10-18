"""Data-source registry. Each provider covers a region and fetches onto a model grid.

Kinds:
    coarse   regional ground surface (~30 m)
    fine     focus-area ground surface (~1 m lidar)
    bedrock  bedrock elevation or depth to bedrock
"""

from __future__ import annotations

from .base import FetchContext, Provider, SourceRecord
from .bedrock import ShangguanDepthToBedrock
from .copernicus import CopernicusGLO30
from .hrdem import NrcanHRDEM
from .mrdem import NrcanMRDEM30
from .opentopography import OpenTopographyGlobalDEM, OpenTopographyLidarCatalog
from .usgs3dep import Usgs3DEP1m
from .wcs import WCS_SERVICES, WcsLidarProvider

PROVIDERS: list[Provider] = [
    NrcanMRDEM30(),
    CopernicusGLO30(),
    OpenTopographyGlobalDEM(),
    Usgs3DEP1m(),
    NrcanHRDEM(),
    *[WcsLidarProvider(cfg) for cfg in WCS_SERVICES],
    OpenTopographyLidarCatalog(),
    ShangguanDepthToBedrock(),
]

KINDS = ("coarse", "fine", "bedrock")


def get_provider(provider_id: str) -> Provider:
    for provider in PROVIDERS:
        if provider.id == provider_id:
            return provider
    raise KeyError(f"Unknown provider '{provider_id}'")


def providers_of_kind(kind: str) -> list[Provider]:
    return [p for p in PROVIDERS if p.kind == kind]


def default_provider_ids(project, api_keys: dict | None = None) -> list[str]:
    """Providers that cover the regional area and do not need a missing API key."""
    bbox = project.regional_grid().geo_bbox()
    api_keys = api_keys or {}
    return [p.id for p in PROVIDERS if p.covers(bbox) and (not p.requires_key or api_keys.get(p.requires_key))]


__all__ = [
    "FetchContext",
    "KINDS",
    "PROVIDERS",
    "Provider",
    "SourceRecord",
    "default_provider_ids",
    "get_provider",
    "providers_of_kind",
]
