"""Dams that control lake levels: dam inventory, measured reservoir levels and published target elevations.

Dams come from the Ontario Dam Inventory in Ontario, otherwise from OpenStreetMap ``waterway=dam|weir``. Each dam is linked to
the nearest corrected lake. Measured reservoir levels come from the Grand River Conservation Authority (GRCA) KiWIS
service; target (rule-curve) elevations exist only in published documents, so they are read from
``data/reservoir_targets.json``, which carries a citation for every entry and can be extended.
"""

from __future__ import annotations

import hashlib
import json
import re
import time
from dataclasses import asdict, dataclass, field
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import ogr, osr

from .bathymetry import MNRF_COVERAGE, Lake, overpass_area, overpass_json
from .geo import GeoBBox, Grid
from .net import session
from .providers.base import SourceRecord

ODI = "https://ws.lioservices.lrc.gov.on.ca/arcgis2/rest/services/LIO_OPEN_DATA/LIO_Open04/MapServer/0"
KIWIS = "https://waterdata.grandriver.ca/KiWIS/KiWIS"
GRCA_COVERAGE = GeoBBox(-81.3, 42.8, -79.7, 44.3)
TARGETS_FILE = Path(__file__).parent / "data" / "reservoir_targets.json"
MATCH_M = 300.0
DUPLICATE_M = 150.0
STATION_M = 1000.0
RECORD_YEARS = 10
LEVEL_CACHE_DAYS = 7
NAME_NOISE = {"dam", "dams", "reservoir", "lake", "marsh", "weir", "the", "of", "pond", "conservation", "area"}

CITATIONS = {
    "odi": (
        "Ontario Ministry of Natural Resources, Ontario Dam Inventory (Land Information Ontario), "
        "Open Government Licence - Ontario. https://geohub.lio.gov.on.ca/datasets/mnrf::ontario-dam-inventory"
    ),
    "osm": "OpenStreetMap contributors, waterway=dam and waterway=weir, ODbL. https://www.openstreetmap.org/copyright",
    "kiwis": (
        "Grand River Conservation Authority, reservoir elevation (HK), near real-time provisional data, "
        "GRCA Open Data Licence. https://data.grandriver.ca/webservices-monitoring.html"
    ),
}


@dataclass
class Dam:
    name: str
    source: str
    source_id: str
    lon: float
    lat: float
    owner: str = ""
    comments: str = ""
    lake: str = ""
    lake_osm_id: str = ""
    lake_distance_m: float | None = None
    dem_level_m: float | None = None
    station: str = ""
    levels: dict | None = None
    target: dict | None = None


@dataclass
class DamsResult:
    dams: list[Dam]
    unmatched: list[str]
    records: list[SourceRecord]
    warnings: list[str] = field(default_factory=list)

    def stats(self) -> dict:
        return {"dams": [asdict(d) for d in self.dams], "unmatched": self.unmatched, "warnings": self.warnings}


def _tokens(name: str) -> set[str]:
    words = re.findall(r"[a-z]+", re.sub(r"['\u2019]", "", name.lower()))
    stems = {w[:-1] if len(w) > 3 and w.endswith("s") else w for w in words}
    return stems - NAME_NOISE


def _same_name(a: str, b: str) -> bool:
    ta, tb = _tokens(a), _tokens(b)
    return bool(ta) and bool(tb) and (ta <= tb or tb <= ta)


def _bbox_key(bbox: GeoBBox) -> str:
    return hashlib.sha1(json.dumps([round(v, 4) for v in (bbox.west, bbox.south, bbox.east, bbox.north)]).encode()).hexdigest()[:12]


# --- dam locations -----------------------------------------------------------------------------------------------


def _odi_query(params: dict) -> dict:
    resp = session().post(f"{ODI}/query", data={"f": "json", **params}, timeout=120)
    resp.raise_for_status()
    data = resp.json()
    if "error" in data:
        raise RuntimeError(f"Ontario Dam Inventory: {data['error'].get('message', data['error'])}")
    return data


def _odi_dams(bbox: GeoBBox) -> list[Dam]:
    ids = _odi_query({
        "geometry": f"{bbox.west},{bbox.south},{bbox.east},{bbox.north}",
        "geometryType": "esriGeometryEnvelope",
        "inSR": 4326,
        "spatialRel": "esriSpatialRelIntersects",
        "returnIdsOnly": "true",
    }).get("objectIds") or []
    dams: list[Dam] = []
    ids = sorted(ids)
    for i in range(0, len(ids), 500):
        data = _odi_query({
            "objectIds": ",".join(map(str, ids[i : i + 500])),
            "outFields": "DAM_ID,DAM_NAME,DAM_OWNERSHIP,GENERAL_COMMENTS",
            "outSR": 4326,
            "returnGeometry": "true",
        })
        for feat in data.get("features", []):
            a, g = feat["attributes"], feat.get("geometry") or {}
            if "x" not in g:
                continue
            dams.append(Dam(
                name=(a.get("DAM_NAME") or "").strip() or "(unnamed dam)",
                source="Ontario Dam Inventory",
                source_id=str(a.get("DAM_ID") or ""),
                lon=g["x"],
                lat=g["y"],
                owner=a.get("DAM_OWNERSHIP") or "",
                comments=(a.get("GENERAL_COMMENTS") or "").strip(),
            ))
    return dams


def _osm_dams(bbox: GeoBBox) -> list[Dam]:
    area = overpass_area(bbox)
    data = overpass_json(
        f'[out:json][timeout:90];(node["waterway"="dam"]{area};way["waterway"="dam"]{area};'
        f'node["waterway"="weir"]{area};way["waterway"="weir"]{area};);out center tags;',
        "dam",
        read_timeout=100,
        attempts=1,
    )
    dams: list[Dam] = []
    for el in data.get("elements", []):
        lat = el.get("lat", (el.get("center") or {}).get("lat"))
        lon = el.get("lon", (el.get("center") or {}).get("lon"))
        if lat is None or lon is None:
            continue
        tags = el.get("tags", {})
        kind = tags.get("waterway", "dam")
        dams.append(Dam(
            name=tags.get("name", "").strip() or f"(unnamed {kind})",
            source=f"OpenStreetMap ({kind})",
            source_id=f"{el['type']}/{el['id']}",
            lon=lon,
            lat=lat,
            owner=tags.get("operator", ""),
        ))
    return dams


def find_dams(bbox: GeoBBox, cache_dir: Path, refresh: bool, log: Callable[[str], None]) -> list[Dam]:
    """Dams and weirs in ``bbox``: the Ontario Dam Inventory where it covers ``bbox``, otherwise OpenStreetMap."""
    cache = cache_dir / f"dams_{_bbox_key(bbox)}.json"
    if cache.exists() and not refresh:
        return [Dam(**d) for d in json.loads(cache.read_text(encoding="utf-8"))]
    log("Dams: querying dam inventories ...")
    odi = _odi_dams(bbox) if MNRF_COVERAGE.intersects(bbox) else []
    osm = [] if odi else _osm_dams(bbox)
    dams = list(odi)
    for dam in osm:
        near = [d for d in odi if _metres(d.lon, d.lat, dam.lon, dam.lat) < DUPLICATE_M]
        if near:
            if near[0].name.startswith("(unnamed") and not dam.name.startswith("(unnamed"):
                near[0].name = dam.name
            continue
        dams.append(dam)
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_text(json.dumps([asdict(d) for d in dams]), encoding="utf-8")
    return dams


def _metres(lon1: float, lat1: float, lon2: float, lat2: float) -> float:
    k = 111_320.0
    return float(np.hypot((lon2 - lon1) * k * np.cos(np.radians((lat1 + lat2) / 2)), (lat2 - lat1) * k))


def match_dams(dams: list[Dam], lakes: list[Lake], grid: Grid) -> tuple[list[Dam], list[Dam]]:
    """Link each dam to the corrected lake within ``MATCH_M`` (named lakes first, then nearest)."""
    src = osr.SpatialReference()
    src.ImportFromEPSG(4326)
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(grid.epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    to_grid = osr.CoordinateTransformation(src, dst)
    shapes = []
    for lake in lakes:
        geom = ogr.CreateGeometryFromJson(json.dumps(lake.geometry)) if lake.geometry else None
        if geom is None:
            continue
        geom.Transform(to_grid)
        xmin, xmax, ymin, ymax = geom.GetEnvelope()
        shapes.append((lake, geom, (xmin - MATCH_M, xmax + MATCH_M, ymin - MATCH_M, ymax + MATCH_M)))
    matched: list[Dam] = []
    unmatched: list[Dam] = []
    for dam in dams:
        x, y, _ = to_grid.TransformPoint(dam.lon, dam.lat)
        if not (grid.xmin <= x <= grid.xmax and grid.ymin <= y <= grid.ymax):
            continue
        point = ogr.Geometry(ogr.wkbPoint)
        point.AddPoint_2D(x, y)
        best = None
        for lake, geom, (x0, x1, y0, y1) in shapes:
            if not (x0 <= x <= x1 and y0 <= y <= y1):
                continue
            dist = geom.Distance(point)
            if dist > MATCH_M:
                continue
            key = (lake.name == "(unnamed)", dist)
            if best is None or key < best[0]:
                best = (key, lake, dist)
        if best is None:
            unmatched.append(dam)
            continue
        _, lake, dist = best
        dam.lake, dam.lake_osm_id, dam.lake_distance_m = lake.name, lake.osm_id, float(dist)
        dam.dem_level_m = lake.water_level_m
        matched.append(dam)
    return matched, unmatched


# --- measured reservoir levels (GRCA KiWIS) ----------------------------------------------------------------------


def _kiwis(params: dict, timeout: float = 120):
    resp = session().get(KIWIS, params={"service": "kisters", "type": "queryServices", "datasource": 0, **params}, timeout=timeout)
    resp.raise_for_status()
    return resp.json()


def kiwis_stations(cache_dir: Path, refresh: bool) -> list[dict]:
    """GRCA stations with an hourly reservoir elevation (HK) series."""
    cache = cache_dir / "grca_kiwis_hk_stations.json"
    if cache.exists() and not refresh and time.time() - cache.stat().st_mtime < LEVEL_CACHE_DAYS * 86400:
        return json.loads(cache.read_text(encoding="utf-8"))
    rows = _kiwis({
        "request": "getTimeseriesList",
        "format": "json",
        "parametertype_name": "HK",
        "ts_name": "07.NRT.PRODUCTION.HOURLY",
        "returnfields": "station_name,station_no,ts_id,station_latitude,station_longitude",
    })
    header, body = rows[0], rows[1:]
    stations = [dict(zip(header, r)) for r in body if r and r[0]]
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_text(json.dumps(stations), encoding="utf-8")
    return stations


def _station_for(dam: Dam, stations: list[dict]) -> dict | None:
    best = None
    for st in stations:
        try:
            dist = _metres(dam.lon, dam.lat, float(st["station_longitude"]), float(st["station_latitude"]))
        except (TypeError, ValueError, KeyError):
            dist = None
        if dist is not None and dist <= STATION_M and (best is None or dist < best[0]):
            best = (dist, st)
    if best:
        return best[1]
    return next((st for st in stations if _same_name(st["station_name"], dam.name)), None)


def kiwis_levels(ts_id: str, dem_level: float | None, cache_dir: Path, refresh: bool) -> dict | None:
    """Statistics of the last ``RECORD_YEARS`` of hourly reservoir elevations, and where the DEM level falls in them."""
    cache = cache_dir / f"grca_kiwis_{ts_id}.json"
    if cache.exists() and not refresh and time.time() - cache.stat().st_mtime < LEVEL_CACHE_DAYS * 86400:
        data = json.loads(cache.read_text(encoding="utf-8"))
    else:
        now = datetime.now(timezone.utc)
        start = now - timedelta(days=365 * RECORD_YEARS)
        data = _kiwis({
            "request": "getTimeseriesValues",
            "format": "json",
            "ts_id": ts_id,
            "from": start.strftime("%Y-%m-%d"),
            "to": now.strftime("%Y-%m-%dT%H:%M:%S"),
            "returnfields": "Timestamp,Value",
        }, timeout=240)
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(data), encoding="utf-8")
    rows = [r for r in (data[0].get("data") if data else []) or [] if r[1] is not None]
    if not rows:
        return None
    stamps = [r[0] for r in rows]
    values = np.asarray([float(r[1]) for r in rows])
    ok = np.isfinite(values) & (values > 0)
    values = values[ok]
    stamps = [s for s, k in zip(stamps, ok) if k]
    if not values.size:
        return None
    months = np.asarray([int(s[5:7]) for s in stamps])
    out = {
        "from": stamps[0][:10],
        "to": stamps[-1][:10],
        "current_m": float(values[-1]),
        "current_time": stamps[-1][:16].replace("T", " ") + " UTC",
        "median_m": float(np.median(values)),
        "p5_m": float(np.percentile(values, 5)),
        "p95_m": float(np.percentile(values, 95)),
        "min_m": float(values.min()),
        "max_m": float(values.max()),
        "monthly_median_m": {str(m): float(np.median(values[months == m])) for m in range(1, 13) if (months == m).any()},
    }
    if dem_level is not None:
        out["dem_percentile"] = float(100.0 * (values <= dem_level).mean())
    return out


# --- published targets -------------------------------------------------------------------------------------------


def load_targets(path: Path = TARGETS_FILE) -> list[dict]:
    if not path.exists():
        return []
    return json.loads(path.read_text(encoding="utf-8")).get("reservoirs", [])


def _target_for(dam: Dam, targets: list[dict]) -> dict | None:
    for entry in targets:
        names = [entry.get("dam", "")] + entry.get("aliases", [])
        if any(_same_name(n, dam.name) for n in names if n):
            return entry
        if dam.station and any(_same_name(n, dam.station) for n in names if n):
            return entry
    return None


# --- driver ------------------------------------------------------------------------------------------------------


def reservoir_dams(
    project,
    grid: Grid,
    lakes: list[Lake],
    log: Callable[[str], None] = print,
    refresh: bool = False,
) -> DamsResult:
    """Dams on the corrected lakes, with measured levels (GRCA) and published target elevations where known."""
    work = project.downloads_dir
    bbox = grid.geo_bbox()
    dams = find_dams(bbox, work, refresh, log)
    matched, unmatched = match_dams(dams, lakes, grid)
    warnings: list[str] = []
    used: set[str] = set()
    for dam in matched:
        used.add("odi" if dam.source == "Ontario Dam Inventory" else "osm")
    if matched and GRCA_COVERAGE.intersects(bbox):
        try:
            stations = kiwis_stations(work, refresh)
        except Exception as exc:  # noqa: BLE001 (levels are optional)
            stations = []
            warnings.append(f"GRCA reservoir levels unavailable: {exc}")
        for dam in matched:
            st = _station_for(dam, stations)
            if st is None:
                continue
            dam.station = st["station_name"]
            try:
                dam.levels = kiwis_levels(str(st["ts_id"]), dam.dem_level_m, work, refresh)
                used.add("kiwis")
            except Exception as exc:  # noqa: BLE001 (levels are optional)
                warnings.append(f"{dam.station}: reservoir levels unavailable: {exc}")
    targets = load_targets()
    citations: dict[str, str] = {}
    for dam in matched:
        dam.target = _target_for(dam, targets)
        if dam.target and dam.target.get("citation"):
            citations[dam.target["citation"]] = dam.target.get("url", "")
    log(
        f"Dams: {len(matched)} dam(s) on corrected lakes, {sum(1 for d in matched if d.levels)} with measured levels, "
        f"{sum(1 for d in matched if d.target and d.target.get('targets'))} with published target elevations"
    )
    records = _records(used, citations, work)
    named = [d.name for d in unmatched if not d.name.startswith("(unnamed") and d.source == "Ontario Dam Inventory"]
    return DamsResult(matched, sorted(set(named)), records, warnings)


def _records(used: set[str], citations: dict[str, str], work: Path) -> list[SourceRecord]:
    records: list[SourceRecord] = []
    if "odi" in used:
        records.append(SourceRecord(
            role="dams", provider="ontario_dam_inventory", title="Ontario Dam Inventory", path=str(work),
            quantity="dam locations", licence="Open Government Licence - Ontario", citation=CITATIONS["odi"], urls=[ODI],
        ))
    if "osm" in used:
        records.append(SourceRecord(
            role="dams", provider="osm_dams", title="OpenStreetMap dams and weirs", path=str(work),
            quantity="dam locations", licence="ODbL", citation=CITATIONS["osm"],
        ))
    if "kiwis" in used:
        records.append(SourceRecord(
            role="reservoir_levels", provider="grca_kiwis", title="GRCA measured reservoir elevations (provisional)",
            path=str(work), quantity="reservoir water level", vertical_datum="as published by GRCA (metres)",
            licence="GRCA Open Data Licence", citation=CITATIONS["kiwis"], urls=[KIWIS],
            notes=f"last {RECORD_YEARS} years of hourly values",
        ))
    for citation, url in citations.items():
        records.append(SourceRecord(
            role="reservoir_targets", provider="published_rule_curves", title="Published reservoir target elevations",
            path=str(TARGETS_FILE), quantity="target reservoir water level", citation=citation,
            urls=[url] if url else [],
        ))
    return records
