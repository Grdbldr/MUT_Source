"""Hydrometric discharge near model outlets: ECCC (Canada) then USGS (United States).

Writes daily/monthly series and long-term averages under ``1_data/flows/``. Observed Q is for
calibration against critical-depth SWBC outflow, not a prescribed well.
"""

from __future__ import annotations

import csv
import hashlib
import json
import math
import time
from calendar import monthrange
from dataclasses import dataclass, field
from datetime import date, datetime, timezone
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import ogr, osr

from .climate import RECORD_YEARS, record_window
from .geo import GeoBBox
from .net import session
from .providers.base import SourceRecord

CACHE_DAYS = 7
MATCH_KM = 25.0
CFS_TO_M3S = 0.028316846592

ECCC_STATIONS = "https://api.weather.gc.ca/collections/hydrometric-stations/items"
ECCC_DAILY = "https://api.weather.gc.ca/collections/hydrometric-daily-mean/items"
USGS_DV = "https://waterservices.usgs.gov/nwis/dv/"

CITATIONS = {
    "eccc": (
        "Environment and Climate Change Canada, National Hydrometric Program (HYDAT) via MSC GeoMet OGC API. "
        "https://api.weather.gc.ca/collections/hydrometric-daily-mean"
    ),
    "usgs": (
        "U.S. Geological Survey, National Water Information System / Water Data APIs. "
        "https://waterdata.usgs.gov/"
    ),
}


@dataclass
class Outlet:
    name: str
    lon: float
    lat: float
    source: str = ""


@dataclass
class GaugeMatch:
    outlet: Outlet
    agency: str
    station_id: str
    station_name: str
    lon: float
    lat: float
    distance_km: float
    daily_path: Path | None = None
    monthly_path: Path | None = None
    lta: dict = field(default_factory=dict)
    monthly_lta: dict = field(default_factory=dict)


@dataclass
class FlowsResult:
    gauges: list[GaugeMatch] = field(default_factory=list)
    gauges_path: Path | None = None
    lta_path: Path | None = None
    summary_path: Path | None = None
    records: list[SourceRecord] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    def stats(self) -> dict:
        return {
            "gauges": [
                {
                    "outlet": g.outlet.name,
                    "agency": g.agency,
                    "station_id": g.station_id,
                    "station_name": g.station_name,
                    "lon": g.lon,
                    "lat": g.lat,
                    "distance_km": g.distance_km,
                    "lta": g.lta,
                    "monthly_lta": g.monthly_lta,
                    "daily": str(g.daily_path) if g.daily_path else None,
                    "monthly": str(g.monthly_path) if g.monthly_path else None,
                }
                for g in self.gauges
            ],
            "warnings": self.warnings,
            "gauges_file": str(self.gauges_path) if self.gauges_path else None,
            "lta_file": str(self.lta_path) if self.lta_path else None,
        }


def flows_dir(project) -> Path:
    return project.output_path / "1_data" / "flows"


def _cache_ok(path: Path, refresh: bool) -> bool:
    return path.exists() and not refresh and time.time() - path.stat().st_mtime < CACHE_DAYS * 86400


def _metres(lon1: float, lat1: float, lon2: float, lat2: float) -> float:
    """Approximate great-circle distance (m)."""
    r = 6371000.0
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dl = math.radians(lon2 - lon1)
    a = math.sin(dphi / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
    return 2 * r * math.asin(min(1.0, math.sqrt(a)))


def find_outlets(project) -> list[Outlet]:
    """Level-1 stream ends from mesh GeoPackages, else project location."""
    outlets: list[Outlet] = []
    mesh_root = project.mesh_root
    if mesh_root.exists():
        for gpkg in sorted(mesh_root.glob("*/*.gpkg")) + sorted((mesh_root / "_streams").glob("*_preview.gpkg") if (mesh_root / "_streams").exists() else []):
            found = _outlets_from_gpkg(gpkg, project.epsg)
            if found:
                outlets.extend(found)
                break
    if not outlets and project.has_boundary and project.boundary_file:
        # Fall back: project site (often near the outlet for watershed delineation)
        outlets.append(Outlet(name="project_location", lon=project.lon, lat=project.lat, source="project"))
    if not outlets:
        outlets.append(Outlet(name="project_location", lon=project.lon, lat=project.lat, source="project"))
    # Deduplicate by rounded lon/lat
    uniq: list[Outlet] = []
    seen: set[tuple[float, float]] = set()
    for o in outlets:
        key = (round(o.lon, 4), round(o.lat, 4))
        if key in seen:
            continue
        seen.add(key)
        uniq.append(o)
    return uniq


def _outlets_from_gpkg(path: Path, epsg: int) -> list[Outlet]:
    """Downstream ends of level-1 stream lines in a mesh/preview GeoPackage."""
    ds = ogr.Open(str(path))
    if ds is None:
        return []
    layer = None
    for i in range(ds.GetLayerCount()):
        lyr = ds.GetLayerByIndex(i)
        name = (lyr.GetName() or "").lower()
        if name in ("streams", "stream", "ranked_streams"):
            layer = lyr
            break
    if layer is None:
        layer = ds.GetLayerByIndex(0)
    if layer is None:
        return []
    src = layer.GetSpatialRef()
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(4326)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    transform = None
    if src is not None and not src.IsSame(dst):
        transform = osr.CoordinateTransformation(src, dst)
    outlets: list[Outlet] = []
    layer.ResetReading()
    for feat in layer:
        level = feat.GetField("level") if feat.GetFieldIndex("level") >= 0 else feat.GetField("stream_level") if feat.GetFieldIndex("stream_level") >= 0 else None
        try:
            if level is not None and int(level) != 1:
                continue
        except (TypeError, ValueError):
            continue
        geom = feat.GetGeometryRef()
        if geom is None:
            continue
        g = geom.Clone()
        if transform is not None:
            g.Transform(transform)
        # LineString: last vertex is downstream
        if g.GetGeometryType() in (ogr.wkbLineString, ogr.wkbLineString25D):
            n = g.GetPointCount()
            if n < 2:
                continue
            x, y, _z = g.GetPoint(n - 1)
        elif g.GetGeometryCount() > 0:
            part = g.GetGeometryRef(g.GetGeometryCount() - 1)
            n = part.GetPointCount()
            if n < 2:
                continue
            x, y, _z = part.GetPoint(n - 1)
        else:
            continue
        name = ""
        for field in ("name", "NAME", "gnis_name", "GNIS_NAME"):
            if feat.GetFieldIndex(field) >= 0:
                name = feat.GetField(field) or ""
                if name:
                    break
        outlets.append(Outlet(name=name or "level1_outlet", lon=float(x), lat=float(y), source=path.name))
    return outlets


def _eccc_stations(bbox: GeoBBox, cache_dir: Path, refresh: bool) -> list[dict]:
    key = hashlib.md5(f"eccc_st|{bbox.as_list()}".encode()).hexdigest()[:12]
    cache = cache_dir / f"eccc_stations_{key}.json"
    if _cache_ok(cache, refresh):
        return json.loads(cache.read_text(encoding="utf-8"))
    params = {
        "bbox": f"{bbox.west},{bbox.south},{bbox.east},{bbox.north}",
        "limit": 1000,
        "f": "json",
    }
    resp = session().get(ECCC_STATIONS, params=params, timeout=120)
    resp.raise_for_status()
    data = resp.json()
    features = data.get("features") or []
    stations = []
    for feat in features:
        props = feat.get("properties") or {}
        geom = feat.get("geometry") or {}
        coords = geom.get("coordinates") or [None, None]
        stations.append({
            "id": props.get("STATION_NUMBER") or props.get("IDENTIFIER"),
            "name": props.get("STATION_NAME") or "",
            "lon": float(coords[0]) if coords[0] is not None else None,
            "lat": float(coords[1]) if coords[1] is not None else None,
            "status": props.get("STATUS_EN") or props.get("STATUS") or "",
        })
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_text(json.dumps(stations), encoding="utf-8")
    return [s for s in stations if s["id"] and s["lon"] is not None and s["lat"] is not None]


def _usgs_sites(lon: float, lat: float, cache_dir: Path, refresh: bool, radius_km: float = MATCH_KM) -> list[dict]:
    """USGS sites near a point via bbox search on the OGC API (fallback: empty)."""
    dlat = radius_km / 111.0
    dlon = radius_km / max(20.0, 111.0 * math.cos(math.radians(lat)))
    bbox = GeoBBox(lon - dlon, lat - dlat, lon + dlon, lat + dlat)
    key = hashlib.md5(f"usgs_st|{bbox.as_list()}".encode()).hexdigest()[:12]
    cache = cache_dir / f"usgs_sites_{key}.json"
    if _cache_ok(cache, refresh):
        return json.loads(cache.read_text(encoding="utf-8"))
    # NWIS site service is more reliable for discharge site discovery
    url = "https://waterservices.usgs.gov/nwis/site/"
    params = {
        "format": "rdb",
        "bBox": f"{bbox.west:.5f},{bbox.south:.5f},{bbox.east:.5f},{bbox.north:.5f}",
        "parameterCd": "00060",
        "siteStatus": "all",
        "hasDataTypeCd": "dv",
    }
    resp = session().get(url, params=params, timeout=120)
    resp.raise_for_status()
    stations = _parse_usgs_rdb_sites(resp.text)
    cache.parent.mkdir(parents=True, exist_ok=True)
    cache.write_text(json.dumps(stations), encoding="utf-8")
    return stations


def _parse_usgs_rdb_sites(text: str) -> list[dict]:
    lines = [ln for ln in text.splitlines() if ln and not ln.startswith("#")]
    if len(lines) < 2:
        return []
    header = lines[0].split("\t")
    # USGS RDB: second data line is field-width codes (e.g. 15s, 50s, …)
    body_start = 2 if len(lines) > 2 and "\t" in lines[1] and not lines[1].startswith("USGS") else 1
    body = lines[body_start:]
    idx = {name: i for i, name in enumerate(header)}
    out = []
    for ln in body:
        cols = ln.split("\t")
        try:
            sid = cols[idx["site_no"]] if "site_no" in idx else cols[1]
            name = cols[idx["station_nm"]] if "station_nm" in idx else ""
            lat = float(cols[idx["dec_lat_va"]]) if "dec_lat_va" in idx else None
            lon = float(cols[idx["dec_long_va"]]) if "dec_long_va" in idx else None
        except (IndexError, ValueError, KeyError):
            continue
        if lat is None or lon is None:
            continue
        out.append({"id": sid, "name": name, "lon": lon, "lat": lat})
    return out


def _nearest(outlet: Outlet, stations: list[dict], max_km: float = MATCH_KM) -> dict | None:
    best = None
    for st in stations:
        dist = _metres(outlet.lon, outlet.lat, st["lon"], st["lat"]) / 1000.0
        if dist <= max_km and (best is None or dist < best[0]):
            best = (dist, st)
    if best is None:
        return None
    dist, st = best
    return {**st, "distance_km": dist}


def _eccc_daily(station_id: str, start: date, end: date, cache_dir: Path, refresh: bool) -> list[dict]:
    key = hashlib.md5(f"eccc_q|{station_id}|{start}|{end}".encode()).hexdigest()[:12]
    cache = cache_dir / f"eccc_daily_{station_id}_{key}.json"
    if _cache_ok(cache, refresh):
        data = json.loads(cache.read_text(encoding="utf-8"))
    else:
        rows = []
        offset = 0
        limit = 10000
        while True:
            params = {
                "STATION_NUMBER": station_id,
                "datetime": f"{start.isoformat()}/{end.isoformat()}",
                "limit": limit,
                "offset": offset,
                "f": "json",
            }
            resp = session().get(ECCC_DAILY, params=params, timeout=180)
            resp.raise_for_status()
            batch = resp.json().get("features") or []
            if not batch:
                break
            rows.extend(batch)
            if len(batch) < limit:
                break
            offset += limit
        data = rows
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(json.dumps(data), encoding="utf-8")
    out = []
    for feat in data:
        props = feat.get("properties") or feat
        q = props.get("DISCHARGE")
        stamp = props.get("DATE") or props.get("datetime")
        if q is None or stamp is None:
            continue
        try:
            out.append({"date": date.fromisoformat(str(stamp)[:10]), "Q_m3s": float(q)})
        except (TypeError, ValueError):
            continue
    out.sort(key=lambda r: r["date"])
    return out


def _usgs_daily(station_id: str, start: date, end: date, cache_dir: Path, refresh: bool) -> list[dict]:
    key = hashlib.md5(f"usgs_q|{station_id}|{start}|{end}".encode()).hexdigest()[:12]
    cache = cache_dir / f"usgs_daily_{station_id}_{key}.rdb"
    if _cache_ok(cache, refresh):
        text = cache.read_text(encoding="utf-8")
    else:
        params = {
            "format": "rdb",
            "sites": station_id,
            "startDT": start.isoformat(),
            "endDT": end.isoformat(),
            "parameterCd": "00060",
            "statCd": "00003",
            "siteStatus": "all",
        }
        resp = session().get(USGS_DV, params=params, timeout=180)
        resp.raise_for_status()
        text = resp.text
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(text, encoding="utf-8")
    return _parse_usgs_dv(text)


def _parse_usgs_dv(text: str) -> list[dict]:
    lines = [ln for ln in text.splitlines() if ln and not ln.startswith("#")]
    if len(lines) < 2:
        return []
    header = lines[0].split("\t")
    body = lines[2:] if len(lines) > 2 else lines[1:]
    # find value column (agency_cd, site_no, datetime, value, ...)
    idx = {name: i for i, name in enumerate(header)}
    date_i = idx.get("datetime", idx.get("dateTime", 2))
    val_i = None
    for name, i in idx.items():
        if name.endswith("_00060_00003") or name == "value":
            val_i = i
            break
    if val_i is None:
        # often 4th column
        val_i = 3 if len(header) > 3 else None
    if val_i is None:
        return []
    out = []
    for ln in body:
        cols = ln.split("\t")
        if len(cols) <= max(date_i, val_i):
            continue
        try:
            d = date.fromisoformat(cols[date_i][:10])
            q_cfs = float(cols[val_i])
        except (ValueError, TypeError):
            continue
        out.append({"date": d, "Q_m3s": q_cfs * CFS_TO_M3S})
    out.sort(key=lambda r: r["date"])
    return out


def _aggregate_q(daily: list[dict]) -> tuple[dict, dict, list[dict]]:
    if not daily:
        return {}, {}, []
    vals = np.asarray([r["Q_m3s"] for r in daily], dtype=float)
    lta = {
        "mean_m3s": float(np.nanmean(vals)),
        "median_m3s": float(np.nanmedian(vals)),
        "p5_m3s": float(np.nanpercentile(vals, 5)),
        "p95_m3s": float(np.nanpercentile(vals, 95)),
        "min_m3s": float(np.nanmin(vals)),
        "max_m3s": float(np.nanmax(vals)),
        "n_days": int(np.isfinite(vals).sum()),
        "from": daily[0]["date"].isoformat(),
        "to": daily[-1]["date"].isoformat(),
    }
    monthly_lta: dict[str, float] = {}
    for m in range(1, 13):
        subset = vals[[r["date"].month == m for r in daily]]
        if subset.size:
            monthly_lta[str(m)] = float(np.nanmedian(subset))
    buckets: dict[tuple[int, int], list[float]] = {}
    for r in daily:
        buckets.setdefault((r["date"].year, r["date"].month), []).append(r["Q_m3s"])
    monthly = []
    for (y, m) in sorted(buckets):
        arr = np.asarray(buckets[(y, m)], dtype=float)
        monthly.append({
            "year": y,
            "month": m,
            "year_month": f"{y:04d}-{m:02d}",
            "days": monthrange(y, m)[1],
            "Q_m3s_mean": float(np.nanmean(arr)),
            "Q_m3s_median": float(np.nanmedian(arr)),
        })
    return lta, monthly_lta, monthly


def _write_q_daily(path: Path, daily: list[dict]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["date", "Q_m3s"])
        for r in daily:
            w.writerow([r["date"].isoformat(), f"{r['Q_m3s']:.6g}"])
    return path


def _write_q_monthly(path: Path, monthly: list[dict]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["year_month", "year", "month", "days", "Q_m3s_mean", "Q_m3s_median"])
        for r in monthly:
            w.writerow([r["year_month"], r["year"], r["month"], r["days"], f"{r['Q_m3s_mean']:.6g}", f"{r['Q_m3s_median']:.6g}"])
    return path


def _write_gauges_geojson(path: Path, gauges: list[GaugeMatch]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    features = []
    for g in gauges:
        features.append({
            "type": "Feature",
            "geometry": {"type": "Point", "coordinates": [g.lon, g.lat]},
            "properties": {
                "outlet": g.outlet.name,
                "agency": g.agency,
                "station_id": g.station_id,
                "station_name": g.station_name,
                "distance_km": g.distance_km,
                "mean_m3s": g.lta.get("mean_m3s"),
                "median_m3s": g.lta.get("median_m3s"),
            },
        })
    path.write_text(json.dumps({"type": "FeatureCollection", "features": features}, indent=2), encoding="utf-8")
    return path


def load_flows(project) -> dict | None:
    path = flows_dir(project) / "flows.json"
    if not path.exists():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def retrieve_flows(
    project,
    log: Callable[[str], None] = print,
    refresh: bool = False,
    is_canceled: Callable[[], bool] | None = None,
    match_km: float = MATCH_KM,
) -> FlowsResult:
    """Match gauges to outlets and download daily discharge for the climate record window."""
    is_canceled = is_canceled or (lambda: False)
    start, end = record_window()
    outlets = find_outlets(project)
    cache = project.downloads_dir
    cache.mkdir(parents=True, exist_ok=True)
    out_dir = flows_dir(project)
    out_dir.mkdir(parents=True, exist_ok=True)
    result = FlowsResult()
    used: set[str] = set()

    # Expand search bbox around all outlets
    lons = [o.lon for o in outlets]
    lats = [o.lat for o in outlets]
    pad = match_km / 80.0
    search = GeoBBox(min(lons) - pad, min(lats) - pad, max(lons) + pad, max(lats) + pad)

    eccc_stations: list[dict] = []
    try:
        if is_canceled():
            raise InterruptedError("Run canceled")
        eccc_stations = _eccc_stations(search, cache, refresh)
        log(f"Flows: {len(eccc_stations)} ECCC station(s) in search area")
    except InterruptedError:
        raise
    except Exception as exc:  # noqa: BLE001
        result.warnings.append(f"ECCC station search failed: {exc}")

    for outlet in outlets:
        if is_canceled():
            raise InterruptedError("Run canceled")
        match = _nearest(outlet, eccc_stations, match_km) if eccc_stations else None
        agency = "eccc"
        daily: list[dict] = []
        if match is not None:
            try:
                daily = _eccc_daily(match["id"], start, end, cache, refresh)
                used.add("eccc")
            except Exception as exc:  # noqa: BLE001
                result.warnings.append(f"ECCC {match['id']}: {exc}")
                match = None
                daily = []
        if match is None or not daily:
            try:
                usgs = _usgs_sites(outlet.lon, outlet.lat, cache, refresh, match_km)
                match = _nearest(outlet, usgs, match_km)
                if match is not None:
                    daily = _usgs_daily(match["id"], start, end, cache, refresh)
                    agency = "usgs"
                    used.add("usgs")
            except Exception as exc:  # noqa: BLE001
                result.warnings.append(f"USGS near {outlet.name}: {exc}")
                match = None
                daily = []
        if match is None or not daily:
            result.warnings.append(f"No discharge gauge within {match_km:g} km of outlet '{outlet.name}'")
            continue
        lta, monthly_lta, monthly = _aggregate_q(daily)
        sid = str(match["id"]).replace("/", "_")
        daily_path = _write_q_daily(out_dir / f"outlet_{sid}_daily.csv", daily)
        monthly_path = _write_q_monthly(out_dir / f"outlet_{sid}_monthly.csv", monthly)
        gauge = GaugeMatch(
            outlet=outlet,
            agency=agency,
            station_id=str(match["id"]),
            station_name=match.get("name") or "",
            lon=float(match["lon"]),
            lat=float(match["lat"]),
            distance_km=float(match["distance_km"]),
            daily_path=daily_path,
            monthly_path=monthly_path,
            lta=lta,
            monthly_lta=monthly_lta,
        )
        result.gauges.append(gauge)
        log(
            f"Flows: {outlet.name} → {agency.upper()} {gauge.station_id} "
            f"({gauge.distance_km:.1f} km), mean Q={lta['mean_m3s']:.2f} m3/s"
        )

    if result.gauges:
        result.gauges_path = _write_gauges_geojson(out_dir / "gauges.geojson", result.gauges)
        lta_payload = {
            "start": start.isoformat(),
            "end": end.isoformat(),
            "gauges": result.stats()["gauges"],
            "citations": {k: CITATIONS[k] for k in used},
            "retrieved": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        }
        result.lta_path = out_dir / "flows_lta.json"
        result.lta_path.write_text(json.dumps(lta_payload, indent=2), encoding="utf-8")
        summary = result.stats()
        summary["citations"] = {k: CITATIONS[k] for k in used}
        result.summary_path = out_dir / "flows.json"
        result.summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
        if "eccc" in used:
            result.records.append(SourceRecord(
                role="discharge", provider="eccc_hydrometric", title="ECCC hydrometric daily discharge",
                path=str(out_dir), quantity="river discharge", licence="Open Government Licence - Canada",
                citation=CITATIONS["eccc"], urls=[ECCC_DAILY],
                notes=f"last {RECORD_YEARS} complete years",
            ))
        if "usgs" in used:
            result.records.append(SourceRecord(
                role="discharge", provider="usgs_nwis", title="USGS daily discharge",
                path=str(out_dir), quantity="river discharge", licence="USGS public domain",
                citation=CITATIONS["usgs"], urls=[USGS_DV],
                notes=f"last {RECORD_YEARS} complete years",
            ))
    else:
        log("Flows: no gauges matched")
    return result
