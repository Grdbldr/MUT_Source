"""Regional precipitation, temperature and PET from Daymet (NA) or Open-Meteo (global).

Writes domain-mean daily and monthly series plus long-term averages under ``1_data/climate/``.
PET is Hargreaves-Samani from Daymet tmax/tmin, or FAO ET0 from Open-Meteo.
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

from .geo import GeoBBox
from .net import session
from .providers.base import SourceRecord

RECORD_YEARS = 10
CACHE_DAYS = 7
LATTICE_DEG = 0.25
MAX_POINTS = 16
# LTA "average" climate: one TR spin-up period (Abdul Prism Cell pattern), not hard SS.
AVERAGE_SPINUP_DAYS = 3000.0
AVERAGE_OC_TIMES = ("1e-4", "60.", "300.0", "600.0", "900.0", "1200.0", "1500.0", "3000.0")
DAYMET_NA = GeoBBox(-178.0, 14.0, -52.0, 83.0)
DAYMET_PIXEL = "https://daymet.ornl.gov/single-pixel/api/data"
OPEN_METEO = "https://archive-api.open-meteo.com/v1/archive"

CITATIONS = {
    "daymet": (
        "Thornton, M.M., et al. 2022. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, "
        "Version 4 R1. ORNL DAAC. https://doi.org/10.3334/ORNLDAAC/2129"
    ),
    "open_meteo": (
        "Open-Meteo Historical Weather API (ERA5 / ERA5-Land), CC BY 4.0. https://open-meteo.com/"
    ),
}


@dataclass
class ClimateResult:
    source: str
    start: str
    end: str
    daily_path: Path | None = None
    monthly_path: Path | None = None
    lta_path: Path | None = None
    summary_path: Path | None = None
    lta: dict = field(default_factory=dict)
    monthly_lta: dict = field(default_factory=dict)
    records: list[SourceRecord] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    n_days: int = 0
    n_months: int = 0

    def stats(self) -> dict:
        return {
            "source": self.source,
            "start": self.start,
            "end": self.end,
            "n_days": self.n_days,
            "n_months": self.n_months,
            "lta": self.lta,
            "monthly_lta": self.monthly_lta,
            "daily": str(self.daily_path) if self.daily_path else None,
            "monthly": str(self.monthly_path) if self.monthly_path else None,
            "lta_file": str(self.lta_path) if self.lta_path else None,
            "warnings": self.warnings,
        }


def climate_dir(project) -> Path:
    return project.output_path / "1_data" / "climate"


def record_window(today: date | None = None) -> tuple[date, date]:
    """Last ``RECORD_YEARS`` complete calendar years ending the year before ``today``."""
    today = today or date.today()
    end_year = today.year - 1
    start_year = end_year - RECORD_YEARS + 1
    return date(start_year, 1, 1), date(end_year, 12, 31)


def lattice_points(bbox: GeoBBox, step: float = LATTICE_DEG, limit: int = MAX_POINTS) -> list[tuple[float, float]]:
    """WGS84 (lon, lat) sample points inside ``bbox`` (corners plus a regular lattice)."""
    west, south, east, north = bbox.west, bbox.south, bbox.east, bbox.north
    if east <= west or north <= south:
        return [((west + east) / 2.0, (south + north) / 2.0)]
    width = max(east - west, 1e-6)
    height = max(north - south, 1e-6)
    nx = max(1, min(int(math.ceil(width / step)), int(math.sqrt(limit))))
    ny = max(1, min(int(math.ceil(height / step)), max(1, limit // nx)))
    xs = np.linspace(west + width / (2 * nx), east - width / (2 * nx), nx) if nx else [(west + east) / 2]
    ys = np.linspace(south + height / (2 * ny), north - height / (2 * ny), ny) if ny else [(south + north) / 2]
    pts = [(float(x), float(y)) for y in ys for x in xs]
    if not pts:
        pts = [((west + east) / 2.0, (south + north) / 2.0)]
    return pts[:limit]


def hargreaves_pet_mm(tmax: float, tmin: float, lat_deg: float, doy: int) -> float:
    """Hargreaves-Samani reference ET (mm/day) from daily max/min temperature (°C)."""
    if not (np.isfinite(tmax) and np.isfinite(tmin)):
        return float("nan")
    if tmax < tmin:
        tmax, tmin = tmin, tmax
    tmean = 0.5 * (tmax + tmin)
    delta_t = max(tmax - tmin, 0.0)
    lat = math.radians(lat_deg)
    dr = 1.0 + 0.033 * math.cos(2.0 * math.pi * doy / 365.0)
    delta = 0.409 * math.sin(2.0 * math.pi * doy / 365.0 - 1.39)
    ws = math.acos(max(-1.0, min(1.0, -math.tan(lat) * math.tan(delta))))
    ra = (24.0 * 60.0 / math.pi) * 0.0820 * dr * (
        ws * math.sin(lat) * math.sin(delta) + math.cos(lat) * math.cos(delta) * math.sin(ws)
    )
    # Convert MJ/m2/day to mm/day equivalent (λ ≈ 2.45 MJ/kg)
    ra_mm = ra / 2.45
    return max(0.0, 0.0023 * ra_mm * (tmean + 17.8) * math.sqrt(delta_t))


def _cache_ok(path: Path, refresh: bool) -> bool:
    return path.exists() and not refresh and time.time() - path.stat().st_mtime < CACHE_DAYS * 86400


def _daymet_csv(lat: float, lon: float, start: date, end: date, cache_dir: Path, refresh: bool) -> list[dict]:
    key = hashlib.md5(f"daymet|{lat:.4f}|{lon:.4f}|{start}|{end}".encode()).hexdigest()[:12]
    cache = cache_dir / f"daymet_{key}.csv"
    if _cache_ok(cache, refresh):
        text = cache.read_text(encoding="utf-8")
    else:
        params = {
            "lat": f"{lat:.5f}",
            "lon": f"{lon:.5f}",
            "vars": "prcp,tmax,tmin",
            "start": start.isoformat(),
            "end": end.isoformat(),
        }
        resp = session().get(DAYMET_PIXEL, params=params, timeout=180)
        resp.raise_for_status()
        text = resp.text
        cache.parent.mkdir(parents=True, exist_ok=True)
        cache.write_text(text, encoding="utf-8")
    return _parse_daymet(text)


def _parse_daymet(text: str) -> list[dict]:
    """Parse Daymet single-pixel CSV (headers like ``prcp (mm/day)``, ``tmax (deg c)``)."""
    all_lines = text.splitlines()
    header_i = next((i for i, ln in enumerate(all_lines) if ln.lower().startswith("year,")), None)
    if header_i is None:
        raise RuntimeError("Daymet response had no CSV header")
    raw = "\n".join(all_lines[header_i:])
    rows = []
    reader = csv.DictReader(raw.splitlines())
    if reader.fieldnames is None:
        raise RuntimeError("Daymet CSV has no field names")

    def col(*needles: str) -> str:
        for name in reader.fieldnames:
            key = name.split("(")[0].strip().lower()
            if key in needles:
                return name
        raise KeyError(needles[0])

    year_c, yday_c = col("year"), col("yday")
    prcp_c, tmax_c, tmin_c = col("prcp"), col("tmax"), col("tmin")
    for row in reader:
        try:
            year = int(row[year_c])
            yday = int(row[yday_c])
            d = datetime.strptime(f"{year} {yday}", "%Y %j").date()
            rows.append({
                "date": d,
                "prcp_mm": float(row[prcp_c]),
                "tmax": float(row[tmax_c]),
                "tmin": float(row[tmin_c]),
            })
        except (KeyError, ValueError, TypeError):
            continue
    return rows


def _open_meteo_batch(
    points: list[tuple[float, float]],
    start: date,
    end: date,
    cache_dir: Path,
    refresh: bool,
) -> list[list[dict]]:
    """Fetch daily P/T/ET0 for up to 10 points per request; return one row list per point."""
    out: list[list[dict]] = []
    chunk = 10
    for i in range(0, len(points), chunk):
        batch = points[i : i + chunk]
        lats = ",".join(f"{lat:.5f}" for _lon, lat in batch)
        lons = ",".join(f"{lon:.5f}" for lon, _lat in batch)
        key = hashlib.md5(f"om|{lats}|{lons}|{start}|{end}".encode()).hexdigest()[:12]
        cache = cache_dir / f"open_meteo_{key}.json"
        if _cache_ok(cache, refresh):
            data = json.loads(cache.read_text(encoding="utf-8"))
        else:
            params = {
                "latitude": lats,
                "longitude": lons,
                "start_date": start.isoformat(),
                "end_date": end.isoformat(),
                "daily": "precipitation_sum,temperature_2m_mean,temperature_2m_max,temperature_2m_min,et0_fao_evapotranspiration",
                "timezone": "GMT",
            }
            resp = session().get(OPEN_METEO, params=params, timeout=180)
            resp.raise_for_status()
            data = resp.json()
            cache.parent.mkdir(parents=True, exist_ok=True)
            cache.write_text(json.dumps(data), encoding="utf-8")
        items = data if isinstance(data, list) else [data]
        if len(items) != len(batch):
            raise RuntimeError(f"Open-Meteo returned {len(items)} series for {len(batch)} points")
        for item in items:
            daily = item.get("daily") or {}
            times = daily.get("time") or []
            p = daily.get("precipitation_sum") or []
            t = daily.get("temperature_2m_mean") or []
            tmax = daily.get("temperature_2m_max") or []
            tmin = daily.get("temperature_2m_min") or []
            et0 = daily.get("et0_fao_evapotranspiration") or []
            rows = []
            for j, stamp in enumerate(times):
                rows.append({
                    "date": date.fromisoformat(stamp[:10]),
                    "prcp_mm": _f(p, j),
                    "tmean": _f(t, j),
                    "tmax": _f(tmax, j),
                    "tmin": _f(tmin, j),
                    "et0_mm": _f(et0, j),
                })
            out.append(rows)
    return out


def _f(arr: list, i: int) -> float:
    if i >= len(arr) or arr[i] is None:
        return float("nan")
    return float(arr[i])


def _mean_series(series_list: list[list[dict]], source: str, mean_lat: float) -> list[dict]:
    """Average aligned daily rows across sample points into domain-mean P/T/PET (m/d, °C)."""
    if not series_list:
        return []
    by_date: dict[date, list[dict]] = {}
    for series in series_list:
        for row in series:
            by_date.setdefault(row["date"], []).append(row)
    out = []
    for d in sorted(by_date):
        rows = by_date[d]
        prcp = np.nanmean([r.get("prcp_mm", float("nan")) for r in rows])
        if source == "daymet":
            tmax = np.nanmean([r.get("tmax", float("nan")) for r in rows])
            tmin = np.nanmean([r.get("tmin", float("nan")) for r in rows])
            tmean = 0.5 * (tmax + tmin) if np.isfinite(tmax) and np.isfinite(tmin) else float("nan")
            pet_mm = hargreaves_pet_mm(tmax, tmin, mean_lat, d.timetuple().tm_yday)
        else:
            tmean = np.nanmean([r.get("tmean", float("nan")) for r in rows])
            pet_mm = np.nanmean([r.get("et0_mm", float("nan")) for r in rows])
            if not np.isfinite(pet_mm):
                tmax = np.nanmean([r.get("tmax", float("nan")) for r in rows])
                tmin = np.nanmean([r.get("tmin", float("nan")) for r in rows])
                pet_mm = hargreaves_pet_mm(tmax, tmin, mean_lat, d.timetuple().tm_yday)
        out.append({
            "date": d,
            "P_m_per_d": float(prcp) / 1000.0 if np.isfinite(prcp) else float("nan"),
            "T_C": float(tmean) if np.isfinite(tmean) else float("nan"),
            "PET_m_per_d": float(pet_mm) / 1000.0 if np.isfinite(pet_mm) else float("nan"),
            "source": source,
        })
    return out


def aggregate_monthly(daily: list[dict]) -> list[dict]:
    buckets: dict[tuple[int, int], list[dict]] = {}
    for row in daily:
        d = row["date"]
        buckets.setdefault((d.year, d.month), []).append(row)
    months = []
    for (y, m) in sorted(buckets):
        rows = buckets[(y, m)]
        n = monthrange(y, m)[1]
        months.append({
            "year": y,
            "month": m,
            "year_month": f"{y:04d}-{m:02d}",
            "days": n,
            "P_m_per_d": float(np.nanmean([r["P_m_per_d"] for r in rows])),
            "T_C": float(np.nanmean([r["T_C"] for r in rows])),
            "PET_m_per_d": float(np.nanmean([r["PET_m_per_d"] for r in rows])),
            "P_m_per_month": float(np.nansum([r["P_m_per_d"] for r in rows])),
            "PET_m_per_month": float(np.nansum([r["PET_m_per_d"] for r in rows])),
            "source": rows[0]["source"],
        })
    return months


def long_term_average(daily: list[dict]) -> dict:
    if not daily:
        return {}
    p = np.asarray([r["P_m_per_d"] for r in daily], dtype=float)
    t = np.asarray([r["T_C"] for r in daily], dtype=float)
    pet = np.asarray([r["PET_m_per_d"] for r in daily], dtype=float)
    return {
        "P_m_per_d": float(np.nanmean(p)),
        "P_m_per_yr": float(np.nanmean(p) * 365.25),
        "T_C": float(np.nanmean(t)),
        "PET_m_per_d": float(np.nanmean(pet)),
        "PET_m_per_yr": float(np.nanmean(pet) * 365.25),
        "n_days": int(np.isfinite(p).sum()),
    }


def monthly_climatology(daily: list[dict]) -> dict:
    """Mean daily rate by calendar month (1–12)."""
    out: dict[str, dict] = {}
    for m in range(1, 13):
        rows = [r for r in daily if r["date"].month == m]
        if not rows:
            continue
        out[str(m)] = {
            "P_m_per_d": float(np.nanmean([r["P_m_per_d"] for r in rows])),
            "T_C": float(np.nanmean([r["T_C"] for r in rows])),
            "PET_m_per_d": float(np.nanmean([r["PET_m_per_d"] for r in rows])),
        }
    return out


def _write_daily_csv(path: Path, daily: list[dict]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["date", "P_m_per_d", "T_C", "PET_m_per_d", "source"])
        for r in daily:
            w.writerow([r["date"].isoformat(), f"{r['P_m_per_d']:.8g}", f"{r['T_C']:.4g}", f"{r['PET_m_per_d']:.8g}", r["source"]])
    return path


def _write_monthly_csv(path: Path, monthly: list[dict]) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["year_month", "year", "month", "days", "P_m_per_d", "T_C", "PET_m_per_d", "P_m_per_month", "PET_m_per_month", "source"])
        for r in monthly:
            w.writerow([
                r["year_month"], r["year"], r["month"], r["days"],
                f"{r['P_m_per_d']:.8g}", f"{r['T_C']:.4g}", f"{r['PET_m_per_d']:.8g}",
                f"{r['P_m_per_month']:.8g}", f"{r['PET_m_per_month']:.8g}", r["source"],
            ])
    return path


def load_climate(project) -> dict | None:
    """Read ``1_data/climate/climate.json`` if present."""
    path = climate_dir(project) / "climate.json"
    if not path.exists():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def load_monthly_climate(project) -> list[dict]:
    path = climate_dir(project) / "climate_monthly.csv"
    if not path.exists():
        return []
    rows = []
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            rows.append({
                "year": int(row["year"]),
                "month": int(row["month"]),
                "year_month": row["year_month"],
                "days": int(row["days"]),
                "P_m_per_d": float(row["P_m_per_d"]),
                "T_C": float(row["T_C"]),
                "PET_m_per_d": float(row["PET_m_per_d"]),
                "source": row.get("source", ""),
            })
    return rows


def load_daily_climate(project) -> list[dict]:
    path = climate_dir(project) / "climate_daily.csv"
    if not path.exists():
        return []
    rows = []
    with open(path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            rows.append({
                "date": date.fromisoformat(row["date"]),
                "P_m_per_d": float(row["P_m_per_d"]),
                "T_C": float(row["T_C"]),
                "PET_m_per_d": float(row["PET_m_per_d"]),
                "source": row.get("source", ""),
            })
    return rows


EXDP_M = 2.0
NEVTOP = 1
SWF_RECH_OPT = 4
NET_RECHARGE_ASC = "NetRecharge.asc"


def net_recharge_m_per_d(lta: dict) -> tuple[float, bool]:
    """Long-term average P−PET (m/d), clamped at zero. Returns ``(net, clamped)``."""
    p = float(lta.get("P_m_per_d", 0.0))
    pet = float(lta.get("PET_m_per_d", 0.0))
    raw = p - pet
    if raw < 0.0:
        return 0.0, True
    return raw, False


def write_constant_arcascii(
    path: Path,
    *,
    xmin: float,
    ymin: float,
    xmax: float,
    ymax: float,
    value: float,
) -> Path:
    """Tiny constant ArcASCII covering ``[xmin,xmax]×[ymin,ymax]`` (projected metres)."""
    path.parent.mkdir(parents=True, exist_ok=True)
    pad_x = max((xmax - xmin) * 0.05, 100.0)
    pad_y = max((ymax - ymin) * 0.05, 100.0)
    x0 = xmin - pad_x
    y0 = ymin - pad_y
    width = max(xmax - xmin + 2 * pad_x, 200.0)
    height = max(ymax - ymin + 2 * pad_y, 200.0)
    cell = max(width, height)
    # 2×2 cells so every sample inside the mesh bbox hits the constant value
    ncols = 2
    nrows = 2
    cellsize = max(cell / ncols, cell / nrows)
    val = f"{value:.8f}"
    row = " ".join([val] * ncols)
    lines = [
        f"ncols {ncols}",
        f"nrows {nrows}",
        f"xllcorner {x0:.6f}",
        f"yllcorner {y0:.6f}",
        f"cellsize {cellsize:.6f}",
        "nodata_value -9999",
        row,
        row,
        "",
    ]
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def write_precipitation_rts(path: Path, daily: list[dict]) -> Path:
    """Single-column RTS file: start, end, factor 1, P (m/d)."""
    path.parent.mkdir(parents=True, exist_ok=True)
    lines = []
    for i, row in enumerate(daily):
        t0 = float(i)
        t1 = float(i + 1)
        lines.append(f"{t0:.2f}\t{t1:.2f}\t1.00\t{row['P_m_per_d']:.8f}")
    path.write_text("\n".join(lines) + ("\n" if lines else ""), encoding="utf-8")
    return path


def write_climate_forcing_instructions(
    path: Path,
    *,
    mode: str,
    monthly: list[dict],
    lta: dict,
    include_swf: bool,
    outlet_rel: str | None,
    observed_notes: list[str],
    include_cln: bool = False,
    net_rate: float | None = None,
    swf_rate: float | None = None,
    net_asc_rel: str | None = None,
    a_mesh_m2: float | None = None,
    a_cln_m2: float | None = None,
) -> Path:
    """``ClimateForcing.instructions`` included from ``_build.mut``.

    ``average``: net P−PET to CLN (GSTR) then SWF RCH (area-reduced); no GWF EVT.
    One long TR stress period (Abdul-style spin-up) so ATS can grow from a tiny Δt.
    ``transient``: monthly P → SWF RCH and PET → GWF EVT.
    """
    lines = [
        "! Climate forcing from MUT Model Builder (retrieved Daymet / Open-Meteo)",
        f"! Mode: {mode}",
    ]
    if mode == "average":
        lines.append(
            "! Average LTA: net P-PET -> CLN GSTR (if present) then SWF RCH (option 4); no GWF EVT"
        )
        lines.append(
            f"! Transient spin-up ({AVERAGE_SPINUP_DAYS:g} d TR), same pattern as Abdul Prism Cell"
        )
    else:
        lines.append("! P -> SWF recharge (option 4); PET -> GWF EVT (NEVTOP=1, EXDP=2 m)")
    lines.append("! Rates in m/d; time unit days.")
    for note in observed_notes:
        lines.append(f"! {note}")
    lines.append("")

    if mode == "average":
        p = float(lta.get("P_m_per_d", 0.0))
        pet = float(lta.get("PET_m_per_d", 0.0))
        net, clamped = net_recharge_m_per_d(lta)
        if net_rate is not None:
            net = float(net_rate)
            clamped = False
        swf = float(swf_rate) if swf_rate is not None else net
        lines += [
            f"! LTA P={p:.8f} m/d, PET={pet:.8f} m/d, net=max(P-PET,0)={net:.8f} m/d"
            + (" (clamped)" if clamped else ""),
        ]
        if a_mesh_m2 is not None:
            lines.append(f"! A_mesh={a_mesh_m2:.3f} m2")
        if a_cln_m2 is not None:
            lines.append(f"! A_cln={a_cln_m2:.3f} m2 (channel width x stream length)")
        lines.append(f"! SWF_RCH={swf:.8f} m/d")
        lines += [
            "",
            "stress period",
            "    type",
            "    TR",
            "    duration",
            f"    {AVERAGE_SPINUP_DAYS:.1f}",
            "end stress period",
            "",
        ]
        if include_cln and net_asc_rel:
            lines += [
                "active domain",
                "cln",
                "    choose all cells",
                "    gstr instance name",
                "    Rain_CLN",
                "    cln gstr",
                "    1.0",
                f"    0.0    {net_asc_rel}",
                "    end gstr",
                "",
            ]
        if include_swf:
            lines += [
                "active domain",
                "swf",
                "    choose all cells",
                "    swf recharge",
                f"    {swf:.8f}",
                f"    {SWF_RECH_OPT}",
                "",
            ]
            if outlet_rel:
                lines += [
                    "    clear chosen cells",
                    "    choose cells from gb nodes",
                    f"    {outlet_rel}",
                    "    swbc zone name",
                    "    Outlet",
                    "    swf critical depth",
                    "",
                ]
    else:
        for i, row in enumerate(monthly, start=1):
            p = float(row["P_m_per_d"])
            pet = float(row["PET_m_per_d"])
            days = int(row["days"])
            lines += [
                f"! ---- SP{i}: {row['year_month']} ----",
                "stress period",
                "    type",
                "    TR",
                "    duration",
                f"    {days:.1f}",
                "end stress period",
                "",
            ]
            if include_swf:
                lines += [
                    "active domain",
                    "swf",
                    "    choose all cells",
                    "    swf recharge",
                    f"    {p:.8f}",
                    f"    {SWF_RECH_OPT}",
                    "",
                ]
                if i == 1 and outlet_rel:
                    lines += [
                        "    clear chosen cells",
                        "    choose cells from gb nodes",
                        f"    {outlet_rel}",
                        "    swbc zone name",
                        "    Outlet",
                        "    swf critical depth",
                        "",
                    ]
            lines += [
                "active domain",
                "gwf",
                "    choose all cells",
                "    gwf evt",
                f"    {pet:.8f}",
                f"    {NEVTOP}",
                f"    {EXDP_M}",
                "",
            ]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def retrieve_climate(
    project,
    bbox: GeoBBox | None = None,
    log: Callable[[str], None] = print,
    refresh: bool = False,
    is_canceled: Callable[[], bool] | None = None,
) -> ClimateResult:
    """Download domain-mean climate for the regional bbox; write files under ``1_data/climate/``."""
    is_canceled = is_canceled or (lambda: False)
    bbox = bbox or project.regional_grid().geo_bbox()
    start, end = record_window()
    points = lattice_points(bbox)
    mean_lat = float(np.mean([lat for _lon, lat in points]))
    cache = project.downloads_dir
    cache.mkdir(parents=True, exist_ok=True)
    out_dir = climate_dir(project)
    out_dir.mkdir(parents=True, exist_ok=True)
    warnings: list[str] = []
    source = ""
    daily: list[dict] = []

    use_daymet = DAYMET_NA.intersects(bbox)
    if use_daymet:
        log(f"Climate: trying Daymet ({len(points)} sample point(s), {start} to {end})")
        series: list[list[dict]] = []
        try:
            for lon, lat in points:
                if is_canceled():
                    raise InterruptedError("Run canceled")
                series.append(_daymet_csv(lat, lon, start, end, cache, refresh))
            daily = _mean_series(series, "daymet", mean_lat)
            source = "daymet"
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001
            warnings.append(f"Daymet unavailable: {exc}")
            log(f"Climate: Daymet failed ({exc}); trying Open-Meteo")
            daily = []

    if not daily:
        log(f"Climate: trying Open-Meteo ({len(points)} sample point(s), {start} to {end})")
        try:
            if is_canceled():
                raise InterruptedError("Run canceled")
            series = _open_meteo_batch(points, start, end, cache, refresh)
            daily = _mean_series(series, "open_meteo", mean_lat)
            source = "open_meteo"
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001
            warnings.append(f"Open-Meteo unavailable: {exc}")
            raise RuntimeError("; ".join(warnings) or str(exc)) from exc

    if not daily:
        raise RuntimeError("No climate daily values retrieved")

    monthly = aggregate_monthly(daily)
    lta = long_term_average(daily)
    monthly_lta = monthly_climatology(daily)
    daily_path = _write_daily_csv(out_dir / "climate_daily.csv", daily)
    monthly_path = _write_monthly_csv(out_dir / "climate_monthly.csv", monthly)
    lta_payload = {
        "source": source,
        "start": start.isoformat(),
        "end": end.isoformat(),
        "bbox": bbox.as_list(),
        "sample_points": [{"lon": lon, "lat": lat} for lon, lat in points],
        "lta": lta,
        "monthly_lta": monthly_lta,
        "citation": CITATIONS[source],
        "retrieved": datetime.now(timezone.utc).isoformat(timespec="seconds"),
    }
    lta_path = out_dir / "climate_lta.json"
    lta_path.write_text(json.dumps(lta_payload, indent=2), encoding="utf-8")
    summary = {
        "source": source,
        "start": start.isoformat(),
        "end": end.isoformat(),
        "n_days": len(daily),
        "n_months": len(monthly),
        "lta": lta,
        "monthly_lta": monthly_lta,
        "daily": str(daily_path),
        "monthly": str(monthly_path),
        "lta_file": str(lta_path),
        "citation": CITATIONS[source],
        "warnings": warnings,
    }
    summary_path = out_dir / "climate.json"
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    log(
        f"Climate: {source}, {len(daily)} days, LTA P={lta['P_m_per_yr']:.3f} m/yr, "
        f"PET={lta['PET_m_per_yr']:.3f} m/yr, T={lta['T_C']:.2f} C"
    )
    record = SourceRecord(
        role="climate",
        provider=source,
        title="Daymet daily weather" if source == "daymet" else "Open-Meteo historical weather (ERA5)",
        path=str(out_dir),
        quantity="precipitation, temperature, PET",
        resolution_m=1000.0 if source == "daymet" else 25000.0,
        licence="Daymet data use policy" if source == "daymet" else "CC BY 4.0",
        citation=CITATIONS[source],
        urls=[DAYMET_PIXEL if source == "daymet" else OPEN_METEO],
        notes=f"domain-mean over {len(points)} points; {RECORD_YEARS} complete years {start} to {end}",
    )
    return ClimateResult(
        source=source,
        start=start.isoformat(),
        end=end.isoformat(),
        daily_path=daily_path,
        monthly_path=monthly_path,
        lta_path=lta_path,
        summary_path=summary_path,
        lta=lta,
        monthly_lta=monthly_lta,
        records=[record],
        warnings=warnings,
        n_days=len(daily),
        n_months=len(monthly),
    )
