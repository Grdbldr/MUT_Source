"""Compare volume-budget IN-OUT series between release and base folders."""

from __future__ import annotations

import math
import re
from pathlib import Path

_MUTVERSION = re.compile(r'AUXDATA\s+MUTVersion\s*=\s*"([^"]+)"', re.IGNORECASE)
_VARS = re.compile(r'^VARIABLES\s*=', re.IGNORECASE)
_QUOTED = re.compile(r'"([^"]*)"')

# Match if every IN-OUT sample is within this absolute or relative tolerance
ABS_TOL = 1.0e-6
REL_TOL = 1.0e-4

BUDGET_NAME = "_posto.modflow.VolumeBudget.tecplot.dat"


def budget_path(model_dir: Path) -> Path:
    return model_dir / BUDGET_NAME


def read_mut_version(path: Path) -> str | None:
    if not path.is_file():
        return None
    try:
        with path.open(encoding="utf-8", errors="replace") as handle:
            for _ in range(8):
                line = handle.readline()
                if not line:
                    break
                match = _MUTVERSION.search(line)
                if match:
                    return match.group(1).strip()
    except OSError:
        return None
    return None


def _parse_variables(line: str) -> list[str]:
    quoted = _QUOTED.findall(line)
    if quoted:
        return quoted
    rhs = line.split("=", 1)[1]
    return [part.strip().strip('"') for part in rhs.split(",") if part.strip()]


def read_inout_series(path: Path) -> tuple[str | None, list[float], list[float]]:
    """Return (MUTVersion, times, IN-OUT values) from an ASCII volume-budget file."""
    version: str | None = None
    times: list[float] = []
    values: list[float] = []
    names: list[str] = []
    inout_idx: int | None = None
    in_data = False
    with path.open(encoding="utf-8", errors="replace") as handle:
        for raw in handle:
            line = raw.strip()
            if not line:
                continue
            if version is None:
                match = _MUTVERSION.search(raw)
                if match:
                    version = match.group(1).strip()
            if not names and _VARS.match(line):
                names = _parse_variables(line)
                for i, name in enumerate(names):
                    if name.strip().upper() == "IN - OUT":
                        inout_idx = i
                        break
                continue
            if line.lower().startswith("zone"):
                in_data = True
                if version is None:
                    match = _MUTVERSION.search(raw)
                    if match:
                        version = match.group(1).strip()
                continue
            if not in_data or inout_idx is None:
                continue
            parts = line.split()
            if len(parts) <= inout_idx:
                continue
            try:
                times.append(float(parts[0]))
                values.append(float(parts[inout_idx]))
            except ValueError:
                continue
    return version, times, values


def _lerp(x0: float, y0: float, x1: float, y1: float, x: float) -> float:
    if x1 == x0:
        return y0
    t = (x - x0) / (x1 - x0)
    return y0 + t * (y1 - y0)


def _interp(src_t: list[float], src_v: list[float], dst_t: list[float]) -> list[float]:
    if not src_t:
        return [math.nan] * len(dst_t)
    out: list[float] = []
    j = 0
    last = len(src_t) - 1
    for x in dst_t:
        while j < last and src_t[j + 1] < x:
            j += 1
        if x <= src_t[0]:
            out.append(src_v[0])
        elif x >= src_t[last]:
            out.append(src_v[last])
        else:
            j2 = min(j + 1, last)
            out.append(_lerp(src_t[j], src_v[j], src_t[j2], src_v[j2], x))
    return out


def max_abs_diff(a: list[float], b: list[float]) -> float:
    if not a or not b or len(a) != len(b):
        return math.nan
    return max(abs(x - y) for x, y in zip(a, b))


def is_match(max_diff: float, scale: float) -> bool:
    if math.isnan(max_diff):
        return False
    return max_diff <= max(ABS_TOL, REL_TOL * max(scale, 1.0e-12))


def compare_folder(release_dir: Path, base_dir: Path, folder: str) -> dict[str, object]:
    """Compare IN-OUT for one verification folder."""
    cur_path = budget_path(release_dir / folder)
    base_path = budget_path(base_dir / folder)
    result: dict[str, object] = {
        "folder": folder,
        "status": "missing",
        "note": "",
        "current_version": None,
        "base_version": None,
        "max_abs_diff": None,
        "n_current": 0,
        "n_base": 0,
    }
    if not cur_path.is_file() and not base_path.is_file():
        result["note"] = "volume-budget files missing in both trees"
        return result
    if not cur_path.is_file():
        result["status"] = "missing_current"
        result["base_version"] = read_mut_version(base_path)
        result["note"] = f"no {BUDGET_NAME} in release folder"
        return result
    if not base_path.is_file():
        result["status"] = "missing_base"
        result["current_version"] = read_mut_version(cur_path)
        result["note"] = f"no {BUDGET_NAME} in baseline folder"
        return result

    cur_ver, cur_t, cur_v = read_inout_series(cur_path)
    base_ver, base_t, base_v = read_inout_series(base_path)
    result["current_version"] = cur_ver
    result["base_version"] = base_ver
    result["n_current"] = len(cur_v)
    result["n_base"] = len(base_v)
    if not cur_v or not base_v:
        result["status"] = "incomparable"
        result["note"] = "could not parse IN - OUT series"
        return result

    # Duplicate timestamps are common at stress-period boundaries (USG writes
    # several budget dumps at the same TOTAL TIME). Interpolating by time then
    # compares later dumps to the first dump at that time and false-flags
    # models such as Abdul. Use index-wise comparison when the time lists match.
    times_match = len(cur_t) == len(base_t) and all(
        abs(a - b) <= 1.0e-9 * max(1.0, abs(a), abs(b)) for a, b in zip(cur_t, base_t)
    )
    if times_match:
        aligned = base_v
    else:
        aligned = _interp(base_t, base_v, cur_t)
    diff = max_abs_diff(cur_v, aligned)
    scale = max(max(abs(v) for v in cur_v), max(abs(v) for v in aligned), 1.0e-12)
    result["max_abs_diff"] = diff
    if is_match(diff, scale):
        result["status"] = "match"
        result["note"] = f"IN-OUT series agree (max |diff| = {diff:.3g})"
    else:
        result["status"] = "differ"
        result["note"] = f"IN-OUT series differ (max |diff| = {diff:.3g})"
    return result


def first_version(results: list[dict[str, object]], key: str) -> str | None:
    for item in results:
        value = item.get(key)
        if isinstance(value, str) and value:
            return value
    return None
