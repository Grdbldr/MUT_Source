"""Lightweight Tecplot header helpers (ASCII plus SZL/PLT fallbacks). stdlib only."""

from __future__ import annotations

import math
import re
from pathlib import Path

_VAR_SPLIT = re.compile(r'"([^"]*)"')
_SOLUTIONTIME = re.compile(r"SOLUTIONTIME\s*=\s*([0-9.Ee+-]+)", re.IGNORECASE)
_ZONE_NODES = re.compile(r"\bN\s*=\s*([0-9]+)", re.IGNORECASE)
_ZONE_ELEMS = re.compile(r"\bE\s*=\s*([0-9]+)", re.IGNORECASE)
_ZONE_I = re.compile(r"\bI\s*=\s*([0-9]+)", re.IGNORECASE)
_AUXDATA = re.compile(r'AUXDATA\s+(\w+)\s*=\s*"([^"]*)"', re.IGNORECASE)
_DOMAIN_FILE = re.compile(
    r"\.(GWF|SWF|CLN)(?:\.(Velocity))?\.tecplot\.(?:szplt|plt|dat)$",
    re.IGNORECASE,
)
_POST_CORE = (
    "z Cell",
    "Layer",
    "Ibound",
    "Initial head",
    "Head",
)


def parse_variable_list(line: str) -> list[str]:
    quoted = _VAR_SPLIT.findall(line)
    if quoted:
        return quoted
    # Fallback: VARIABLES = a,b,c
    if "=" in line:
        rhs = line.split("=", 1)[1]
        return [part.strip().strip('"') for part in rhs.split(",") if part.strip()]
    return []


def is_binary_tecplot(path: Path) -> bool:
    """True for TecIO SZL (.szplt) and leftover classic .plt (headers are not ASCII)."""
    name = path.name.lower()
    return name.endswith(".szplt") or name.endswith(".plt")


def _split_tecvars(text: str) -> list[str]:
    parts = [p.strip().strip('"') for p in text.split(",") if p.strip().strip('"')]
    if len(parts) < 4 or parts[0].upper() != "X" or parts[1].upper() != "Y":
        return []
    return parts


def _cr_tecvars(data: bytes) -> list[str]:
    """SZL files store names at the tail as X\\rY\\rZ\\rName\\r... (not X,Y,Z,)."""
    marker = b"X\rY\rZ\r"
    idx = data.find(marker)
    if idx < 0:
        return []
    names: list[str] = []
    current: list[int] = []
    for byte in data[idx:]:
        if byte == 13:
            if not current:
                break
            try:
                name = bytes(current).decode("ascii")
            except UnicodeDecodeError:
                break
            if not name or any(ord(ch) < 32 or ord(ch) >= 127 for ch in name):
                break
            names.append(name)
            current = []
            continue
        if 32 <= byte < 127:
            current.append(byte)
            continue
        break
    if len(names) >= 4 and names[0].upper() == "X" and names[1].upper() == "Y":
        return names
    return []


def _variables_from_binary(path: Path, nbytes: int = 1_048_576) -> list[str]:
    """Recover TecIO variable names from an SZL/PLT file (head comma-list or tail CR-list)."""
    tail = 524_288
    try:
        size = path.stat().st_size
        with path.open("rb") as handle:
            data = handle.read(nbytes)
            if size > nbytes + tail:
                handle.seek(size - tail)
                data += handle.read(tail)
            elif size > nbytes:
                data += handle.read()
    except OSError:
        return []
    markers: list[tuple[bytes, str]] = [
        (b"X,Y,Z,", "ascii"),
        ("X,Y,Z,".encode("utf-16le"), "utf-16le"),
        ("X,Y,Z,".encode("utf-16be"), "utf-16be"),
    ]
    comma_names: list[str] = []
    for marker, encoding in markers:
        idx = data.find(marker)
        if idx < 0:
            continue
        chunk = data[idx : idx + 8000]
        if encoding != "ascii":
            if b"\x00\x00" in chunk:
                chunk = chunk[: chunk.find(b"\x00\x00") + 2]
            try:
                text = chunk.decode(encoding, errors="ignore")
            except LookupError:
                continue
        else:
            nul = chunk.find(b"\x00")
            if nul > 0:
                chunk = chunk[:nul]
            try:
                text = chunk.decode("ascii", errors="ignore")
            except UnicodeDecodeError:
                continue
        text = "".join(ch if 32 <= ord(ch) < 127 else "," for ch in text)
        names = _split_tecvars(text)
        if names:
            comma_names = names
            break
    cr_names = _cr_tecvars(data)
    if len(cr_names) >= len(comma_names):
        return cr_names
    return comma_names


def _ascii_sibling(path: Path) -> Path | None:
    if path.suffix.lower() == ".szplt":
        sibling = path.with_suffix(".dat")
        if sibling.is_file():
            return sibling
    return None


def ascii_sibling(path: Path) -> Path | None:
    """Public wrapper: ASCII .tecplot.dat next to a .szplt, if present."""
    return _ascii_sibling(path)


def _build_variables(domain: str) -> list[str]:
    d = domain.upper()
    if d == "GWF":
        return [
            "X",
            "Y",
            "Z",
            f"{d} Layer",
            f"{d} Zone",
            f"{d} Cell Top",
            f"{d} Cell Bottom",
            f"{d} Kh",
            f"{d} Kv",
            f"{d} Ss",
            f"{d} Sy",
            f"{d} Alpha",
            f"{d} Beta",
            f"{d} Sr",
            f"{d} Brooks",
            f"{d} Initial head",
        ]
    top = f"{d} cell  top" if d == "SWF" else f"{d} cell top"
    return [
        "X",
        "Y",
        "Z",
        f"{d} Zone",
        top,
        f"{d} {d}-GWF connection length",
        f"{d} Initial Depth",
        f"{d} Cell area",
    ]


def _post_variables(domain: str) -> list[str]:
    d = domain.upper()
    sat = "Saturation" if d == "GWF" else "Depth"
    names = ["X", "Y", "Z"]
    names.extend(f"{d} {item}" for item in _POST_CORE)
    names.append(f"{d} {sat}")
    # SWF–GWF exchange is present on coupled models (Abdul, hillslope, …).
    # Include it in the SZL-parse fallback so SWF_Infiltration.lay is still
    # written when the binary name scrape fails. Other optional CBB terms stay
    # omitted.
    if d == "SWF":
        names.extend((f"{d} to GWF", f"{d} to GWF (areal flux)"))
    return names


def _velocity_variables(domain: str) -> list[str]:
    d = domain.upper()
    return [
        "X",
        "Y",
        "Z",
        f"{d} Head",
        "Darcy Vx",
        "Darcy Vy",
        "Darcy Vz",
        "Average Linear Vx",
        "Average Linear Vy",
        "Average Linear Vz",
    ]


def schema_variables(path: Path) -> list[str]:
    """MUT FE variable lists when the SZL header cannot be parsed.

    Post SWF includes GWF flux names so an infiltration layout can still be
    written. Other optional CBB terms are omitted.
    """
    match = _DOMAIN_FILE.search(path.name)
    if not match:
        return []
    domain = match.group(1).upper()
    if match.group(2):
        return _velocity_variables(domain)
    lower = path.name.lower()
    if lower.startswith("_buildo"):
        return _build_variables(domain)
    if lower.startswith("_posto"):
        return _post_variables(domain)
    return []


def read_tecplot_header(path: Path, max_header_lines: int = 40) -> dict:
    """Return variables, title, zone node/element counts from the file head."""
    info: dict = {
        "path": path,
        "title": "",
        "variables": [],
        "n_nodes": None,
        "n_elements": None,
        "first_solution_time": None,
    }
    if not path.is_file():
        return info
    if is_binary_tecplot(path):
        names = _variables_from_binary(path)
        if len(names) < 4:
            sibling = _ascii_sibling(path)
            if sibling:
                names = read_tecplot_header(sibling, max_header_lines).get("variables") or []
        if len(names) < 4:
            names = schema_variables(path)
        info["variables"] = names
        return info
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for _ in range(max_header_lines):
            line = handle.readline()
            if not line:
                break
            stripped = line.strip()
            lower = stripped.lower()
            if lower.startswith("title"):
                quoted = _VAR_SPLIT.findall(stripped)
                info["title"] = quoted[0] if quoted else stripped.split("=", 1)[-1].strip()
            elif "variable" in lower:
                info["variables"] = parse_variable_list(stripped)
            if "zone" in lower:
                st = _SOLUTIONTIME.search(stripped)
                if st:
                    info["first_solution_time"] = float(st.group(1))
                n_match = _ZONE_NODES.search(stripped)
                e_match = _ZONE_ELEMS.search(stripped)
                if n_match:
                    info["n_nodes"] = int(n_match.group(1))
                if e_match:
                    info["n_elements"] = int(e_match.group(1))
                for key, val in _AUXDATA.findall(stripped):
                    lower_key = key.lower()
                    if lower_key == "timeunits":
                        info["time_units"] = val
                    elif lower_key == "lengthunits":
                        info["length_units"] = val
                break
    return info


def last_solution_time(path: Path) -> float | None:
    """Scan ZONE lines for the last SOLUTIONTIME (works on large ASCII files)."""
    if not path.is_file() or is_binary_tecplot(path):
        return None
    last: float | None = None
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if "SOLUTIONTIME" in line.upper():
                match = _SOLUTIONTIME.search(line)
                if match:
                    last = float(match.group(1))
    return last


def read_point_xyz(path: Path) -> list[tuple[float, float, float]]:
    """XYZ rows from an ASCII POINT scatter file (after the ZONE header)."""
    points: list[tuple[float, float, float]] = []
    if not path.is_file() or is_binary_tecplot(path):
        return points
    n_expect: int | None = None
    in_data = False
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped:
                continue
            upper = stripped.upper()
            if upper.startswith("ZONE"):
                match = _ZONE_I.search(stripped)
                if match:
                    n_expect = (n_expect or 0) + int(match.group(1))
                else:
                    n_expect = None
                in_data = True
                continue
            if not in_data:
                continue
            if upper.startswith("TITLE") or "VARIABLE" in upper or upper.startswith("ZONE"):
                continue
            parts = stripped.replace(",", " ").split()
            if len(parts) < 3:
                continue
            try:
                points.append((float(parts[0]), float(parts[1]), float(parts[2])))
            except ValueError:
                continue
            if n_expect is not None and len(points) >= n_expect:
                break
    return points


def var_index(variables: list[str], *needles: str) -> int | None:
    """1-based Tecplot variable index matching any needle (case-insensitive)."""
    lowered = [v.lower() for v in variables]
    for needle in needles:
        want = needle.lower()
        for i, name in enumerate(lowered, start=1):
            if name == want:
                return i
        for i, name in enumerate(lowered, start=1):
            if want in name:
                return i
    return None


def var_name(variables: list[str], *needles: str) -> str | None:
    """Return the actual variable name matching any needle, or None."""
    idx = var_index(variables, *needles)
    if idx is None:
        return None
    return variables[idx - 1]


def mm_per_year_factor(length_units: str = "", time_units: str = "") -> float:
    """Convert a length/time flux to millimetres per year (365.25-day year)."""
    length = (length_units or "meters").strip().lower()
    time = (time_units or "seconds").strip().lower()
    if length in {"m", "meter", "meters", "metre", "metres"}:
        to_mm = 1000.0
    elif length in {"ft", "foot", "feet"}:
        to_mm = 304.8
    elif length in {"cm", "centimeter", "centimeters", "centimetre", "centimetres"}:
        to_mm = 10.0
    elif length in {"mm", "millimeter", "millimeters", "millimetre", "millimetres"}:
        to_mm = 1.0
    else:
        to_mm = 1000.0
    if time in {"s", "sec", "second", "seconds"}:
        per_year = 365.25 * 24.0 * 3600.0
    elif time in {"min", "minute", "minutes"}:
        per_year = 365.25 * 24.0 * 60.0
    elif time in {"h", "hr", "hour", "hours"}:
        per_year = 365.25 * 24.0
    elif time in {"d", "day", "days"}:
        per_year = 365.25
    elif time in {"y", "yr", "year", "years"}:
        per_year = 1.0
    else:
        per_year = 365.25 * 24.0 * 3600.0
    return to_mm * per_year


def nice_abs_limit(value: float) -> float:
    """Smallest 1-2-3-5 * 10^k that is at least *value*."""
    if not math.isfinite(value) or value <= 0:
        return 1.0
    mag = 10 ** math.floor(math.log10(value))
    for cand in (1.0, 2.0, 3.0, 5.0, 10.0):
        limit = cand * mag
        if limit >= value * 0.999:
            return limit
    return 10.0 * mag


def symmetric_contour_levels(limit: float, n_each_side: int = 4) -> list[float]:
    """Zero-centered geometric levels (Abdul-style -30,-10,-1,-0.1,0,...)."""
    outer = nice_abs_limit(limit) if math.isfinite(limit) and limit > 0 else 30.0
    steps = [1.0, 1.0 / 3.0, 1.0 / 30.0, 1.0 / 300.0][:n_each_side]
    positives = [outer * s for s in steps]
    return [-p for p in positives] + [0.0] + list(reversed(positives))


def _comment_hints(var_name: str) -> tuple[str, ...]:
    lower = var_name.lower()
    if "areal flux" in lower:
        return ("areal flux",)
    if "to gwf" in lower:
        return ("cbb_gwf",)
    if "to swf" in lower:
        return ("cbb_swf",)
    if "to recharge" in lower:
        return ("cbb_recharge",)
    return (lower,)


_INT_TOKEN = re.compile(r"^[-+]?\d+$")


def _int_only_line(text: str) -> bool:
    toks = text.split()
    return bool(toks) and all(_INT_TOKEN.fullmatch(tok) for tok in toks)


def ascii_var_absmax(path: Path, *needles: str) -> float | None:
    """Max |value| of a named variable in an ASCII Tecplot file (or .dat sibling)."""
    src = path
    if not src.is_file():
        return None
    if is_binary_tecplot(src):
        sibling = _ascii_sibling(src)
        if sibling is None:
            return None
        src = sibling
    header = read_tecplot_header(src)
    variables = header.get("variables") or []
    name = var_name(variables, *needles)
    if not name:
        return None
    hints = _comment_hints(name)
    skip_areal = hints == ("cbb_gwf",)
    found = 0.0
    any_val = False
    collecting = False
    with src.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            stripped = line.strip()
            if stripped.startswith("#"):
                comment = stripped.lstrip("#").strip().lower()
                if skip_areal and "areal" in comment:
                    collecting = False
                    continue
                collecting = any(hint in comment for hint in hints)
                continue
            if not collecting:
                continue
            lower = stripped.lower()
            if lower.startswith("zone") or _int_only_line(stripped):
                collecting = False
                continue
            for tok in stripped.replace(",", " ").split():
                try:
                    value = abs(float(tok))
                except ValueError:
                    continue
                any_val = True
                if value > found:
                    found = value
    return found if any_val else None
