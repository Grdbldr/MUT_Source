"""Lightweight Tecplot ASCII header helpers (stdlib only)."""

from __future__ import annotations

import re
from pathlib import Path

_VAR_SPLIT = re.compile(r'"([^"]*)"')
_SOLUTIONTIME = re.compile(r"SOLUTIONTIME\s*=\s*([0-9.Ee+-]+)", re.IGNORECASE)
_ZONE_NODES = re.compile(r"\bN\s*=\s*([0-9]+)", re.IGNORECASE)
_ZONE_ELEMS = re.compile(r"\bE\s*=\s*([0-9]+)", re.IGNORECASE)


def parse_variable_list(line: str) -> list[str]:
    quoted = _VAR_SPLIT.findall(line)
    if quoted:
        return quoted
    # Fallback: VARIABLES = a,b,c
    if "=" in line:
        rhs = line.split("=", 1)[1]
        return [part.strip().strip('"') for part in rhs.split(",") if part.strip()]
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
                break
    return info


def last_solution_time(path: Path) -> float | None:
    """Scan ZONE lines for the last SOLUTIONTIME (works on large ASCII files)."""
    if not path.is_file():
        return None
    last: float | None = None
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if "SOLUTIONTIME" in line.upper():
                match = _SOLUTIONTIME.search(line)
                if match:
                    last = float(match.group(1))
    return last


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
