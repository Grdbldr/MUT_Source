"""Parse NAM, listing, volume-budget Tecplot, and observation files."""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

from inventory import ArtifactInventory
from tecplot_io import last_solution_time, read_tecplot_header

_DISC = re.compile(r"PERCENT DISCREPANCY\s*=\s*([+-]?\d+(?:\.\d+)?)", re.IGNORECASE)
_ELAPSED = re.compile(r"Elapsed run time:\s*(.+)", re.IGNORECASE)
_RUN_END = re.compile(r"Run end date and time[^:]*:\s*(.+)", re.IGNORECASE)
_NORMAL = re.compile(r"Normal termination", re.IGNORECASE)
_NAM_LINE = re.compile(r"^(\S+)\s+(\S+)\s+(\S.*)$")


@dataclass
class BudgetSnapshot:
    time: float | None = None
    in_recharge: float | None = None
    out_swbc: float | None = None
    in_total: float | None = None
    out_total: float | None = None
    in_minus_out: float | None = None
    percent_discrepancy: float | None = None
    columns: dict[str, float] = field(default_factory=dict)


@dataclass
class UsgRunInfo:
    packages: list[tuple[str, str]] = field(default_factory=list)
    nam_header: str = ""
    elapsed: str = ""
    run_end: str = ""
    terminated_ok: bool = False
    lst_max_abs_discrepancy: float | None = None
    lst_final_discrepancy: float | None = None
    budget: BudgetSnapshot | None = None
    budget_max_abs_disc: float | None = None
    last_times: dict[str, float] = field(default_factory=dict)
    obs_headers: dict[str, list[str]] = field(default_factory=dict)
    obs_final: dict[str, dict[str, float]] = field(default_factory=dict)


def _parse_nam(path: Path, info: UsgRunInfo) -> None:
    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    if lines:
        info.nam_header = lines[0].lstrip("# ").strip()
    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        match = _NAM_LINE.match(stripped)
        if match:
            info.packages.append((match.group(1), match.group(3).strip()))


def _parse_lst_tail(path: Path, info: UsgRunInfo) -> None:
    """Read the listing file in chunks: discrepancies from a scan, status from the tail."""
    max_abs = 0.0
    last_disc: float | None = None
    saw_normal = False
    elapsed = ""
    run_end = ""
    try:
        with path.open("r", encoding="utf-8", errors="replace") as handle:
            # Tail-first for status, then a limited discrepancy scan via budget file is preferred.
            handle.seek(0, 2)
            size = handle.tell()
            handle.seek(max(0, size - 200_000))
            tail = handle.read()
            if _NORMAL.search(tail):
                saw_normal = True
            m_el = _ELAPSED.search(tail)
            if m_el:
                elapsed = m_el.group(1).strip()
            m_end = _RUN_END.search(tail)
            if m_end:
                run_end = m_end.group(1).strip()
            for match in _DISC.finditer(tail):
                val = float(match.group(1))
                last_disc = val
                max_abs = max(max_abs, abs(val))
    except OSError:
        return
    info.terminated_ok = saw_normal or bool(elapsed)
    info.elapsed = elapsed
    info.run_end = run_end
    if last_disc is not None:
        info.lst_final_discrepancy = last_disc
        info.lst_max_abs_discrepancy = max_abs


def _col_map(header: str) -> list[str]:
    cols = re.findall(r'"([^"]+)"', header)
    if cols:
        return cols
    if "=" in header:
        return [c.strip().strip('"') for c in header.split("=", 1)[1].split(",") if c.strip()]
    return []


def _parse_budget(path: Path, info: UsgRunInfo) -> None:
    if not path.is_file():
        return
    columns: list[str] = []
    max_abs = 0.0
    last: BudgetSnapshot | None = None
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if "VARIABLE" in line.upper() and not columns:
                columns = _col_map(line)
                continue
            if not columns:
                continue
            if not line.strip() or line.strip().lower().startswith("zone") or line.strip().startswith("Title"):
                continue
            parts = line.split()
            if len(parts) < 2:
                continue
            try:
                values = [float(p) for p in parts[: len(columns)]]
            except ValueError:
                continue
            row = dict(zip(columns, values))
            disc = row.get("PERCENT DISCREPANCY")
            if disc is not None:
                max_abs = max(max_abs, abs(disc))
            last = BudgetSnapshot(
                time=values[0] if values else None,
                in_recharge=_pick(row, "IN_RECHARGE"),
                out_swbc=_pick(row, "OUT_SWBC"),
                in_total=_pick(row, "IN_TOTAL IN", "TOTAL IN"),
                out_total=_pick(row, "TOTAL OUT"),
                in_minus_out=_pick(row, "IN - OUT"),
                percent_discrepancy=disc,
                columns=row,
            )
    info.budget = last
    info.budget_max_abs_disc = max_abs if last else None


def _pick(row: dict[str, float], *names: str) -> float | None:
    for name in names:
        if name in row:
            return row[name]
    lowered = {k.lower(): v for k, v in row.items()}
    for name in names:
        if name.lower() in lowered:
            return lowered[name.lower()]
    return None


def _parse_obs(path: Path) -> tuple[list[str], dict[str, float]]:
    header = read_tecplot_header(path)
    variables = header.get("variables") or []
    last_vals: dict[str, float] = {}
    last_line = ""
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped or stripped.lower().startswith("variable") or stripped.lower().startswith("zone") or stripped.lower().startswith("title"):
                continue
            parts = stripped.split()
            try:
                [float(p) for p in parts[:2]]
            except ValueError:
                continue
            last_line = stripped
    if last_line and variables:
        nums = [float(p) for p in last_line.split()[: len(variables)]]
        last_vals = dict(zip(variables, nums))
    return variables, last_vals


def _last_step_period_time(path: Path) -> float | None:
    if not path.is_file():
        return None
    last: float | None = None
    with path.open("r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            parts = line.split()
            if len(parts) >= 3:
                try:
                    last = float(parts[-1])
                except ValueError:
                    continue
    return last


def parse_usg(inv: ArtifactInventory) -> UsgRunInfo:
    info = UsgRunInfo()
    if inv.nam:
        _parse_nam(inv.nam, info)
    if inv.lst:
        _parse_lst_tail(inv.lst, info)
    budget = inv.post_file("VolumeBudget")
    if budget:
        _parse_budget(budget, info)
    last_from_spt = _last_step_period_time(inv.model_dir / "_posto.StepPeriodTime")
    for domain in ("GWF", "SWF", "CLN"):
        post = inv.post_file(domain)
        if post:
            t = last_from_spt if last_from_spt is not None else last_solution_time(post)
            if t is not None:
                info.last_times[domain] = t
    for key, path in inv.obs_tecplot.items():
        variables, final = _parse_obs(path)
        info.obs_headers[key] = variables
        info.obs_final[key] = final
    return info
