"""Optional Tecplot batch export of section layouts to PNG."""

from __future__ import annotations

import os
import re
import shutil
import subprocess
from pathlib import Path

from inventory import ArtifactInventory
from write_layouts import LayoutFile, apply_symmetric_legend_to_layout


def _as_posix(path: Path) -> str:
    return str(path).replace("\\", "/")


_DIVERGING_CMINMAX = re.compile(
    r"ColorMapName\s*=\s*'Diverging - Blue/Red'.*?CMin\s*=\s*([^\s]+).*?CMax\s*=\s*([^\s]+)",
    re.DOTALL | re.IGNORECASE,
)


def write_export_macro(inv: ArtifactInventory, layouts: list[LayoutFile]) -> Path:
    inv.layouts_dir.mkdir(parents=True, exist_ok=True)
    inv.imagery_dir.mkdir(parents=True, exist_ok=True)
    lines = ["#!MC 1410\n"]
    for item in layouts:
        if item.section_id == "model_documentation":
            continue
        png_name = item.section_id + ".png"
        png_path = (inv.imagery_dir / png_name).resolve()
        lay_path = item.path.resolve()
        lines.append(f'$!OpenLayout "{_as_posix(lay_path)}"\n')
        lines.append("$!ExportSetup ExportFormat = PNG\n")
        lines.append("$!ExportSetup ImageWidth = 1800\n")
        lines.append("$!ExportSetup ConvertTo256Colors = No\n")
        lines.append(f'$!ExportSetup ExportFName = "{_as_posix(png_path)}"\n')
        lines.append("$!Export\n  ExportRegion = AllFrames\n")
    lines.append("$!Quit\n")
    path = inv.layouts_dir / "export_figures.mcr"
    path.write_text("".join(lines), encoding="utf-8")
    return path


def find_tec360() -> str | None:
    found = shutil.which("tec360")
    if found:
        return found
    home = os.environ.get("ProgramFiles", r"C:\Program Files")
    candidates = list(Path(home).glob("Tecplot/Tecplot 360*/bin/tec360.exe"))
    if candidates:
        return str(sorted(candidates)[-1])
    return None


def _tec360_cmd(tec360: str, macro: Path) -> list[str]:
    if os.name == "nt":
        return ["cmd", "/c", "start", "/wait", "", tec360, "-b", "-p", str(macro)]
    return [tec360, "-b", "-p", str(macro)]


def _run_tec360(
    tec360: str,
    macro: Path,
    cwd: Path,
    timeout_s: int,
) -> tuple[bool, str]:
    try:
        proc = subprocess.run(
            _tec360_cmd(tec360, macro),
            cwd=str(cwd),
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired:
        return False, f"tec360 timed out after {timeout_s}s"
    except OSError as exc:
        return False, f"tec360 failed to start: {exc}"
    if proc.returncode != 0:
        err = (proc.stderr or proc.stdout or "").strip()[:500]
        return False, f"tec360 exit {proc.returncode}: {err}"
    return True, "ok"


def _parse_diverging_cminmax(text: str) -> tuple[float | None, float | None]:
    match = _DIVERGING_CMINMAX.search(text)
    if not match:
        return None, None
    try:
        return float(match.group(1)), float(match.group(2))
    except ValueError:
        return None, None


def _adjust_symmetric_legends(
    inv: ArtifactInventory,
    layouts: list[LayoutFile],
    tec360: str,
    timeout_s: int,
) -> str:
    pending = [item for item in layouts if item.symmetric_legend]
    if not pending:
        return ""
    notes: list[str] = []
    slice_s = max(30, min(timeout_s, 180))
    for item in pending:
        saved = inv.layouts_dir / f"{item.section_id}.saved.lay"
        macro = inv.layouts_dir / f"range_{item.section_id}.mcr"
        macro.write_text(
            "#!MC 1410\n"
            f'$!OpenLayout "{_as_posix(item.path.resolve())}"\n'
            f'$!SaveLayout "{_as_posix(saved.resolve())}"\n'
            "$!Quit\n",
            encoding="utf-8",
        )
        ok, msg = _run_tec360(tec360, macro, inv.model_dir, slice_s)
        if not ok or not saved.is_file():
            notes.append(f"{item.section_id}: range pass skipped ({msg})")
            if saved.is_file():
                saved.unlink()
            continue
        cmin, cmax = _parse_diverging_cminmax(
            saved.read_text(encoding="utf-8", errors="replace")
        )
        saved.unlink()
        if cmin is None or cmax is None:
            notes.append(f"{item.section_id}: no CMin/CMax in saved layout")
            continue
        limit = max(abs(cmin), abs(cmax), 1e-12)
        apply_symmetric_legend_to_layout(item.path, limit)
        notes.append(f"{item.section_id}: legend +/-{limit:g}")
    return "; ".join(notes)


def export_figures(
    inv: ArtifactInventory,
    layouts: list[LayoutFile],
    timeout_s: int = 600,
) -> tuple[bool, str]:
    tec360 = find_tec360()
    if tec360 is None:
        return False, "tec360 not found on PATH; skipped PNG export"

    extra = _adjust_symmetric_legends(inv, layouts, tec360, timeout_s)

    macro = write_export_macro(inv, layouts)
    # Tecplot 360 EX: -b batch, -p macro file. On Windows, start /wait is required
    # because tec360.exe returns immediately (see Tecplot "Batch Mode and Windows").
    ok, msg = _run_tec360(tec360, macro, inv.model_dir, timeout_s)
    pngs = list(inv.imagery_dir.glob("*.png"))
    prefix = (extra + "; ") if extra else ""
    if not ok and not pngs:
        return False, prefix + msg
    if not pngs:
        return False, (
            prefix
            + f"tec360 finished but wrote no PNG files; open {macro.name} in Tecplot to debug"
        )
    return True, prefix + f"exported {len(pngs)} PNG file(s) via {tec360}"
