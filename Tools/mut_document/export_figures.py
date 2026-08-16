"""Optional Tecplot batch export of section layouts to PNG."""

from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

from inventory import ArtifactInventory
from write_layouts import LayoutFile


def _as_posix(path: Path) -> str:
    return str(path).replace("\\", "/")


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


def export_figures(
    inv: ArtifactInventory,
    layouts: list[LayoutFile],
    timeout_s: int = 600,
) -> tuple[bool, str]:
    macro = write_export_macro(inv, layouts)
    tec360 = find_tec360()
    if tec360 is None:
        return False, "tec360 not found on PATH; skipped PNG export"
    # Tecplot 360 EX: -b batch, -p macro file. On Windows, start /wait is required
    # because tec360.exe returns immediately (see Tecplot "Batch Mode and Windows").
    if os.name == "nt":
        cmd = ["cmd", "/c", "start", "/wait", "", tec360, "-b", "-p", str(macro)]
    else:
        cmd = [tec360, "-b", "-p", str(macro)]
    try:
        proc = subprocess.run(
            cmd,
            cwd=str(inv.model_dir),
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired:
        return False, f"tec360 timed out after {timeout_s}s"
    except OSError as exc:
        return False, f"tec360 failed to start: {exc}"
    pngs = list(inv.imagery_dir.glob("*.png"))
    if proc.returncode != 0 and not pngs:
        err = (proc.stderr or proc.stdout or "").strip()[:500]
        return False, f"tec360 exit {proc.returncode}: {err}"
    if not pngs:
        return False, (
            f"tec360 finished (exit {proc.returncode}) but wrote no PNG files; "
            f"open {macro.name} in Tecplot to debug"
        )
    return True, f"exported {len(pngs)} PNG file(s) via {tec360}"
