"""Export each stacked frame of BatchComparisonPlots.lay to PNG."""

from __future__ import annotations

import sys
from pathlib import Path

from parse_batch import parse_layout_frames, slug

_DOC = Path(__file__).resolve().parent.parent / "mut_document"
if str(_DOC) not in sys.path:
    sys.path.insert(0, str(_DOC))

from export_figures import _as_posix, _run_tec360, find_tec360  # noqa: E402


def write_frame_export_macro(
    layout_path: Path,
    frames: list[tuple[int, str]],
    png_dir: Path,
    macro_path: Path,
) -> Path:
    png_dir.mkdir(parents=True, exist_ok=True)
    macro_path.parent.mkdir(parents=True, exist_ok=True)
    lines = ["#!MC 1410\n"]
    lines.append(f'$!OpenLayout "{_as_posix(layout_path.resolve())}"\n')
    lines.append("$!ExportSetup ExportFormat = PNG\n")
    lines.append("$!ExportSetup ImageWidth = 1800\n")
    lines.append("$!ExportSetup ConvertTo256Colors = No\n")
    for number, name in frames:
        png_path = (png_dir / f"{slug(name)}.png").resolve()
        lines.append("$!FrameControl ActivateByNumber\n")
        lines.append(f"  Frame = {number}\n")
        lines.append(f'$!ExportSetup ExportFName = "{_as_posix(png_path)}"\n')
        lines.append("$!Export\n  ExportRegion = CurrentFrame\n")
    lines.append("$!Quit\n")
    macro_path.write_text("".join(lines), encoding="utf-8")
    return macro_path


def export_comparison_frames(
    layout_path: Path,
    png_dir: Path,
    cwd: Path,
    timeout_s: int = 1800,
) -> tuple[bool, str, list[tuple[int, str, Path]]]:
    """Export one PNG per layout frame. Returns (ok, message, exported list)."""
    frames = parse_layout_frames(layout_path)
    if not frames:
        return False, f"no $!FrameName entries in {layout_path}", []

    tec360 = find_tec360()
    if tec360 is None:
        return False, "tec360 not found on PATH; skipped PNG export", []

    macro = cwd / "_verify_export.mcr"
    write_frame_export_macro(layout_path, frames, png_dir, macro)
    ok, msg = _run_tec360(tec360, macro, cwd, timeout_s)
    exported: list[tuple[int, str, Path]] = []
    for number, name in frames:
        png = png_dir / f"{slug(name)}.png"
        if png.is_file() and png.stat().st_size > 0:
            exported.append((number, name, png))
    if not exported:
        detail = msg if not ok else f"tec360 finished but wrote no PNG files; open {macro}"
        return False, detail, []
    note = f"exported {len(exported)}/{len(frames)} PNG file(s) via {tec360}"
    if not ok:
        note = f"{note} ({msg})"
    return True, note, exported
