#!/usr/bin/env python3
"""Run verification models and write the User's Guide release-comparison appendix.

Never copies MUT_Examples into Examples-Release. Work in the release tree;
publish selected inputs afterward with ToRepos.bat (see verify_release.ps1).

    python mut_verify.py
    python mut_verify.py --skip-batch --skip-pdf
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

_HERE = Path(__file__).resolve().parent
_REPO = _HERE.parent.parent
if str(_HERE) not in sys.path:
    sys.path.insert(0, str(_HERE))

from compare_budgets import compare_folder, first_version  # noqa: E402
from export_comparison import export_comparison_frames  # noqa: E402
from parse_batch import parse_folder_list, parse_layout_frames, slug  # noqa: E402
from run_batch import run_batch  # noqa: E402
from write_comparison_tex import write_comparison_tex  # noqa: E402

_DEFAULT_RELEASE = Path(r"C:\Work\Examples-Release")
_DEFAULT_BASE = Path(r"C:\Work\Examples-Base")
_MUTVERSION = re.compile(r"MUTVersion\s*=\s*'([^']+)'")


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Run VerificationFolder.List models in Examples-Release, export "
            "BatchComparisonPlots.lay frames, and write the User's Guide appendix."
        )
    )
    parser.add_argument(
        "--release-dir",
        type=Path,
        default=_DEFAULT_RELEASE,
        help=r"Working/run tree (default: C:\Work\Examples-Release)",
    )
    parser.add_argument(
        "--base-dir",
        type=Path,
        default=_DEFAULT_BASE,
        help=r"Previous-version results (default: C:\Work\Examples-Base)",
    )
    parser.add_argument(
        "--guide-dir",
        type=Path,
        default=_REPO / "Docs" / "User's Guide",
        help="User's Guide directory (default: Docs/User's Guide in this repo)",
    )
    parser.add_argument(
        "--skip-batch",
        action="store_true",
        help="Do not run mut _build / usgs_1 / mut _post for each listed folder",
    )
    parser.add_argument(
        "--skip-export",
        action="store_true",
        help="Do not run tec360 PNG export",
    )
    parser.add_argument(
        "--skip-pdf",
        action="store_true",
        help="Write TeX only; do not run Docs/build_mut_guide.bat",
    )
    parser.add_argument(
        "--batch-timeout",
        type=int,
        default=3600,
        help="Seconds per model folder for mut/usgs (default: 3600)",
    )
    parser.add_argument(
        "--export-timeout",
        type=int,
        default=1800,
        help="Seconds to wait for tec360 frame export (default: 1800)",
    )
    return parser.parse_args(argv)


def mut_version_from_source() -> str | None:
    gr = _REPO / "GeneralRoutines.f90"
    if not gr.is_file():
        return None
    text = gr.read_text(encoding="utf-8", errors="replace")
    match = _MUTVERSION.search(text)
    if not match:
        return None
    raw = match.group(1).strip()
    return re.sub(r"\s+(DEBUG|RELEASE)\s*$", "", raw, flags=re.IGNORECASE).strip()


def build_guide_pdf(guide_dir: Path) -> tuple[bool, str]:
    bat = _REPO / "Docs" / "build_mut_guide.bat"
    if not bat.is_file():
        return False, f"missing {bat}"
    try:
        proc = subprocess.run(
            ["cmd", "/c", str(bat)],
            cwd=str(bat.parent),
            capture_output=True,
            text=True,
            timeout=180,
        )
    except subprocess.TimeoutExpired:
        return False, "build_mut_guide.bat timed out"
    except OSError as exc:
        return False, f"failed to start pdflatex script: {exc}"
    if proc.returncode != 0:
        tail = (proc.stdout or proc.stderr or "")[-800:]
        return False, f"guide PDF failed:\n{tail}"
    pdf = guide_dir / "MUT User's Guide.pdf"
    if pdf.is_file():
        return True, str(pdf)
    return False, "pdflatex finished but PDF is missing"


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    release_dir = args.release_dir.resolve()
    base_dir = args.base_dir.resolve()
    guide_dir = args.guide_dir.resolve()
    layout = release_dir / "BatchComparisonPlots.lay"
    batch_file = release_dir / "VerificationFolder.List"
    tex_path = guide_dir / "ReleaseComparison.tex"
    imagery_dir = guide_dir / "Imagery" / "verification"

    if not release_dir.is_dir():
        print(f"error: release dir not found: {release_dir}", file=sys.stderr)
        return 2
    if not batch_file.is_file():
        print(f"error: {batch_file} not found", file=sys.stderr)
        return 2
    if not layout.is_file():
        print(f"error: {layout} not found", file=sys.stderr)
        return 2

    folders = parse_folder_list(batch_file)
    print(f"release: {release_dir}")
    print(f"baseline: {base_dir}")
    print(f"models in VerificationFolder.List: {len(folders)}")
    print("note: never copying MUT_Examples into the release tree")

    batch_results: list[tuple[str, bool, str]] = []
    if args.skip_batch:
        print("skipping verification batch")
    else:
        print(
            "running verification batch (mut _build / usgs_1 / mut _post per folder); "
            f"progress also in {release_dir / '_verify_batch.log'}",
            flush=True,
        )
        batch_results, summary = run_batch(
            release_dir, folders, timeout_s=args.batch_timeout
        )
        print(f"batch: {summary}")
        for folder, ok, msg in batch_results:
            print(f"  {'ok' if ok else 'FAIL'}  {folder}  {msg}")

    frames = parse_layout_frames(layout)
    print(f"layout frames: {len(frames)}")

    exported: list[tuple[int, str, Path]] = []
    export_note = ""
    if args.skip_export:
        export_note = "Tecplot PNG export skipped"
        print("skipping Tecplot PNG export")
        for number, name in frames:
            png = imagery_dir / f"{slug(name)}.png"
            if png.is_file():
                exported.append((number, name, png))
    else:
        ok, export_note, exported = export_comparison_frames(
            layout, imagery_dir, release_dir, timeout_s=args.export_timeout
        )
        print(("export: " if ok else "export skipped: ") + export_note)

    if not base_dir.is_dir():
        print(f"warning: baseline dir not found: {base_dir}", file=sys.stderr)

    budget_rows = [compare_folder(release_dir, base_dir, folder) for folder in folders]
    current_version = first_version(budget_rows, "current_version") or mut_version_from_source()
    base_version = first_version(budget_rows, "base_version")

    write_comparison_tex(
        tex_path,
        imagery_dir,
        frames,
        exported,
        budget_rows,
        folders,
        current_version,
        base_version,
        export_note,
    )
    print(f"wrote {tex_path}")

    if args.skip_pdf:
        print("skipping User's Guide PDF")
        return 0

    ok, msg = build_guide_pdf(guide_dir)
    print(("pdf: " if ok else "pdf failed: ") + msg)
    return 0


if __name__ == "__main__":
    sys.exit(main())
