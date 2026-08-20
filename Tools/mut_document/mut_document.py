#!/usr/bin/env python3
"""Create a model-folder documentation PDF and Tecplot layouts after mut _build.

Run from a MUT model directory (or pass the path):

    python mut_document.py
    python mut_document.py C:\\path\\to\\model
    python mut_document.py --skip-export --skip-pdf
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

_HERE = Path(__file__).resolve().parent
if str(_HERE) not in sys.path:
    sys.path.insert(0, str(_HERE))

from build_pdf import build_pdf  # noqa: E402
from export_figures import export_figures  # noqa: E402
from inventory import scan_model_folder  # noqa: E402
from parse_mut import parse_build  # noqa: E402
from parse_usg import parse_usg  # noqa: E402
from write_layouts import write_layouts  # noqa: E402
from write_tex import write_tex  # noqa: E402


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate Docs/<folder>.pdf and Tecplot layouts for a MUT model folder."
    )
    parser.add_argument(
        "model_dir",
        nargs="?",
        default=".",
        help="Model folder containing _build.mut (default: current directory)",
    )
    parser.add_argument(
        "--skip-export",
        action="store_true",
        help="Do not run tec360 to export PNG figures",
    )
    parser.add_argument(
        "--skip-pdf",
        action="store_true",
        help="Write TeX and layouts only; do not run pdflatex",
    )
    parser.add_argument(
        "--export-timeout",
        type=int,
        default=600,
        help="Seconds to wait for tec360 batch export (default: 600)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    model_dir = Path(args.model_dir).resolve()
    if not model_dir.is_dir():
        print(f"error: not a directory: {model_dir}", file=sys.stderr)
        return 2

    inv = scan_model_folder(model_dir)
    if inv.build_mut is None:
        print(f"error: no _build.mut in {model_dir}", file=sys.stderr)
        return 2
    if not inv.has_build:
        print(
            "warning: _build.mut is present but _buildo.eco / Tecplot build files "
            "were not found; run mut _build first for a complete dossier",
            file=sys.stderr,
        )

    # Never touch hand-made layouts in the model root
    for protected in ("_build.lay", "_post.lay"):
        path = model_dir / protected
        if path.is_file():
            print(f"note: leaving {protected} unchanged")

    print(f"model: {inv.folder_name}")
    print(f"domains: {', '.join(inv.domains) or '(none)'}")
    print(f"build: {inv.has_build}  usgs: {inv.has_usgs}  post: {inv.has_post}")
    fe_stems = {n.lower() for n in ("GWF", "SWF", "CLN")}
    fe_stems.update(f"{n}.velocity" for n in ("GWF", "SWF", "CLN"))
    fe_stems.update(f"{n}_velocity" for n in ("GWF", "SWF", "CLN"))
    for table in (inv.build_tecplot, inv.post_tecplot):
        for key, path in table.items():
            if key.lower() not in fe_stems:
                continue
            if path.name.lower().endswith(".tecplot.dat"):
                szplt = path.with_suffix(".szplt").name
                print(
                    f"note: {path.name} is ASCII; current MUT writes {szplt} "
                    "for this file. Re-run mut _build / mut _post to load SZL "
                    "instead of converting .dat."
                )

    build = parse_build(inv)
    usg = parse_usg(inv)
    layouts = write_layouts(inv, usg, build)
    print(f"wrote {len(layouts)} layout file(s) under {inv.layouts_dir}")

    if args.skip_export:
        print("skipping Tecplot PNG export")
    else:
        ok, msg = export_figures(inv, layouts, timeout_s=args.export_timeout)
        print(("export: " if ok else "export skipped: ") + msg)

    tex_path = write_tex(inv, build, usg, layouts)
    print(f"wrote {tex_path}")

    if args.skip_pdf:
        print("skipping pdflatex")
        return 0

    ok, msg, pdf = build_pdf(tex_path)
    print(("pdf: " if ok else "pdf failed: ") + msg)
    if pdf:
        print(f"PDF: {pdf}")
        return 0
    return 1


if __name__ == "__main__":
    sys.exit(main())
