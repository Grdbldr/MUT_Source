"""Build the documentation PDF with pdflatex."""

from __future__ import annotations

import shutil
import subprocess
from pathlib import Path


def find_pdflatex() -> str | None:
    found = shutil.which("pdflatex")
    if found:
        return found
    return None


def build_pdf(tex_path: Path, timeout_s: int = 180) -> tuple[bool, str, Path | None]:
    engine = find_pdflatex()
    if engine is None:
        return False, "pdflatex not found on PATH (install MiKTeX)", None
    docs_dir = tex_path.parent
    pdf_path = tex_path.with_suffix(".pdf")
    cmd = [
        engine,
        "-interaction=nonstopmode",
        "-halt-on-error",
        tex_path.name,
    ]
    log_bits: list[str] = []
    for pass_no in (1, 2):
        try:
            proc = subprocess.run(
                cmd,
                cwd=str(docs_dir),
                capture_output=True,
                text=True,
                timeout=timeout_s,
            )
        except subprocess.TimeoutExpired:
            return False, f"pdflatex timed out on pass {pass_no}", None
        except OSError as exc:
            return False, f"pdflatex failed to start: {exc}", None
        log_bits.append(f"pass {pass_no} exit {proc.returncode}")
        if proc.returncode != 0:
            err = (proc.stdout or "")[-1500:]
            return False, "pdflatex failed:\n" + err, None
    if pdf_path.is_file():
        return True, "; ".join(log_bits), pdf_path
    return False, "pdflatex finished but PDF is missing", None
