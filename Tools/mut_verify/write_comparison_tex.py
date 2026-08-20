"""Emit Docs/User's Guide/ReleaseComparison.tex and copy PNGs into Imagery/verification."""

from __future__ import annotations

import shutil
from pathlib import Path

from parse_batch import frame_folder_hint, match_folder, slug

_SPECIAL = {
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
}


def tex_escape(text: str) -> str:
    return "".join(_SPECIAL.get(ch, ch) for ch in text)


def write_stub(tex_path: Path) -> None:
    """Committed placeholder so pdflatex works before a verification run."""
    tex_path.parent.mkdir(parents=True, exist_ok=True)
    tex_path.write_text(
        r"""\section{Comparison results}
This section compares the verification models listed in \texttt{VerificationFolder.List}
against the previous-version baseline (\texttt{C:\textbackslash Work\textbackslash Examples-Base}).
Solid lines are the current \mut\ release; dashed lines are the baseline.

Run \texttt{Tools/verify\_release.ps1} from the \mut\ source repository to
rebuild the batch, export the \texttt{BatchComparisonPlots.lay} frames, and
replace this placeholder with the comparison figures.
""",
        encoding="utf-8",
        newline="\n",
    )


# Durable explanations keyed by VerificationFolder.List folder (lowercase).
# These survive appendix regeneration; keep in sync with Modifications.tex 2025.014.
_FOLDER_NOTES = {
    "3_0_swf_chd": (
        r"Cause: version 2025.014 began writing both start and end heads on "
        r"\swf\ CHD records (USG requires both). The 2025.012 baseline wrote "
        r"only the assigned head. The outlet cell (node 202) now has start and "
        r"end heads both equal to the assigned value $1.000001$ (the cell "
        r"starting head already matches the CHD). Completing the two-head "
        r"record changes \texttt{OUT\_SWF CONST HEAD} through the run "
        r"(about $0.3873$ to $0.3857$~m$^3$/s at $t=163$~s). Sibling example "
        r"\texttt{3\_SWF} uses a critical-depth outlet instead of CHD and is "
        r"unchanged."
    ),
    "3_1_cln_for_swf": (
        r"Cause: the same 2025.014 CHD change, for \cln. The outlet record is "
        r"now a start-to-end ramp from the cell starting head to the assigned "
        r"CHD $1.000001$ (they differ only in the last decimal place). The "
        r"2025.012 file had a single assigned head, which USG applied as an "
        r"instantaneous shock. That produced a first-step "
        r"\texttt{IN\_CLN STORAGE} pulse of about $0.00415$ and a $1\%$ "
        r"mass-balance discrepancy; the ramp removes the shock."
    ),
}

_INTERPRETATION = r"""
Numeric status uses the ASCII volume-budget \texttt{IN - OUT} series, compared
record-by-record when both files list the same \texttt{TOTAL TIME} values.
\mfus\ often writes several budget dumps at the same time at a stress-period
boundary. Comparing by interpolated time would pair later dumps with the first
dump at that time and can false-flag models such as
\texttt{6\_Abdul\_Prism\_Cell}. With index-wise comparison those examples match
the 2025.012 baseline bit-for-bit (349 records, max $|$diff$|$ of 0).

The only remaining differences versus 2025.012 are \texttt{3\_0\_SWF\_CHD} and
\texttt{3\_1\_CLN\_for\_SWF}. From version 2025.014, \cln\ and \swf\ CHD records
in \texttt{Modflow.chd} write both a start head (the cell starting head) and an
end head (the assigned constant head). \mfus\ requires both values and
interpolates between them over the stress period so the assigned head is not
applied as an instantaneous shock. Version 2025.012 wrote only the assigned
head on those records. \gwf\ CHD records already used two heads (both equal to
the assigned value). See also the 2025.014 note in
Appendix~\ref{chapter:modifications}.
"""


def _status_phrase(status: str) -> str:
    return {
        "match": "No change",
        "differ": "Responses differ",
        "missing": "Budget files missing",
        "missing_current": "Current budget missing",
        "missing_base": "Baseline budget missing",
        "incomparable": "Could not compare",
        "not_run": "Not compared",
    }.get(status, status)


def _summary_table(rows: list[dict[str, object]]) -> str:
    if not rows:
        return ""
    lines = [
        r"\begin{center}",
        r"\begin{tabular}{lll}",
        r"\hline",
        r"Model & Status & IN--OUT max $|$diff$|$ \\",
        r"\hline",
    ]
    for row in rows:
        folder = tex_escape(str(row.get("folder", "")))
        status = tex_escape(_status_phrase(str(row.get("status", ""))))
        diff = row.get("max_abs_diff")
        diff_s = f"{diff:.3g}" if isinstance(diff, float) else "---"
        lines.append(f"{folder} & {status} & {tex_escape(diff_s)} \\\\")
    lines.extend([r"\hline", r"\end{tabular}", r"\end{center}", ""])
    return "\n".join(lines)


def write_comparison_tex(
    tex_path: Path,
    imagery_dir: Path,
    frames: list[tuple[int, str]],
    exported: list[tuple[int, str, Path]],
    budget_rows: list[dict[str, object]],
    folders: list[str],
    current_version: str | None,
    base_version: str | None,
    export_note: str,
) -> Path:
    imagery_dir.mkdir(parents=True, exist_ok=True)
    png_by_number = {number: path for number, _name, path in exported}
    budget_by_folder = {
        str(row["folder"]).lower(): row for row in budget_rows if row.get("folder")
    }

    cur = current_version or "unknown"
    base = base_version or "unknown"
    parts: list[str] = [
        r"\section{Comparison results}",
        "This section reports the verification-model comparison for the current "
        r"\mut\ release against the previous-version baseline. "
        "Each figure is one frame from \\texttt{BatchComparisonPlots.lay}: "
        "solid lines are the current run, dashed lines are the baseline. "
        "Coincident lines mean the responses match.",
        "",
        f"Current volume-budget \\texttt{{MUTVersion}}: \\texttt{{{tex_escape(cur)}}}.",
        f"Baseline volume-budget \\texttt{{MUTVersion}}: \\texttt{{{tex_escape(base)}}}.",
        "",
    ]
    skip_note = export_note.strip().lower() in {
        "",
        "tecplot png export skipped",
    }
    if export_note and not skip_note:
        parts.append(tex_escape(export_note) + ".")
        parts.append("")
    parts.append(_INTERPRETATION.strip())
    parts.append("")
    parts.append(_summary_table(budget_rows))

    def append_frame(number: int, name: str, folder: str | None) -> None:
        row = budget_by_folder.get(folder.lower()) if folder else None
        parts.append(r"\section{" + tex_escape(name) + "}")
        if row:
            parts.append(
                tex_escape(
                    _status_phrase(str(row["status"])) + ". " + str(row.get("note", ""))
                )
            )
            parts.append("")
            folder_note = _FOLDER_NOTES.get(folder.lower()) if folder else None
            if folder_note:
                parts.append(folder_note)
                parts.append("")
        png_src = png_by_number.get(number)
        stem = slug(name)
        if png_src is None:
            existing = imagery_dir / f"{stem}.png"
            if existing.is_file():
                png_src = existing
        if png_src and png_src.is_file():
            dest = imagery_dir / f"{stem}.png"
            if png_src.resolve() != dest.resolve():
                shutil.copy2(png_src, dest)
            rel = f"verification/{stem}"
            parts.append(r"\begin{figure}[htbp]")
            parts.append(r"  \centering")
            parts.append(rf"  \includegraphics[width=0.95\textwidth]{{{rel}}}")
            parts.append(rf"  \caption{{{tex_escape(name)}: current release versus baseline.}}")
            parts.append(rf"  \label{{fig:verify-{stem}}}")
            parts.append(r"\end{figure}")
            parts.append(r"\clearpage")
            parts.append("")
        else:
            parts.append(
                "Tecplot PNG export was skipped or failed for this frame. "
                r"Open \texttt{BatchComparisonPlots.lay} in \tecplot\ 360 to inspect it."
            )
            parts.append("")

    grouped: dict[str, list[tuple[int, str]]] = {name.lower(): [] for name in folders}
    unmatched: list[tuple[int, str]] = []
    for number, name in frames:
        folder = match_folder(frame_folder_hint(name), folders)
        if folder is None:
            unmatched.append((number, name))
        else:
            grouped[folder.lower()].append((number, name))

    for folder in folders:
        group = grouped.get(folder.lower(), [])
        if group:
            for number, name in group:
                append_frame(number, name, folder)
            continue
        row = budget_by_folder.get(folder.lower())
        parts.append(r"\section{" + tex_escape(folder) + "}")
        if row:
            parts.append(
                tex_escape(
                    _status_phrase(str(row["status"])) + ". " + str(row.get("note", ""))
                )
            )
            parts.append("")
        parts.append(
            r"\texttt{BatchComparisonPlots.lay} does not currently plot this model."
        )
        parts.append("")

    for number, name in unmatched:
        append_frame(number, name, None)

    tex_path.parent.mkdir(parents=True, exist_ok=True)
    tex_path.write_text("\n".join(parts).rstrip() + "\n", encoding="utf-8", newline="\n")
    return tex_path
