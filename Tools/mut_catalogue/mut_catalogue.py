"""Scan MUT project trees and write a User's Guide-style catalogue PDF.

The catalogue lists viewable files (Tecplot layouts, PowerPoint, PDF, QGIS
projects, Visual Studio, Excel, Word) with a colour per type. Link text is
the full path. Regenerate from the MUT_Source root with:

    Docs\\build_mut_catalogue.bat
"""

from __future__ import annotations

import datetime as _dt
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = REPO_ROOT / "Docs" / "Catalogue"
TEX_NAME = "MUT Catalogue.tex"

SKIP_DIR_NAMES = {".git", ".vs", "x64", "__pycache__"}
COMMENT_LIMIT = 8

_BANNER = re.compile(r"^!\s*-{3,}")
_COMMENT = re.compile(r"^\s*!(.*)$")
_NATURAL = re.compile(r"(\d+)")


@dataclass(frozen=True)
class Kind:
    heading: str
    color: str
    extensions: frozenset[str]
    singular: str
    plural: str


KINDS: tuple[Kind, ...] = (
    Kind("Tecplot layouts", "teal", frozenset({".lay"}), "Tecplot layout", "Tecplot layouts"),
    Kind("PowerPoint", "orange", frozenset({".pptx", ".ppt", ".pptm"}), "PowerPoint file", "PowerPoint files"),
    Kind("PDF", "blue", frozenset({".pdf"}), "PDF file", "PDF files"),
    Kind("QGIS projects", "green", frozenset({".qgz", ".qgs"}), "QGIS project", "QGIS projects"),
    Kind("Visual Studio", "purple", frozenset({".sln", ".vfproj"}), "Visual Studio file", "Visual Studio files"),
    Kind("Excel", "olive", frozenset({".xlsx", ".xls"}), "Excel workbook", "Excel workbooks"),
    Kind("Word", "brown", frozenset({".docx"}), "Word document", "Word documents"),
)

_EXT_KIND = {ext: kind for kind in KINDS for ext in kind.extensions}


@dataclass(frozen=True)
class RootSpec:
    path: Path
    chapter: str
    # models-plus-shared: one section per _build.mut, plus one shared section.
    # models-plus-top: one section per _build.mut, plus one section per
    # top-level folder (and the root) for everything else.
    # single: one section for the whole tree.
    mode: str
    blurb: str


ROOTS: tuple[RootSpec, ...] = (
    RootSpec(
        Path(r"C:\_repo\GrdBldr\MUT_Examples"),
        "MUT Examples",
        "models-plus-shared",
        r"Model folders and shared files under",
    ),
    RootSpec(
        Path(r"C:\_repo\GrdBldr\MUT_Source"),
        "MUT Source",
        "single",
        r"The \mut\ source repository under",
    ),
    RootSpec(
        Path(r"C:\_repo\Work_v2\KURT_Model"),
        "KURT Model",
        "models-plus-top",
        r"KAERI/KURT model folders and site data under",
    ),
    RootSpec(
        Path(r"C:\_repo\Work_v2\KAERI Report"),
        "KAERI Report",
        "single",
        r"Report sources and slides under",
    ),
)

# Non-model KURT sections, in this order, before any other leftover folders.
_KURT_EXTRA_ORDER = ("0_incoming", "1_data", "_QGIS", "")


@dataclass
class Section:
    title: str
    folder: Path
    is_model: bool
    files: list[Path] = field(default_factory=list)
    order: tuple = ()


def natural_key(text: str) -> list[tuple]:
    """Explorer-like order: case-insensitive, numeric runs compared as integers."""
    key: list[tuple] = []
    for part in _NATURAL.split(text):
        if part.isdigit():
            key.append((0, int(part)))
        else:
            key.append((1, part.casefold()))
    return key


def _whole_line_comment(line: str) -> str | None:
    """Same whole-line ``!`` rule as Tools/mut_document/parse_mut.py."""
    stripped = line.strip()
    match = _COMMENT.match(stripped)
    if match and not stripped.startswith("!!"):
        return match.group(1).strip()
    return None


def leading_comments(path: Path, limit: int = COMMENT_LIMIT) -> list[str]:
    """Up to ``limit`` leading whole-line comments, skipping dash banners."""
    try:
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return []
    found: list[str] = []
    for line in lines:
        if len(found) >= limit:
            break
        if not line.strip():
            continue
        comment = _whole_line_comment(line)
        if comment is None:
            break
        if _BANNER.match(line.strip()):
            continue
        comment = comment.replace("\ufffd", "").strip()
        if not comment or set(comment) <= set("- "):
            continue
        found.append(comment)
    return found


def _skip_dir(path: Path) -> bool:
    if path.name in SKIP_DIR_NAMES:
        return True
    try:
        return path.resolve() == OUT_DIR.resolve()
    except OSError:
        return False


def _kind_for(path: Path) -> Kind | None:
    if path.name.startswith("~$"):
        return None
    return _EXT_KIND.get(path.suffix.lower())


def _walk(root: Path) -> tuple[list[Path], list[Path]]:
    """Return (model directories, catalogued files) under root."""
    models: list[Path] = []
    files: list[Path] = []
    for dirpath, dirnames, filenames in os.walk(root):
        current = Path(dirpath)
        kept: list[str] = []
        for name in dirnames:
            child = current / name
            if not _skip_dir(child):
                kept.append(name)
        dirnames[:] = kept
        if "_build.mut" in filenames:
            models.append(current)
        for name in filenames:
            path = current / name
            if _kind_for(path) is not None:
                files.append(path)
    return models, files


def _owning_model(path: Path, models: list[Path]) -> Path | None:
    best: Path | None = None
    for model in models:
        try:
            path.relative_to(model)
        except ValueError:
            continue
        if best is None or len(model.parts) > len(best.parts):
            best = model
    return best


def _extra_title(key: str, root: RootSpec) -> str:
    if root.mode == "models-plus-shared":
        return "Shared files"
    if root.mode == "single":
        return root.path.name
    if key == "":
        return "Project root"
    return key


def _extra_order(key: str, root: RootSpec) -> tuple:
    if root.mode == "models-plus-top":
        if key in _KURT_EXTRA_ORDER:
            return (0, _KURT_EXTRA_ORDER.index(key), key.casefold())
        return (1, *natural_key(key))
    return (0, *natural_key(key))


def collect(root: RootSpec) -> list[Section]:
    models, files = _walk(root.path)
    by_model: dict[Path, Section] = {}
    extras: dict[str, Section] = {}

    if root.mode != "single":
        for model in models:
            rel = model.relative_to(root.path).as_posix()
            by_model[model] = Section(
                title=rel,
                folder=model,
                is_model=True,
                order=(0, *natural_key(rel)),
            )

    for path in files:
        model = _owning_model(path, models)
        if model is not None and root.mode != "single":
            by_model[model].files.append(path)
            continue
        if root.mode == "single":
            key = ""
        elif root.mode == "models-plus-shared":
            key = "shared"
        else:
            rel = path.relative_to(root.path)
            key = "" if len(rel.parts) == 1 else rel.parts[0]
        if key not in extras:
            title = _extra_title(key, root)
            folder = root.path if key in {"", "shared"} else root.path / key
            extras[key] = Section(
                title=title,
                folder=folder,
                is_model=False,
                order=(1, *_extra_order(key, root)),
            )
        extras[key].files.append(path)

    sections = [s for s in by_model.values() if s.files or s.is_model]
    sections.extend(s for s in extras.values() if s.files)
    for section in sections:
        section.files.sort(key=lambda p: natural_key(p.relative_to(section.folder).as_posix()))
    sections.sort(key=lambda s: s.order)
    return sections


def tex_escape(text: str) -> str:
    specials = {
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
    return "".join(specials.get(ch, ch) for ch in text)


def _makeindex_quote(text: str) -> str:
    return "".join(f'"{ch}' if ch in '!"@|' else ch for ch in text)


def index_cmd(sort: str, visible: str) -> str:
    printed = r"\texttt{" + tex_escape(visible) + "}"
    return r"\index{" + _makeindex_quote(tex_escape(sort)) + "@" + _makeindex_quote(printed) + "}"


def path_cmd(path: Path) -> str:
    """Typewriter path. Emit \\path directly so '_' is read with url catcodes."""
    text = path.resolve().as_posix()
    _check_verbatim(text, "path")
    return r"{\urlstyle{tt}\path{" + text + "}}"


def _check_verbatim(text: str, what: str) -> None:
    if any(ch in text for ch in "{}\n\r"):
        raise ValueError(f"{what} contains a character that cannot sit in a LaTeX path: {text}")
    if any(ord(ch) > 126 for ch in text):
        raise ValueError(f"{what} contains a non-ASCII character: {text}")


def link_line(kind: Kind, path: Path) -> str:
    uri = path.resolve().as_uri()
    display = path.resolve().as_posix()
    _check_verbatim(uri, "file URI")
    _check_verbatim(display, "path")
    return (
        r"{\hypersetup{urlcolor="
        + kind.color
        + ",filecolor="
        + kind.color
        + r"}\href{"
        + uri
        + r"}{\nolinkurl{"
        + display
        + "}}"
        + index_cmd(path.name, path.name)
        + r"}\par"
    )


def _count_sentence(files: list[Path]) -> str:
    phrases: list[str] = []
    for kind in KINDS:
        count = sum(1 for path in files if _kind_for(path) is kind)
        if count == 0:
            continue
        word = kind.singular if count == 1 else kind.plural
        phrases.append(f"{count} {word}")
    if not phrases:
        return ""
    if len(phrases) == 1:
        body = phrases[0]
    elif len(phrases) == 2:
        body = f"{phrases[0]} and {phrases[1]}"
    else:
        body = ", ".join(phrases[:-1]) + ", and " + phrases[-1]
    return body + "."


def _layout_groups(section: Section) -> tuple[list[Path], list[Path]]:
    project: list[Path] = []
    generated: list[Path] = []
    for path in section.files:
        if _kind_for(path) is not KINDS[0]:
            continue
        rel = path.relative_to(section.folder)
        parts = [part.casefold() for part in rel.parts]
        if len(parts) >= 2 and parts[0] == "docs" and parts[1] == "layouts":
            generated.append(path)
        else:
            project.append(path)
    return project, generated


def _section_indexes(section: Section) -> list[str]:
    lines = [index_cmd(section.title, section.title)]
    leaf = section.folder.name
    if leaf != section.title:
        lines.append(index_cmd(leaf, leaf))
    return lines


def _render_file_group(kind: Kind, paths: list[Path]) -> list[str]:
    if not paths:
        return []
    lines = [r"\subsection*{" + kind.heading + "}"]
    if kind is KINDS[0]:
        return lines  # layouts are rendered by the caller
    lines.append(r"{\footnotesize\raggedright\setlength{\parskip}{0.2\baselineskip}\setlength{\parindent}{0pt}%")
    for path in paths:
        lines.append(link_line(kind, path))
    lines.append("}")
    return lines


def _render_layouts(section: Section) -> list[str]:
    project, generated = _layout_groups(section)
    if not project and not generated:
        return []
    lines = [r"\subsection*{Tecplot layouts}"]
    groups: list[tuple[str | None, list[Path]]]
    if project and generated:
        groups = [("Project folder", project), ("Docs/layouts", generated)]
    else:
        groups = [(None, project or generated)]
    for heading, paths in groups:
        if heading:
            lines.append(r"\paragraph*{" + heading + "}")
        lines.append(r"{\footnotesize\raggedright\setlength{\parskip}{0.2\baselineskip}\setlength{\parindent}{0pt}%")
        for path in paths:
            lines.append(link_line(KINDS[0], path))
        lines.append("}")
    return lines


def _render_section(section: Section) -> list[str]:
    bookmark = tex_escape(section.title.replace("\\", "/"))
    lines = [
        r"\section{\texorpdfstring{\texttt{" + tex_escape(section.title) + "}}{" + bookmark + "}}",
        *_section_indexes(section),
        "Folder " + path_cmd(section.folder) + ".",
        "",
    ]
    if section.is_model:
        comments = leading_comments(section.folder / "_build.mut")
        if comments:
            lines.append(r"Comments at the top of \texttt{\_build.mut}:")
            lines.append(r"\begin{quote}")
            lines.append(r" \\ ".join(tex_escape(c) for c in comments))
            lines.append(r"\end{quote}")
            lines.append("")
        if (section.folder / "_post.mut").is_file():
            lines.append(r"This folder includes \texttt{\_post.mut}.")
            lines.append("")
    sentence = _count_sentence(section.files)
    if sentence:
        lines.append(sentence)
        lines.append("")
    lines.extend(_render_layouts(section))
    by_kind: dict[Kind, list[Path]] = {kind: [] for kind in KINDS}
    for path in section.files:
        kind = _kind_for(path)
        if kind is not None and kind is not KINDS[0]:
            by_kind[kind].append(path)
    for kind in KINDS[1:]:
        lines.extend(_render_file_group(kind, by_kind[kind]))
    lines.append("")
    return lines


def _legend() -> str:
    bits = []
    for kind in KINDS:
        bits.append(r"\textcolor{" + kind.color + "}{" + kind.heading + "}")
    return "Link colours: " + ", ".join(bits) + "."


def render(chapters: list[tuple[RootSpec, list[Section]]], generated: _dt.date) -> str:
    stamp = f"{generated.day} {generated.strftime('%B %Y')}"
    roots = "\\\\\n".join(path_cmd(spec.path) for spec in ROOTS)
    body: list[str] = []
    for spec, sections in chapters:
        body.append(r"\chapter{" + spec.chapter + "}")
        body.append(spec.blurb + " " + path_cmd(spec.path) + ".")
        body.append("")
        for section in sections:
            body.extend(_render_section(section))
    return r"""% GENERATED FILE - do not edit by hand.
% Regenerate from the MUT_Source root with:
%   Docs\build_mut_catalogue.bat
\documentclass[12pt,oneside,openany]{book}
\usepackage[cm]{fullpage}
\usepackage{makeidx}
\usepackage[svgnames]{xcolor}
\usepackage{hyperref}
\hypersetup{
    unicode=false,
    pdftoolbar=true,
    pdfmenubar=true,
    pdffitwindow=false,
    pdfstartview={FitH},
    pdftitle={MUT Project Catalogue},
    pdfauthor={MUT},
    pdfsubject={Catalogue of MUT projects and viewable files},
    colorlinks=true,
    breaklinks=true,
    linkcolor=blue,
    citecolor=green,
    filecolor=cyan,
    urlcolor=magenta
}
\usepackage{xurl}
\newcommand{\fpath}[1]{{\urlstyle{tt}\path{#1}}}

\newcommand{\windows}{\textsc{Microsoft Windows}}
\newcommand{\vstudio}{\textsc{Microsoft Visual Studio}}
\newcommand{\dbase}{\textsc{Microsoft Access}}
\newcommand{\excel}{\textsc{Microsoft Excel}}
\newcommand{\mut}{\textsc{Mut}}
\newcommand{\mf}{\textsc{Modflow}}
\newcommand{\mfu}{\textsc{Modflow-Usg}}
\newcommand{\mfus}{\textsc{Modflow-Usg$^{Swf}$}}
\newcommand{\tecplot}{\textsc{Tecplot}}
\newcommand{\github}{\textsc{GitHub}}
\newcommand{\ifort}{\textsc{Intel Fortran}}
\newcommand{\gb}{\textsc{Grid Builder}}
\newcommand{\gwv}{\textsc{Groundwater Vistas}}
\newcommand{\hgs}{\textsc{HydroGeoSphere}}
\newcommand{\qgis}{\textsc{QGIS}}
\newcommand{\saga}{\textsc{Saga Next Gen}}

\makeindex
\begin{document}
\setlength{\parindent}{0in}
\setlength{\parskip}{0.1in}
\setcounter{secnumdepth}{1}
\setcounter{tocdepth}{1}

\begin{titlepage}
    \vspace*{1in}
    \centering
    {\bfseries\Huge MUT Project Catalogue\par}
    \vspace{0.4in}
    {\Large """ + stamp + r"""\par}
    \vspace{0.8in}
    {\raggedright
    Viewable files in:\par
    \vspace{0.3in}
    """ + roots + r"""
    }
    \vfill
\end{titlepage}

\pagestyle{empty}
\tableofcontents
\vspace{1em}
""" + _legend() + r"""

\clearpage
\pagestyle{plain}
""" + "\n".join(body) + r"""
\printindex
\end{document}
"""


def find_tool(name: str) -> str | None:
    return shutil.which(name)


def _run(cmd: list[str], cwd: Path, timeout_s: int) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd,
        cwd=str(cwd),
        capture_output=True,
        text=True,
        timeout=timeout_s,
        check=False,
    )


def build_pdf(tex_path: Path, timeout_s: int = 180) -> None:
    engine = find_tool("pdflatex")
    indexer = find_tool("makeindex")
    if engine is None:
        raise SystemExit("pdflatex not found on PATH (install MiKTeX)")
    if indexer is None:
        raise SystemExit("makeindex not found on PATH (install MiKTeX)")
    cwd = tex_path.parent
    passes = (
        [engine, "-interaction=nonstopmode", "-halt-on-error", "-file-line-error", tex_path.name],
        [indexer, tex_path.stem],
        [engine, "-interaction=nonstopmode", "-halt-on-error", "-file-line-error", tex_path.name],
        [engine, "-interaction=nonstopmode", "-halt-on-error", "-file-line-error", tex_path.name],
    )
    for cmd in passes:
        label = Path(cmd[0]).name
        print(f"Running {label} {' '.join(cmd[1:])}")
        try:
            proc = _run(cmd, cwd, timeout_s)
        except subprocess.TimeoutExpired as exc:
            raise SystemExit(f"{label} timed out") from exc
        if proc.returncode != 0:
            log = (proc.stdout or "") + "\n" + (proc.stderr or "")
            tail = log[-2500:]
            raise SystemExit(f"{label} failed (exit {proc.returncode}):\n{tail}")
    pdf = tex_path.with_suffix(".pdf")
    if not pdf.is_file():
        raise SystemExit("pdflatex finished but the PDF is missing")
    print(f"Wrote {pdf}")


def main() -> None:
    chapters: list[tuple[RootSpec, list[Section]]] = []
    for spec in ROOTS:
        if not spec.path.is_dir():
            raise SystemExit(f"Catalogue root not found: {spec.path}")
        sections = collect(spec)
        n_files = sum(len(section.files) for section in sections)
        print(f"{spec.chapter}: {len(sections)} sections, {n_files} files")
        chapters.append((spec, sections))
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    tex_path = OUT_DIR / TEX_NAME
    tex_path.write_text(render(chapters, _dt.date.today()), encoding="utf-8")
    print(f"Wrote {tex_path}")
    build_pdf(tex_path)


if __name__ == "__main__":
    try:
        main()
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
