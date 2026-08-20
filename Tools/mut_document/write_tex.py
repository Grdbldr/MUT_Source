"""Emit LaTeX chapters for the model-folder documentation PDF."""

from __future__ import annotations

import datetime as _dt
from itertools import groupby
from pathlib import Path

from inventory import ArtifactInventory
from parse_mut import MutBuildInfo
from parse_usg import UsgRunInfo
from tecplot_io import read_tecplot_header
from write_layouts import LayoutFile

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


def path_tex(text: str) -> str:
    return r"\path{" + text.replace("\\", "/") + "}"


def _listing(text: str, language: str = "") -> str:
    body = text.rstrip() + "\n"
    lang = f"[language={language}]" if language else ""
    return (
        r"\begin{lstlisting}" + lang + "\n"
        + body
        + r"\end{lstlisting}"
        + "\n"
    )


def _figure(rel: str, caption: str, label: str) -> str:
    return (
        "\\begin{figure}[htbp]\n"
        "  \\centering\n"
        f"  \\includegraphics[width=0.95\\textwidth]{{{rel}}}\n"
        f"  \\caption{{{tex_escape(caption)}}}\n"
        f"  \\label{{{label}}}\n"
        "\\end{figure}\n"
    )


def _maybe_figure(inv: ArtifactInventory, stem: str, caption: str, label: str) -> str:
    for ext in (".png", ".jpg", ".pdf"):
        path = inv.imagery_dir / f"{stem}{ext}"
        if path.is_file():
            return _figure(f"imagery/{path.name}", caption, label)
    return (
        f"Open the Tecplot layout {path_tex('Docs/layouts/' + stem + '.lay')} "
        "in Tecplot~360 to inspect this section. "
        "If \\texttt{tec360} is on the PATH, re-run \\texttt{mut\\_document.py} "
        "without \\texttt{--skip-export} to embed PNG figures.\n\n"
    )


def _velocity_files(inv: ArtifactInventory) -> list[tuple[str, Path]]:
    found: list[tuple[str, Path]] = []
    for domain in ("GWF", "SWF", "CLN"):
        path = inv.post_file(f"{domain}.Velocity", f"{domain}_Velocity")
        if path:
            found.append((domain, path))
    return found


def _non_xyz_names(path: Path) -> list[str]:
    names = read_tecplot_header(path).get("variables") or []
    return [n for n in names if n and n.upper() not in ("X", "Y", "Z")]


def _velocity_section(inv: ArtifactInventory, results_stem: str) -> str:
    found = _velocity_files(inv)
    if not found:
        return ""
    lines = [
        "\\section{Velocity}\n",
        "Darcy and average-linear velocity are written by \\texttt{mut \\_post} "
        "to a separate Tecplot file per domain, not the head/saturation results file. "
        "Average-linear velocity uses GWF porosity and, when present, CLN infill porosity. "
        "Vectors are the domain Velocity frame in "
        "\\texttt{Docs/layouts/\\textit{domain}\\_Results.lay} "
        "(for example \\texttt{GWF\\_Results.lay}). "
        "Assign Tecplot 3-D vectors to \\texttt{Darcy Vx/Vy/Vz} "
        "(\\texttt{\\$!GlobalThreeDVector}); turn ShowVector on only after U/V/W are set.\n\n",
    ]
    items: list[str] = []
    for domain, path in found:
        extras = _non_xyz_names(path)
        if extras:
            items.append(f"{path.name}: {', '.join(extras)}")
        else:
            items.append(
                f"{path.name}: {domain} Head, Darcy Vx/Vy/Vz, "
                "Average Linear Vx/Vy/Vz (when the linear binary exists)"
            )
    lines.append(_item_list(items))
    lines.append(
        _maybe_figure(
            inv,
            results_stem,
            "Results layout, including the velocity vector frame",
            "fig:results-vel",
        )
    )
    return "".join(lines)


def _item_list(items: list[str]) -> str:
    if not items:
        return "None recorded.\n"
    lines = ["\\begin{itemize}"]
    for item in items:
        lines.append(f"  \\item {tex_escape(item)}")
    lines.append("\\end{itemize}\n")
    return "\n".join(lines) + "\n"


def write_tex(
    inv: ArtifactInventory,
    build: MutBuildInfo,
    usg: UsgRunInfo,
    layouts: list[LayoutFile],
) -> Path:
    inv.docs_dir.mkdir(parents=True, exist_ok=True)
    inv.imagery_dir.mkdir(parents=True, exist_ok=True)
    title = inv.folder_name.replace("_", " ")
    generated = _dt.datetime.now().strftime("%Y-%m-%d %H:%M")
    main_name = inv.folder_name + ".tex"
    main_path = inv.docs_dir / main_name

    layout_by_id = {item.section_id: item for item in layouts}

    parts: list[str] = []
    parts.append(_preamble(title, inv.folder_name))
    parts.append("\\begin{document}\n")
    parts.append("\\maketitle\n\\tableofcontents\n\\newpage\n")

    parts.append(_chapter_identification(inv, build, usg, generated))
    parts.append(_chapter_problem(inv, build))
    parts.append(_chapter_data(inv, build))
    parts.append(_chapter_mesh(inv, build, layout_by_id))
    parts.append(_chapter_materials(inv, build, layout_by_id))
    parts.append(_chapter_ics(inv, build, layout_by_id))
    parts.append(_chapter_bcs(inv, build, layout_by_id))
    if inv.has_usgs or inv.has_post:
        parts.append(_chapter_results(inv, build, usg, layout_by_id))
    else:
        parts.append(
            "\\chapter{Simulation results}\n"
            "No \\texttt{usgs\\_1} listing file or \\texttt{mut \\_post} Tecplot "
            "output was found in this folder. Run \\texttt{usgs\\_1} then "
            "\\texttt{mut \\_post}; MUT will expand this chapter unless "
            "\\texttt{no model documentation} is set at post.\n"
        )

    parts.append(_chapter_layouts(layouts))
    parts.append("\\end{document}\n")
    main_path.write_text("".join(parts), encoding="utf-8")
    return main_path


def _preamble(title: str, pdf_title: str) -> str:
    return rf"""\documentclass[11pt,oneside]{{report}}
\usepackage[margin=1in]{{geometry}}
\usepackage{{graphicx}}
\usepackage{{booktabs}}
\usepackage{{longtable}}
\usepackage{{hyperref}}
\usepackage{{listings}}
\usepackage{{xcolor}}
\usepackage{{xurl}}
\usepackage{{float}}
\hypersetup{{
  colorlinks=true,
  linkcolor=blue,
  urlcolor=magenta,
  pdftitle={{{tex_escape(pdf_title)}}}
}}
\lstset{{
  basicstyle=\ttfamily\footnotesize,
  breaklines=true,
  columns=fullflexible,
  frame=single,
  showstringspaces=false,
  keepspaces=true
}}
\graphicspath{{{{imagery/}}{{imagery_user/}}}}
\title{{{tex_escape(title)}}}
\author{{MUT model documentation (automatic)}}
\date{{\today}}
"""


def _chapter_identification(
    inv: ArtifactInventory, build: MutBuildInfo, usg: UsgRunInfo, generated: str
) -> str:
    domains = ", ".join(inv.domains) if inv.domains else "none detected"
    rows = [
        ("Folder", inv.folder_name),
        ("Path", str(inv.model_dir)),
        ("MUT version", build.mut_version or "unknown"),
        ("Build date (from .eco)", build.run_date or "unknown"),
        ("Documentation generated", generated),
        ("Time units", build.time_units or "not stated"),
        ("Length units", build.length_units or "not stated"),
        ("Domains", domains),
        ("mut \\_build", "yes" if inv.has_build else "incomplete"),
        ("usgs\\_1 listing", "yes" if inv.has_usgs else "no"),
        ("mut \\_post", "yes" if inv.has_post else "no"),
    ]
    lines = [
        "\\chapter{Identification}\n",
        "This dossier was generated from MUT build artifacts in the parent folder. "
        "It summarises the numerical model construction and, when present, "
        "USG simulation and post-processed results.\n\n",
        "\\begin{tabular}{ll}\n\\toprule\nItem & Value \\\\\n\\midrule\n",
    ]
    for key, val in rows:
        use_path = ("\\" in val) or ("/" in val)
        lines.append(f"{key} & {path_tex(val) if use_path else tex_escape(val)} \\\\\n")
    lines.append("\\bottomrule\n\\end{tabular}\n\n")
    if usg.nam_header:
        lines.append("NAM header: " + tex_escape(usg.nam_header) + "\n\n")
    if usg.packages:
        lines.append("\\section{NAM packages}\n")
        lines.append("\\begin{tabular}{ll}\n\\toprule\nType & File \\\\\n\\midrule\n")
        for ptype, fname in usg.packages:
            lines.append(f"{tex_escape(ptype)} & {path_tex(fname)} \\\\\n")
        lines.append("\\bottomrule\n\\end{tabular}\n")
    return "".join(lines)


def _chapter_problem(inv: ArtifactInventory, build: MutBuildInfo) -> str:
    lines = ["\\chapter{Problem description}\n"]
    if inv.overview_tex:
        lines.append("% User-authored narrative\n\\input{overview}\n\n")
    synopsis = [
        c
        for c in build.comments
        if c and not c.startswith("-----") and "----" not in c[:8]
    ]
    if synopsis:
        lines.append("The following comments were taken from \\texttt{\\_build.mut} "
                     "(whole-line \\texttt{!} comments):\n")
        lines.append(_item_list(synopsis[:40]))
    else:
        lines.append(
            "No whole-line comments were found in \\texttt{\\_build.mut}. "
            "Add a short problem statement as comments at the top of that file, "
            "or place optional prose in \\texttt{Docs/overview.tex}.\n"
        )
    if inv.user_images:
        lines.append("\\section{User imagery}\n")
        for img in inv.user_images:
            rel = f"imagery_user/{img.name}"
            lines.append(_figure(rel, img.stem.replace("_", " "), f"fig:user-{img.stem}"))
    return "".join(lines)


def _chapter_data(inv: ArtifactInventory, build: MutBuildInfo) -> str:
    lines = [
        "\\chapter{Raw data and conceptual construction}\n",
        "Files referenced by \\texttt{\\_build.mut} and its \\texttt{include} files "
        "are listed below. GIS preparation (QGIS rasters and shapefiles) is not "
        "reproduced automatically; add figures under \\texttt{Docs/imagery\\_user/} "
        "if those steps should appear here.\n\n",
    ]
    if build.database_path:
        lines.append("Local databases: " + path_tex(build.database_path) + "\n\n")
    if build.include_files:
        lines.append("\\section{Include files}\n")
        lines.append(_item_list(build.include_files))
        for inc, excerpt in build.include_excerpts.items():
            lines.append("\\subsection{" + tex_escape(Path(inc).name) + "}\n")
            lines.append(path_tex(inc) + "\n\n")
            if excerpt.strip():
                lines.append(_listing(excerpt[:4000]))
    if build.referenced_files:
        lines.append("\\section{Referenced files}\n")
        lines.append("\\begin{tabular}{lll}\n\\toprule\nAs written & Resolved path & Present \\\\\n\\midrule\n")
        seen: set[str] = set()
        for raw, resolved, exists in build.referenced_files:
            key = raw + "|" + resolved
            if key in seen:
                continue
            seen.add(key)
            flag = "yes" if exists else "no"
            lines.append(
                f"{path_tex(raw)} & {path_tex(resolved)} & {flag} \\\\\n"
            )
        lines.append("\\bottomrule\n\\end{tabular}\n")
    return "".join(lines)


def _chapter_mesh(inv: ArtifactInventory, build: MutBuildInfo, layouts: dict[str, LayoutFile]) -> str:
    lines = ["\\chapter{Mesh}\n"]
    if build.mesh_path:
        lines.append("Template mesh: " + path_tex(build.mesh_path) + "\n\n")
    stats = []
    if build.mesh_nodes:
        stats.append(f"Nodes: {build.mesh_nodes}")
    if build.mesh_elements:
        stats.append(f"Elements: {build.mesh_elements}")
    if build.mesh_type:
        stats.append(f"Element type: {build.mesh_type}")
    if stats:
        lines.append(_item_list(stats))
    if build.layers:
        lines.append("\\section{GWF layers}\n")
        lines.append("\\begin{tabular}{llll}\n\\toprule\nName & Sublayers & Elevation & Offset \\\\\n\\midrule\n")
        for layer in build.layers:
            lines.append(
                f"{tex_escape(layer.name)} & {tex_escape(layer.n_sublayers or '')} & "
                f"{tex_escape(layer.elevation or '')} & {tex_escape(layer.offset or '')} \\\\\n"
            )
        lines.append("\\bottomrule\n\\end{tabular}\n\n")
    if inv.build_mut:
        text = inv.build_mut.read_text(encoding="utf-8", errors="replace")
        if len(text) < 12000:
            lines.append("\\section{Build instructions}\n")
            lines.append(_listing(text))
    lines.append("\\section{Visualization}\n")
    stem = inv.domains[0] if inv.domains else "GWF"
    lines.append(_maybe_figure(inv, stem, "Mesh construction", "fig:mesh"))
    return "".join(lines)


def _chapter_materials(inv: ArtifactInventory, build: MutBuildInfo, layouts: dict[str, LayoutFile]) -> str:
    lines = ["\\chapter{Materials}\n"]
    if build.materials_db:
        lines.append("\\section{Databases}\n")
        lines.append("\\begin{tabular}{ll}\n\\toprule\nDomain & File \\\\\n\\midrule\n")
        for domain, fname in build.materials_db.items():
            lines.append(f"{tex_escape(domain)} & {path_tex(fname)} \\\\\n")
        lines.append("\\bottomrule\n\\end{tabular}\n\n")
    if build.assignments:
        lines.append("\\section{Zone assignments}\n")
        for assign in build.assignments:
            title = f"{assign.domain} zone {assign.zone or '?'} → material {assign.material_number}"
            if assign.material_name:
                title += f" ({assign.material_name})"
            lines.append("\\subsection{" + tex_escape(title) + "}\n")
            if assign.n_cells:
                lines.append(f"Cells assigned: {tex_escape(assign.n_cells)}\n\n")
            if assign.properties:
                lines.append(_item_list(assign.properties))
    lines.append("\\section{Visualization}\n")
    stem = f"{inv.domains[0]}_Variables" if inv.domains else "GWF_Variables"
    lines.append(_maybe_figure(inv, stem, "Material properties", "fig:materials"))
    return "".join(lines)


def _chapter_ics(inv: ArtifactInventory, build: MutBuildInfo, layouts: dict[str, LayoutFile]) -> str:
    lines = ["\\chapter{Initial conditions}\n"]
    if build.ics:
        lines.append(_item_list(build.ics))
    else:
        lines.append("No initial-condition summary was parsed from \\texttt{\\_buildo.eco}.\n")
    lines.append("\\section{Visualization}\n")
    stem = f"{inv.domains[0]}_Variables" if inv.domains else "GWF_Variables"
    lines.append(_maybe_figure(inv, stem, "Initial conditions", "fig:ics"))
    return "".join(lines)


def _chapter_bcs(inv: ArtifactInventory, build: MutBuildInfo, layouts: dict[str, LayoutFile]) -> str:
    lines = ["\\chapter{Stress periods, boundary conditions, and output control}\n"]
    if build.stress_periods:
        lines.append("\\section{Stress periods}\n")
        lines.append("\\begin{tabular}{llll}\n\\toprule\nNo. & Type & Duration & Max step \\\\\n\\midrule\n")
        for sp in build.stress_periods:
            lines.append(
                f"{sp.index} & {tex_escape(sp.type or '')} & "
                f"{tex_escape(sp.duration or '')} & {tex_escape(sp.tmaxat or '')} \\\\\n"
            )
        lines.append("\\bottomrule\n\\end{tabular}\n\n")
    if build.recharge:
        lines.append("\\section{Recharge}\n")
        lines.append(tex_escape(build.recharge) + "\n\n")
    if build.bcs:
        lines.append("\\section{Boundary conditions}\n")
        show_sp = any(item.stress_period is not None for item in build.bcs)
        for sp, group in groupby(build.bcs, key=lambda item: item.stress_period):
            texts = [item.text for item in group]
            if show_sp:
                heading = f"Stress period {sp}" if sp is not None else "General"
                lines.append(f"\\subsection{{{heading}}}\n")
            lines.append(_item_list(texts))
    if build.observations:
        lines.append("\\section{Observation points}\n")
        lines.append(
            "\\begin{longtable}{llll}\n\\toprule\nDomain & Name & Cell & Location \\\\\n\\midrule\n"
        )
        for obs in build.observations:
            lines.append(
                f"{tex_escape(obs.domain)} & {tex_escape(obs.name)} & "
                f"{tex_escape(obs.cell or '')} & {tex_escape((obs.xyz or '')[:60])} \\\\\n"
            )
        lines.append("\\bottomrule\n\\end{longtable}\n\n")
    if build.oc_times:
        lines.append("\\section{Output control times}\n")
        lines.append(_item_list(build.oc_times))
    if build.sms_set:
        lines.append("\\section{Solver}\n")
        lines.append("SMS parameter set: " + tex_escape(build.sms_set) + "\n\n")
    lines.append("\\section{Visualization}\n")
    scatter_stem = None
    for candidate in ("GWF_scatter", "SWF_scatter", "CLN_scatter"):
        if (inv.layouts_dir / f"{candidate}.lay").is_file():
            scatter_stem = candidate
            break
    vis_stem = scatter_stem or (
        "GWF_Results" if "GWF" in inv.domains else (inv.domains[0] + "_Results" if inv.domains else "GWF_Results")
    )
    lines.append(_maybe_figure(inv, vis_stem, "Boundary conditions and observations", "fig:bcs"))
    return "".join(lines)


def _fmt_num(val: float | None, spec: str = ".4g") -> str:
    if val is None:
        return "n/a"
    return format(val, spec)


def _chapter_results(
    inv: ArtifactInventory,
    build: MutBuildInfo,
    usg: UsgRunInfo,
    layouts: dict[str, LayoutFile],
) -> str:
    lines = ["\\chapter{Simulation results}\n"]
    lines.append(
        "This chapter reports USG run status and post-processed fields. "
        "The checks below are inventory numbers, not a scientific interpretation.\n\n"
    )
    lines.append("\\section{Run status}\n")
    status_items = [
        f"Terminated successfully: {'yes' if usg.terminated_ok else 'unknown / not indicated in listing tail'}",
        f"Run end: {usg.run_end or 'not found'}",
        f"Elapsed: {usg.elapsed or 'not found'}",
    ]
    if usg.last_times:
        for domain, t in usg.last_times.items():
            status_items.append(f"Last {domain} solution time: {t:g}")
    lines.append(_item_list(status_items))

    lines.append("\\section{Physical realism: volumetric budget}\n")
    if usg.budget:
        b = usg.budget
        lines.append(
            "At the last tabulated budget time "
            f"({tex_escape(_fmt_num(b.time))}):\n"
        )
        items = [
            f"IN recharge: {_fmt_num(b.in_recharge)}",
            f"OUT SWBC: {_fmt_num(b.out_swbc)}",
            f"IN total: {_fmt_num(b.in_total)}",
            f"OUT total: {_fmt_num(b.out_total)}",
            f"IN minus OUT: {_fmt_num(b.in_minus_out)}",
            f"Percent discrepancy: {_fmt_num(b.percent_discrepancy)}",
        ]
        if usg.budget_max_abs_disc is not None:
            items.append(f"Maximum |percent discrepancy| over the run: {_fmt_num(usg.budget_max_abs_disc)}")
        lines.append(_item_list(items))
        if b.in_recharge and b.out_swbc and b.in_recharge != 0:
            ratio = b.out_swbc / b.in_recharge
            lines.append(
                "Late-time SWBC outflow / recharge inflow = "
                + tex_escape(f"{ratio:.4f}")
                + ". Values near 1 indicate approximate equilibrium between applied "
                "rainfall and outlet discharge (storage change small).\n\n"
            )
        if b.percent_discrepancy is not None and abs(b.percent_discrepancy) < 1.0:
            lines.append(
                "The final percent discrepancy is small ($<1\\%$), which is consistent "
                "with an acceptable fluid-mass balance for this run.\n\n"
            )
        elif b.percent_discrepancy is not None:
            lines.append(
                "The final percent discrepancy is not negligible; inspect the volume-budget "
                "layout and the listing file before relying on the heads.\n\n"
            )
    else:
        lines.append(
            "No \\texttt{\\_posto.modflow.VolumeBudget.tecplot.dat} file was found. "
            "Run \\texttt{mut \\_post} after the USG simulation.\n\n"
        )
    if usg.lst_final_discrepancy is not None:
        lines.append(
            f"Listing-file tail percent discrepancy: {tex_escape(_fmt_num(usg.lst_final_discrepancy))}.\n\n"
        )

    gwf = "GWF_Results" if (inv.layouts_dir / "GWF_Results.lay").is_file() else (
        "GWF" if "GWF" in inv.domains else (inv.domains[0] if inv.domains else "GWF")
    )
    swf = "SWF_Results" if (inv.layouts_dir / "SWF_Results.lay").is_file() else gwf
    lines.append("\\section{Heads, saturation, and depth}\n")
    lines.append(_maybe_figure(inv, gwf, "Simulated heads / saturation / depth", "fig:results-heads"))

    budget_stem = "GWF_VolumeBudget" if (inv.layouts_dir / "GWF_VolumeBudget.lay").is_file() else gwf
    lines.append("\\section{Volume budget plot}\n")
    lines.append(_maybe_figure(inv, budget_stem, "Volumetric water budget", "fig:results-budget"))

    if usg.obs_final:
        lines.append("\\section{Observation points (final tabulated values)}\n")
        for key, row in usg.obs_final.items():
            lines.append("\\subsection{" + tex_escape(key) + "}\n")
            # Show a compact subset: time + head/depth columns
            show = list(row.items())[:12]
            lines.append(_item_list([f"{k}: {_fmt_num(v)}" for k, v in show]))
        obs_stem = "GWF_Observations" if (inv.layouts_dir / "GWF_Observations.lay").is_file() else gwf
        lines.append(_maybe_figure(inv, obs_stem, "Observation time series", "fig:results-obs"))

    vel_stem = gwf
    if not inv.post_file("GWF.Velocity") and inv.post_file("SWF.Velocity"):
        vel_stem = swf
    lines.append(_velocity_section(inv, vel_stem))

    lines.append(_derived_views_section(inv))

    return "".join(lines)


def _derived_views_section(inv: ArtifactInventory) -> str:
    """Unique 3-D result views from the User's Guide, only when the layout exists."""
    has_wt = (inv.layouts_dir / "GWF_WaterTable.lay").is_file()
    has_inf = (inv.layouts_dir / "SWF_Infiltration.lay").is_file()
    has_slices = (inv.layouts_dir / "GWF_SaturationSlices.lay").is_file()
    if not (has_wt or has_inf or has_slices):
        return ""
    lines = [
        "\\section{Derived 3-D views}\n",
        "These layouts follow the Tecplot recipes in the MUT User's Guide "
        "(water-table isosurface, SWF--GWF infiltration map, saturation slices). "
        "A subsection appears only when the required post-process variables exist.\n\n",
    ]
    if has_wt:
        lines.append("\\subsection{Water table isosurface}\n")
        lines.append(
            "Pressure head is computed as \\texttt{GWF Head} minus \\texttt{GWF z Cell}. "
            "The iso-surface is drawn at pressure head $=0$ and flood-coloured by hydraulic head.\n\n"
        )
        lines.append(
            _maybe_figure(
                inv,
                "GWF_WaterTable",
                "Water table isosurface (pressure head = 0), flooded by GWF Head",
                "fig:results-watertable",
            )
        )
    if has_inf:
        lines.append("\\subsection{Infiltration}\n")
        lines.append(
            "Cell-by-cell SWF-to-GWF flux (areal flux when present) is converted to "
            "millimetres per year, with a leading minus so infiltration is positive. "
            "The legend uses a diverging blue/red colour map in Banded distribution, "
            "centred on zero: red is infiltration, blue is groundwater discharge to "
            "the surface domain.\n\n"
        )
        lines.append(
            _maybe_figure(
                inv,
                "SWF_Infiltration",
                "Infiltration [mm/year]; red = infiltration, blue = GWF discharge to SWF",
                "fig:results-infiltration",
            )
        )
    if has_slices:
        lines.append("\\subsection{Saturation slices}\n")
        lines.append(
            "X-plane slices through the GWF domain, flood-coloured by saturation, "
            "with the primary slice at mid-$X$ and five intermediate slices "
            "between start and end locations inset from the data extents.\n\n"
        )
        lines.append(
            _maybe_figure(
                inv,
                "GWF_SaturationSlices",
                "GWF saturation slices",
                "fig:results-satslices",
            )
        )
    return "".join(lines)


def _chapter_layouts(layouts: list[LayoutFile]) -> str:
    example = layouts[0].path.name if layouts else "GWF.lay"
    lines = [
        "\\chapter{Tecplot layouts}\n",
        "Generated layouts live under \\texttt{Docs/layouts/} and are not written "
        "over existing \\texttt{\\_build.lay} or \\texttt{\\_post.lay} files. "
        "Each domain has Tecplot layouts (Tecplot~360 EX 2018 does not "
        "accept multi-page \\texttt{\\$!PAGECONTROL} commands): "
        "\\texttt{GWF\\_Mesh.lay} (also copied as \\texttt{GWF.lay}), "
        "\\texttt{GWF\\_Variables.lay}, \\texttt{GWF\\_scatter.lay}, "
        "\\texttt{GWF\\_Results.lay}, and "
        "\\texttt{GWF\\_Observations.lay}, "
        "and the same pattern for SWF and CLN. "
        "The mesh, variables, and results layouts load finite-element "
        "\\texttt{.tecplot.szplt} files when present (ASCII \\texttt{.tecplot.dat} "
        "if requested). Scatter, observation, and volume-budget files remain ASCII. "
        "The mesh layout colours zones with named legends, mesh lines, and lighting. "
        "The variables layout has one frame per non-$xyz$ build field. "
        "The scatter layout overlays every build-time point file on one page "
        "(domain \\texttt{\\_CELLS} at the back, boundary conditions and observation "
        "locations in transparent frames on top, linked 3-D view). "
        "CELLS uses 0.5-size spheres; other sets use size 1.0. "
        "Each layer has a scatter-symbol legend stacked at the upper left, "
        "with $X$ shifted so the spheres line up vertically. "
        "Observation-point frames label each sphere with its assigned name "
        "to the right of the symbol. "
        "The results layout has one frame per post-processed field, plus velocity. "
        "The observations layout has one frame per series: square frames, head on the top row, "
        "saturation (or depth) on the bottom, sites left-to-right alphabetically "
        "(further columns may sit off the initial paper view). "
        "When post-process fields allow it, additional layouts are written: "
        "\\texttt{GWF\\_VolumeBudget.lay} (listing-file volumetric rates, own page), "
        "\\texttt{GWF\\_WaterTable.lay} (pressure-head iso-surface at zero), "
        "\\texttt{SWF\\_Infiltration.lay} (SWF-to-GWF flux in mm/year, Banded diverging legend), "
        "and \\texttt{GWF\\_SaturationSlices.lay} (X-plane saturation slices). "
        "Cartesian 3-D frames use $3\\times$ vertical exaggeration, the Tecplot "
        "factory XYZ view ($\\psi=60$, $\\theta=240$), a contour legend in the "
        "upper left, and 8~point Helvetica bold axis titles and tick labels "
        "(Tecplot \\texttt{SizeUnits = Point}, not percent of frame). "
        "Results and derived 3-D frames show the current solution time centred "
        "below the title, with the zone auxiliary time units "
        "(\\texttt{\\&(solutiontime) \\&(AUXZONE[1]:TimeUnits)}). "
        "Contour legend numbers are integers when the values are readable "
        "whole numbers (roughly 1 to $10^{6}$); otherwise two significant figures "
        "(saturation uses two decimal places). "
        "XY time-series frames use the same 8~point axis text. "
        "Frames on the variables, scatter, and results layouts share a linked 3-D view.\n\n",
        "From the model folder:\n",
        _listing(f"tec360 Docs/layouts/{example}"),
    ]
    for item in layouts:
        lines.append("\\section{" + tex_escape(item.path.name) + "}\n")
        lines.append(_item_list(item.frames) if item.frames else "No frames.\n")
    return "".join(lines)
