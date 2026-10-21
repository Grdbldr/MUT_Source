"""Markdown run report: project, grids, sources with accuracy and datum, statistics, caveats."""

from __future__ import annotations

import re
from datetime import datetime
from pathlib import Path

from .boundary import read_properties
from .geo import Grid

LABELS = {
    "ground_top": "Ground surface (top of surficial layer)",
    "ground_top_focus": "Ground surface, focus area",
    "bedrock_bottom": "Bedrock surface (bottom of surficial layer)",
    "surficial_thickness": "Surficial layer thickness",
    "ground_top_water_surface": "Ground surface with lake water surfaces (DEM before bathymetry)",
    "ground_top_focus_water_surface": "Ground surface with lake water surfaces (DEM before bathymetry)",
    "lake_depth": "Lake depth below the water surface",
}
MAX_LAKE_ROWS = 40
METHOD_TEXT = {"measured": "measured survey", "estimated": "ESTIMATED", "local": "local file"}


def bathymetry_lines(project, result) -> list[str]:
    """The "Lake bathymetry" section: which lakes were lowered, how, and the caveats."""
    base = project.output_path
    opts = project.options
    lines = ["## Lake bathymetry", ""]
    if not opts.lake_bathymetry:
        return lines + ["Off: lakes show the water surface of the DEM (`lake_bathymetry: false`).", ""]
    stats = result.stats.get("bathymetry")
    if stats and stats.get("error"):
        return lines + [
            f"**Bathymetry was NOT applied: {stats['error']}. Lakes show the DEM water surface; run again to retry.**",
            "",
        ]
    lakes = [l for l in (stats or {}).get("lakes", []) if l["method"] != "skipped"]
    if not lakes:
        return lines + [f"No lake of at least {opts.lake_min_area_ha:g} ha was corrected; lakes show the DEM water surface.", ""]
    counts = stats.get("counts", {})
    detail = ", ".join(f"{n} {METHOD_TEXT.get(m, m)}" for m, n in sorted(counts.items()))
    water = next((p for k, p in result.outputs.items() if k.endswith("_water_surface")), None)
    lines += [
        f"**Bathymetry was applied: the ground surface in {len(lakes)} lake(s) is the lake bottom, not the water "
        f"surface ({detail}).**",
        "",
        "Lidar DEMs show lakes as a flat water surface. For each lake the water level is the median DEM value inside "
        "it, and the bottom is that level minus the depth.",
    ]
    if water is not None:
        lines.append(f"The DEM before the correction is kept as `{_rel(water, base)}`.")
    if result.lakes_file:
        lines.append(f"Lake outlines with these attributes: `{_rel(result.lakes_file, base)}`.")
    lines += [
        "",
        "| Lake | Area (ha) | Water level (m) | DEM flat (%) | Depth from | Max depth (m) | Mean depth (m) | Notes |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for lake in sorted(lakes, key=lambda l: -l["area_ha"])[:MAX_LAKE_ROWS]:
        method = " + ".join(METHOD_TEXT.get(m, m) for m in lake["method"].split("+"))
        note = lake["note"] or (f"{lake['samples']} survey samples" if lake["samples"] else "")
        lines.append(
            f"| {lake['name']} | {lake['area_ha']:,.1f} | {lake['water_level_m']:.2f} | {lake['flat_percent']:.0f} | "
            f"{method} | {lake['max_depth_m']:.1f} | {lake['mean_depth_m']:.1f} | {note} |"
        )
    if len(lakes) > MAX_LAKE_ROWS:
        lines.append(f"| ... {len(lakes) - MAX_LAKE_ROWS} smaller lakes, see the lakes file | | | | | | | |")
    lines += ["", "Caveats:", ""]
    if counts.get("estimated"):
        lines.append(
            "- ESTIMATED lake bottoms are not surveys. Depth rises linearly with distance from shore to a maximum depth "
            "predicted from the slope of the surrounding land (Hollister et al., 2011). Depth errors of tens of percent or more "
            "are to be expected, and dammed or dredged lakes do not follow the shape of the land around them. Replace them with measured bathymetry "
            "(data role `bathymetry_elevation` or `bathymetry_depth_m`) when available."
        )
    if counts.get("measured"):
        lines.append(
            "- Measured depths are below the water level at the time of the survey (many Ontario surveys date from the "
            "1950s-1990s). Regulated reservoirs may have been at a different level then than at the lidar flight."
        )
    clipped = [l["name"] for l in stats.get("lakes", []) if l["method"] == "skipped" and "beyond the grid" in l["note"]]
    if clipped:
        lines.append(
            f"- Not corrected because they extend beyond the grid and have no survey here: {', '.join(clipped[:10])}"
            + (f" and {len(clipped) - 10} more" if len(clipped) > 10 else "")
            + ". They keep the DEM water surface."
        )
    lines.append(
        f"- Only lakes, ponds and reservoirs of at least {opts.lake_min_area_ha:g} ha mapped in OpenStreetMap are "
        "corrected; rivers keep the DEM surface. A low 'DEM flat' percentage means the DEM water surface is noisy "
        "and the water level less certain."
    )
    return lines + [""]


def _m(value) -> str:
    return "" if value is None else f"{value:.2f}"


def _target_text(target: dict | None) -> str:
    if not target:
        return "no published target found"
    parts = [
        f"{t['when']}: {t['elevation_m']:.2f} m"
        if t.get("label", "Target lake level") == "Target lake level"
        else f"{t['label']} ({t['when']}): {t['elevation_m']:.2f} m"
        for t in target.get("targets", [])
    ]
    if target.get("normal_high_m") is not None:
        parts.append(f"normal high {target['normal_high_m']:.2f} m")
    if target.get("maximum_high_m") is not None and not any(
        abs(t["elevation_m"] - target["maximum_high_m"]) < 1e-6 for t in target.get("targets", [])
    ):
        parts.append(f"maximum {target['maximum_high_m']:.2f} m")
    return "; ".join(parts) if parts else "rule curve not published as numbers"


def dams_lines(result) -> list[str]:
    """The "Dams and reservoir levels" section: dams on corrected lakes, measured levels and published targets."""
    stats = result.stats.get("dams")
    if stats is None:
        return []
    lines = ["## Dams and reservoir levels", ""]
    if stats.get("error"):
        return lines + [f"Dam and reservoir level lookup failed: {stats['error']}. Run again to retry.", ""]
    dams = stats.get("dams", [])
    if not dams:
        return lines + ["No dam was found within 300 m of a corrected lake.", ""]
    lines += [
        "Dams within 300 m of a corrected lake. The DEM water level is the lake surface in the lidar DEM, i.e. the "
        "level on the flight date; the lake bottoms above are relative to it.",
        "",
        "| Dam | Owner | Lake | DEM water level (m) | Measured level: current / median / 5-95% range (m) | "
        "DEM level percentile | Published targets |",
        "|---|---|---|---|---|---|---|",
    ]
    ranked = sorted(dams, key=lambda d: (d.get("target") is None, d.get("levels") is None, d["name"].startswith("("), d["name"]))
    for dam in ranked[:MAX_LAKE_ROWS]:
        lv = dam.get("levels")
        measured = (
            f"{lv['current_m']:.2f} / {lv['median_m']:.2f} / {lv['p5_m']:.2f}-{lv['p95_m']:.2f}" if lv else ""
        )
        pct = f"{lv['dem_percentile']:.0f}" if lv and lv.get("dem_percentile") is not None else ""
        lines.append(
            f"| {dam['name']} | {dam.get('owner', '')} | {dam['lake']} | {_m(dam.get('dem_level_m'))} | {measured} | "
            f"{pct} | {_target_text(dam.get('target'))} |"
        )
    if len(ranked) > MAX_LAKE_ROWS:
        lines.append(f"| ... {len(ranked) - MAX_LAKE_ROWS} more dams and weirs without levels or targets | | | | | | |")
    lines.append("")
    for dam in ranked:
        target, lv = dam.get("target"), dam.get("levels")
        if not target and not lv:
            continue
        text = [f"**{dam['name']}** ({dam['lake']})."]
        if target and target.get("targets"):
            text.append("Published targets: " + _target_text(target) + ".")
        if target and target.get("notes"):
            text.append(target["notes"])
        if dam.get("dem_level_m") is not None:
            text.append(f"DEM water surface {dam['dem_level_m']:.2f} m.")
        if lv:
            text.append(
                f"Measured at {dam['station']} ({lv['from']} to {lv['to']}): current {lv['current_m']:.2f} m "
                f"({lv['current_time']}), median {lv['median_m']:.2f} m, range {lv['min_m']:.2f}-{lv['max_m']:.2f} m"
                + (f"; the DEM level is at the {lv['dem_percentile']:.0f}th percentile." if lv.get("dem_percentile") is not None else ".")
            )
            monthly = lv.get("monthly_median_m") or {}
            if monthly:
                months = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()
                text.append(
                    "Monthly medians: " + ", ".join(f"{months[int(m) - 1]} {v:.2f}" for m, v in sorted(monthly.items(), key=lambda kv: int(kv[0]))) + " m."
                )
        if target and target.get("citation"):
            text.append(f"Targets: {target['citation']}")
        lines += [" ".join(text), ""]
    if stats.get("unmatched"):
        names = stats["unmatched"]
        lines += [
            f"Other inventoried dams in the grid, not on a corrected lake: {', '.join(names[:30])}"
            + (f" and {len(names) - 30} more" if len(names) > 30 else "")
            + ".",
            "",
        ]
    lines += [
        "Caveats:",
        "",
        "- Measured reservoir levels are provisional near real-time data from the dam owner.",
        "- Elevations published by the dam owner may use an older vertical datum (e.g. CGVD28) than the lidar DEM "
        "(CGVD2013); differences of a few decimetres are possible.",
        "- Target elevations come from published documents and may have been revised. Add or edit them in "
        "`core/data/reservoir_targets.json` of the plugin, with a citation.",
        "",
    ]
    return lines


def climate_lines(project, result) -> list[str]:
    """Domain-mean precipitation, temperature and PET."""
    stats = result.stats.get("climate")
    if stats is None:
        return []
    lines = ["## Climate", ""]
    if stats.get("error"):
        return lines + [f"Climate retrieval failed: {stats['error']}. Run again to retry.", ""]
    if not project.options.climate_retrieval:
        return lines + ["Off: `climate_retrieval: false`.", ""]
    lta = stats.get("lta") or {}
    if not lta:
        return lines + ["No climate series was written.", ""]
    months = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()
    monthly = stats.get("monthly_lta") or {}
    base = project.output_path
    daily = stats.get("daily")
    monthly_path = stats.get("monthly")
    lines += [
        f"**Source:** {stats.get('source', '?')} for {stats.get('start')} to {stats.get('end')} "
        f"({stats.get('n_days', 0)} days, {stats.get('n_months', 0)} months).",
        "",
        f"- Long-term average precipitation: **{lta.get('P_m_per_yr', float('nan')):.3f} m/yr** "
        f"({lta.get('P_m_per_d', float('nan')):.5f} m/d)",
        f"- Long-term average PET: **{lta.get('PET_m_per_yr', float('nan')):.3f} m/yr** "
        f"({lta.get('PET_m_per_d', float('nan')):.5f} m/d)",
        f"- Long-term average temperature: **{lta.get('T_C', float('nan')):.2f} °C**",
        "",
    ]
    if daily:
        lines.append(f"- Daily series: `{_rel(Path(daily), base)}`")
    if monthly_path:
        lines.append(f"- Monthly series: `{_rel(Path(monthly_path), base)}`")
    if stats.get("lta_file"):
        lines.append(f"- Long-term average JSON: `{_rel(Path(stats['lta_file']), base)}`")
    lines.append("")
    if monthly:
        lines += [
            "| Month | P (m/d) | PET (m/d) | T (°C) |",
            "|---|---|---|---|",
        ]
        for m in range(1, 13):
            row = monthly.get(str(m))
            if not row:
                continue
            lines.append(
                f"| {months[m - 1]} | {row['P_m_per_d']:.5f} | {row['PET_m_per_d']:.5f} | {row['T_C']:.2f} |"
            )
        lines.append("")
    lines += [
        "Daymet PET uses Hargreaves–Samani from daily tmax/tmin; Open-Meteo uses FAO ET0. "
        "Build MUT project can write these rates as SWF recharge and GWF EVT.",
        "",
        "Caveats:",
        "",
        "- Values are a domain mean over sample points in the regional bounding box, not a full gridded field.",
        "- Reanalysis and Daymet differ from gauge observations; use local climate data when available.",
        "",
    ]
    return lines


def flows_lines(project, result) -> list[str]:
    """Observed river discharge near level-1 outlets."""
    stats = result.stats.get("flows")
    if stats is None:
        return []
    lines = ["## River flows", ""]
    if stats.get("error"):
        return lines + [f"River-flow retrieval failed: {stats['error']}. Run again to retry.", ""]
    gauges = stats.get("gauges") or []
    if not gauges:
        warn = stats.get("warnings") or []
        extra = f" ({'; '.join(warn[:3])})" if warn else ""
        return lines + [f"No discharge gauge was matched near a model outlet.{extra}", ""]
    months = "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split()
    base = project.output_path
    lines += [
        "Gauges nearest each level-1 stream outlet (ECCC HYDAT in Canada, USGS elsewhere). "
        "Observed discharge is for calibration against model critical-depth (SWBC) outflow; "
        "it is not prescribed as a well.",
        "",
        "| Outlet | Agency | Station | Distance (km) | Mean Q (m³/s) | Median Q (m³/s) | 5–95% (m³/s) | Record |",
        "|---|---|---|---|---|---|---|---|",
    ]
    for g in gauges:
        lta = g.get("lta") or {}
        lines.append(
            f"| {g.get('outlet', '')} | {g.get('agency', '').upper()} | "
            f"{g.get('station_id', '')} {g.get('station_name', '')} | "
            f"{g.get('distance_km', float('nan')):.1f} | "
            f"{lta.get('mean_m3s', float('nan')):.2f} | {lta.get('median_m3s', float('nan')):.2f} | "
            f"{lta.get('p5_m3s', float('nan')):.2f}–{lta.get('p95_m3s', float('nan')):.2f} | "
            f"{lta.get('from', '')} to {lta.get('to', '')} |"
        )
    lines.append("")
    for g in gauges:
        monthly = g.get("monthly_lta") or {}
        if monthly:
            lines.append(
                f"**{g.get('station_id')}** monthly median Q: "
                + ", ".join(
                    f"{months[int(m) - 1]} {v:.2f}"
                    for m, v in sorted(monthly.items(), key=lambda kv: int(kv[0]))
                )
                + " m³/s."
            )
            lines.append("")
        if g.get("daily"):
            lines.append(f"- Daily: `{_rel(Path(g['daily']), base)}`")
        if g.get("monthly"):
            lines.append(f"- Monthly: `{_rel(Path(g['monthly']), base)}`")
        lines.append("")
    if stats.get("gauges_file"):
        lines.append(f"Gauge locations: `{_rel(Path(stats['gauges_file']), base)}`.")
        lines.append("")
    lines += [
        "Caveats:",
        "",
        "- The nearest gauge may lie upstream or on a tributary; check the name and drainage area.",
        "- Provisional real-time values can be revised in HYDAT / NWIS.",
        "",
    ]
    return lines


def _rel(path: Path, base: Path) -> str:
    try:
        return str(Path(path).relative_to(base))
    except ValueError:
        return str(path)


def source_lines(records) -> list[str]:
    lines: list[str] = []
    if not records:
        lines.append("No data sources contributed.")
    for rec in records:
        lines += [
            f"### {rec.title} ({rec.role})",
            "",
            f"- Quantity: {rec.quantity}; nominal resolution: {rec.resolution_m if rec.resolution_m is not None else '?'} m",
            f"- Vertical accuracy: {rec.vertical_accuracy or 'unknown'}",
            f"- Vertical datum: {rec.vertical_datum or 'unknown'}",
            f"- Licence: {rec.licence or 'unknown'}",
            f"- Retrieved: {rec.retrieved}",
            f"- Notes: {rec.notes}",
        ]
        if rec.citation:
            lines.append(f"- Citation: {rec.citation}")
        if rec.urls:
            shown = rec.urls[:5]
            more = f" (+{len(rec.urls) - 5} more, see sources.json)" if len(rec.urls) > 5 else ""
            lines.append("- Sources: " + ", ".join(f"<{u}>" if u.startswith("http") else f"`{u}`" for u in shown) + more)
        lines.append("")
    return lines


def focus_lines(project) -> list[str]:
    """The "Focus areas" section of the regional report."""
    base = project.output_path
    lines = ["## Focus areas", ""]
    if not project.focus_areas:
        return lines + [
            "None defined yet. Use 'New focus area...' in QGIS (or `qgis_mut_builder.cmd focus-add`) to add a "
            "subregion with a ~1 m ground surface.",
            "",
        ]
    lines += ["| Name | Type | Status | Cell size | Report |", "|---|---|---|---|---|"]
    for focus in project.focus_areas:
        report = project.focus_dir(focus) / f"{focus.slug}_focus_report.md"
        link = f"`{_rel(report, base)}`" if report.exists() else ""
        lines.append(f"| {focus.name} | {focus.mode} | {focus.status} | {focus.resolution_m:g} m | {link} |")
    return lines + [""]


def write_focus_report(project, focus, result, grid: Grid, path: Path) -> Path:
    base = project.output_path
    props = read_properties(Path(focus.boundary_file))
    area = None
    try:
        from .boundary import boundary_area_km2

        area = boundary_area_km2(Path(focus.boundary_file), project.epsg)
    except RuntimeError:
        pass
    lines = [
        f"# {project.name}: focus area '{focus.name}'",
        "",
        f"Generated {datetime.now().isoformat(timespec='seconds')} by MUT Model Builder.",
        "",
        "## Outline",
        "",
        f"- Type: {focus.mode}",
        f"- Source: {focus.source or 'user'}",
    ]
    if focus.feature_name:
        lines.append(f"- Feature: {focus.feature_name}")
    if focus.delineation != "none":
        method = "regional ground surface (GRASS r.watershed)" if focus.delineation == "regional_dem" else "Global Watersheds (MERIT-Hydro)"
        lines.append(f"- Local watershed delineated from the {method}")
    if focus.outlet_lat is not None and focus.outlet_lon is not None:
        lines.append(f"- Outlet: latitude {focus.outlet_lat:.6f}, longitude {focus.outlet_lon:.6f}")
    lines += [
        f"- File: `{_rel(Path(focus.boundary_file), base)}`",
        f"- Area: {area:,.2f} km2" if area else "- Area: unknown",
    ]
    if props.get("citation"):
        lines.append(f"- Citation: {props['citation']}")
    lines += [
        "",
        "## Grid",
        "",
        f"- {grid.width} x {grid.height} cells at {grid.res:g} m (EPSG:{grid.epsg}), outline extent + {focus.margin_m:g} m "
        f"margin, x {grid.xmin:.0f} to {grid.xmax:.0f}, y {grid.ymin:.0f} to {grid.ymax:.0f}",
        "",
        "## Outputs",
        "",
        "| Surface | File | Min (m) | Max (m) | Mean (m) | Coverage |",
        "|---|---|---|---|---|---|",
    ]
    for key, out in result.outputs.items():
        s = result.stats.get(key, {})
        lines.append(
            f"| {LABELS.get(key, key)} | `{_rel(out, base)}` | {s.get('min', float('nan')):.2f} | "
            f"{s.get('max', float('nan')):.2f} | {s.get('mean', float('nan')):.2f} | {s.get('coverage_percent', 0):.1f}% |"
        )
    if not result.outputs:
        lines.append("| (none) | | | | | |")
    lines.append("")
    if result.outputs:
        lines += bathymetry_lines(project, result)
        lines += dams_lines(result)
    lines += ["## Data sources", ""]
    lines += source_lines(result.records)
    lines += ["## Checks and caveats", ""]
    inside = result.stats.get("inside_outline_coverage_percent")
    if inside:
        lines.append(f"- The high-resolution DEM covers {inside['value']:.1f}% of the focus outline.")
    diff = result.stats.get("focus_minus_regional")
    if diff:
        lines.append(
            f"- Focus DEM (averaged to {project.options.coarse_resolution_m:g} m) minus the regional ground surface: mean "
            f"{diff['mean']:+.2f} m, standard deviation {diff['std']:.2f} m over {diff['cells']} cells. A large mean "
            "offset usually means different vertical datums or a surface model compared with bare ground."
        )
    if focus.delineation == "global_watersheds":
        lines.append(
            "- The outline was delineated automatically from MERIT-Hydro (~90 m). Check it against the hillshade."
        )
    elif focus.delineation == "regional_dem":
        lines.append(
            f"- The outline was delineated from the {project.options.coarse_resolution_m:g} m regional ground surface. "
            "In flat wetlands and near drains the divide can be off by a few cells; check it against the hillshade."
        )
    for warning in result.warnings:
        lines.append(f"- Warning: {warning}")
    lines.append("")
    if result.suggestions:
        lines += ["## Suggested next steps", ""] + [f"- {s}" for s in result.suggestions] + [""]
    lines += [
        "## Reproduce",
        "",
        f"`Tools\\qgis_mut_builder.cmd focus-run \"{project.yaml_path}\" --name \"{focus.name}\"`",
        "",
    ]
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def regional_report_path(project) -> Path:
    return project.output_path / f"{project.slug}_surfaces_report.md"


def update_focus_section(project) -> Path | None:
    """Rewrite the "Focus areas" section of an existing regional report."""
    path = regional_report_path(project)
    if not path.exists():
        return None
    text = path.read_text(encoding="utf-8")
    section = "\n".join(focus_lines(project))
    pattern = re.compile(r"## Focus areas\n.*?(?=\n## )", re.S)
    if pattern.search(text):
        text = pattern.sub(lambda _m: section.rstrip("\n"), text, count=1)
    else:
        text = text.replace("## Data sources", section + "\n## Data sources", 1)
    path.write_text(text, encoding="utf-8")
    return path


def write_report(project, result, regional: Grid) -> Path:
    base = project.output_path
    lines: list[str] = [
        f"# {project.name}: surficial layer surfaces",
        "",
        f"Generated {datetime.now().isoformat(timespec='seconds')} by MUT Model Builder.",
        "",
    ]
    if project.description:
        lines += ["## Description", "", project.description.strip(), ""]

    lines += [
        "## Location and grid",
        "",
        f"- Project location: latitude {project.lat:.6f}, longitude {project.lon:.6f}",
        f"- Projection: EPSG:{project.epsg}",
        f"- Regional grid: {regional.width} x {regional.height} cells at {regional.res:g} m, "
        f"x {regional.xmin:.0f} to {regional.xmax:.0f}, y {regional.ymin:.0f} to {regional.ymax:.0f}",
        f"- Existing-data folder: {project.data_folder or '(none)'}",
        "",
        "## Outputs",
        "",
        "| Surface | File | Min (m) | Max (m) | Mean (m) | Coverage |",
        "|---|---|---|---|---|---|",
    ]
    for key, path in result.outputs.items():
        s = result.stats.get(key, {})
        lines.append(
            f"| {LABELS.get(key, key)} | `{_rel(path, base)}` | {s.get('min', float('nan')):.2f} | "
            f"{s.get('max', float('nan')):.2f} | {s.get('mean', float('nan')):.2f} | {s.get('coverage_percent', 0):.1f}% |"
        )
    if not result.outputs:
        lines.append("| (none) | | | | | |")
    lines.append("")
    if result.outputs:
        lines += bathymetry_lines(project, result)
        lines += dams_lines(result)
    lines += climate_lines(project, result)
    lines += flows_lines(project, result)

    if project.has_boundary:
        area = result.stats.get("boundary", {}).get("area_km2")
        lines += [
            "## Model boundary",
            "",
            f"- Type: {project.boundary_mode}",
            f"- Name: {project.boundary_name or '(unnamed)'}",
            f"- Source: {project.boundary_source or 'user'}",
            f"- File: `{_rel(Path(project.boundary_file), base)}`",
            f"- Area: {area:,.1f} km2" if area else "- Area: unknown",
            f"- Regional grid = boundary extent + {project.boundary_margin_km:g} km margin; rasters stay rectangular.",
        ]
        citation = read_properties(Path(project.boundary_file)).get("citation")
        if citation:
            lines.append(f"- Citation: {citation}")
        lines.append("")
        inside = result.stats.get("inside_boundary", {})
        if inside:
            lines += ["| Inside the boundary | Min (m) | Max (m) | Mean (m) |", "|---|---|---|---|"]
            for key, s in inside.items():
                lines.append(f"| {LABELS.get(key, key)} | {s['min']:.2f} | {s['max']:.2f} | {s['mean']:.2f} |")
            lines.append("")

    lines += focus_lines(project)
    lines += ["## Data sources", ""]
    lines += source_lines(result.records)

    clipped = result.stats.get("bedrock_clipped_cells")
    lines += ["## Checks and caveats", ""]
    if clipped:
        lines.append(
            f"- Bedrock was lowered to {project.options.min_thickness_m:g} m below ground in {clipped['count']} cells "
            f"({clipped['percent']:.1f}%) so the surficial layer is never thinner than that."
        )
    if any(r.provider == "shangguan2017_dtb" for r in result.records):
        lines.append(
            "- The bedrock surface comes from a global 250 m model (Shangguan et al., 2017). It is coarse and uncertain "
            "(errors of tens of metres are common) and should be replaced with a surface built from local boreholes, "
            "well logs or geophysics when they become available."
        )
    if project.boundary_mode == "watershed" and "Global Watersheds" in project.boundary_source:
        lines.append(
            "- The watershed boundary was delineated automatically from MERIT-Hydro (~90 m). Automated delineation "
            "can follow the wrong channel near confluences and in flat terrain: check the outline against the rivers."
        )
    for warning in result.warnings:
        lines.append(f"- Warning: {warning}")
    lines.append("")

    if result.suggestions:
        lines += ["## Suggested next steps", ""]
        lines += [f"- {s}" for s in result.suggestions]
        lines.append("")

    lines += [
        "## Reproduce",
        "",
        f"Project file: `{_rel(project.yaml_path, base)}`. Re-run headless with "
        f"`Tools\\qgis_mut_builder.cmd run \"{project.yaml_path}\"`.",
        "",
    ]
    path = regional_report_path(project)
    path.write_text("\n".join(lines), encoding="utf-8")
    return path
