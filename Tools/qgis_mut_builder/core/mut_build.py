"""Generate a starter ``2_models/<name>/_build.mut`` from a built mesh and regional surfaces."""

from __future__ import annotations

import csv
import json
import shutil
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Callable

import numpy as np

from .climate import (
    NET_RECHARGE_ASC,
    load_climate,
    load_daily_climate,
    load_monthly_climate,
    net_recharge_m_per_d,
    write_climate_forcing_instructions,
    write_constant_arcascii,
    write_precipitation_rts,
)
from .cln_export import (
    cln_files,
    export_cln,
    has_stream_geometry,
    load_stream_paths,
    sample_raster,
    stream_preview_candidates,
)
from .flows import load_flows, retrieve_flows
from .lake_obs import (
    INSTRUCTIONS_NAME as LAKE_OBS_INSTRUCTIONS,
    find_lake_level_obs_points,
    write_swf_lake_level_obs_instructions,
)
from .mesh import GB_PATH_MAX, mesh_outputs, mut_snippet, outlet_node_mask, write_nchos
from .net import prepare_network
from .project import MeshSpec, ModelSpec, Project, StreamOptions

# Starter material IDs from USERBIN GWF.csv / SWF.csv / CLN.csv
DEFAULT_GWF_MATERIAL = 6  # Borden sand (surficial)
DEFAULT_GWF_BEDROCK_MATERIAL = 21  # Fractured crystalline rock
DEFAULT_SWF_MATERIAL = 4  # Grass
DEFAULT_CLN_MATERIAL = 3  # Channel (rectangular 10 m × 1 m)
CLN_CHANNEL_WIDTH_M = 10.0  # plan width for Channel material 3 (matches DEFAULT_CLN_MATERIAL)
BEDROCK_EXTENSION_M = 200.0  # flat model base = min(surficial base) - this
SURFICIAL_SUBLAYERS = 10
BEDROCK_SUBLAYERS = 5
LOG_SUBLAYER_DECADE = 10.0  # deepest/shallowest proportional thickness


def log_proportional_weights(n: int, decade: float = LOG_SUBLAYER_DECADE) -> list[float]:
    """Geometric relative thicknesses (top→bottom) with deepest/shallowest = ``decade``."""
    if n < 1:
        raise ValueError(f"n must be >= 1, got {n}")
    if n == 1:
        return [1.0]
    r = float(decade) ** (1.0 / (n - 1))
    return [r**i for i in range(n)]


def format_proportional_weight(w: float) -> str:
    """Compact decimal for MUT proportional sublayering lines."""
    return f"{w:.6g}"


@dataclass
class BuildMutResult:
    ok: bool = False
    model_dir: Path | None = None
    outputs: dict[str, Path] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)


def _rel_to(model_dir: Path, target: Path) -> str:
    """Windows-style relative path from ``model_dir`` to ``target`` (MUT path convention)."""
    rel = Path(os_path_rel(model_dir, target))
    return str(rel).replace("/", "\\")


def os_path_rel(start: Path, target: Path) -> str:
    try:
        return str(target.resolve().relative_to(start.resolve()))
    except ValueError:
        return str(Path(os_relpath(str(target), str(start))))


def os_relpath(path: str, start: str) -> str:
    import os

    return os.path.relpath(path, start)


def read_mesh_xy(xy_csv: Path) -> np.ndarray:
    """Node coordinates in mesh order from ``*_xyCoord.csv``."""
    rows = []
    with open(xy_csv, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append((float(row["xcoord"]), float(row["ycoord"])))
    if not rows:
        raise ValueError(f"No nodes in {xy_csv}")
    return np.asarray(rows, dtype=float)


def write_elevation_list(path: Path, values: np.ndarray, header: str) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    lines = [header] + [f"{v:.6f}" for v in values]
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def nchos_paths(project: Project, spec: MeshSpec) -> list[Path]:
    """``.nchos.Stream level k`` then ``.nchos.Streams``, if present."""
    prefix = mesh_outputs(project, spec)["gb_prefix"]
    levels = sorted(prefix.parent.glob(f"{prefix.name}.nchos.Stream level *"), key=lambda p: p.name)
    streams = prefix.with_name(f"{prefix.name}.nchos.Streams")
    out = list(levels)
    if streams.exists():
        out.append(streams)
    return out


def _mesh_block(project: Project, spec: MeshSpec) -> str:
    """Mesh load instructions (no stream comments - those are handled separately)."""
    rel = f"..\\..\\2_mesh\\{spec.slug}\\{spec.slug}"
    return (
        f"! Delaunay mesh '{spec.name}' from MUT Model Builder; path assumes this folder is\n"
        f"! two levels below {project.output_path} (edit otherwise; MUT reads at most {GB_PATH_MAX} characters).\n"
        "nodal control volumes\n"
        "2d mesh from gb\n"
        f"{rel}\n"
    )


def _cln_block(project: Project, spec: MeshSpec, model_dir: Path) -> str:
    files = cln_files(project, spec)
    if not files:
        primary = Path(spec.cln_xyz_file) if spec.cln_xyz_file else None
        files = [primary] if primary and primary.exists() else []
    if not files:
        return ""
    # One generate cln domain (MUT resets the domain each time that block is entered);
    # multiple cln from xyz list calls inside it append rivers as separate zones.
    lines = ["! Streams as CLN (keep regional mesh); MUT splits at GWF faces"]
    for i, path in enumerate(files, start=1):
        rel = _rel_to(model_dir, path)
        stem = f"Streams_CLN_{i}" if len(files) > 1 else "Streams_CLN"
        lines += [
            "",
            "cln mesh intersection",
            rel,
            f"{stem}_split.dat",
        ]
    lines += ["", "generate cln domain"]
    for i, _path in enumerate(files, start=1):
        stem = f"Streams_CLN_{i}" if len(files) > 1 else "Streams_CLN"
        lines += [
            "    cln from xyz list",
            f"    {stem}_split.xyzList",
        ]
    lines += [
        "end generate cln domain",
        "",
        "cln materials database",
        "CLN.csv",
        "",
        "active domain",
        "cln",
        "    clear chosen zones",
        "    choose all zones",
        "    ! Channel 10 m × 1 m",
        "    chosen zones use cln material number",
        f"    {DEFAULT_CLN_MATERIAL}",
    ]
    return "\n".join(lines) + "\n"


def _nchos_block(project: Project, spec: MeshSpec, model_dir: Path) -> str:
    files = nchos_paths(project, spec)
    if not files:
        return ""
    lines = [
        "! Stream nodes on the template mesh (SWF / tmplt only; not layered GWF)",
        "active domain",
        "swf",
        "    clear chosen cells",
    ]
    for path in files:
        lines.append("    choose cells from gb nodes")
        lines.append(f"    {_rel_to(model_dir, path)}")
    return "\n".join(lines) + "\n"


def _ensure_outlet_nchos(project: Project, spec: MeshSpec, log: Callable[[str], None] = print) -> Path | None:
    """Write or refresh ``.nchos.Outlet`` (downstream level-1 ends on the mesh boundary).

    Rematches ranked stream paths onto the mesh so outlets are true sinks (not every level-1 node
    on the domain edge). Existing meshes get a correct critical-depth selection on Build MUT without
    remeshing. Returns the path when at least one outlet node is flagged.
    """
    from .stream_map import load_mesh_from_csvs, map_paths_to_levels

    outs = mesh_outputs(project, spec)
    prefix = outs["gb_prefix"]
    outlet_path = prefix.with_name(f"{prefix.name}.nchos.Outlet")
    xy_csv, id_csv = outs["xy_csv"], outs["idnode_csv"]
    if not xy_csv.exists() or not id_csv.exists():
        return outlet_path if outlet_path.exists() else None
    try:
        mesh = load_mesh_from_csvs(xy_csv, id_csv)
    except (OSError, ValueError) as exc:
        log(f"  WARNING: could not rebuild outlet .nchos from mesh CSVs: {exc}")
        return outlet_path if outlet_path.exists() else None
    if mesh.level is None or not (mesh.level == 1).any():
        if outlet_path.exists():
            outlet_path.unlink()
        return None

    outlet_indices: list[int] | None = None
    try:
        paths = load_stream_paths(project, spec)
    except (OSError, ValueError, RuntimeError) as exc:
        log(f"  WARNING: could not load stream paths for Outlet refresh: {exc}")
        paths = []
    if paths and mesh.triangles is not None and len(mesh.triangles):
        ds = (spec.streams.spacing_m if spec.streams else 0.0) or spec.boundary_spacing_m
        try:
            _, _, outlet_indices = map_paths_to_levels(paths, mesh.xy, mesh.triangles, ds)
        except (OSError, ValueError, RuntimeError) as exc:
            log(f"  WARNING: could not remap streams for Outlet refresh: {exc}")
            outlet_indices = None

    if outlet_indices is None:
        # No path rematch — keep a previously written Outlet rather than inventing all boundary∩L1.
        if outlet_path.exists():
            log(f"  Keeping existing Outlet .nchos (stream-path rematch unavailable): {outlet_path.name}")
            return outlet_path
        log("  WARNING: no stream paths to locate critical-depth Outlet nodes")
        return None

    mask = outlet_node_mask(mesh, outlet_indices=outlet_indices)
    if not mask.any():
        if outlet_path.exists():
            outlet_path.unlink()
        log("  WARNING: no downstream level-1 outlet on the mesh boundary for critical-depth Outlet")
        return None
    write_nchos(outlet_path, "Outlet", mask)
    log(f"  Outlet .nchos: {int(mask.sum())} downstream level-1 node(s) on boundary -> {outlet_path.name}")
    return outlet_path


def _outlet_nchos(project: Project, spec: MeshSpec, log: Callable[[str], None] = print) -> Path | None:
    """``.nchos.Outlet`` (downstream level-1 ends on boundary) for critical-depth SWBC; never Stream level 1."""
    path = _ensure_outlet_nchos(project, spec, log=log)
    if path is not None and "Stream level" in path.name:
        raise RuntimeError(
            f"Internal error: critical-depth outlet resolved to Stream level file {path.name}"
        )
    return path


def _mesh_area_m2(project: Project, spec: MeshSpec) -> float:
    """Mesh plan area from ``mesh.json`` (triangle total), else boundary area."""
    summary = mesh_outputs(project, spec)["summary"]
    if summary.exists():
        data = json.loads(summary.read_text(encoding="utf-8"))
        stats = data.get("stats") or {}
        for key in ("final", "smoothed", "initial"):
            block = stats.get(key) or {}
            area = (block.get("area_m2") or {}).get("total")
            if area and float(area) > 0:
                return float(area)
        bnd = stats.get("boundary") or {}
        if bnd.get("area_km2"):
            return float(bnd["area_km2"]) * 1.0e6
    return 0.0


def _xyz_polyline_length_m(path: Path) -> float:
    """Sum segment lengths from an ``ID X Y Z`` XYZ list."""
    coords: list[tuple[float, float]] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        parts = line.split()
        if len(parts) < 4:
            continue
        try:
            coords.append((float(parts[1]), float(parts[2])))
        except ValueError:
            continue
    if len(coords) < 2:
        return 0.0
    xy = np.asarray(coords, dtype=float)
    return float(np.hypot(*np.diff(xy, axis=0).T).sum())


def _stream_length_m(project: Project, spec: MeshSpec) -> float:
    """Total ranked-stream length (m) for CLN footprint accounting."""
    try:
        paths = load_stream_paths(project, spec)
        if paths:
            return float(sum(p.length_m for p in paths))
    except (FileNotFoundError, ValueError, OSError):
        pass
    total = 0.0
    for xyz in cln_files(project, spec):
        total += _xyz_polyline_length_m(xyz)
    return total


def _cln_area_m2(project: Project, spec: MeshSpec) -> float:
    """Plan channel area = Channel width × stream length."""
    return CLN_CHANNEL_WIDTH_M * _stream_length_m(project, spec)


def _oc_times(monthly: list[dict], mode: str) -> list[str]:
    if mode == "average":
        from .climate import AVERAGE_OC_TIMES

        return list(AVERAGE_OC_TIMES)
    if not monthly:
        return ["1.0"]
    # Cumulative month-end times; keep at most 24 print times (every Nth month)
    ends = []
    t = 0.0
    for row in monthly:
        t += float(row["days"])
        ends.append(t)
    if len(ends) <= 24:
        return [f"{x:.1f}" for x in ends]
    step = max(1, len(ends) // 24)
    chosen = ends[step - 1 :: step]
    if ends[-1] not in chosen:
        chosen.append(ends[-1])
    return [f"{x:.1f}" for x in chosen[:24]]


def _copy_observed_flows(project: Project, model_dir: Path) -> list[str]:
    """Copy outlet Q CSVs into the model folder; return comment lines for _build.mut."""
    flows = load_flows(project)
    if not flows:
        return []
    notes = []
    for g in flows.get("gauges") or []:
        sid = str(g.get("station_id", "unknown")).replace("/", "_")
        for key, suffix in (("daily", "daily"), ("monthly", "monthly")):
            src = g.get(key)
            if not src or not Path(src).exists():
                continue
            dest = model_dir / f"ObservedOutletQ_{sid}_{suffix}.csv"
            shutil.copy2(src, dest)
        lta = g.get("lta") or {}
        notes.append(
            f"Observed outlet Q: {g.get('agency', '').upper()} {g.get('station_id')} "
            f"({g.get('station_name', '')}), mean {lta.get('mean_m3s', float('nan')):.2f} m3/s "
            f"({lta.get('from', '')} to {lta.get('to', '')}); see ObservedOutletQ_{sid}_*.csv "
            f"(compare to SWBC Outlet, do not prescribe as a well)"
        )
    return notes


def write_build_mut(
    project: Project,
    spec: MeshSpec,
    model: ModelSpec,
    model_dir: Path,
    *,
    has_bedrock: bool,
    model_base_m: float,
    include_swf: bool,
    include_cln: bool,
    include_nchos: bool,
    climate_mode: str = "none",
    oc_times: list[str] | None = None,
    lake_level_obs: bool = False,
) -> str:
    min_thik = project.options.min_thickness_m
    surf_w = log_proportional_weights(SURFICIAL_SUBLAYERS)
    bed_w = log_proportional_weights(BEDROCK_SUBLAYERS)
    desc = project.description.strip() or "(no description)"
    header = (
        f"! {project.name} - auto-generated by MUT Model Builder\n"
        f"! {desc}\n"
        f"! Mesh: {spec.name} (stream_mode={spec.stream_mode})\n"
        f"! Generated {datetime.now().isoformat(timespec='seconds')}\n"
        "build modflow usg\n"
        "\n"
        "    units of time\n"
        "    days\n"
        "\n"
    )
    body = ["    ! --- template mesh ---"]
    for line in _mesh_block(project, spec).splitlines():
        body.append(f"    {line}" if line else "")

    if include_swf:
        body += [
            "",
            "    ! --- SWF at ground surface ---",
            "    generate swf domain",
            "        top elevation",
            "            elevation from list file",
            "            GroundElevation.list",
            "        end top elevation",
            "    end generate swf domain",
        ]

    body += [
        "",
        "    ! --- GWF: Surficial (10 log-proportional) + Bedrock (5 log-proportional) ---",
        f"    ! Model base elevation constant = min(surficial base) - {BEDROCK_EXTENSION_M:g} m",
        f"    !   -> {model_base_m:.6f} m",
        "    generate layered gwf domain",
        "        top elevation",
        "            elevation from list file",
        "            GroundElevation.list",
        "        end top elevation",
        "",
        "        new layer",
        "            layer name",
        "            Surficial",
        "            proportional sublayering",
        f"            {SURFICIAL_SUBLAYERS}",
    ]
    body += [f"            {format_proportional_weight(w)}" for w in surf_w]
    if has_bedrock:
        body += [
            "            elevation from list file",
            "            BedrockElevation.list",
        ]
    else:
        body += [
            "            ! No bedrock_bottom.tif; offset from ground by min thickness",
            "            elevation from list file",
            "            GroundElevation.list",
            "            offset base",
            f"            {-min_thik}",
        ]
    body += [
        "            minimum layer thickness",
        f"            {min_thik}",
        "        end new layer",
        "",
        "        new layer",
        "            layer name",
        "            Bedrock",
        "            proportional sublayering",
        f"            {BEDROCK_SUBLAYERS}",
    ]
    body += [f"            {format_proportional_weight(w)}" for w in bed_w]
    body += [
        "            elevation constant",
        f"            {model_base_m:.6f}",
        "        end new layer",
        "    end generate layered gwf domain",
        "",
        "    gwf materials database",
        "    GWF.csv",
        "",
        "    active domain",
        "    gwf",
        "        clear chosen zones",
        "        choose zone number",
        "        1",
        "        clear chosen cells",
        "        choose cells by chosen zones",
        "        ! Borden sand",
        "        chosen cells use gwf material number",
        f"        {DEFAULT_GWF_MATERIAL}",
        "",
        "        clear chosen zones",
        "        choose zone number",
        "        2",
        "        clear chosen cells",
        "        choose cells by chosen zones",
        "        ! Fractured crystalline rock",
        "        chosen cells use gwf material number",
        f"        {DEFAULT_GWF_BEDROCK_MATERIAL}",
    ]

    if include_swf:
        body += [
            "",
            "    swf materials database",
            "    SWF.csv",
            "",
            "    active domain",
            "    swf",
            "        clear chosen zones",
            "        choose zone number",
            "        1",
            "        ! Grass",
            "        chosen zones use swf material number",
            f"        {DEFAULT_SWF_MATERIAL}",
        ]
        if include_nchos and nchos_paths(project, spec):
            body.append("")
            for line in _nchos_block(project, spec, model_dir).splitlines():
                body.append(f"    {line}" if line else "")
        if lake_level_obs:
            body += [
                "",
                "    ! --- lake level observation points ---",
                f"    include {LAKE_OBS_INSTRUCTIONS}",
            ]

    if include_cln:
        body.append("")
        for line in _cln_block(project, spec, model_dir).splitlines():
            body.append(f"    {line}" if line else "")

    body += [
        "",
        "    ! --- initial conditions ---",
        "    active domain",
        "    gwf",
        "        gwf initial head equals surface elevation",
    ]
    if include_swf:
        body += [
            "",
            "    active domain",
            "    swf",
            "        choose all cells",
            "        swf initial depth",
            "        1.0d-4",
        ]
    if climate_mode in ("average", "transient"):
        if climate_mode == "average":
            climate_note = "P-ET -> CLN GSTR then SWF RCH; TR spin-up (no GWF EVT)"
        else:
            climate_note = "P -> SWF RCH, PET -> GWF EVT"
        body += [
            "",
            f"    ! --- climate forcing ({climate_note}) ---",
            "    include ClimateForcing.instructions",
            "",
        ]
    else:
        body += [
            "",
            "    ! --- stress period ---",
            "    stress period",
            "        type",
            "        SS",
            "        duration",
            "        1.0",
            "    end stress period",
            "",
        ]

    times = oc_times or ["1.0"]
    body += [
        "    generate output control file",
    ]
    for t in times:
        body.append(f"        {t}")
    body += [
        "    end generate output control file",
        "",
        "    sms database",
        "    SMS.csv",
        "    sms parameter set number",
        "    1",
        "",
        "end build modflow usg",
        "",
    ]
    return header + "\n".join(body)


def build_mut_project(
    project: Project,
    spec: MeshSpec,
    model: ModelSpec,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
) -> BuildMutResult:
    """Sample elevations, write list files and ``_build.mut``, record ``ModelSpec`` on the project."""
    progress = progress or (lambda pct: None)
    result = BuildMutResult()
    outs = mesh_outputs(project, spec)
    grd = outs["folder"] / f"{spec.slug}.grd"
    if not grd.exists():
        raise FileNotFoundError(f"Mesh '{spec.name}' is not built ({outs['folder']})")

    xy_csv = outs["xy_csv"]
    if not xy_csv.exists():
        raise FileNotFoundError(f"Mesh node CSV not found: {xy_csv}")
    ground = project.surfaces_dir / "ground_top.tif"
    if not ground.exists():
        raise FileNotFoundError(f"Regional ground surface not found: {ground}")
    bedrock = project.surfaces_dir / "bedrock_bottom.tif"
    has_bedrock = bedrock.exists()

    include_swf = model.include_swf
    include_nchos = model.include_stream_nchos and bool(nchos_paths(project, spec))
    if include_nchos and not include_swf:
        result.warnings.append("Stream .nchos selections need the SWF domain; enabling SWF")
        include_swf = True

    # CLN: only when requested. Do not auto-export from stream preview for SWF-mapped/embedded meshes.
    want_cln = model.include_cln
    swf_streams = spec.stream_mode in ("embedded", "mapped")
    have_cln = bool(cln_files(project, spec)) or bool(spec.cln_xyz_file) or spec.stream_mode == "cln"
    if want_cln and not have_cln and not swf_streams and has_stream_geometry(project, spec):
        log("  No Streams_CLN.xyzList on this mesh yet; exporting CLN from ranked streams ...")
        if spec.streams is None:
            spec.streams = StreamOptions()
        cln_result = export_cln(project, spec, log=log)
        if not cln_result.ok:
            raise RuntimeError("CLN export from stream preview failed")
        # reload mesh metadata after export_cln saved project.yaml
        refreshed = project.get_mesh(spec.name)
        if refreshed is not None:
            spec = refreshed
        have_cln = True
        result.warnings.append("Exported Streams_CLN.xyzList from project stream preview during build")
    elif want_cln and not have_cln:
        previews = stream_preview_candidates(project, spec)
        hint = f" (found preview: {previews[0].name})" if previews else ""
        result.warnings.append(
            "Include CLN was requested but no Streams_CLN.xyzList was found"
            + hint
            + "; run Add streams... → Find streams, then Attach streams as CLN"
        )
    include_cln = want_cln and have_cln

    model_dir = project.model_dir(model)
    model_dir.mkdir(parents=True, exist_ok=True)
    log(f"Build MUT project '{model.name}' in {model_dir}")
    log(f"  Mesh: {spec.name} (stream_mode={spec.stream_mode})")
    progress(10)

    xy = read_mesh_xy(xy_csv)
    log(f"  Sampling ground at {len(xy):,} mesh nodes ...")
    z_ground = sample_raster(ground, xy)
    filled = int(np.isnan(z_ground).sum())
    if filled:
        # nearest-neighbour fill from finite samples
        finite = np.isfinite(z_ground)
        if not finite.any():
            raise ValueError(f"No ground elevations sampled from {ground}")
        from scipy.spatial import cKDTree

        tree = cKDTree(xy[finite])
        _, idx = tree.query(xy[~finite], k=1)
        z_ground[~finite] = z_ground[finite][idx]
        result.warnings.append(f"Filled {filled} ground elevation(s) from nearest nodes")
    write_elevation_list(model_dir / "GroundElevation.list", z_ground, "Ground elevation (m)")
    progress(40)

    min_thik = project.options.min_thickness_m
    if has_bedrock:
        log(f"  Sampling bedrock at {len(xy):,} mesh nodes ...")
        z_bed = sample_raster(bedrock, xy)
        bad = np.isnan(z_bed)
        if bad.any():
            z_bed[bad] = z_ground[bad] - min_thik
            result.warnings.append(f"Filled {int(bad.sum())} bedrock value(s) as ground - min thickness")
        # enforce bottom <= top - min thickness
        too_thick = z_bed > z_ground - min_thik
        if too_thick.any():
            z_bed[too_thick] = z_ground[too_thick] - min_thik
            result.warnings.append(f"Enforced minimum thickness at {int(too_thick.sum())} node(s)")
        write_elevation_list(model_dir / "BedrockElevation.list", z_bed, "Bedrock elevation (m)")
        z_surf_base_min = float(np.min(z_bed))
    else:
        result.warnings.append(
            "No bedrock_bottom.tif; Surficial uses offset base from ground; "
            "Bedrock still extends below"
        )
        z_surf_base_min = float(np.min(z_ground)) - min_thik
    model_base_m = z_surf_base_min - BEDROCK_EXTENSION_M
    log(
        f"  Model base elevation constant = {model_base_m:.3f} m "
        f"(min surficial base {z_surf_base_min:.3f} - {BEDROCK_EXTENSION_M:g} m); "
        f"Surficial {SURFICIAL_SUBLAYERS} + Bedrock {BEDROCK_SUBLAYERS} log-proportional sublayers"
    )
    progress(60)

    climate_mode = model.climate
    climate_summary = load_climate(project)
    monthly = load_monthly_climate(project)
    daily = load_daily_climate(project)
    if climate_mode in ("average", "transient") and (not climate_summary or (climate_mode == "transient" and not monthly)):
        result.warnings.append(
            f"Climate mode '{climate_mode}' requested but 1_data/climate/ is missing or incomplete; "
            "using a steady SS stub without climate forcing"
        )
        climate_mode = "none"
    if climate_mode == "average" and not (climate_summary or {}).get("lta"):
        result.warnings.append("No long-term average climate; using SS stub")
        climate_mode = "none"

    # Refresh outlet gauges once streams exist on the mesh (regional run may have used the project location only).
    if project.options.climate_retrieval:
        try:
            prepare_network()
            log("  Refreshing outlet river-flow gauges ...")
            retrieve_flows(project, log=log)
        except Exception as exc:  # noqa: BLE001
            result.warnings.append(f"Outlet flow refresh failed: {exc}")

    observed_notes = _copy_observed_flows(project, model_dir)
    outlet = _outlet_nchos(project, spec, log=log) if include_swf else None
    outlet_rel = _rel_to(model_dir, outlet) if outlet is not None else None
    if include_swf and climate_mode in ("average", "transient"):
        if outlet_rel:
            log(f"  Critical-depth SWBC Outlet file: {outlet_rel}")
        else:
            result.warnings.append(
                "No .nchos.Outlet for critical-depth SWBC (need level-1 stream ends on the mesh "
                "boundary). ClimateForcing will omit swf critical depth — not Stream level 1."
            )
    oc_times = ["1.0"]
    if climate_mode in ("average", "transient"):
        lta = (climate_summary or {}).get("lta") or {}
        net_asc_rel = None
        a_mesh = None
        a_cln = None
        net_rate = None
        swf_rate = None
        if climate_mode == "average":
            net_rate, clamped = net_recharge_m_per_d(lta)
            if clamped:
                result.warnings.append(
                    f"LTA PET exceeds P; net recharge clamped to 0 "
                    f"(P={lta.get('P_m_per_d', 0):.6g}, PET={lta.get('PET_m_per_d', 0):.6g} m/d)"
                )
            a_mesh = _mesh_area_m2(project, spec)
            a_cln = _cln_area_m2(project, spec) if include_cln else 0.0
            use_cln_partition = bool(include_cln and a_cln > 0 and a_mesh > 0)
            if include_cln and a_cln <= 0:
                result.warnings.append("CLN included but stream length is zero; precip goes to SWF only")
            if a_mesh <= 0:
                result.warnings.append(
                    "Mesh area unavailable; SWF RCH uses full net rate (CLN precip omitted to avoid double-count)"
                )
                swf_rate = net_rate
                a_cln = 0.0
            elif use_cln_partition:
                if a_cln >= a_mesh:
                    result.warnings.append(
                        f"CLN footprint ({a_cln:.0f} m2) >= mesh area ({a_mesh:.0f} m2); "
                        "SWF RCH set to 0, all net precip on CLN"
                    )
                    swf_rate = 0.0
                else:
                    swf_rate = net_rate * (a_mesh - a_cln) / a_mesh
            else:
                swf_rate = net_rate
                a_cln = 0.0
            if use_cln_partition:
                xmin, ymin = float(xy[:, 0].min()), float(xy[:, 1].min())
                xmax, ymax = float(xy[:, 0].max()), float(xy[:, 1].max())
                asc_path = write_constant_arcascii(
                    model_dir / NET_RECHARGE_ASC,
                    xmin=xmin,
                    ymin=ymin,
                    xmax=xmax,
                    ymax=ymax,
                    value=net_rate,
                )
                result.outputs["net_recharge_asc"] = asc_path
                net_asc_rel = NET_RECHARGE_ASC
                log(
                    f"  Net P-ET={net_rate:.6g} m/d; A_mesh={a_mesh:.3g} m2; "
                    f"A_cln={a_cln:.3g} m2; SWF_RCH={swf_rate:.6g} m/d"
                )
            else:
                log(f"  Net P-ET={net_rate:.6g} m/d -> SWF RCH (no CLN partition)")
        forcing = write_climate_forcing_instructions(
            model_dir / "ClimateForcing.instructions",
            mode=climate_mode,
            monthly=monthly,
            lta=lta,
            include_swf=include_swf,
            outlet_rel=outlet_rel,
            observed_notes=observed_notes,
            include_cln=include_cln and climate_mode == "average" and bool(net_asc_rel),
            net_rate=net_rate,
            swf_rate=swf_rate,
            net_asc_rel=net_asc_rel,
            a_mesh_m2=a_mesh,
            a_cln_m2=a_cln,
        )
        result.outputs["climate_forcing"] = forcing
        climate_text = forcing.read_text(encoding="utf-8")
        if "swf critical depth" in climate_text and "Stream level" in climate_text:
            raise RuntimeError(
                f"{forcing.name} still references a Stream level .nchos for critical depth; "
                "expected .nchos.Outlet only"
            )
        log(f"  Wrote {forcing.name} ({climate_mode})" + (f"; Outlet={outlet_rel}" if outlet_rel else ""))
        if daily:
            rts = write_precipitation_rts(model_dir / "Precipitation.rts", daily)
            result.outputs["precipitation_rts"] = rts
            log(f"  Wrote {rts.name} ({len(daily)} daily rates; optional RTS)")
        oc_times = _oc_times(monthly, climate_mode)
    elif observed_notes:
        # Still document observed Q even without climate forcing
        (model_dir / "ObservedFlows.txt").write_text("\n".join(observed_notes) + "\n", encoding="utf-8")

    want_lake_obs = bool(model.lake_level_obs)
    lake_obs_written = False
    if include_swf and want_lake_obs:
        lakes_gj = project.surfaces_dir / "lakes.geojson"
        lake_depth = project.surfaces_dir / "lake_depth.tif"
        points, lake_warns = find_lake_level_obs_points(lakes_gj, lake_depth, epsg=project.epsg)
        for w in lake_warns:
            result.warnings.append(w)
        if points:
            obs_path = write_swf_lake_level_obs_instructions(model_dir / LAKE_OBS_INSTRUCTIONS, points)
            result.outputs["lake_level_obs"] = obs_path
            lake_obs_written = True
            log(f"  Wrote {obs_path.name} ({len(points)} lake observation point(s))")
        elif not lake_warns:
            result.warnings.append("Lake level observation points requested but none were written")
    elif want_lake_obs and not include_swf:
        result.warnings.append("Lake level observation points need the SWF domain; skipped")

    text = write_build_mut(
        project,
        spec,
        model,
        model_dir,
        has_bedrock=has_bedrock,
        model_base_m=model_base_m,
        include_swf=include_swf,
        include_cln=include_cln,
        include_nchos=include_nchos,
        climate_mode=climate_mode,
        oc_times=oc_times,
        lake_level_obs=lake_obs_written,
    )
    mut_path = model_dir / "_build.mut"
    mut_path.write_text(text, encoding="utf-8")
    progress(85)

    # Also refresh mesh mut_snippet for convenience
    levels = []
    for p in nchos_paths(project, spec):
        if "Stream level" in p.name:
            try:
                levels.append(int(p.name.rsplit(" ", 1)[-1]))
            except ValueError:
                pass
    has_outlet = bool(outlet is not None and Path(outlet).exists())
    outs["snippet"].write_text(
        mut_snippet(project, spec, levels or None, has_outlet=has_outlet), encoding="utf-8"
    )

    model.status = "built"
    model.include_swf = include_swf
    model.include_cln = include_cln
    model.include_stream_nchos = include_nchos
    model.climate = climate_mode
    model.lake_level_obs = want_lake_obs and include_swf
    project.set_model(model)
    project.save()

    result.ok = True
    result.model_dir = model_dir
    result.outputs["mut"] = mut_path
    result.outputs["ground"] = model_dir / "GroundElevation.list"
    if has_bedrock:
        result.outputs["bedrock"] = model_dir / "BedrockElevation.list"
    for w in result.warnings:
        log(f"WARNING: {w}")
    log(f"Wrote {mut_path}")
    progress(100)
    return result
