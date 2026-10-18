#!/usr/bin/env python3
"""Run MUT Model Builder without the QGIS GUI (use QGIS's Python: python-qgis-ltr.bat).

    python-qgis-ltr.bat run_headless.py init  C:\\Work\\Site\\project.yaml --name Site --lat 36.42 --lon 127.37
    python-qgis-ltr.bat run_headless.py init  C:\\Work\\Site\\project.yaml --name Site --watershed "Geum River"
    python-qgis-ltr.bat run_headless.py watersheds "Boulder Creek" --lat 40.02 --lon -105.27 --delineate 1
    python-qgis-ltr.bat run_headless.py inventory C:\\Work\\Site\\project.yaml
    python-qgis-ltr.bat run_headless.py run   C:\\Work\\Site\\project.yaml
    python-qgis-ltr.bat run_headless.py focus-search C:\\Work\\Site\\project.yaml "Luther Marsh"
    python-qgis-ltr.bat run_headless.py focus-add C:\\Work\\Site\\project.yaml --name Luther --feature "Luther Marsh"
    python-qgis-ltr.bat run_headless.py focus-run C:\\Work\\Site\\project.yaml --name Luther
    python-qgis-ltr.bat run_headless.py mesh C:\\Work\\Site\\project.yaml --name Regional --spacing-m 300 --points 5000
    python-qgis-ltr.bat run_headless.py streams C:\\Work\\Site\\project.yaml --name Regional --stream-levels 3
    python-qgis-ltr.bat run_headless.py mesh C:\\Work\\Site\\project.yaml --name Regional --streams auto --stream-levels 2

``init`` writes a new project.yaml (output folder defaults to the YAML folder).
Model boundary (default: a square of --regional-km around the project location):
    --boundary-rect W,S,E,N    rectangle in decimal degrees (write --boundary-rect=W,... when W is negative)
    --watershed NAME           named watershed (--pick N chooses the candidate, --outlet K the outlet for rivers)
    --watershed-at-site        watershed draining to the project location
    --boundary-file PATH       polygons from a vector file
Without --lat/--lon the project location is the boundary centroid.
``watersheds`` lists named watershed candidates (and river outlets with --delineate N).
``inventory`` scans the data folder and stores suggested roles in the YAML.
``run`` builds the regional surfaces, writes the report and a QGIS project (.qgz).
``run`` and ``focus-run`` refuse grids above 250 million cells before downloading unless --allow-large-grid is given.
``focus-search`` lists features inside the regional area; ``focus-add`` defines a focus area (rectangle, vector
file, or a feature and its local watershed) without downloading; ``focus-run`` builds its ~1 m ground surface.
``mesh`` builds a 2D Delaunay triangle mesh (fixed boundary and --fixed-points nodes, about --points free interior
nodes, optional --smooth passes) and writes Grid Builder files for MUT's ``2d mesh from gb`` under 2_mesh/<name>.
With --streams it also fixes nodes along ranked rivers and streams (--stream-levels, --stream-min-km, ...) and writes
``.nchos`` node selections per level; ``streams`` only finds and ranks them into a preview GeoPackage.
Set OPENTOPOGRAPHY_API_KEY to enable the OpenTopography fallbacks.
"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

_HERE = Path(__file__).resolve().parent
if str(_HERE) not in sys.path:
    sys.path.insert(0, str(_HERE))

from core import boundary as bnd  # noqa: E402
from core.geo import MAX_GRID_CELLS, GridTooLargeError, utm_epsg  # noqa: E402
from core.inventory import scan_folder  # noqa: E402
from core.pipeline import run_pipeline  # noqa: E402
from core.project import DataFile, Project  # noqa: E402
from core.providers import default_provider_ids  # noqa: E402


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="MUT Model Builder (headless)")
    sub = parser.add_subparsers(dest="command", required=True)

    init = sub.add_parser("init", help="Create a new project.yaml")
    init.add_argument("yaml", type=Path)
    init.add_argument("--name", required=True)
    init.add_argument("--lat", type=float, help="Project location latitude (default: boundary centroid)")
    init.add_argument("--lon", type=float, help="Project location longitude (default: boundary centroid)")
    init.add_argument("--description", default="")
    init.add_argument("--data", default="", help="Folder of existing project data")
    init.add_argument("--output", default="", help="Output folder (default: YAML folder)")
    init.add_argument("--regional-km", type=float, default=20.0, help="Half-width of the square boundary")
    init.add_argument("--epsg", type=int, default=0, help="Projected CRS (default: UTM zone of the project location)")
    group = init.add_mutually_exclusive_group()
    group.add_argument("--boundary-rect", help="W,S,E,N in decimal degrees")
    group.add_argument("--watershed", help="Name of a watershed, basin or river")
    group.add_argument("--watershed-at-site", action="store_true", help="Watershed draining to the project location")
    group.add_argument("--boundary-file", type=Path, help="Vector file with the boundary polygon(s)")
    init.add_argument("--pick", type=int, default=0, help="Index of the --watershed candidate (default 0)")
    init.add_argument("--outlet", type=int, default=0, help="Index of the river outlet, largest first (default 0)")
    init.add_argument("--margin-km", type=float, default=1.0, help="Margin around the boundary (default 1 km)")

    ws = sub.add_parser("watersheds", help="List watershed candidates for a name")
    ws.add_argument("name")
    ws.add_argument("--lat", type=float)
    ws.add_argument("--lon", type=float)
    ws.add_argument("--delineate", type=int, help="Delineate outlets for this river candidate index")

    inv = sub.add_parser("inventory", help="Scan the data folder and store suggested roles")
    inv.add_argument("yaml", type=Path)

    run = sub.add_parser("run", help="Build surfaces, report and QGIS project")
    run.add_argument("yaml", type=Path)
    run.add_argument("--no-qgz", action="store_true", help="Skip writing the .qgz QGIS project")
    run.add_argument("--refresh", action="store_true", help="Ignore cached downloads")
    _large_grid_arg(run)
    _lake_args(run)

    fs = sub.add_parser("focus-search", help="List OpenStreetMap features with this name inside the regional area")
    fs.add_argument("yaml", type=Path)
    fs.add_argument("name")

    fa = sub.add_parser("focus-add", help="Define (but do not build) a focus area")
    fa.add_argument("yaml", type=Path)
    fa.add_argument("--name", required=True, help="Focus area name")
    how = fa.add_mutually_exclusive_group(required=True)
    how.add_argument("--rect", help="W,S,E,N in decimal degrees")
    how.add_argument("--file", type=Path, help="Vector file with the outline polygon(s)")
    how.add_argument("--feature", help="Name of a feature inside the regional area, e.g. 'Luther Marsh'")
    fa.add_argument("--pick", type=int, default=0, help="Index of the --feature candidate (default 0)")
    fa.add_argument(
        "--delineate",
        choices=("dem", "global", "none"),
        default="dem",
        help="Feature outline: local watershed from the regional DEM (default), Global Watersheds, or the feature itself",
    )
    fa.add_argument("--margin-m", type=float, default=200.0, help="Margin around the outline (default 200 m)")
    fa.add_argument("--res-m", type=float, default=0.0, help="Cell size (default: the project's focus cell size)")

    fr = sub.add_parser("focus-run", help="Download and build the high-resolution DEM of a focus area")
    fr.add_argument("yaml", type=Path)
    fr.add_argument("--name", required=True)
    fr.add_argument("--no-qgz", action="store_true", help="Skip rewriting the .qgz QGIS project")
    fr.add_argument("--refresh", action="store_true", help="Ignore cached downloads")
    _large_grid_arg(fr)
    _lake_args(fr)

    me = sub.add_parser("mesh", help="Build a 2D Delaunay triangle mesh of the model area for MUT")
    me.add_argument("yaml", type=Path)
    me.add_argument("--name", required=True, help="Mesh name")
    me.add_argument("--spacing-m", type=float, default=300.0, help="Boundary point spacing (default 300 m)")
    me.add_argument("--points", type=int, default=5000, help="Number of interior points (default 5000)")
    me.add_argument(
        "--simplify-m",
        type=float,
        default=0.0,
        help="Remove boundary detail smaller than this (default 0: mesh edges follow the boundary exactly)",
    )
    me.add_argument("--fixed-points", type=Path, help="Vector file of fixed points (wells, shafts, observation points)")
    me.add_argument("--smooth", type=int, default=0, help="Laplacian smoothing passes on free nodes (default 0)")
    me.add_argument("--preview", action="store_true", help="Only report the estimated spacing and point counts")
    me.add_argument("--no-qgz", action="store_true", help="Skip rewriting the .qgz QGIS project")
    _stream_args(me, "Add fixed nodes and mesh edges along streams from this source")

    st = sub.add_parser("streams", help="Find and rank the streams for a mesh (preview GeoPackage, no mesh)")
    st.add_argument("yaml", type=Path)
    st.add_argument("--name", required=True, help="Mesh name (names the preview file)")
    st.add_argument("--spacing-m", type=float, default=300.0, help="Boundary point spacing (default 300 m)")
    st.add_argument("--simplify-m", type=float, default=0.0, help="Boundary simplification tolerance (default 0)")
    st.add_argument("--refresh", action="store_true", help="Ignore cached stream downloads")
    _stream_args(st, "Stream source (default auto)", default="auto")
    return parser.parse_args(argv)


def _stream_args(parser: argparse.ArgumentParser, help_text: str, default: str | None = None) -> None:
    parser.add_argument(
        "--streams",
        choices=("auto", "ohn", "nhdplus", "osm", "dem"),
        default=default,
        help=f"{help_text}: auto (Ontario Hydro Network, NHDPlus HR, then OpenStreetMap), ohn, nhdplus, osm, or dem",
    )
    parser.add_argument("--stream-levels", type=int, default=2, help="1 = main river only, 2 = and its tributaries, ... (default 2)")
    parser.add_argument("--stream-min-km", type=float, default=5.0, help="Shortest stream kept, inside the boundary (default 5 km)")
    parser.add_argument("--stream-spacing-m", type=float, default=0.0, help="Stream node spacing (default: boundary spacing)")
    parser.add_argument("--main-river", default="", help="Name of the main river (followed at confluences for level 1)")
    parser.add_argument("--canals", action="store_true", help="Include OpenStreetMap canals")
    parser.add_argument("--dem-threshold-km2", type=float, default=10.0, help="DEM streams: smallest drainage area (default 10 km2)")


def _stream_options(args):
    from core.project import StreamOptions

    if not args.streams:
        return None
    return StreamOptions(
        source=args.streams,
        levels=args.stream_levels,
        min_length_km=args.stream_min_km,
        spacing_m=args.stream_spacing_m,
        main_river=args.main_river,
        include_canals=args.canals,
        dem_threshold_km2=args.dem_threshold_km2,
    )


def _large_grid_arg(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--allow-large-grid",
        action="store_true",
        help=f"Run even when the grid exceeds {MAX_GRID_CELLS / 1e6:,.0f} million cells (otherwise refused before downloading)",
    )


def _lake_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--bathymetry",
        choices=("on", "off"),
        help="Correct lakes with bathymetry (default: the project's lake_bathymetry option, normally on); saved",
    )
    parser.add_argument("--lake-min-ha", type=float, help="Smallest lake corrected, in hectares (saved)")


def _apply_lake_args(args, project: Project) -> None:
    if getattr(args, "bathymetry", None):
        project.options.lake_bathymetry = args.bathymetry == "on"
    if getattr(args, "lake_min_ha", None) is not None:
        project.options.lake_min_area_ha = args.lake_min_ha


def _log(msg: str) -> None:
    print(msg, flush=True)


def _inventory(project: Project) -> None:
    if not project.data_folder:
        _log("No data folder set; nothing to scan.")
        return
    region = project.regional_grid().geo_bbox()
    items = scan_folder(Path(project.data_folder), centre_lat=project.lat, region=region)
    project.data_files = [DataFile(str(i.path), i.suggested_role) for i in items]
    for item in items:
        _log(f"  {item.suggested_role:18s} {item.path}  ({item.summary})")


def _near(args) -> tuple[float, float] | None:
    return (args.lat, args.lon) if args.lat is not None and args.lon is not None else None


def _list(candidates: list[bnd.BoundaryCandidate]) -> None:
    for i, cand in enumerate(candidates):
        _log(f"  [{i}] {cand.kind:7s} {cand.display}")


def _pick_watershed(args) -> bnd.BoundaryCandidate:
    near = _near(args)
    if args.watershed_at_site:
        if near is None:
            raise SystemExit("--watershed-at-site needs --lat and --lon")
        return bnd.delineate(*near)
    candidates = bnd.search_watersheds(args.watershed, near, _log)
    _list(candidates)
    if not candidates or not 0 <= args.pick < len(candidates):
        raise SystemExit(f"No watershed candidate {args.pick} for '{args.watershed}'")
    chosen = candidates[args.pick]
    if chosen.kind == "river":
        outlets = bnd.river_outlets(chosen, near, _log)
        _list(outlets)
        if not outlets or not 0 <= args.outlet < len(outlets):
            raise SystemExit(f"No outlet {args.outlet} could be delineated for {chosen.label}")
        chosen = outlets[args.outlet]
    _log(f"Using: {chosen.display}")
    return chosen


def _init(args, yaml_path: Path) -> int:
    output = args.output or str(yaml_path.parent)
    near = _near(args)
    boundary_given = args.boundary_rect or args.watershed or args.watershed_at_site or args.boundary_file
    if near is None and not boundary_given:
        raise SystemExit("Give --lat/--lon or a boundary option")
    lat, lon = near if near is not None else (0.0, 0.0)
    project = Project(
        name=args.name,
        lat=lat,
        lon=lon,
        output_folder=output,
        description=args.description,
        data_folder=args.data,
        regional_buffer_km=args.regional_km,
        epsg=args.epsg,
        boundary_margin_km=args.margin_km,
    )
    if args.boundary_rect:
        w, s, e, n = (float(v) for v in args.boundary_rect.split(","))
        bnd.apply_geometry(project, bnd.rectangle_geometry(w, s, e, n), "rectangle", "Rectangle", "user (decimal degrees)")
    elif args.boundary_file:
        bnd.apply_geometry(project, bnd.file_geometry(args.boundary_file), "file", args.boundary_file.stem, str(args.boundary_file))
    elif args.watershed or args.watershed_at_site:
        bnd.apply_candidate(project, _pick_watershed(args))
    if near is None:
        project.lat, project.lon = bnd.boundary_centroid(Path(project.boundary_file))
        if not args.epsg:
            project.epsg = utm_epsg(project.lat, project.lon)
        _log(f"Project location set to the boundary centroid: {project.lat:.5f}, {project.lon:.5f}")
    project.providers = default_provider_ids(project)
    _inventory(project)
    project.save(yaml_path)
    _log(f"Wrote {yaml_path} (CRS EPSG:{project.epsg})")
    return 0


def _write_qgz(project: Project, result=None) -> None:
    from qgis.core import QgsApplication

    prefix = os.environ.get("QGIS_PREFIX_PATH")
    if prefix:
        QgsApplication.setPrefixPath(prefix, True)
    app = QgsApplication([], False)
    app.initQgis()
    try:
        from core.qgis_project import write_qgz

        _log(f"QGIS project: {write_qgz(project, result)}")
    finally:
        app.exitQgis()


def _focus_search(project: Project, name: str) -> list:
    from core import local_watershed as lw

    found = lw.search_features(name, project.regional_grid().geo_bbox())
    for i, f in enumerate(found):
        area = lw.feature_area_km2(f.geometry, project.epsg)
        _log(f"  [{i}] {f.display}" + (f" {area:,.2f} km2" if area else ""))
    if not found:
        _log(f"No feature named like '{name}' inside the regional area.")
    return found


def _focus_add(args, project: Project) -> int:
    from core import local_watershed as lw
    from core.focus import define_focus, describe, grid_summary
    from core.project import FocusArea

    res = args.res_m or project.options.fine_resolution_m
    focus = FocusArea(name=args.name, margin_m=args.margin_m, resolution_m=res)
    citation = ""
    if args.rect:
        w, s, e, n = (float(v) for v in args.rect.split(","))
        geometry = bnd.rectangle_geometry(w, s, e, n)
        focus.mode, focus.source = "rectangle", "user (decimal degrees)"
    elif args.file:
        geometry = bnd.file_geometry(args.file)
        focus.mode, focus.source = "file", str(args.file)
    else:
        found = _focus_search(project, args.feature)
        if not found or not 0 <= args.pick < len(found):
            raise SystemExit(f"No feature candidate {args.pick} for '{args.feature}'")
        feature = found[args.pick]
        _log(f"Using: {feature.display}")
        delineation = {"dem": "regional_dem", "global": "global_watersheds", "none": "none"}[args.delineate]
        outline = lw.outline_from_feature(project, feature, delineation, _log)
        geometry, citation = outline.geometry, outline.citation
        focus.mode = "feature" if delineation == "none" else "feature_watershed"
        focus.delineation = delineation
        focus.feature_name = feature.label
        focus.source = outline.source
        focus.outlet_lat, focus.outlet_lon = outline.outlet_lat, outline.outlet_lon
    define_focus(project, focus, geometry, citation)
    summary = grid_summary(project, focus)
    _log(f"Focus area '{focus.name}' defined: {describe(summary, res)}")
    if summary["warning"]:
        _log(f"WARNING: {summary['warning']}")
    _log(f"Outline: {focus.boundary_file}. Check it in QGIS, then run: focus-run \"{project.yaml_path}\" --name \"{focus.name}\"")
    return 0


def _focus_run(args, project: Project) -> int:
    from core.focus import build_focus

    focus = project.get_focus(args.name)
    if focus is None:
        names = ", ".join(f.name for f in project.focus_areas) or "none"
        raise SystemExit(f"No focus area '{args.name}' in {project.yaml_path} (defined: {names})")
    api_keys = {"opentopography": os.environ.get("OPENTOPOGRAPHY_API_KEY", "")}
    try:
        result = build_focus(
            project, focus, api_keys=api_keys, log=_log, refresh=args.refresh, allow_large_grid=args.allow_large_grid
        )
    except GridTooLargeError as exc:
        raise SystemExit(f"ERROR: {exc}") from None
    if not args.no_qgz:
        _write_qgz(project)
    _log(f"Report: {result.report_path}")
    return 0 if result.ok else 1


def _mesh(args, project: Project) -> int:
    from core.mesh import build_mesh, preview
    from core.project import MeshSpec

    spec = MeshSpec(
        name=args.name,
        boundary_spacing_m=args.spacing_m,
        boundary_tolerance_m=args.simplify_m,
        interior_points=args.points,
        fixed_points_file=str(args.fixed_points.resolve()) if args.fixed_points else "",
        smoothing_iterations=args.smooth,
        streams=_stream_options(args),
    )
    if args.preview:
        p = preview(project, spec)
        _log(
            f"Area {p['area_km2']:,.2f} km2; {p['boundary_points']:,} boundary points (shortest side "
            f"{p['shortest_boundary_side_m']:,.1f} m); interior spacing about "
            f"{p['interior_spacing_m']:,.1f} m; about {p['total_points']:,} points in total"
        )
        return 0
    result = build_mesh(project, spec, log=_log)
    if not args.no_qgz:
        _write_qgz(project)
    _log(f"MUT instructions: {result.outputs.get('snippet')}")
    return 0 if result.ok else 1


def _streams(args, project: Project) -> int:
    from core.mesh import preview_streams
    from core.project import MeshSpec

    spec = MeshSpec(
        name=args.name,
        boundary_spacing_m=args.spacing_m,
        boundary_tolerance_m=args.simplify_m,
        streams=_stream_options(args),
    )
    network, path = preview_streams(project, spec, log=_log, refresh=args.refresh)
    for msg in network.warnings:
        _log(f"  {msg}")
    _log(f"Source: {network.citation}")
    return 0


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)

    if args.command == "watersheds":
        candidates = bnd.search_watersheds(args.name, _near(args), _log)
        _list(candidates)
        if args.delineate is not None:
            _list(bnd.river_outlets(candidates[args.delineate], _near(args), _log))
        return 0

    yaml_path: Path = args.yaml.resolve()
    if args.command == "init":
        return _init(args, yaml_path)

    project = Project.load(yaml_path)

    if args.command == "inventory":
        _inventory(project)
        project.save(yaml_path)
        _log(f"Updated {yaml_path}")
        return 0

    if args.command in ("focus-search", "focus-add"):
        from core.net import prepare_network

        prepare_network()
        if args.command == "focus-search":
            _focus_search(project, args.name)
            return 0
        return _focus_add(args, project)

    if args.command in ("mesh", "streams"):
        if args.streams:
            from core.net import prepare_network

            prepare_network()
        return _mesh(args, project) if args.command == "mesh" else _streams(args, project)

    if not project.data_files and project.data_folder:
        _log("No data roles in the project file; using suggested roles from a folder scan:")
        _inventory(project)
    _apply_lake_args(args, project)
    if args.bathymetry or args.lake_min_ha is not None:
        project.save()

    if args.command == "focus-run":
        return _focus_run(args, project)

    api_keys = {"opentopography": os.environ.get("OPENTOPOGRAPHY_API_KEY", "")}
    try:
        result = run_pipeline(
            project, api_keys=api_keys, log=_log, refresh=args.refresh, allow_large_grid=args.allow_large_grid
        )
    except GridTooLargeError as exc:
        raise SystemExit(f"ERROR: {exc}") from None
    if not args.no_qgz:
        _write_qgz(project, result)
    _log(f"Report: {result.report_path}")
    return 0 if result.ok else 1


if __name__ == "__main__":
    sys.exit(main())
