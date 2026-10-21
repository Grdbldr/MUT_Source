"""Export ranked stream polylines as MUT ``cln from xyz list`` files (Z from the regional ground surface).

Each ranked stream path becomes its own XYZ list so disconnected rivers are not falsely linked.
``_build.mut`` uses ``cln mesh intersection`` to split segments at GWF faces; the plugin does not.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import gdal, ogr

from .mesh import mesh_outputs, streams_preview_path
from .project import MeshSpec, Project
from .streams import StreamNetwork, StreamPath

CLN_XYZ_NAME = "Streams_CLN.xyzList"  # primary (longest level-1) list; also listed first in cln.json


@dataclass
class ClnExportResult:
    ok: bool = False
    path: Path | None = None
    paths: list[Path] = field(default_factory=list)
    n_points: int = 0
    n_paths: int = 0
    warnings: list[str] = field(default_factory=list)
    outputs: dict[str, Path] = field(default_factory=dict)


def cln_xyz_path(project: Project, spec: MeshSpec) -> Path:
    """Primary ``2_mesh/<slug>/Streams_CLN.xyzList``."""
    return project.mesh_dir(spec) / CLN_XYZ_NAME


def _resample_line(coords: np.ndarray, ds: float) -> np.ndarray:
    """Vertices along ``coords`` at about ``ds`` metres, keeping the ends."""
    if len(coords) < 2 or ds <= 0:
        return np.asarray(coords, dtype=float)
    out = [coords[0]]
    carry = 0.0
    for a, b in zip(coords[:-1], coords[1:]):
        seg = b - a
        length = float(np.hypot(*seg))
        if length <= 0:
            continue
        while carry + length >= ds:
            t = (ds - carry) / length
            out.append(a + t * seg)
            a = out[-1]
            seg = b - a
            length = float(np.hypot(*seg))
            carry = 0.0
            if length <= 1e-9:
                break
        carry += length
    if len(out) == 1 or not np.allclose(out[-1], coords[-1]):
        out.append(coords[-1])
    return np.asarray(out, dtype=float)


def sample_raster(path: Path, xy: np.ndarray) -> np.ndarray:
    """Bilinear sample of band 1 at ``xy`` (n, 2); nodata / outside → NaN."""
    ds = gdal.Open(str(path))
    if ds is None:
        raise FileNotFoundError(f"Raster not found: {path}")
    band = ds.GetRasterBand(1)
    gt = ds.GetGeoTransform()
    nodata = band.GetNoDataValue()
    inv = gdal.InvGeoTransform(gt)
    if inv is None:
        raise RuntimeError(f"Could not invert geotransform for {path}")
    z = np.full(len(xy), np.nan, dtype=float)
    for i, (x, y) in enumerate(xy):
        px, py = gdal.ApplyGeoTransform(inv, float(x), float(y))
        ix, iy = int(np.floor(px)), int(np.floor(py))
        if ix < 0 or iy < 0 or ix + 1 >= ds.RasterXSize or iy + 1 >= ds.RasterYSize:
            continue
        buf = band.ReadAsArray(ix, iy, 2, 2)
        if buf is None:
            continue
        vals = buf.astype(float)
        if nodata is not None:
            vals[vals == nodata] = np.nan
        vals[np.abs(vals) > 1e20] = np.nan
        if not np.isfinite(vals).all():
            finite = vals[np.isfinite(vals)]
            if finite.size:
                z[i] = float(finite[0])
            continue
        fx, fy = px - ix, py - iy
        z[i] = (
            (1 - fx) * (1 - fy) * vals[0, 0]
            + fx * (1 - fy) * vals[0, 1]
            + (1 - fx) * fy * vals[1, 0]
            + fx * fy * vals[1, 1]
        )
    return z


def _fill_nan(z: np.ndarray) -> int:
    """Fill NaNs from nearest finite neighbour along the chain; returns how many were filled."""
    missing = int(np.isnan(z).sum())
    if not missing:
        return 0
    for i in range(len(z)):
        if np.isfinite(z[i]):
            continue
        for j in range(1, len(z)):
            for k in (i - j, i + j):
                if 0 <= k < len(z) and np.isfinite(z[k]):
                    z[i] = z[k]
                    break
            if np.isfinite(z[i]):
                break
    return missing


def paths_from_gpkg(path: Path, layer_name: str = "streams") -> list[StreamPath]:
    """Read ranked stream lines from a preview or mesh GeoPackage."""
    ds = ogr.Open(str(path))
    if ds is None:
        raise FileNotFoundError(f"Stream GeoPackage not found: {path}")
    layer = ds.GetLayerByName(layer_name)
    if layer is None:
        raise ValueError(f"No '{layer_name}' layer in {path}")
    paths: list[StreamPath] = []
    for feat in layer:
        geom = feat.GetGeometryRef()
        if geom is None:
            continue
        coords = np.array([[geom.GetX(i), geom.GetY(i)] for i in range(geom.GetPointCount())], dtype=float)
        if len(coords) < 2:
            continue
        level = int(feat.GetField("level") or 1)
        name = str(feat.GetField("name") or "")
        paths.append(StreamPath(level=level, name=name, coords=coords))
    return paths


def stream_preview_candidates(project: Project, spec: MeshSpec | None = None) -> list[Path]:
    """Ranked-stream GeoPackages: mesh-named preview first, then any ``_streams/*_preview.gpkg``."""
    root = project.mesh_root / "_streams"
    ordered: list[Path] = []
    if spec is not None:
        named = streams_preview_path(project, spec)
        if named.exists():
            ordered.append(named)
    if root.is_dir():
        for path in sorted(root.glob("*_preview.gpkg")) + sorted(root.glob("*preview*.gpkg")):
            if path not in ordered and path.exists():
                ordered.append(path)
    return ordered


def has_stream_geometry(project: Project, spec: MeshSpec) -> bool:
    """True if the mesh GPKG or a project stream preview has ranked stream lines."""
    try:
        return bool(load_stream_paths(project, spec))
    except (FileNotFoundError, ValueError, OSError):
        return False


def load_stream_paths(project: Project, spec: MeshSpec) -> list[StreamPath]:
    """Prefer the mesh GPKG streams layer, else a Find-streams preview GPKG (any slug in ``_streams/``)."""
    mesh_gpkg = mesh_outputs(project, spec)["gpkg"]
    if mesh_gpkg.exists():
        try:
            found = paths_from_gpkg(mesh_gpkg)
            if found:
                return found
        except ValueError:
            pass
    for preview in stream_preview_candidates(project, spec):
        try:
            found = paths_from_gpkg(preview)
            if found:
                return found
        except ValueError:
            continue
    raise FileNotFoundError(
        f"No ranked streams for mesh '{spec.name}'. Run Find streams (Add streams...) first; "
        f"looked in {mesh_gpkg} and {project.mesh_root / '_streams'}."
    )


def write_xyz_list(path: Path, xy: np.ndarray, z: np.ndarray, header: str = "ID X Y Z") -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    lines = [header]
    for i, ((x, y), zi) in enumerate(zip(xy, z), start=1):
        if not np.isfinite(zi):
            raise ValueError(f"Missing ground elevation at stream point {i} ({x:.3f}, {y:.3f})")
        lines.append(f"{i} {x:.4f} {y:.4f} {zi:.4f}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


def cln_files(project: Project, spec: MeshSpec) -> list[Path]:
    """XYZ lists recorded for this mesh (from ``cln.json`` or the primary file)."""
    mesh_dir = project.mesh_dir(spec)
    summary = mesh_dir / "cln.json"
    if summary.exists():
        data = json.loads(summary.read_text(encoding="utf-8"))
        files = [Path(p) if Path(p).is_absolute() else mesh_dir / p for p in data.get("xyz_files", [])]
        files = [p for p in files if p.exists()]
        if files:
            return files
    primary = cln_xyz_path(project, spec)
    return [primary] if primary.exists() else []


def cln_snippet(project: Project, spec: MeshSpec) -> str:
    """MUT instructions assuming the model folder is two levels below the project folder."""
    files = cln_files(project, spec) or [cln_xyz_path(project, spec)]
    lines = [
        f"! Streams as CLN for mesh '{spec.name}' (MUT Model Builder); keep the regional mesh unchanged.",
        "! Place these after generate layered gwf domain so the intersection has a GWF mesh.",
        "! One generate cln domain only (MUT resets the domain on each entry); lists append as zones.",
    ]
    for i, path in enumerate(files, start=1):
        rel = f"..\\..\\2_mesh\\{spec.slug}\\{path.name}"
        stem = f"Streams_CLN_{i}" if len(files) > 1 else "Streams_CLN"
        lines += ["", "cln mesh intersection", rel, f"{stem}_split.dat"]
    lines += ["", "generate cln domain"]
    for i, _path in enumerate(files, start=1):
        stem = f"Streams_CLN_{i}" if len(files) > 1 else "Streams_CLN"
        lines += ["    cln from xyz list", f"    {stem}_split.xyzList"]
    lines += ["end generate cln domain", ""]
    return "\n".join(lines)


def export_cln(
    project: Project,
    spec: MeshSpec,
    network: StreamNetwork | None = None,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
) -> ClnExportResult:
    """Write stream XYZ lists beside the mesh, update ``MeshSpec``, save ``project.yaml``."""
    progress = progress or (lambda pct: None)
    result = ClnExportResult()
    mesh_dir = project.mesh_dir(spec)
    if not mesh_dir.exists() or not (mesh_dir / f"{spec.slug}.grd").exists():
        raise FileNotFoundError(
            f"Mesh '{spec.name}' is not built yet ({mesh_dir}). Build the regional mesh first, then attach streams as CLN."
        )
    ground = project.surfaces_dir / "ground_top.tif"
    if not ground.exists():
        raise FileNotFoundError(f"Regional ground surface not found: {ground}")

    progress(5)
    paths = list(network.paths) if network is not None else load_stream_paths(project, spec)
    if not paths:
        raise RuntimeError("No stream paths to export")

    ds = (spec.streams.spacing_m if spec.streams else 0.0) or spec.boundary_spacing_m
    log(f"CLN export for mesh '{spec.name}': {len(paths)} stream path(s), spacing {ds:g} m")
    progress(15)

    # Longest level-1 first (primary Streams_CLN.xyzList), then remaining by level then length.
    ordered = sorted(paths, key=lambda p: (0 if p.level == 1 else p.level, -p.length_m))
    written: list[Path] = []
    total_pts = 0
    for i, path in enumerate(ordered):
        xy = _resample_line(path.coords, ds)
        z = sample_raster(ground, xy)
        filled = _fill_nan(z)
        if filled:
            result.warnings.append(f"Path '{path.name or i+1}': filled {filled} elevation(s) from neighbours")
        if int(np.isnan(z).sum()):
            raise ValueError(f"Stream path '{path.name or i+1}' has points with no ground elevation in {ground}")
        if i == 0:
            out = cln_xyz_path(project, spec)
        else:
            out = mesh_dir / f"Streams_CLN_{i+1:02d}.xyzList"
        write_xyz_list(out, xy, z)
        written.append(out)
        total_pts += len(xy)
        log(f"  {out.name}: {len(xy):,} points, level {path.level}" + (f", {path.name}" if path.name else ""))
        progress(15 + 70 * (i + 1) / len(ordered))

    # Drop stale numbered lists from a previous export with more paths.
    keep = {p.name for p in written}
    for old in mesh_dir.glob("Streams_CLN*.xyzList"):
        if old.name not in keep:
            old.unlink()

    snippet = mesh_dir / "cln_snippet.txt"
    summary = {
        "generated": datetime.now().isoformat(timespec="seconds"),
        "mesh": spec.name,
        "points": total_pts,
        "paths": len(written),
        "spacing_m": ds,
        "ground": str(ground),
        "xyz": str(written[0]),
        "xyz_files": [p.name for p in written],
        "warnings": result.warnings,
    }
    (mesh_dir / "cln.json").write_text(json.dumps(summary, indent=2), encoding="utf-8")
    spec.stream_mode = "cln"
    spec.cln_xyz_file = str(written[0])
    project.set_mesh(spec)
    project.save()
    snippet.write_text(cln_snippet(project, spec), encoding="utf-8")

    result.ok = True
    result.path = written[0]
    result.paths = written
    result.n_points = total_pts
    result.n_paths = len(written)
    result.outputs = {"xyz": written[0], "snippet": snippet, "summary": mesh_dir / "cln.json"}
    log(f"Wrote {len(written)} CLN XYZ list(s) ({total_pts:,} points); primary {written[0]}")
    log(f"CLN MUT snippet: {snippet}")
    progress(100)
    return result
