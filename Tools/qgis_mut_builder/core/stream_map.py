"""Map ranked stream polylines onto an existing mesh for SWF ``.nchos`` (no remesh, no CLN).

Each stream is resampled, snapped to nearest mesh nodes within half the stream spacing, then consecutive
snaps are joined by shortest paths on the undirected triangle graph. Node ``stream_level`` and ``.nchos``
files are updated; the triangulation is left unchanged.
"""

from __future__ import annotations

import csv
import heapq
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np
from scipy.spatial import cKDTree


@dataclass
class StreamMapResult:
    ok: bool = False
    n_stream_nodes: int = 0
    n_paths: int = 0
    warnings: list[str] = field(default_factory=list)
    outputs: dict[str, Path] = field(default_factory=dict)
    levels: list[int] = field(default_factory=list)


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


def load_mesh_from_csvs(xy_csv: Path, idnode_csv: Path):
    """Rebuild a ``Mesh`` from ``*_xyCoord.csv`` and ``*_idNode.csv`` (1-based triangle indices)."""
    from .mesh import SOURCES, Mesh

    rows = []
    with open(xy_csv, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            rows.append(row)
    if not rows:
        raise ValueError(f"No nodes in {xy_csv}")
    xy = np.asarray([(float(r["xcoord"]), float(r["ycoord"])) for r in rows], dtype=float)
    fixed = np.asarray([bool(int(r.get("fixed", 0))) for r in rows], dtype=bool)
    source = np.zeros(len(rows), np.int8)
    for i, r in enumerate(rows):
        name = (r.get("source") or "interior").strip()
        source[i] = SOURCES.index(name) if name in SOURCES else SOURCES.index("interior")
    level = np.asarray([int(float(r.get("stream_level") or 0)) for r in rows], dtype=np.int8)
    tris = []
    with open(idnode_csv, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            tris.append((int(row["POINTA"]) - 1, int(row["POINTB"]) - 1, int(row["POINTC"]) - 1))
    if not tris:
        raise ValueError(f"No triangles in {idnode_csv}")
    return Mesh(xy, fixed, source, triangles=np.asarray(tris, dtype=np.int64), level=level)


def mesh_adjacency(triangles: np.ndarray, xy: np.ndarray) -> list[list[tuple[int, float]]]:
    """Undirected adjacency list with Euclidean edge lengths."""
    n = len(xy)
    adj: list[set[int]] = [set() for _ in range(n)]
    for a, b, c in triangles:
        for u, v in ((a, b), (b, c), (c, a)):
            adj[int(u)].add(int(v))
            adj[int(v)].add(int(u))
    out: list[list[tuple[int, float]]] = []
    for i, nbrs in enumerate(adj):
        xi, yi = xy[i]
        out.append([(j, float(np.hypot(xy[j, 0] - xi, xy[j, 1] - yi))) for j in sorted(nbrs)])
    return out


def shortest_path(adj: list[list[tuple[int, float]]], start: int, end: int) -> list[int]:
    """Dijkstra shortest path; empty if unreachable."""
    if start == end:
        return [start]
    dist = {start: 0.0}
    prev: dict[int, int] = {}
    heap = [(0.0, start)]
    while heap:
        d, u = heapq.heappop(heap)
        if d > dist.get(u, np.inf):
            continue
        if u == end:
            break
        for v, w in adj[u]:
            nd = d + w
            if nd < dist.get(v, np.inf):
                dist[v] = nd
                prev[v] = u
                heapq.heappush(heap, (nd, v))
    if end not in prev and start != end:
        return []
    path = [end]
    while path[-1] != start:
        path.append(prev[path[-1]])
    path.reverse()
    return path


def _boundary_edge_nodes(triangles: np.ndarray, n_nodes: int) -> np.ndarray:
    """True for nodes incident to an edge that belongs to exactly one triangle."""
    mask = np.zeros(n_nodes, dtype=bool)
    counts: dict[tuple[int, int], int] = {}
    for a, b, c in triangles:
        for u, v in ((int(a), int(b)), (int(b), int(c)), (int(c), int(a))):
            e = (u, v) if u < v else (v, u)
            counts[e] = counts.get(e, 0) + 1
    for (u, v), cnt in counts.items():
        if cnt == 1:
            mask[u] = True
            mask[v] = True
    return mask


def map_paths_to_levels(
    paths,
    xy: np.ndarray,
    triangles: np.ndarray,
    ds: float,
    max_snap: float | None = None,
) -> tuple[np.ndarray, list[str], list[int]]:
    """Return per-node stream levels (0 = none), warnings, and level-1 outlet node indices.

    Outlet indices are the downstream ends of mapped level-1 chains that lie on a mesh boundary
    edge. Lower level wins on conflict. Each path needs ``.level``, ``.name`` and ``.coords``.
    """
    max_snap = ds * 0.5 if max_snap is None else max_snap
    levels = np.zeros(len(xy), np.int8)
    warnings: list[str] = []
    outlets: list[int] = []
    if not paths:
        return levels, warnings, outlets
    tree = cKDTree(xy)
    adj = mesh_adjacency(triangles, xy)
    on_bnd = _boundary_edge_nodes(triangles, len(xy))
    # Process higher levels first so lower levels overwrite on conflict.
    ordered = sorted(paths, key=lambda p: -p.level)
    far = 0
    for path in ordered:
        samples = _resample_line(path.coords, ds)
        if len(samples) < 1:
            continue
        dist, idx = tree.query(samples, k=1)
        snaps: list[int] = []
        for d, i in zip(np.atleast_1d(dist), np.atleast_1d(idx)):
            if d <= max_snap:
                snaps.append(int(i))
            else:
                far += 1
        compact: list[int] = []
        for i in snaps:
            if not compact or compact[-1] != i:
                compact.append(i)
        if not compact:
            warnings.append(f"Stream '{path.name or path.level}' had no nodes within {max_snap:g} m of the mesh")
            continue
        chain = [compact[0]]
        for a, b in zip(compact[:-1], compact[1:]):
            seg = shortest_path(adj, a, b)
            if not seg:
                warnings.append(
                    f"Stream '{path.name or path.level}': no mesh path between nodes {a + 1} and {b + 1}"
                )
                chain.append(b)
                continue
            chain.extend(seg[1:])
        for i in chain:
            cur = int(levels[i])
            lv = int(path.level)
            levels[i] = lv if cur == 0 else min(cur, lv)
        if int(path.level) == 1 and chain:
            chosen = next((i for i in reversed(chain) if on_bnd[i]), None)
            if chosen is not None:
                outlets.append(chosen)
    if far:
        warnings.append(f"{far} stream sample(s) farther than {max_snap:g} m from any mesh node were skipped")
    return levels, warnings, outlets


def _update_gpkg_stream_levels(gpkg: Path, levels: np.ndarray) -> None:
    """Set ``nodes.stream_level`` in place (keeps elements and streams layers)."""
    if not gpkg.exists():
        return
    from osgeo import ogr

    ds = ogr.Open(str(gpkg), 1)
    if ds is None:
        return
    layer = ds.GetLayerByName("nodes")
    if layer is None:
        ds = None
        return
    if layer.GetLayerDefn().GetFieldIndex("stream_level") < 0:
        layer.CreateField(ogr.FieldDefn("stream_level", ogr.OFTInteger))
    layer.StartTransaction()
    layer.ResetReading()
    for feat in layer:
        nid = int(feat.GetField("id"))
        if 1 <= nid <= len(levels):
            feat.SetField("stream_level", int(levels[nid - 1]))
            layer.SetFeature(feat)
    layer.CommitTransaction()
    ds = None


def map_streams_to_mesh(
    project,
    spec,
    network=None,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
) -> StreamMapResult:
    """Snap ranked streams onto the existing mesh and write ``.nchos`` for SWF (no remesh, no CLN)."""
    from .cln_export import load_stream_paths
    from .mesh import mesh_outputs, mut_snippet, nchos_files, write_csvs
    from .project import StreamOptions

    progress = progress or (lambda pct: None)
    result = StreamMapResult()
    outs = mesh_outputs(project, spec)
    if not outs["xy_csv"].exists() or not outs["idnode_csv"].exists():
        raise FileNotFoundError(
            f"Mesh '{spec.name}' CSVs not found under {outs['folder']}. Build the mesh first."
        )
    if not (outs["folder"] / f"{spec.slug}.grd").exists():
        raise FileNotFoundError(f"Mesh '{spec.name}' is not built yet ({outs['folder']})")

    progress(5)
    mesh = load_mesh_from_csvs(outs["xy_csv"], outs["idnode_csv"])
    if network is not None:
        paths = list(network.paths)
        citation = network.citation
        source = network.source
        net_summary = network.summary()
    else:
        paths = load_stream_paths(project, spec)
        citation = ""
        source = (spec.streams.source if spec.streams else "auto")
        net_summary = {"paths": len(paths)}
    if not paths:
        raise RuntimeError("No stream paths to map; run Find streams first")

    ds = (spec.streams.spacing_m if spec.streams else 0.0) or spec.boundary_spacing_m
    log(f"Map streams onto mesh '{spec.name}': {len(paths)} path(s), spacing {ds:g} m (no remesh)")
    progress(20)

    levels, warnings, outlet_indices = map_paths_to_levels(paths, mesh.xy, mesh.triangles, ds)
    for msg in warnings:
        result.warnings.append(msg)
        log(f"WARNING: {msg}")
    mesh.level = levels
    progress(70)

    write_csvs(mesh, outs["xy_csv"], outs["idnode_csv"])
    nchos = nchos_files(mesh, outs["gb_prefix"], outlet_indices=outlet_indices)
    _update_gpkg_stream_levels(outs["gpkg"], levels)
    level_list = sorted(int(v) for v in np.unique(levels) if v > 0)
    has_outlet = "nchos_outlet" in nchos
    outs["snippet"].write_text(
        mut_snippet(project, spec, level_list, has_outlet=has_outlet), encoding="utf-8"
    )

    summary = {
        "mesh": {
            "name": spec.name,
            "stream_mode": "mapped",
            "boundary_spacing_m": spec.boundary_spacing_m,
        },
        "crs": f"EPSG:{project.epsg}",
        "streams": {
            **net_summary,
            "mode": "mapped",
            "spacing_m": ds,
            "stream_nodes": int((levels > 0).sum()),
            "nodes_by_level": {str(k): int((levels == k).sum()) for k in level_list},
            "outlet_nodes": len(set(outlet_indices)),
            "source": source,
            "citation": citation,
        },
        "warnings": result.warnings,
    }
    if outs["summary"].exists():
        try:
            prior = json.loads(outs["summary"].read_text(encoding="utf-8"))
            prior["streams"] = summary["streams"]
            prior["warnings"] = list(prior.get("warnings") or []) + result.warnings
            if "mesh" in prior and isinstance(prior["mesh"], dict):
                prior["mesh"]["stream_mode"] = "mapped"
                prior["mesh"]["cln_xyz_file"] = ""
            outs["summary"].write_text(json.dumps(prior, indent=2), encoding="utf-8")
        except (OSError, ValueError, TypeError):
            outs["summary"].write_text(json.dumps(summary, indent=2), encoding="utf-8")
    else:
        outs["summary"].write_text(json.dumps(summary, indent=2), encoding="utf-8")

    spec.stream_mode = "mapped"
    spec.cln_xyz_file = ""
    if spec.streams is None and network is not None:
        spec.streams = StreamOptions(source=network.source)
    project.set_mesh(spec)
    project.save()
    progress(100)

    result.ok = True
    result.n_stream_nodes = int((levels > 0).sum())
    result.n_paths = len(paths)
    result.levels = level_list
    result.outputs = {"snippet": outs["snippet"], "summary": outs["summary"], **nchos}
    n_out = len(set(outlet_indices))
    log(
        f"Mapped {result.n_stream_nodes:,} stream nodes on {len(paths)} path(s); "
        f"{n_out} level-1 outlet node(s); .nchos written beside the mesh"
    )
    return result
