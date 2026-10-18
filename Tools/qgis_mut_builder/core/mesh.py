"""2D Delaunay triangle mesh of the regional model area, for MUT's ``2d mesh from gb`` instruction.

Fixed points lie along the regional boundary (every ``boundary_spacing_m``), along optional ranked streams (see
``streams.py``) and at optional user points; free points fill the interior on a hexagonal lattice sized to give about
``interior_points`` nodes. The Delaunay triangulation is clipped to the polygon formed by the boundary points, missing
boundary and stream edges are recovered by inserting fixed midpoints, and free nodes may be moved by Laplacian
smoothing. Nodes keep their ``fixed`` flag in every output so
later relaxation can respect it.
"""

from __future__ import annotations

import csv
import json
import math
import struct
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np
import shapely
from osgeo import gdal, ogr, osr
from scipy.spatial import Delaunay, cKDTree
from shapely import wkt as shapely_wkt
from shapely.geometry import LineString, MultiPolygon, Polygon

from .boundary import boundary_wkt
from .project import MeshSpec, Project

SOURCES = ("boundary", "interior", "user", "inserted", "stream")
EDGE_RECOVERY_PASSES = 6
LARGE_MESH_NODES = 500_000
GB_PATH_MAX = 80
NCHOS_HEADER = 80

Chain = tuple[list[int], int]  # (node indices along a stream, downstream last; stream level)


@dataclass
class Mesh:
    xy: np.ndarray  # (n, 2) float64
    fixed: np.ndarray  # (n,) bool
    source: np.ndarray  # (n,) int8 index into SOURCES
    triangles: np.ndarray = field(default_factory=lambda: np.zeros((0, 3), dtype=np.int64))  # 0-based, CCW
    level: np.ndarray | None = None  # (n,) int8 stream level of the node, 0 = not on a stream

    def __post_init__(self) -> None:
        if self.level is None:
            self.level = np.zeros(len(self.xy), np.int8)

    @property
    def n_nodes(self) -> int:
        return len(self.xy)


@dataclass
class MeshResult:
    ok: bool = False
    mesh: Mesh | None = None
    interior_spacing_m: float = 0.0
    outputs: dict[str, Path] = field(default_factory=dict)
    stats: dict = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)


def mesh_outputs(project: Project, spec: MeshSpec) -> dict[str, Path]:
    folder = project.mesh_dir(spec)
    stem = spec.slug
    return {
        "folder": folder,
        "gb_prefix": folder / stem,
        "gpkg": folder / f"{stem}.gpkg",
        "xy_csv": folder / f"{stem}_xyCoord.csv",
        "idnode_csv": folder / f"{stem}_idNode.csv",
        "summary": folder / "mesh.json",
        "snippet": folder / "mut_snippet.txt",
    }


# ---------------------------------------------------------------- geometry


def model_polygon(project: Project) -> Polygon | MultiPolygon:
    """Regional boundary in the project CRS."""
    if not project.has_boundary or not project.boundary_file or not Path(project.boundary_file).exists():
        grid = project.regional_grid()
        return shapely.box(grid.xmin, grid.ymin, grid.xmax, grid.ymax)
    geom = shapely_wkt.loads(boundary_wkt(Path(project.boundary_file), project.epsg))
    geom = shapely.make_valid(geom)
    polys = [g for g in getattr(geom, "geoms", [geom]) if isinstance(g, (Polygon, MultiPolygon))]
    if not polys:
        raise ValueError("The model boundary contains no polygon")
    return shapely.unary_union(polys)


def _parts(geom) -> list[Polygon]:
    return [g for g in getattr(geom, "geoms", [geom]) if isinstance(g, Polygon) and not g.is_empty]


def _resample_ring(coords: np.ndarray, ds: float, tolerance: float = 0.0) -> np.ndarray:
    """Points along a closed ring: every ring vertex, with sides longer than ``ds`` split into equal pieces.

    ``tolerance`` > 0 first removes vertices with Douglas-Peucker (the mesh outline then stays within ``tolerance``
    of the ring); 0 drops only duplicate and collinear vertices, so mesh edges lie exactly on the ring.
    """
    ring = shapely.LinearRing(coords)
    simple = shapely.simplify(ring, max(tolerance, 1e-6 * ds), preserve_topology=True)
    corners = np.asarray(simple.coords)[:-1]
    if len(corners) < 3:
        corners = np.asarray(ring.coords)[:-1]
    out = []
    for a, b in zip(corners, np.roll(corners, -1, axis=0)):
        n = max(1, math.ceil(np.hypot(*(b - a)) / ds - 1e-9))
        t = np.arange(n)[:, None] / n
        out.append(a + t * (b - a))
    return np.vstack(out)


def boundary_points(
    polygon, ds: float, tolerance: float = 0.0
) -> tuple[np.ndarray, list[np.ndarray], Polygon | MultiPolygon]:
    """Fixed points on every exterior ring and hole; returns (points, rings of point indices, mesh polygon).

    The mesh polygon joins the boundary points with straight sides; triangles are clipped to it. With
    ``tolerance`` 0 it is the model boundary itself.
    """
    pts: list[np.ndarray] = []
    rings: list[np.ndarray] = []
    parts = []
    start = 0
    for part in _parts(polygon):
        shell = _resample_ring(np.asarray(part.exterior.coords), ds, tolerance)
        holes = [_resample_ring(np.asarray(h.coords), ds, tolerance) for h in part.interiors]
        holes = [h for h in holes if len(h) >= 3]
        for ring in [shell] + holes:
            pts.append(ring)
            rings.append(np.arange(start, start + len(ring)))
            start += len(ring)
        parts.append(Polygon(shell, holes))
    if not pts:
        raise ValueError("The model boundary is empty")
    mesh_poly = shapely.make_valid(MultiPolygon(parts) if len(parts) > 1 else parts[0])
    return np.vstack(pts), rings, mesh_poly


def _hex_lattice(polygon, h: float) -> np.ndarray:
    xmin, ymin, xmax, ymax = polygon.bounds
    c = polygon.centroid
    dy = h * math.sqrt(3.0) / 2.0
    j0, j1 = math.floor((ymin - c.y) / dy) - 1, math.ceil((ymax - c.y) / dy) + 1
    i0, i1 = math.floor((xmin - c.x) / h) - 1, math.ceil((xmax - c.x) / h) + 1
    jj, ii = np.mgrid[j0 : j1 + 1, i0 : i1 + 1]
    x = c.x + (ii + 0.5 * (jj % 2)) * h
    y = c.y + jj * dy
    return np.column_stack([x.ravel(), y.ravel()])


def _lattice_inside(polygon, h: float, fixed_tree: cKDTree | None) -> np.ndarray:
    pts = _hex_lattice(polygon, h)
    inner = polygon.buffer(-0.5 * h)
    if inner.is_empty:
        return np.zeros((0, 2))
    pts = pts[shapely.contains_xy(inner, pts[:, 0], pts[:, 1])]
    if fixed_tree is not None and len(pts):
        dist, _ = fixed_tree.query(pts, k=1)
        pts = pts[dist >= 0.5 * h]
    return pts


def estimated_spacing(area_m2: float, n: int) -> float:
    """Hexagonal-lattice spacing giving ``n`` points over ``area_m2``."""
    return math.sqrt(2.0 * area_m2 / (math.sqrt(3.0) * max(n, 1)))


def interior_points(polygon, n: int, fixed_xy: np.ndarray | None = None) -> tuple[np.ndarray, float]:
    """About ``n`` free points on a hexagonal lattice inside ``polygon``; returns (points, spacing).

    Lattice points closer than half the spacing to any of ``fixed_xy`` are dropped.
    """
    tree = cKDTree(fixed_xy) if fixed_xy is not None and len(fixed_xy) else None
    h0 = estimated_spacing(polygon.area, n)
    best_pts, best_h = _lattice_inside(polygon, h0, tree), h0
    lo, hi = 0.3 * h0, 2.0 * h0
    for _ in range(25):
        if abs(len(best_pts) - n) <= max(1, n // 200):
            break
        mid = 0.5 * (lo + hi)
        pts = _lattice_inside(polygon, mid, tree)
        if abs(len(pts) - n) < abs(len(best_pts) - n):
            best_pts, best_h = pts, mid
        if len(pts) > n:
            lo = mid
        else:
            hi = mid
    return best_pts, best_h


def user_points(path: Path, epsg: int, polygon) -> np.ndarray:
    """Points (and vertices of any line/polygon features) from a vector file, in ``epsg``, inside ``polygon``.

    CSV files need x/y columns (x, xcoord, easting, lon, ...) and are taken to be in ``epsg`` unless they carry a CRS.
    """
    options = []
    if path.suffix.lower() == ".csv":
        options = [
            "X_POSSIBLE_NAMES=x,xcoord,x_coord,easting,east,lon,longitude",
            "Y_POSSIBLE_NAMES=y,ycoord,y_coord,northing,north,lat,latitude",
            "KEEP_GEOM_COLUMNS=NO",
        ]
    ds = gdal.OpenEx(str(path), gdal.OF_VECTOR, open_options=options)
    if ds is None:
        raise RuntimeError(f"Cannot open fixed point file {path}")
    layer = ds.GetLayer(0)
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    srs = layer.GetSpatialRef()
    transform = None
    if srs is not None:
        src = srs.Clone()
        src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
        if not src.IsSame(dst):
            transform = osr.CoordinateTransformation(src, dst)
    xy = []
    for feat in layer:
        geom = feat.GetGeometryRef()
        if geom is None:
            continue
        geom = geom.Clone()
        if transform is not None:
            geom.Transform(transform)
        g = shapely_wkt.loads(geom.ExportToWkt())
        xy.extend(shapely.get_coordinates(g)[:, :2].tolist())
    if not xy:
        return np.zeros((0, 2))
    arr = np.unique(np.asarray(xy, dtype=float), axis=0)
    return arr[shapely.contains_xy(polygon, arr[:, 0], arr[:, 1])]


def _resample_path(line: LineString, anchors: list[float], ds: float) -> np.ndarray:
    """Points along ``line`` at every anchor distance, with the stretches between split into pieces up to ``ds``."""
    pos = np.unique(np.clip(np.asarray(anchors, float), 0.0, line.length))
    out = [pos[:1]]
    for a, b in zip(pos[:-1], pos[1:]):
        if b - a < 1e-6:
            continue
        n = max(1, math.ceil((b - a) / ds - 1e-9))
        out.append(a + (b - a) * np.arange(1, n + 1) / n)
    return shapely.get_coordinates(shapely.line_interpolate_point(line, np.concatenate(out)))


def stream_points(
    paths: list,
    mesh_poly,
    bxy: np.ndarray,
    rings: list[list[int]],
    ds: float,
    ds_boundary: float,
) -> tuple[np.ndarray, np.ndarray, np.ndarray, list[Chain], np.ndarray]:
    """Fixed nodes along ranked streams (``streams.StreamPath``) inside the mesh polygon.

    Paths are resampled every ``ds`` with nodes at every junction and end; nodes of different streams closer than
    ``ds / 4`` merge (the lower level keeps its position); outlets on the boundary join the boundary rings (``rings``
    is updated in place); other stream nodes within ``ds_boundary / 4`` of the boundary are dropped.

    Returns (boundary points, boundary levels, stream points, chains, stream point levels); chain indices refer to
    the boundary points followed by the stream points.
    """
    blevel = np.zeros(len(bxy), np.int8)
    paths = sorted(paths, key=lambda p: p.level)
    if not paths:
        return bxy, blevel, np.zeros((0, 2)), [], np.zeros(0, np.int8)
    lines = [LineString(p.coords) for p in paths]
    ends = shapely.points(np.array([c for p in paths for c in (p.coords[0], p.coords[-1])]))
    anchors = [[0.0, ln.length] for ln in lines]
    tree = shapely.STRtree(lines)
    pts_idx, line_idx = tree.query(shapely.buffer(ends, 0.5), predicate="intersects")
    for k, j in zip(pts_idx, line_idx):
        if k // 2 != j:
            anchors[j].append(lines[j].project(ends[k]))

    pts, lev, chains_local = [], [], []
    for p, ln, anc in zip(paths, lines, anchors):
        xy = _resample_path(ln, anc, ds)
        chains_local.append((list(range(len(pts), len(pts) + len(xy))), p.level))
        pts.extend(xy.tolist())
        lev.extend([p.level] * len(xy))
    pts, lev = np.asarray(pts), np.asarray(lev, np.int8)

    # Merge nearby nodes: union-find keeping the lowest level (then first) node as representative.
    parent = np.arange(len(pts))

    def find(i: int) -> int:
        while parent[i] != i:
            parent[i] = parent[parent[i]]
            i = parent[i]
        return i

    order_key = lev.astype(np.int64) * len(pts) + np.arange(len(pts))
    for i, j in sorted(cKDTree(pts).query_pairs(0.25 * ds), key=lambda ij: min(order_key[ij[0]], order_key[ij[1]])):
        ri, rj = find(i), find(j)
        if ri != rj:
            if order_key[rj] < order_key[ri]:
                ri, rj = rj, ri
            parent[rj] = ri
    rep = np.array([find(i) for i in range(len(pts))])
    reps = np.unique(rep)
    rep_level = np.zeros(len(pts), np.int8)
    for i in range(len(pts)):
        r = rep[i]
        rep_level[r] = lev[i] if rep_level[r] == 0 else min(rep_level[r], lev[i])

    # Outlets on the boundary join the rings; other nodes close to the boundary are dropped.
    boundary = mesh_poly.boundary
    dist = shapely.distance(shapely.points(pts[reps]), boundary)
    target = np.full(len(pts), -2, np.int64)  # -2 stream node, -1 dropped, >= 0 boundary index
    btree = cKDTree(bxy)
    seg_ring, seg_pos, seg_a, seg_b = [], [], [], []
    for r, ring in enumerate(rings):
        for k in range(len(ring)):
            seg_ring.append(r)
            seg_pos.append(k)
            seg_a.append(bxy[ring[k]])
            seg_b.append(bxy[ring[(k + 1) % len(ring)]])
    seg_a, seg_b = np.asarray(seg_a), np.asarray(seg_b)
    new_b: list[np.ndarray] = []
    inserts: dict[tuple[int, int], list[tuple[float, int]]] = {}
    tol_on = 1e-6 * ds + 1e-6
    for r_i, d in zip(reps, dist):
        if d < tol_on:
            near_d, near_i = btree.query(pts[r_i])
            if near_d < 0.25 * ds_boundary:
                target[r_i] = int(near_i)
            else:
                ab = seg_b - seg_a
                t = np.clip(((pts[r_i] - seg_a) * ab).sum(axis=1) / np.maximum((ab * ab).sum(axis=1), 1e-30), 0, 1)
                s = int(np.argmin(np.hypot(*(seg_a + t[:, None] * ab - pts[r_i]).T)))
                idx = len(bxy) + len(new_b)
                new_b.append(pts[r_i])
                inserts.setdefault((seg_ring[s], seg_pos[s]), []).append((float(t[s]), idx))
                target[r_i] = idx
        elif d < 0.25 * ds_boundary:
            target[r_i] = -1
    if new_b:
        bxy = np.vstack([bxy, new_b])
        blevel = np.zeros(len(bxy), np.int8)
        by_ring: dict[int, list[tuple[int, list[int]]]] = {}
        for (r, k), items in inserts.items():
            by_ring.setdefault(r, []).append((k, [i for _, i in sorted(items)]))
        for r, items in by_ring.items():
            ring = list(rings[r])
            for k, ids in sorted(items, reverse=True):
                ring[k + 1 : k + 1] = ids
            rings[r] = ring
    keep = [r for r in reps if target[r] == -2]
    for n, r in enumerate(keep):
        target[r] = len(bxy) + n
    sxy = pts[keep] if keep else np.zeros((0, 2))
    slevel = rep_level[keep] if keep else np.zeros(0, np.int8)
    for r in reps:
        if 0 <= target[r] < len(bxy):
            b = int(target[r])
            blevel[b] = rep_level[r] if blevel[b] == 0 else min(blevel[b], rep_level[r])

    # Chains in final indices, split where nodes were dropped or a link leaves the mesh polygon.
    all_xy = np.vstack([bxy, sxy]) if len(sxy) else bxy
    inside = mesh_poly.buffer(1e-6 * ds)
    chains: list[Chain] = []
    for local, level in chains_local:
        ids: list[int] = []
        for i in local:
            t = int(target[rep[i]])
            if t < 0:
                if len(ids) > 1:
                    chains.append((ids, level))
                ids = []
                continue
            if ids and ids[-1] == t:
                continue
            if ids and not shapely.covered_by(LineString([all_xy[ids[-1]], all_xy[t]]), inside):
                if len(ids) > 1:
                    chains.append((ids, level))
                ids = []
            ids.append(t)
        if len(ids) > 1:
            chains.append((ids, level))
    return bxy, blevel, sxy, chains, slevel


def densify_chains(xy: np.ndarray, chains: list[Chain], step: float) -> np.ndarray:
    """Points every ``step`` along the stream chains (for keeping the interior lattice off the streams)."""
    out = []
    for ids, _ in chains:
        c = xy[ids]
        for a, b in zip(c[:-1], c[1:]):
            n = max(1, math.ceil(np.hypot(*(b - a)) / step))
            out.append(a + (b - a) * (np.arange(n) / n)[:, None])
        out.append(c[-1:])
    return np.vstack(out) if out else np.zeros((0, 2))


# ---------------------------------------------------------------- triangulation


def _orient_ccw(xy: np.ndarray, tris: np.ndarray) -> np.ndarray:
    a, b, c = xy[tris[:, 0]], xy[tris[:, 1]], xy[tris[:, 2]]
    cross = (b[:, 0] - a[:, 0]) * (c[:, 1] - a[:, 1]) - (b[:, 1] - a[:, 1]) * (c[:, 0] - a[:, 0])
    tris = tris.copy()
    flip = cross < 0
    tris[flip, 1], tris[flip, 2] = tris[flip, 2], tris[flip, 1].copy()
    return tris


def _areas(xy: np.ndarray, tris: np.ndarray) -> np.ndarray:
    a, b, c = xy[tris[:, 0]], xy[tris[:, 1]], xy[tris[:, 2]]
    return 0.5 * ((b[:, 0] - a[:, 0]) * (c[:, 1] - a[:, 1]) - (b[:, 1] - a[:, 1]) * (c[:, 0] - a[:, 0]))


def _edge_set(tris: np.ndarray) -> set[tuple[int, int]]:
    e = np.vstack([tris[:, [0, 1]], tris[:, [1, 2]], tris[:, [2, 0]]])
    e.sort(axis=1)
    return set(map(tuple, e.tolist()))


def _delaunay_clipped(xy: np.ndarray, polygon, min_area: float) -> np.ndarray:
    tri = Delaunay(xy)
    tris = tri.simplices.astype(np.int64)
    cen = xy[tris].mean(axis=1)
    keep = shapely.contains_xy(polygon, cen[:, 0], cen[:, 1])
    tris = _orient_ccw(xy, tris[keep])
    return tris[_areas(xy, tris) > min_area]


def triangulate(
    mesh: Mesh,
    rings: list[list[int]],
    polygon,
    spacing: float,
    log=print,
    chains: list[Chain] | None = None,
) -> tuple[Mesh, list[str]]:
    """Delaunay triangulation clipped to ``polygon``; missing boundary and stream edges are recovered by fixed midpoints.

    ``rings`` are closed and ``chains`` open sequences of node indices; both are updated in place.
    """
    warnings: list[str] = []
    chains = chains if chains is not None else []
    min_area = 1e-9 * spacing * spacing
    for attempt in range(EDGE_RECOVERY_PASSES + 1):
        tris = _delaunay_clipped(mesh.xy, polygon, min_area)
        edges = _edge_set(tris)
        missing: list[tuple[str, int, int]] = []
        for r, ring in enumerate(rings):
            for k in range(len(ring)):
                i, j = ring[k], ring[(k + 1) % len(ring)]
                if (min(i, j), max(i, j)) not in edges:
                    missing.append(("ring", r, k))
        for c, (ids, _) in enumerate(chains):
            for k in range(len(ids) - 1):
                i, j = ids[k], ids[k + 1]
                if (min(i, j), max(i, j)) not in edges:
                    missing.append(("chain", c, k))
        if not missing:
            break
        n_ring = sum(1 for m in missing if m[0] == "ring")
        n_chain = len(missing) - n_ring
        if attempt == EDGE_RECOVERY_PASSES:
            if n_ring:
                warnings.append(f"{n_ring} boundary edge(s) could not be recovered; check the mesh along the boundary")
            if n_chain:
                warnings.append(f"{n_chain} stream edge(s) could not be recovered; check the mesh along the streams")
            break
        log(f"  Recovering {n_ring} boundary and {n_chain} stream edge(s) by inserting fixed midpoints")
        new_xy, new_level, inserts = [], [], {}
        n = mesh.n_nodes
        for kind, r, k in missing:
            ids = rings[r] if kind == "ring" else chains[r][0]
            i, j = ids[k], ids[(k + 1) % len(ids)]
            new_xy.append(0.5 * (mesh.xy[i] + mesh.xy[j]))
            new_level.append(0 if kind == "ring" else chains[r][1])
            inserts.setdefault((kind, r), []).append((k, n))
            n += 1
        mesh = Mesh(
            np.vstack([mesh.xy, new_xy]),
            np.concatenate([mesh.fixed, np.ones(len(new_xy), bool)]),
            np.concatenate([mesh.source, np.full(len(new_xy), SOURCES.index("inserted"), np.int8)]),
            level=np.concatenate([mesh.level, np.asarray(new_level, np.int8)]),
        )
        for (kind, r), items in inserts.items():
            ids = list(rings[r] if kind == "ring" else chains[r][0])
            for k, idx in sorted(items, reverse=True):
                ids.insert(k + 1, idx)
            if kind == "ring":
                rings[r] = ids
            else:
                chains[r] = (ids, chains[r][1])
    mesh.triangles = tris
    return _drop_unused(mesh, rings, chains), warnings


def _drop_unused(mesh: Mesh, rings: list[list[int]], chains: list[Chain] | None = None) -> Mesh:
    used = np.zeros(mesh.n_nodes, bool)
    used[mesh.triangles.ravel()] = True
    if used.all():
        return mesh
    new_id = np.cumsum(used) - 1
    for r, ring in enumerate(rings):
        rings[r] = [int(new_id[i]) for i in ring if used[i]]
    for c, (ids, level) in enumerate(chains or []):
        chains[c] = ([int(new_id[i]) for i in ids if used[i]], level)
    return Mesh(mesh.xy[used], mesh.fixed[used], mesh.source[used], new_id[mesh.triangles], mesh.level[used])


def smooth(
    mesh: Mesh,
    rings: list[list[int]],
    polygon,
    spacing: float,
    iterations: int,
    log=print,
    is_canceled: Callable[[], bool] = lambda: False,
    chains: list[Chain] | None = None,
) -> tuple[Mesh, list[str]]:
    """Laplacian smoothing of free nodes (fixed nodes never move), re-triangulating after each pass."""
    warnings: list[str] = []
    for it in range(iterations):
        if is_canceled():
            raise InterruptedError("Run canceled")
        tris = mesh.triangles
        n = mesh.n_nodes
        e = np.vstack([tris[:, [0, 1]], tris[:, [1, 2]], tris[:, [2, 0]]])
        e = np.vstack([e, e[:, ::-1]])
        e = np.unique(e, axis=0)
        sums = np.zeros((n, 2))
        np.add.at(sums, e[:, 0], mesh.xy[e[:, 1]])
        counts = np.bincount(e[:, 0], minlength=n).astype(float)
        free = ~mesh.fixed & (counts > 0)
        target = mesh.xy.copy()
        target[free] = sums[free] / counts[free, None]
        inside = shapely.contains_xy(polygon, target[:, 0], target[:, 1])
        move = free & inside
        moved = float(np.hypot(*(target[move] - mesh.xy[move]).T).max()) if move.any() else 0.0
        xy = mesh.xy.copy()
        xy[move] = target[move]
        mesh = Mesh(xy, mesh.fixed, mesh.source, level=mesh.level)
        mesh, w = triangulate(mesh, rings, polygon, spacing, log, chains)
        warnings = w
        log(f"  Smoothing pass {it + 1}/{iterations}: largest move {moved:,.1f} m")
        if moved < 1e-3 * spacing:
            break
    return mesh, warnings


def quality(mesh: Mesh) -> dict:
    xy, tris = mesh.xy, mesh.triangles
    if not len(tris):
        return {"nodes": mesh.n_nodes, "elements": 0}
    a, b, c = xy[tris[:, 0]], xy[tris[:, 1]], xy[tris[:, 2]]
    la, lb, lc = np.hypot(*(b - c).T), np.hypot(*(c - a).T), np.hypot(*(a - b).T)

    def angle(opp, s1, s2):
        return np.degrees(np.arccos(np.clip((s1**2 + s2**2 - opp**2) / (2 * s1 * s2), -1.0, 1.0)))

    angles = np.column_stack([angle(la, lb, lc), angle(lb, lc, la), angle(lc, la, lb)])
    min_ang = angles.min(axis=1)
    area = _areas(xy, tris)
    edges = np.concatenate([la, lb, lc])
    return {
        "nodes": int(mesh.n_nodes),
        "elements": int(len(tris)),
        "fixed_nodes": int(mesh.fixed.sum()),
        "free_nodes": int((~mesh.fixed).sum()),
        "nodes_by_source": {s: int((mesh.source == i).sum()) for i, s in enumerate(SOURCES)},
        "min_angle_deg": float(min_ang.min()),
        "mean_min_angle_deg": float(min_ang.mean()),
        "max_angle_deg": float(angles.max()),
        "elements_min_angle_below_20_percent": float(100.0 * (min_ang < 20.0).mean()),
        "area_m2": {"min": float(area.min()), "max": float(area.max()), "mean": float(area.mean()), "total": float(area.sum())},
        "edge_length_m": {"min": float(edges.min()), "max": float(edges.max()), "mean": float(edges.mean())},
    }


def element_min_angles(mesh: Mesh) -> np.ndarray:
    xy, tris = mesh.xy, mesh.triangles
    a, b, c = xy[tris[:, 0]], xy[tris[:, 1]], xy[tris[:, 2]]
    la, lb, lc = np.hypot(*(b - c).T), np.hypot(*(c - a).T), np.hypot(*(a - b).T)
    cosines = [
        (lb**2 + lc**2 - la**2) / (2 * lb * lc),
        (lc**2 + la**2 - lb**2) / (2 * lc * la),
        (la**2 + lb**2 - lc**2) / (2 * la * lb),
    ]
    return np.degrees(np.arccos(np.clip(np.max(cosines, axis=0), -1.0, 1.0)))


# ---------------------------------------------------------------- writers


def _fortran_record(handle, data: bytes) -> None:
    marker = struct.pack("<i", len(data))
    handle.write(marker + data + marker)


def write_gridbuilder(mesh: Mesh, prefix: Path, title: str = "") -> list[Path]:
    """Grid Builder ``.grd/.xyc/.in3/.ean`` (Fortran unformatted sequential, 4-byte record markers, 1-based ids)."""
    prefix.parent.mkdir(parents=True, exist_ok=True)
    grd, xyc, in3, ean = (prefix.with_name(prefix.name + ext) for ext in (".grd", ".xyc", ".in3", ".ean"))
    grd.write_text(
        f"{title or prefix.name}\n"
        f"{mesh.n_nodes} nodes, {len(mesh.triangles)} triangles (MUT Model Builder Delaunay mesh)\n",
        encoding="utf-8",
    )
    with open(xyc, "wb") as f:
        _fortran_record(f, struct.pack("<i", mesh.n_nodes))
        _fortran_record(f, np.ascontiguousarray(mesh.xy, dtype="<f8").tobytes())
    with open(in3, "wb") as f:
        _fortran_record(f, struct.pack("<i", len(mesh.triangles)))
        _fortran_record(f, np.ascontiguousarray(mesh.triangles + 1, dtype="<i4").tobytes())
    with open(ean, "wb") as f:
        _fortran_record(f, np.ones(len(mesh.triangles), dtype="<i4").tobytes())
    return [grd, xyc, in3, ean]


def read_gridbuilder(prefix: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """(xy, 1-based triangles, zones) read back from Grid Builder files."""

    def records(path: Path) -> list[bytes]:
        data, out, pos = path.read_bytes(), [], 0
        while pos < len(data):
            (n,) = struct.unpack_from("<i", data, pos)
            out.append(data[pos + 4 : pos + 4 + n])
            if struct.unpack_from("<i", data, pos + 4 + n)[0] != n:
                raise ValueError(f"Bad record marker in {path}")
            pos += 8 + n
        return out

    def ext(e: str) -> Path:
        return prefix.with_name(prefix.name + e)

    xr = records(ext(".xyc"))
    n = struct.unpack("<i", xr[0])[0]
    xy = np.frombuffer(xr[1], dtype="<f8").reshape(n, 2)
    tr = records(ext(".in3"))
    ne = struct.unpack("<i", tr[0])[0]
    tris = np.frombuffer(tr[1], dtype="<i4").reshape(ne, 3)
    zones = np.frombuffer(records(ext(".ean"))[0], dtype="<i4")
    return xy, tris, zones


def write_gpkg(mesh: Mesh, path: Path, epsg: int) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists():
        path.unlink()
    driver = ogr.GetDriverByName("GPKG")
    ds = driver.CreateDataSource(str(path))
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg)

    nodes = ds.CreateLayer("nodes", srs, ogr.wkbPoint)
    fields = (("id", ogr.OFTInteger), ("fixed", ogr.OFTInteger), ("source", ogr.OFTString), ("stream_level", ogr.OFTInteger))
    for name, kind in fields:
        nodes.CreateField(ogr.FieldDefn(name, kind))
    defn = nodes.GetLayerDefn()
    nodes.StartTransaction()
    for i, ((x, y), fx, src, lev) in enumerate(zip(mesh.xy, mesh.fixed, mesh.source, mesh.level), start=1):
        feat = ogr.Feature(defn)
        feat.SetField("id", i)
        feat.SetField("fixed", int(fx))
        feat.SetField("source", SOURCES[int(src)])
        feat.SetField("stream_level", int(lev))
        pt = ogr.Geometry(ogr.wkbPoint)
        pt.AddPoint_2D(float(x), float(y))
        feat.SetGeometry(pt)
        nodes.CreateFeature(feat)
    nodes.CommitTransaction()

    elements = ds.CreateLayer("elements", srs, ogr.wkbPolygon)
    for name, kind in (("id", ogr.OFTInteger), ("n1", ogr.OFTInteger), ("n2", ogr.OFTInteger), ("n3", ogr.OFTInteger)):
        elements.CreateField(ogr.FieldDefn(name, kind))
    elements.CreateField(ogr.FieldDefn("area", ogr.OFTReal))
    elements.CreateField(ogr.FieldDefn("min_angle", ogr.OFTReal))
    defn = elements.GetLayerDefn()
    areas = _areas(mesh.xy, mesh.triangles)
    angles = element_min_angles(mesh)
    elements.StartTransaction()
    for e, tri in enumerate(mesh.triangles):
        feat = ogr.Feature(defn)
        feat.SetField("id", e + 1)
        for k in range(3):
            feat.SetField(f"n{k + 1}", int(tri[k]) + 1)
        feat.SetField("area", float(areas[e]))
        feat.SetField("min_angle", float(angles[e]))
        ring = ogr.Geometry(ogr.wkbLinearRing)
        for k in (0, 1, 2, 0):
            ring.AddPoint_2D(float(mesh.xy[tri[k], 0]), float(mesh.xy[tri[k], 1]))
        poly = ogr.Geometry(ogr.wkbPolygon)
        poly.AddGeometry(ring)
        feat.SetGeometry(poly)
        elements.CreateFeature(feat)
    elements.CommitTransaction()
    ds = None
    return path


def write_csvs(mesh: Mesh, xy_path: Path, idnode_path: Path) -> tuple[Path, Path]:
    with open(xy_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["id", "xcoord", "ycoord", "fixed", "source", "stream_level"])
        for i, ((x, y), fx, src, lev) in enumerate(zip(mesh.xy, mesh.fixed, mesh.source, mesh.level), start=1):
            w.writerow([i, f"{x:.4f}", f"{y:.4f}", int(fx), SOURCES[int(src)], int(lev)])
    with open(idnode_path, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["POINTA", "POINTB", "POINTC"])
        w.writerows((mesh.triangles + 1).tolist())
    return xy_path, idnode_path


def write_nchos(path: Path, header: str, mask: np.ndarray) -> Path:
    """Node selection for MUT ``choose gb nodes``: an 80-character header record, then one logical per node."""
    with open(path, "wb") as f:
        _fortran_record(f, header[:NCHOS_HEADER].ljust(NCHOS_HEADER).encode("ascii", "replace"))
        _fortran_record(f, np.where(mask, -1, 0).astype("<i4").tobytes())
    return path


def nchos_files(mesh: Mesh, prefix: Path) -> dict[str, Path]:
    """``<prefix>.nchos.Stream level k`` (nodes on level-k streams) and ``.nchos.Streams`` (all stream nodes)."""
    for old in prefix.parent.glob(prefix.name + ".nchos.*"):
        old.unlink()
    out: dict[str, Path] = {}
    levels = sorted(int(v) for v in np.unique(mesh.level) if v > 0)
    if not levels:
        return out
    for k in levels:
        name = f"Stream level {k}"
        out[f"nchos_level{k}"] = write_nchos(prefix.with_name(f"{prefix.name}.nchos.{name}"), name, mesh.level == k)
    out["nchos_streams"] = write_nchos(prefix.with_name(f"{prefix.name}.nchos.Streams"), "Streams", mesh.level > 0)
    return out


def mut_snippet(project: Project, spec: MeshSpec, stream_levels: list[int] | None = None) -> str:
    """MUT instructions for a model folder two levels below the project folder (e.g. ``2_models/<model>``)."""
    rel = f"..\\..\\2_mesh\\{spec.slug}\\{spec.slug}"
    text = (
        f"! Delaunay mesh '{spec.name}' from MUT Model Builder; the path assumes the model folder is\n"
        f"! two levels below {project.output_path} (edit it otherwise; MUT reads at most {GB_PATH_MAX} characters).\n"
        "nodal control volumes\n"
        "2d mesh from gb\n"
        f"{rel}\n"
    )
    if stream_levels:
        text += (
            "\n! Stream nodes: one flag per mesh node, so use them in the 'tmplt' or 'swf' active domain (not in a\n"
            "! layered GWF domain). With nodal control volumes 'choose cells from gb nodes' picks the SWF cells:\n"
            "!   active domain\n!   swf\n!   clear chosen cells\n"
        )
        for k in stream_levels:
            text += f"!   choose cells from gb nodes\n!   {rel}.nchos.Stream level {k}\n"
        text += f"!   choose cells from gb nodes\n!   {rel}.nchos.Streams\n"
    return text


def write_outputs(project: Project, spec: MeshSpec, result: MeshResult, network=None) -> dict[str, Path]:
    out = mesh_outputs(project, spec)
    mesh = result.mesh
    out["folder"].mkdir(parents=True, exist_ok=True)
    files = write_gridbuilder(mesh, out["gb_prefix"], spec.name)
    outputs = {"gb_" + p.suffix[1:]: p for p in files}
    outputs["gpkg"] = write_gpkg(mesh, out["gpkg"], project.epsg)
    if network is not None:
        from .streams import write_streams

        write_streams(network, out["gpkg"], project.epsg)
    outputs.update(nchos_files(mesh, out["gb_prefix"]))
    outputs["xy_csv"], outputs["idnode_csv"] = write_csvs(mesh, out["xy_csv"], out["idnode_csv"])
    levels = sorted(int(v) for v in np.unique(mesh.level) if v > 0)
    out["snippet"].write_text(mut_snippet(project, spec, levels), encoding="utf-8")
    outputs["snippet"] = out["snippet"]
    summary = {
        "mesh": asdict(spec),
        "crs": f"EPSG:{project.epsg}",
        "interior_spacing_m": result.interior_spacing_m,
        "outputs": {k: str(v) for k, v in outputs.items()},
        "stats": result.stats,
        "warnings": result.warnings,
    }
    if network is not None:
        summary["streams"] = network.summary()
    out["summary"].write_text(json.dumps(summary, indent=2), encoding="utf-8")
    outputs["summary"] = out["summary"]
    return outputs


# ---------------------------------------------------------------- driver


def _shortest_side(xy: np.ndarray, rings) -> float:
    sides = [np.hypot(*(xy[np.roll(r, -1)] - xy[r]).T).min() for r in rings if len(r) > 1]
    return float(min(sides)) if sides else 0.0


def preview(project: Project, spec: MeshSpec) -> dict:
    """Area, interior spacing estimate and boundary point count, without building the mesh."""
    polygon = model_polygon(project)
    bxy, rings, mesh_poly = boundary_points(polygon, spec.boundary_spacing_m, spec.boundary_tolerance_m)
    h = estimated_spacing(mesh_poly.area, spec.interior_points)
    return {
        "area_km2": mesh_poly.area / 1e6,
        "boundary_points": int(len(bxy)),
        "shortest_boundary_side_m": _shortest_side(bxy, rings),
        "interior_spacing_m": h,
        "total_points": int(len(bxy) + spec.interior_points),
    }


def streams_preview_path(project: Project, spec: MeshSpec) -> Path:
    return project.mesh_root / "_streams" / f"{spec.slug}_preview.gpkg"


def preview_streams(
    project: Project,
    spec: MeshSpec,
    log: Callable[[str], None] = print,
    is_canceled: Callable[[], bool] | None = None,
    refresh: bool = False,
):
    """Find and rank the streams for ``spec`` without meshing; returns (StreamNetwork, preview GeoPackage)."""
    from .streams import find_streams, write_streams

    if spec.streams is None:
        raise ValueError("The mesh has no stream options")
    polygon = model_polygon(project)
    _, _, mesh_poly = boundary_points(polygon, spec.boundary_spacing_m, spec.boundary_tolerance_m)
    ds = spec.streams.spacing_m or spec.boundary_spacing_m
    network = find_streams(project, spec.streams, mesh_poly, ds, log, refresh, is_canceled)
    path = streams_preview_path(project, spec)
    path.unlink(missing_ok=True)
    write_streams(network, path, project.epsg)
    nodes = sum(max(1, math.ceil(p.length_m / ds)) for p in network.paths)
    log(f"About {nodes:,} fixed stream nodes at {ds:g} m spacing; preview: {path}")
    return network, path


def build_mesh(
    project: Project,
    spec: MeshSpec,
    log: Callable[[str], None] = print,
    progress: Callable[[float], None] | None = None,
    is_canceled: Callable[[], bool] | None = None,
) -> MeshResult:
    """Build the mesh, write GB files, GeoPackage, CSVs, ``mesh.json`` and ``mut_snippet.txt``; save the project."""
    progress = progress or (lambda pct: None)
    is_canceled = is_canceled or (lambda: False)
    result = MeshResult()

    def warn(msg: str) -> None:
        result.warnings.append(msg)
        log(f"WARNING: {msg}")

    log(f"Mesh '{spec.name}': boundary spacing {spec.boundary_spacing_m:g} m, {spec.interior_points:,} interior points")
    if spec.boundary_tolerance_m > 0:
        log(f"  Boundary simplified with tolerance {spec.boundary_tolerance_m:g} m")
    polygon = model_polygon(project)
    bxy, rings_arr, mesh_poly = boundary_points(polygon, spec.boundary_spacing_m, spec.boundary_tolerance_m)
    rings = [list(map(int, r)) for r in rings_arr]
    offset = float(shapely.hausdorff_distance(mesh_poly.boundary, polygon.boundary))
    shortest = _shortest_side(bxy, rings_arr)
    result.stats["boundary"] = {
        "points": int(len(bxy)),
        "rings": len(rings),
        "tolerance_m": spec.boundary_tolerance_m,
        "max_offset_from_model_boundary_m": offset,
        "shortest_side_m": shortest,
    }
    log(f"  Boundary: {len(bxy):,} fixed points on {len(rings)} ring(s), area {mesh_poly.area / 1e6:,.2f} km2")
    log(f"  Mesh outline is within {offset:,.2f} m of the model boundary; shortest boundary side {shortest:,.1f} m")
    if shortest < 0.1 * spec.boundary_spacing_m:
        warn(
            f"The model boundary has sides as short as {shortest:,.1f} m, which give small cells there; set a boundary "
            "simplification tolerance to remove fine detail"
        )
    progress(5)

    network = None
    blevel = np.zeros(len(bxy), np.int8)
    sxy, slevel, chains = np.zeros((0, 2)), np.zeros(0, np.int8), []
    if spec.streams is not None:
        from .streams import find_streams

        ds_stream = spec.streams.spacing_m or spec.boundary_spacing_m
        network = find_streams(project, spec.streams, mesh_poly, ds_stream, log, is_canceled=is_canceled)
        for msg in network.warnings:
            log(f"  {msg}")
        bxy, blevel, sxy, chains, slevel = stream_points(
            network.paths, mesh_poly, bxy, rings, ds_stream, spec.boundary_spacing_m
        )
        outlets = int((blevel > 0).sum())
        result.stats["streams"] = {
            **network.summary(),
            "spacing_m": ds_stream,
            "stream_nodes": int(len(sxy)),
            "boundary_nodes_on_streams": outlets,
            "chains": len(chains),
        }
        log(f"  Streams: {len(sxy):,} fixed nodes along {len(chains):,} chain(s), {outlets} on the boundary")
    if is_canceled():
        raise InterruptedError("Run canceled")
    progress(10)

    uxy = np.zeros((0, 2))
    if spec.fixed_points_file:
        path = Path(spec.fixed_points_file)
        if not path.exists():
            raise FileNotFoundError(f"Fixed point file not found: {path}")
        uxy = user_points(path, project.epsg, mesh_poly)
        if len(uxy):
            tree = cKDTree(bxy)
            dist, _ = tree.query(uxy, k=1)
            close = dist < 0.25 * spec.boundary_spacing_m
            if close.any():
                warn(f"{int(close.sum())} fixed point(s) closer than {0.25 * spec.boundary_spacing_m:g} m to the boundary were skipped")
                uxy = uxy[~close]
            if len(uxy) and len(sxy):
                ds_stream = result.stats["streams"]["spacing_m"]
                dist, _ = cKDTree(sxy).query(uxy, k=1)
                close = dist < 0.25 * ds_stream
                if close.any():
                    warn(f"{int(close.sum())} fixed point(s) closer than {0.25 * ds_stream:g} m to a stream node were skipped")
                    uxy = uxy[~close]
        log(f"  Fixed user points: {len(uxy):,} inside the boundary ({path.name})")
    if is_canceled():
        raise InterruptedError("Run canceled")

    avoid = uxy
    if chains:
        h0 = estimated_spacing(mesh_poly.area, spec.interior_points)
        step = 0.25 * min(result.stats["streams"]["spacing_m"], h0)
        avoid = np.vstack([uxy, densify_chains(np.vstack([bxy, sxy]), chains, step)])
    ixy, h = interior_points(mesh_poly, spec.interior_points, avoid)
    result.interior_spacing_m = h
    log(f"  Interior: {len(ixy):,} free points on a hexagonal lattice, spacing {h:,.1f} m")
    if len(uxy) > 1:
        pairs = cKDTree(uxy).query_pairs(0.1 * h)
        if pairs:
            warn(f"{len(pairs)} pair(s) of fixed points are closer than {0.1 * h:,.1f} m; expect thin triangles there")
    if len(ixy) == 0:
        warn("No interior points fit inside the boundary; increase the number of interior points")
    progress(30)

    xy = np.vstack([bxy, sxy, uxy, ixy])
    if len(xy) > LARGE_MESH_NODES:
        warn(f"{len(xy):,} nodes: triangulation and MUT runs will be slow")
    source = np.concatenate(
        [
            np.full(len(bxy), SOURCES.index("boundary"), np.int8),
            np.full(len(sxy), SOURCES.index("stream"), np.int8),
            np.full(len(uxy), SOURCES.index("user"), np.int8),
            np.full(len(ixy), SOURCES.index("interior"), np.int8),
        ]
    )
    fixed = source != SOURCES.index("interior")
    level = np.concatenate([blevel, slevel, np.zeros(len(uxy) + len(ixy), np.int8)]).astype(np.int8)
    mesh = Mesh(xy, fixed, source, level=level)
    spacing = min(h, spec.boundary_spacing_m, result.stats.get("streams", {}).get("spacing_m", math.inf))
    mesh, w = triangulate(mesh, rings, mesh_poly, spacing, log, chains)
    result.stats["initial"] = quality(mesh)
    log(f"  Delaunay: {len(mesh.triangles):,} triangles, minimum angle {result.stats['initial']['min_angle_deg']:.1f} deg")
    progress(60)

    if spec.smoothing_iterations > 0:
        mesh, w = smooth(mesh, rings, mesh_poly, spacing, spec.smoothing_iterations, log, is_canceled, chains)
        result.stats["smoothed"] = quality(mesh)
        log(f"  Smoothed: minimum angle {result.stats['smoothed']['min_angle_deg']:.1f} deg")
    for msg in w:
        warn(msg)
    progress(80)

    final = quality(mesh)
    result.stats["final"] = final
    covered = final["area_m2"]["total"] / mesh_poly.area if mesh_poly.area else 0.0
    if abs(covered - 1.0) > 1e-3:
        warn(f"Triangles cover {100 * covered:.2f}% of the boundary polygon")
    result.mesh = mesh
    result.ok = final["elements"] > 0
    if chains:
        edges = _edge_set(mesh.triangles)
        on = sum((min(a, b), max(a, b)) in edges for ids, _ in chains for a, b in zip(ids[:-1], ids[1:]))
        total = sum(len(ids) - 1 for ids, _ in chains)
        result.stats["streams"]["edges_on_streams"] = f"{on} of {total}"
        result.stats["streams"]["nodes_by_level"] = {
            str(k): int((mesh.level == k).sum()) for k in sorted(set(mesh.level.tolist())) if k > 0
        }
    result.outputs = write_outputs(project, spec, result, network)
    rel = mut_snippet(project, spec).splitlines()[-1]
    if len(rel) > GB_PATH_MAX:
        warn(f"The Grid Builder path in mut_snippet.txt is longer than {GB_PATH_MAX} characters; shorten the mesh name")
    elif chains and len(f"{rel}.nchos.Stream level {spec.streams.levels}") > GB_PATH_MAX:
        warn(f"The stream node file paths are longer than {GB_PATH_MAX} characters; shorten the mesh name")
    spec.status = "built" if result.ok else "defined"
    project.set_mesh(spec)
    project.save()
    progress(100)
    log(
        f"Done: {final['nodes']:,} nodes ({final['fixed_nodes']:,} fixed), {final['elements']:,} triangles; "
        f"files in {mesh_outputs(project, spec)['folder']}"
    )
    return result
