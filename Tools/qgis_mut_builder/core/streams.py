"""River and stream networks inside the model boundary, ranked into levels for the mesh.

Sources, tried in this order by ``auto``: the Ontario Hydro Network (Land Information Ontario), USGS NHDPlus HR
(United States) and OpenStreetMap waterways (worldwide). ``dem`` extracts streams from the regional ground surface
with GRASS ``r.stream.extract`` instead. All lines are oriented downstream.

Levels follow Hack's ordering: level 1 is the main river of each network leaving the model area, traced upstream
from its outlet along the branch with the most stream length upstream (a stand-in for drainage area; a named main
river, or NHDPlus level paths, take precedence); level k+1 are the streams joining a level-k stream, traced the same
way, kept when at least ``min_length_km`` of them lies inside the boundary. Networks ending inside the area start at
level 2. Small gaps in the source lines are joined first.
"""

from __future__ import annotations

import hashlib
import json
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

import numpy as np
import shapely
from osgeo import gdal, ogr, osr
from shapely.geometry import LineString
from shapely.prepared import prep

from . import boundary as bnd
from .bathymetry import MNRF, overpass_area, overpass_json
from .geo import GeoBBox, transformer
from .net import session
from .project import STREAM_SOURCES
from .raster import cache_ok, write_cache_meta

SOURCES = STREAM_SOURCES
SOURCE_LABELS = {
    "auto": "Automatic (online)",
    "ohn": "Ontario Hydro Network",
    "nhdplus": "USGS NHDPlus HR",
    "osm": "OpenStreetMap",
    "dem": "From the regional DEM (GRASS)",
}
OHN_URL = f"{MNRF}/26"
NHD_URL = "https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer/3"
ONTARIO = GeoBBox(-95.2, 41.6, -74.3, 56.9)
NHD_CANAL_DITCH = 336
NHD_SKIP_FTYPES = {420, 566}  # underground conduit, coastline
SNAP_M = 1.0
JOIN_M = 50.0
GAP_M = 300.0
OUTLET_M = 1000.0
NAME_SHARE = 0.3
OSM_TILE_DEG = 0.5
OSM_SPLITS = 2

CITATIONS = {
    "ohn": (
        "Ontario Ministry of Natural Resources, Ontario Hydro Network (OHN) - Watercourse (Land Information Ontario), "
        "Open Government Licence - Ontario. https://geohub.lio.gov.on.ca/datasets/mnrf::ontario-hydro-network-ohn-watercourse"
    ),
    "nhdplus": (
        "U.S. Geological Survey, NHDPlus High Resolution (NetworkNHDFlowline), public domain. "
        "https://www.usgs.gov/national-hydrography/nhdplus-high-resolution"
    ),
    "osm": "Waterways (c) OpenStreetMap contributors, ODbL. https://www.openstreetmap.org/copyright",
    "dem": "Streams extracted from the regional ground surface with GRASS GIS r.stream.extract (Jasiewicz and Metz, 2011).",
}


@dataclass
class RawLine:
    coords: np.ndarray  # (n, 2) in the project CRS, downstream order
    name: str = ""
    path_id: str = ""  # NHDPlus level path: the source's own main-stem identity


@dataclass
class StreamPath:
    level: int
    name: str
    coords: np.ndarray  # downstream order, inside the boundary

    @property
    def length_m(self) -> float:
        return float(np.hypot(*np.diff(self.coords, axis=0).T).sum())


@dataclass
class StreamNetwork:
    source: str
    citation: str
    paths: list[StreamPath] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    @property
    def main_rivers(self) -> list[str]:
        level1 = sorted((p for p in self.paths if p.level == 1), key=lambda p: -p.length_m)
        return [p.name or "(unnamed)" for p in level1]

    def summary(self) -> dict:
        levels: dict[int, dict] = {}
        for p in self.paths:
            s = levels.setdefault(p.level, {"streams": 0, "length_km": 0.0, "names": []})
            s["streams"] += 1
            s["length_km"] += p.length_m / 1000.0
            if p.name and p.name not in s["names"]:
                s["names"].append(p.name)
        return {
            "source": self.source,
            "source_label": SOURCE_LABELS.get(self.source, self.source),
            "citation": self.citation,
            "main_river": (self.main_rivers or [""])[0],
            "levels": {str(k): levels[k] for k in sorted(levels)},
            "warnings": self.warnings,
        }

    def describe(self) -> str:
        s = self.summary()
        lines = [f"Streams ({s['source_label']}); main river: {s['main_river'] or '(unnamed)'}"]
        for k, v in s["levels"].items():
            names = ", ".join(v["names"][:6]) + (", ..." if len(v["names"]) > 6 else "")
            lines.append(
                f"  level {k}: {v['streams']} stream(s), {v['length_km']:,.1f} km" + (f" ({names})" if names else "")
            )
        return "\n".join(lines)


# ---------------------------------------------------------------- downloads


def _cache_key(bbox: GeoBBox, *parts) -> str:
    key = [round(v, 3) for v in bbox.as_list()] + list(parts)
    return hashlib.sha1(json.dumps(key, default=str).encode()).hexdigest()[:12]


def _arcgis_query(url: str, params: dict, what: str) -> dict:
    resp = session().post(f"{url}/query", data={"f": "json", **params}, timeout=180)
    resp.raise_for_status()
    data = resp.json()
    if "error" in data:
        raise RuntimeError(f"{what}: {data['error'].get('message', data['error'])}")
    return data


def _arcgis_lines(
    url: str, bbox: GeoBBox, fields: str, epsg: int, what: str, log, is_canceled, batch: int = 1000
) -> list[dict]:
    """All line features of an ArcGIS layer in ``bbox`` as {"attributes", "paths"} in ``epsg``, fetched by id batches."""
    ids = _arcgis_query(url, {
        "geometry": f"{bbox.west},{bbox.south},{bbox.east},{bbox.north}",
        "geometryType": "esriGeometryEnvelope",
        "inSR": 4326,
        "spatialRel": "esriSpatialRelIntersects",
        "returnIdsOnly": "true",
    }, what).get("objectIds") or []
    log(f"  {what}: {len(ids):,} line(s) in the model area")
    features: list[dict] = []
    pending = [sorted(ids)[i : i + batch] for i in range(0, len(ids), batch)]
    done = 0
    while pending:
        if is_canceled():
            raise InterruptedError("Run canceled")
        chunk = pending.pop(0)
        try:
            data = _arcgis_query(url, {
                "objectIds": ",".join(map(str, chunk)),
                "outFields": fields,
                "outSR": epsg,
                "returnGeometry": "true",
                "maxAllowableOffset": 1,
            }, what)
        except Exception:  # noqa: BLE001 (large responses fail; retry in halves)
            if len(chunk) == 1:
                continue
            half = len(chunk) // 2
            pending[:0] = [chunk[:half], chunk[half:]]
            continue
        for feat in data.get("features", []):
            features.append({"attributes": feat.get("attributes", {}), "paths": (feat.get("geometry") or {}).get("paths", [])})
        done += len(chunk)
        if done % 10000 < len(chunk):
            log(f"  {what}: {done:,} of {len(ids):,} downloaded")
    return features


def _cached(path: Path, refresh: bool, fetch: Callable[[], list]) -> list:
    if path.exists() and not refresh:
        return json.loads(path.read_text(encoding="utf-8"))
    data = fetch()
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data), encoding="utf-8")
    return data


def ohn_lines(bbox: GeoBBox, epsg: int, cache_dir: Path, refresh: bool, log, is_canceled) -> list[RawLine]:
    path = cache_dir / f"streams_ohn_{_cache_key(bbox, epsg)}.json"
    feats = _cached(
        path,
        refresh,
        lambda: _arcgis_lines(
            OHN_URL, bbox, "OFFICIAL_NAME_LABEL,WATERCOURSE_TYPE", epsg, "Ontario Hydro Network", log, is_canceled
        ),
    )
    out = []
    for f in feats:
        name = (f["attributes"].get("OFFICIAL_NAME_LABEL") or "").strip()
        out += [RawLine(np.asarray(p, float)[:, :2], name) for p in f["paths"] if len(p) >= 2]
    return out


def nhd_lines(
    bbox: GeoBBox, epsg: int, canals: bool, cache_dir: Path, refresh: bool, log, is_canceled
) -> list[RawLine]:
    fields = "gnis_name,levelpathi,streamleve,hydroseq,ftype"
    path = cache_dir / f"streams_nhdplus_{_cache_key(bbox, epsg, fields)}.json"
    feats = _cached(
        path,
        refresh,
        lambda: _arcgis_lines(NHD_URL, bbox, fields, epsg, "USGS NHDPlus HR", log, is_canceled, 1000),
    )
    skip = NHD_SKIP_FTYPES if canals else NHD_SKIP_FTYPES | {NHD_CANAL_DITCH}
    out = []
    for f in feats:
        a = f["attributes"]
        if a.get("ftype") in skip:
            continue
        name = (a.get("gnis_name") or "").strip()
        lp = a.get("levelpathi")
        out += [RawLine(np.asarray(p, float)[:, :2], name, str(lp) if lp else "") for p in f["paths"] if len(p) >= 2]
    return out


def _tiles(bbox: GeoBBox, size: float) -> list[GeoBBox]:
    nx = max(1, int(np.ceil((bbox.east - bbox.west) / size)))
    ny = max(1, int(np.ceil((bbox.north - bbox.south) / size)))
    xs = np.linspace(bbox.west, bbox.east, nx + 1)
    ys = np.linspace(bbox.south, bbox.north, ny + 1)
    return [GeoBBox(xs[i], ys[j], xs[i + 1], ys[j + 1]) for j in range(ny) for i in range(nx)]


def _quarters(b: GeoBBox) -> list[GeoBBox]:
    mx, my = (b.west + b.east) / 2, (b.south + b.north) / 2
    return [GeoBBox(b.west, b.south, mx, my), GeoBBox(mx, b.south, b.east, my), GeoBBox(b.west, my, mx, b.north), GeoBBox(mx, my, b.east, b.north)]


def osm_lines(
    bbox: GeoBBox, epsg: int, canals: bool, cache_dir: Path, refresh: bool, log, is_canceled
) -> list[RawLine]:
    kinds = "river|stream|canal" if canals else "river|stream"
    path = cache_dir / f"streams_osm_{_cache_key(bbox, kinds)}.json"

    def query(tile: GeoBBox, depth: int, ways: dict) -> None:
        if is_canceled():
            raise InterruptedError("Run canceled")
        try:
            data = overpass_json(
                f'[out:json][timeout:180];way["waterway"~"^({kinds})$"]{overpass_area(tile)};out tags geom;',
                "waterway",
                read_timeout=240,
            )
        except RuntimeError:
            if depth >= OSM_SPLITS:
                raise
            for part in _quarters(tile):
                query(part, depth + 1, ways)
            return
        for e in data.get("elements", []):
            if e.get("type") == "way" and len(e.get("geometry") or []) >= 2:
                ways[e["id"]] = {
                    "name": (e.get("tags") or {}).get("name", ""),
                    "points": [(p["lon"], p["lat"]) for p in e["geometry"]],
                }

    def fetch() -> list:
        tiles = _tiles(bbox, OSM_TILE_DEG)
        ways: dict = {}
        for n, tile in enumerate(tiles, start=1):
            log(f"  OpenStreetMap: querying waterways, tile {n} of {len(tiles)} ...")
            query(tile, 0, ways)
        return list(ways.values())

    ways = _cached(path, refresh, fetch)
    to_xy = transformer(4326, epsg)
    out = []
    for w in ways:
        pts = to_xy.TransformPoints(w["points"])
        out.append(RawLine(np.asarray(pts, float)[:, :2], w["name"]))
    return out


def dem_lines(project, threshold_km2: float, log, is_canceled) -> list[RawLine]:
    """Streams from ``ground_top.tif`` with GRASS r.stream.extract, oriented by flow accumulation."""
    from . import local_watershed as lw

    if not lw.grass_available():
        raise RuntimeError("GRASS (qgis_process) was not found; choose an online stream source")
    acc, _ = lw.hydro_rasters(project, log)
    dem = project.surfaces_dir / "ground_top.tif"
    res = lw._grid_of(dem).res
    cells = max(1, int(round(threshold_km2 * 1e6 / (res * res))))
    folder = project.mesh_root / "_streams"
    folder.mkdir(parents=True, exist_ok=True)
    out = folder / f"dem_{threshold_km2:g}km2.gpkg"
    signature = {"dem": str(dem), "mtime": dem.stat().st_mtime, "threshold_cells": cells}
    if not cache_ok(out, signature, False):
        out.unlink(missing_ok=True)
        log(f"  Extracting streams draining at least {threshold_km2:g} km2 ({cells:,} cells) with GRASS r.stream.extract ...")
        lw._run(
            "grass:r.stream.extract",
            {
                "elevation": dem,
                "accumulation": acc,
                "threshold": cells,
                "stream_vector": out,
                "GRASS_OUTPUT_TYPE_PARAMETER": 2,
            },
            log,
        )
        write_cache_meta(out, signature)
    if is_canceled():
        raise InterruptedError("Run canceled")
    ds = ogr.Open(str(out))
    if ds is None:
        raise RuntimeError(f"r.stream.extract wrote no stream vector ({out})")
    acc_ds = gdal.Open(str(acc))
    band, gt = acc_ds.GetRasterBand(1), acc_ds.GetGeoTransform()
    acc_arr = np.abs(band.ReadAsArray().astype(float))

    def acc_at(x: float, y: float) -> float:
        c, r = int((x - gt[0]) / gt[1]), int((y - gt[3]) / gt[5])
        if 0 <= r < acc_arr.shape[0] and 0 <= c < acc_arr.shape[1]:
            return float(acc_arr[r, c])
        return 0.0

    lines = []
    layer = ds.GetLayer(0)
    for feat in layer:
        geom = feat.GetGeometryRef()
        if geom is None:
            continue
        parts = [geom.GetGeometryRef(i) for i in range(geom.GetGeometryCount())] if geom.GetGeometryCount() else [geom]
        for part in parts:
            pts = np.asarray(part.GetPoints() or [], float)
            if len(pts) < 2:
                continue
            pts = pts[:, :2]
            if acc_at(*pts[0]) > acc_at(*pts[-1]):
                pts = pts[::-1]
            lines.append(RawLine(pts))
    return lines


def _geo_bbox(polygon, epsg: int) -> GeoBBox:
    xmin, ymin, xmax, ymax = polygon.bounds
    to_geo = transformer(epsg, 4326)
    corners = [to_geo.TransformPoint(x, y)[:2] for x in (xmin, xmax) for y in (ymin, ymax)]
    lons, lats = [c[0] for c in corners], [c[1] for c in corners]
    pad = 0.01
    return GeoBBox(min(lons) - pad, min(lats) - pad, max(lons) + pad, max(lats) + pad)


def _source_order(opts, bbox: GeoBBox) -> list[str]:
    if opts.source != "auto":
        return [opts.source]
    order = []
    if ONTARIO.intersects(bbox):
        order.append("ohn")
    centre = ((bbox.south + bbox.north) / 2, (bbox.west + bbox.east) / 2)
    if bnd.in_wbd_coverage(centre):
        order.append("nhdplus")
    return order + ["osm"]


# ---------------------------------------------------------------- network and levels


@dataclass
class _Edge:
    a: int  # upstream node
    b: int  # downstream node
    coords: np.ndarray
    name: str
    path_id: str
    length: float


def _edges(lines: list[RawLine]) -> list[_Edge]:
    """Lines split at every vertex shared with another line's end or interior, joined at nodes snapped to 1 m."""

    def key(p) -> tuple[int, int]:
        return (int(round(p[0] / SNAP_M)), int(round(p[1] / SNAP_M)))

    ends: set = set()
    uses: dict = defaultdict(int)
    for ln in lines:
        ends.add(key(ln.coords[0]))
        ends.add(key(ln.coords[-1]))
        for k in {key(p) for p in ln.coords}:
            uses[k] += 1
    node_id: dict = {}

    def nid(k) -> int:
        return node_id.setdefault(k, len(node_id))

    edges: list[_Edge] = []
    for ln in lines:
        keys = [key(p) for p in ln.coords]
        start = 0
        for i in range(1, len(keys)):
            if i == len(keys) - 1 or keys[i] in ends or uses[keys[i]] > 1:
                seg = ln.coords[start : i + 1]
                if keys[start] != keys[i]:
                    length = float(np.hypot(*np.diff(seg, axis=0).T).sum())
                    edges.append(_Edge(nid(keys[start]), nid(keys[i]), seg, ln.name, ln.path_id, length))
                start = i
    return edges


def _names_agree(a: str, b: str) -> bool:
    return not a or not b or a.lower() == b.lower()


def _join_gaps(edges: list[_Edge], tolerance: float, gap: float) -> tuple[list[_Edge], int]:
    """Join loose downstream ends to another network: to any line within ``tolerance`` (T-junctions that share no
    vertex, digitizing gaps; the line is split there), or to the start of a line within ``gap`` whose name does not
    differ (a stream broken at a pond or culvert). Each network joins at most once and never into itself, so no loops
    form. Returns (edges, joins)."""
    net = _Network(edges, [])
    root: dict[int, int] = {}
    for s in net.sinks():
        stack = [s]
        while stack:
            i = stack.pop()
            root[i] = s
            stack += net.in_edges.get(edges[i].a, [])
    geoms = [LineString(e.coords) for e in edges]
    tree = shapely.STRtree(geoms)
    heads = [i for i, e in enumerate(edges) if net.active[i] and not net.in_edges.get(e.a)]
    head_tree = shapely.STRtree([shapely.Point(edges[i].coords[0]) for i in heads]) if heads else None

    candidates: list[tuple[float, int, int, bool]] = []  # (distance, sink edge, target edge, onto line)
    for s in net.sinks():
        p = shapely.Point(edges[s].coords[-1])
        best = None
        for c in tree.query(p, predicate="dwithin", distance=tolerance):
            if root.get(int(c), s) == s:
                continue
            d = p.distance(geoms[c])
            if best is None or d < best[0]:
                best = (d, s, int(c), True)
        if best is None and head_tree is not None:
            for k in head_tree.query(p, predicate="dwithin", distance=gap):
                h = heads[int(k)]
                if root.get(h, s) == s or not _names_agree(edges[s].name, edges[h].name):
                    continue
                d = p.distance(shapely.Point(edges[h].coords[0]))
                if best is None or d < best[0]:
                    best = (d, s, h, False)
        if best is not None:
            candidates.append(best)

    group = {s: s for s in net.sinks()}

    def find(s: int) -> int:
        while group[s] != s:
            group[s] = group[group[s]]
            s = group[s]
        return s

    accepted = []
    for d, s, c, onto in sorted(candidates):
        a, b = find(s), find(root[c])
        if a != b:
            group[a] = b
            accepted.append((d, s, c, onto))

    n_nodes = 1 + max(max(e.a, e.b) for e in edges)
    splits: dict[int, list[tuple[float, int, np.ndarray]]] = defaultdict(list)
    joins: list[_Edge] = []
    n_joined = len(accepted)
    for d, s, c, onto in accepted:
        p = shapely.Point(edges[s].coords[-1])
        if not onto:
            q = edges[c].coords[0]
            joins.append(_Edge(edges[s].b, edges[c].a, np.vstack([edges[s].coords[-1], q]), edges[s].name, edges[s].path_id, d))
            continue
        g = geoms[c]
        t = g.project(p)
        if t <= 1e-6 or t >= g.length - 1e-6:
            node = edges[c].a if t <= 1e-6 else edges[c].b
            q = np.asarray(g.coords[0] if t <= 1e-6 else g.coords[-1])[:2]
        else:
            node = edges[s].b if d < SNAP_M else n_nodes
            if node == n_nodes:
                n_nodes += 1
            q = np.asarray(g.interpolate(t).coords[0])[:2]
            splits[c].append((t, node, q))
        if node != edges[s].b:
            joins.append(_Edge(edges[s].b, node, np.vstack([edges[s].coords[-1], q]), edges[s].name, edges[s].path_id, d))
    if not splits and not joins:
        return edges, 0
    out: list[_Edge] = []
    for i, e in enumerate(edges):
        if i not in splits:
            out.append(e)
            continue
        cum = np.concatenate([[0.0], np.cumsum(np.hypot(*np.diff(e.coords, axis=0).T))])
        a, start_t, start_xy = e.a, 0.0, e.coords[0]
        for t, node, q in sorted(splits[i], key=lambda v: v[0]) + [(cum[-1], e.b, e.coords[-1])]:
            if node == a:
                continue
            inner = e.coords[(cum > start_t) & (cum < t)]
            seg = np.vstack([start_xy, inner, q]) if len(inner) else np.vstack([start_xy, q])
            out.append(_Edge(a, node, seg, e.name, e.path_id, float(t - start_t)))
            a, start_t, start_xy = node, t, q
    return out + joins, n_joined


class _Network:
    def __init__(self, edges: list[_Edge], warnings: list[str]) -> None:
        self.edges = edges
        out_edges: dict[int, list[int]] = defaultdict(list)
        in_edges: dict[int, list[int]] = defaultdict(list)
        for i, e in enumerate(edges):
            out_edges[e.a].append(i)
            in_edges[e.b].append(i)
        # Braids and distributaries: each node keeps one downstream edge (same name or path, else the longest).
        dropped = set()
        for node, outs in out_edges.items():
            if len(outs) < 2:
                continue
            ins = in_edges.get(node, [])
            names = {edges[i].name for i in ins if edges[i].name} | {edges[i].path_id for i in ins if edges[i].path_id}
            keep = max(outs, key=lambda i: ((edges[i].name in names) or (edges[i].path_id in names), edges[i].length))
            dropped.update(i for i in outs if i != keep)
        self.active = [i not in dropped for i in range(len(edges))]
        self.in_edges: dict[int, list[int]] = defaultdict(list)
        self.out_edge: dict[int, int] = {}
        for i, e in enumerate(edges):
            if self.active[i]:
                self.in_edges[e.b].append(i)
                self.out_edge[e.a] = i
        self.upstream = self._upstream_lengths(warnings)
        if dropped:
            warnings.append(f"{len(dropped)} side channel or distributary segment(s) were left out of the ranking")

    def _upstream_lengths(self, warnings: list[str]) -> np.ndarray:
        """Stream length upstream of (and including) each edge, in topological order; cycles are cut."""
        up = np.zeros(len(self.edges))
        indeg = defaultdict(int)
        for i, e in enumerate(self.edges):
            if self.active[i]:
                indeg[e.a] += 0
                indeg[e.b] += 1
        heads = [n for n, d in indeg.items() if d == 0]
        seen = 0
        while heads:
            node = heads.pop()
            i = self.out_edge.get(node)
            if i is None:
                continue
            seen += 1
            up[i] = self.edges[i].length + sum(up[j] for j in self.in_edges.get(node, []))
            nb = self.edges[i].b
            indeg[nb] -= 1
            if indeg[nb] == 0:
                heads.append(nb)
        cyclic = [i for i, e in enumerate(self.edges) if self.active[i] and up[i] == 0]
        if cyclic:
            for i in cyclic:
                self.active[i] = False
                e = self.edges[i]
                if i in self.in_edges.get(e.b, []):
                    self.in_edges[e.b].remove(i)
                if self.out_edge.get(e.a) == i:
                    del self.out_edge[e.a]
            warnings.append(f"{len(cyclic)} segment(s) in loops were left out of the ranking")
        return up

    def sinks(self) -> list[int]:
        """Most downstream edge of each network."""
        return [i for i, e in enumerate(self.edges) if self.active[i] and e.b not in self.out_edge]

    def trace(self, first: int, prefer: str = "") -> list[int]:
        """Edges from ``first`` upstream (downstream-first order), following the main branch at each confluence."""
        edges, up = self.edges, self.upstream
        path = [first]
        name = prefer or edges[first].name
        pid = edges[first].path_id
        node = edges[first].a
        guard = set(path)
        while True:
            cands = [c for c in self.in_edges.get(node, []) if c not in guard]
            if not cands:
                break
            best = max(cands, key=lambda c: up[c])
            if pid:
                same = [c for c in cands if edges[c].path_id == pid]
                if same:
                    best = max(same, key=lambda c: up[c])
            elif name:
                same = [c for c in cands if edges[c].name.lower() == name.lower()]
                if same:
                    nb = max(same, key=lambda c: up[c])
                    if up[nb] >= NAME_SHARE * up[best]:
                        best = nb
            if not name and edges[best].name:
                name = edges[best].name
            path.append(best)
            guard.add(best)
            node = edges[best].a
        return path

    def coords(self, path: list[int]) -> np.ndarray:
        parts = [self.edges[i].coords for i in reversed(path)]
        out = [parts[0]] + [p[1:] for p in parts[1:]]
        return np.vstack(out)

    def path_name(self, path: list[int]) -> str:
        """Name covering most of the path's length."""
        lengths: dict[str, float] = defaultdict(float)
        for i in path:
            if self.edges[i].name:
                lengths[self.edges[i].name] += self.edges[i].length
        return max(lengths, key=lengths.get) if lengths else ""


def _clip(coords: np.ndarray, polygon, min_part: float) -> list[np.ndarray]:
    """Parts of a downstream-ordered line inside ``polygon``, each kept in downstream order."""
    line = LineString(coords)
    inside = shapely.intersection(line, polygon)
    parts = [g for g in getattr(inside, "geoms", [inside]) if isinstance(g, LineString) and not g.is_empty]
    if len(parts) > 1:
        merged = shapely.line_merge(shapely.MultiLineString(parts))
        parts = [g for g in getattr(merged, "geoms", [merged]) if isinstance(g, LineString)]
    out = []
    for g in parts:
        if g.length < min_part:
            continue
        xy = np.asarray(g.coords)[:, :2]
        if line.project(shapely.Point(xy[0])) > line.project(shapely.Point(xy[-1])):
            xy = xy[::-1]
        out.append(xy)
    return out


def rank(
    lines: list[RawLine],
    polygon,
    levels: int,
    min_length_m: float,
    main_river: str = "",
    min_part_m: float = 0.0,
) -> tuple[list[StreamPath], list[str]]:
    """Rank ``lines`` into levels 1..``levels`` and clip them to ``polygon``."""
    warnings: list[str] = []
    edges = _edges(lines)
    if not edges:
        return [], warnings
    edges, joined = _join_gaps(edges, JOIN_M, GAP_M)
    net = _Network(edges, warnings)
    if joined:
        warnings.append(
            f"{joined} stream(s) ending within {JOIN_M:g} m of another stream, or {GAP_M:g} m of the start of one, "
            "were joined to it"
        )
    prepared = prep(polygon)
    out: list[StreamPath] = []

    def inside_parts(path: list[int]) -> list[np.ndarray]:
        xy = net.coords(path)
        if not prepared.intersects(LineString(xy)):
            return []
        return _clip(xy, polygon, min_part_m)

    # Rivers leaving the model area are level 1; networks ending well inside it (at a lake, or a gap in the data)
    # start at level 2, unless no stream reaches the boundary.
    seeds: dict[int, list[int]] = {1: [], 2: []}
    boundary = polygon.boundary
    for sink in net.sinks():
        if net.upstream[sink] < min_length_m:
            continue
        end = shapely.Point(net.edges[sink].coords[-1])
        interior = prepared.contains(end) and end.distance(boundary) > OUTLET_M
        seeds[2 if interior else 1].append(sink)
    if not seeds[1]:
        seeds = {1: seeds[2], 2: []}
    elif seeds[2]:
        warnings.append(f"{len(seeds[2])} stream network(s) ending inside the boundary were ranked from level 2")

    current: list[list[int]] = []
    for level in range(1, levels + 1):
        nxt: list[list[int]] = []
        for sink in seeds.get(level, []):
            path = net.trace(sink, main_river if level == 1 else "")
            parts = inside_parts(path)
            if sum(LineString(p).length for p in parts) >= min_length_m:
                nxt.append(path)
                name = net.path_name(path)
                out += [StreamPath(level, name, p) for p in parts]
        for parent in current:
            on_path = set(parent)
            for i in parent:
                for c in net.in_edges.get(net.edges[i].a, []):
                    if c in on_path or net.upstream[c] < min_length_m:
                        continue
                    path = net.trace(c)
                    parts = inside_parts(path)
                    if sum(LineString(p).length for p in parts) >= min_length_m:
                        nxt.append(path)
                        name = net.path_name(path)
                        out += [StreamPath(level, name, p) for p in parts]
        current = nxt
    return out, warnings


# ---------------------------------------------------------------- driver


def find_streams(
    project,
    opts,
    polygon,
    spacing_m: float,
    log: Callable[[str], None] = print,
    refresh: bool = False,
    is_canceled: Callable[[], bool] | None = None,
) -> StreamNetwork:
    """Streams inside ``polygon`` (project CRS) from the source in ``opts`` (``auto`` tries the online sources)."""
    is_canceled = is_canceled or (lambda: False)
    bbox = _geo_bbox(polygon, project.epsg)
    cache = project.downloads_dir
    errors: list[str] = []
    for source in _source_order(opts, bbox):
        log(f"Streams: trying {SOURCE_LABELS[source]}")
        try:
            if source == "ohn":
                lines = ohn_lines(bbox, project.epsg, cache, refresh, log, is_canceled)
            elif source == "nhdplus":
                lines = nhd_lines(bbox, project.epsg, opts.include_canals, cache, refresh, log, is_canceled)
            elif source == "osm":
                lines = osm_lines(bbox, project.epsg, opts.include_canals, cache, refresh, log, is_canceled)
            else:
                lines = dem_lines(project, opts.dem_threshold_km2, log, is_canceled)
        except InterruptedError:
            raise
        except Exception as exc:  # noqa: BLE001 (try the next source)
            errors.append(f"{SOURCE_LABELS[source]}: {exc}")
            log(f"  {SOURCE_LABELS[source]} failed: {exc}")
            continue
        if is_canceled():
            raise InterruptedError("Run canceled")
        paths, warnings = rank(
            lines, polygon, opts.levels, opts.min_length_km * 1000.0, opts.main_river, 0.5 * spacing_m
        )
        if not paths:
            log(f"  {SOURCE_LABELS[source]}: no streams of at least {opts.min_length_km:g} km inside the boundary")
            continue
        net = StreamNetwork(source, CITATIONS[source], paths, warnings)
        if errors:
            net.warnings.insert(0, "Skipped stream sources: " + "; ".join(errors))
        log(net.describe())
        return net
    raise RuntimeError("No streams found inside the boundary" + (": " + "; ".join(errors) if errors else ""))


def write_streams(network: StreamNetwork, path: Path, epsg: int, layer_name: str = "streams") -> Path:
    """Ranked streams as a line layer (``level``, ``name``, ``source``, ``length_m``); adds to an existing GeoPackage."""
    path.parent.mkdir(parents=True, exist_ok=True)
    driver = ogr.GetDriverByName("GPKG")
    ds = driver.Open(str(path), 1) if path.exists() else driver.CreateDataSource(str(path))
    if ds.GetLayerByName(layer_name) is not None:
        ds.DeleteLayer(layer_name)
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg)
    layer = ds.CreateLayer(layer_name, srs, ogr.wkbLineString)
    for name, kind in (("level", ogr.OFTInteger), ("name", ogr.OFTString), ("source", ogr.OFTString), ("length_m", ogr.OFTReal)):
        layer.CreateField(ogr.FieldDefn(name, kind))
    defn = layer.GetLayerDefn()
    layer.StartTransaction()
    for p in sorted(network.paths, key=lambda p: -p.level):
        feat = ogr.Feature(defn)
        feat.SetField("level", p.level)
        feat.SetField("name", p.name)
        feat.SetField("source", network.source)
        feat.SetField("length_m", p.length_m)
        geom = ogr.Geometry(ogr.wkbLineString)
        for x, y in p.coords:
            geom.AddPoint_2D(float(x), float(y))
        feat.SetGeometry(geom)
        layer.CreateFeature(feat)
    layer.CommitTransaction()
    ds = None
    return path
