"""Model boundary: rectangle, local file, or a watershed found online by name.

Watershed sources:
    USGS Watershed Boundary Dataset (WBD)   named hydrologic units (HUC2-HUC12), United States
    OpenStreetMap (Nominatim, Overpass)     named rivers and streams, worldwide
    Global Watersheds (mghydro.com)         delineates the area draining to an outlet (MERIT-Hydro)

A named river becomes a watershed by delineating at several outlets along it;
the user chooses among the outlets (largest first) because automatic snapping
can pick a tributary or a neighbouring river at confluences.
"""

from __future__ import annotations

import json
import math
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

from osgeo import ogr, osr

from .net import session

WBD = "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer"
WBD_LAYERS = {1: "huc2", 2: "huc4", 3: "huc6", 4: "huc8", 5: "huc10", 6: "huc12"}
NOMINATIM = "https://nominatim.openstreetmap.org"
OVERPASS = "https://overpass-api.de/api/interpreter"
MGHYDRO = "https://mghydro.com/app/watershed_api"

RIVER_TYPES = {"river", "stream", "canal", "brook", "creek"}
BASIN_TYPES = {"drainage_basin", "basin", "catchment", "watershed", "water_basin"}
OUTLET_FRACTIONS = (0.0, 0.03, 0.1, 0.25, 0.5, 0.75, 0.97, 1.0)

CITATIONS = {
    "wbd": "U.S. Geological Survey, Watershed Boundary Dataset (WBD), public domain. https://www.usgs.gov/national-hydrography/watershed-boundary-dataset",
    "osm": "Feature geometry (c) OpenStreetMap contributors, ODbL. https://www.openstreetmap.org/copyright",
    "mghydro": (
        "Watershed delineated with Global Watersheds (Heberger, M., https://mghydro.com/watersheds) from MERIT-Hydro "
        "(Yamazaki et al., 2019, Water Resources Research 55, 5053-5073, https://doi.org/10.1029/2019WR024873; "
        "CC BY-NC 4.0 or ODbL). Automated delineation can be wrong: check the outline."
    ),
}


@dataclass
class BoundaryCandidate:
    label: str
    source: str
    kind: str  # "polygon" (geometry ready or fetchable) or "river" (needs outlets delineated)
    area_km2: float | None = None
    detail: str = ""
    geometry: dict | None = None
    ref: dict = field(default_factory=dict)
    citation: str = ""

    @property
    def display(self) -> str:
        area = f", {self.area_km2:,.0f} km2" if self.area_km2 else ""
        detail = f" - {self.detail}" if self.detail else ""
        return f"{self.label} [{self.source}{area}]{detail}"


def _area(text) -> float | None:
    try:
        return float(str(text).replace(",", ""))
    except (TypeError, ValueError):
        return None


def in_wbd_coverage(near: tuple[float, float] | None) -> bool:
    if near is None:
        return True
    from .providers.usgs3dep import Usgs3DEP1m

    lat, lon = near
    return any(box.west <= lon <= box.east and box.south <= lat <= box.north for box in Usgs3DEP1m.coverage)


def search_wbd(
    name: str,
    near: tuple[float, float] | None = None,
    log: Callable[[str], None] = print,
    limit: int = 8,
) -> list[BoundaryCandidate]:
    """Named WBD hydrologic units; ``near`` restricts the search to units within ~2 degrees (much faster)."""
    pattern = name.upper().replace("'", "''")

    def query(layer: int, huc: str) -> list[dict]:
        params = {
            "where": f"UPPER(name) LIKE '%{pattern}%'",
            "outFields": f"objectid,name,{huc},areasqkm,states",
            "returnGeometry": "false",
            "f": "json",
            "resultRecordCount": limit,
        }
        if near is not None:
            lat, lon = near
            params.update(
                {
                    "geometry": f"{lon - 2},{lat - 2},{lon + 2},{lat + 2}",
                    "geometryType": "esriGeometryEnvelope",
                    "inSR": 4326,
                    "spatialRel": "esriSpatialRelIntersects",
                }
            )
        resp = session().get(f"{WBD}/{layer}/query", params=params, timeout=90)
        if resp.status_code >= 400:
            raise RuntimeError(f"HTTP {resp.status_code}")
        return resp.json().get("features", [])

    found: list[BoundaryCandidate] = []
    with ThreadPoolExecutor(max_workers=len(WBD_LAYERS)) as pool:
        futures = {layer: pool.submit(query, layer, huc) for layer, huc in WBD_LAYERS.items()}
    for layer, huc in WBD_LAYERS.items():
        try:
            features = futures[layer].result()
        except Exception as exc:  # noqa: BLE001 (WBD server is unreliable; other layers and sources still searched)
            log(f"USGS WBD {huc.upper()} search failed ({exc}); server busy, other levels still listed")
            continue
        for feat in features:
            a = feat["attributes"]
            found.append(
                BoundaryCandidate(
                    label=a.get("name", "?"),
                    source=f"USGS WBD {huc.upper()}",
                    kind="polygon",
                    area_km2=_area(a.get("areasqkm")),
                    detail=f"{huc.upper()} {a.get(huc, '')}, {a.get('states', '')}",
                    ref={"wbd_layer": layer, "objectid": a["objectid"]},
                    citation=CITATIONS["wbd"],
                )
            )
    return found


def search_osm(name: str, near: tuple[float, float] | None = None, limit: int = 10) -> list[BoundaryCandidate]:
    params = {"q": name, "format": "jsonv2", "limit": limit, "accept-language": "en"}
    if near is not None:
        lat, lon = near
        params.update({"viewbox": f"{lon - 1.5},{lat + 1.5},{lon + 1.5},{lat - 1.5}", "bounded": 0})
    resp = session().get(f"{NOMINATIM}/search", params=params, timeout=60)
    resp.raise_for_status()
    found: list[BoundaryCandidate] = []
    seen: set[str] = set()
    for item in resp.json():
        kind_type = item.get("type", "")
        if item.get("display_name") in seen:
            continue
        if kind_type in RIVER_TYPES or item.get("category") == "waterway":
            kind = "river"
        elif kind_type in BASIN_TYPES:
            kind = "polygon"
        else:
            continue
        seen.add(item.get("display_name"))
        found.append(
            BoundaryCandidate(
                label=item.get("display_name", "?"),
                source="OpenStreetMap",
                kind=kind,
                detail=f"{item.get('category')}={kind_type}",
                ref={
                    "osm_type": item.get("osm_type"),
                    "osm_id": item.get("osm_id"),
                    "lat": float(item["lat"]),
                    "lon": float(item["lon"]),
                    "name": item.get("name") or name,
                },
                citation=CITATIONS["osm"],
            )
        )
    return found


def search_watersheds(name: str, near: tuple[float, float] | None = None, log: Callable[[str], None] = print) -> list[BoundaryCandidate]:
    """Named watershed candidates: WBD hydrologic units first, then OpenStreetMap rivers and basins."""
    name = name.strip()
    if not name:
        return []
    found = search_wbd(name, near, log) if in_wbd_coverage(near) else []
    try:
        found += search_osm(name, near)
    except Exception as exc:  # noqa: BLE001 (OSM is optional when WBD found something)
        log(f"OpenStreetMap search failed ({exc})")
    log(f"{len(found)} candidate(s) for '{name}'")
    return found


def wbd_geometry(candidate: BoundaryCandidate) -> dict:
    layer = candidate.ref["wbd_layer"]
    params = {
        "objectIds": candidate.ref["objectid"],
        "outFields": "name",
        "returnGeometry": "true",
        "outSR": 4326,
        "geometryPrecision": 6,
        "maxAllowableOffset": 0.0001,
        "f": "geojson",
    }
    resp = session().get(f"{WBD}/{layer}/query", params=params, timeout=180)
    resp.raise_for_status()
    features = resp.json().get("features", [])
    if not features:
        raise RuntimeError(f"WBD returned no geometry for {candidate.label}")
    return features[0]["geometry"]


def osm_polygon(candidate: BoundaryCandidate) -> dict:
    ref = candidate.ref
    osm_id = f"{ref['osm_type'][0].upper()}{ref['osm_id']}"
    resp = session().get(f"{NOMINATIM}/lookup", params={"osm_ids": osm_id, "format": "jsonv2", "polygon_geojson": 1}, timeout=120)
    resp.raise_for_status()
    items = resp.json()
    if not items or items[0].get("geojson", {}).get("type") not in ("Polygon", "MultiPolygon"):
        raise RuntimeError(f"OpenStreetMap has no polygon for {candidate.label}")
    return items[0]["geojson"]


def _river_points(candidate: BoundaryCandidate, radius_km: float = 60.0) -> list[tuple[float, float]]:
    """(lon, lat) vertices of the river: whole relation from Nominatim, or same-named ways nearby from Overpass."""
    ref = candidate.ref
    if ref.get("osm_type") == "relation":
        osm_id = f"R{ref['osm_id']}"
        resp = session().get(f"{NOMINATIM}/lookup", params={"osm_ids": osm_id, "format": "jsonv2", "polygon_geojson": 1}, timeout=120)
        resp.raise_for_status()
        geom = (resp.json() or [{}])[0].get("geojson", {})
        if geom.get("type") == "LineString":
            return [tuple(p) for p in geom["coordinates"]]
        if geom.get("type") == "MultiLineString":
            return [tuple(p) for part in geom["coordinates"] for p in part]
    name = ref["name"].replace('"', '\\"')
    query = (
        f'[out:json][timeout:90];way["waterway"]["name"="{name}"]'
        f"(around:{int(radius_km * 1000)},{ref['lat']},{ref['lon']});out geom;"
    )
    resp = session().post(OVERPASS, data={"data": query}, timeout=180)
    resp.raise_for_status()
    points = [(p["lon"], p["lat"]) for way in resp.json().get("elements", []) for p in way.get("geometry", [])]
    return points or [(ref["lon"], ref["lat"])]


def delineate(lat: float, lon: float, precision: str = "high") -> BoundaryCandidate:
    """Watershed draining to (lat, lon) from Global Watersheds (MERIT-Hydro)."""
    resp = session().get(MGHYDRO, params={"lat": lat, "lng": lon, "precision": precision, "simplify": "true"}, timeout=900)
    if resp.status_code in (400, 404):
        raise RuntimeError(f"No watershed for outlet {lat:.4f}, {lon:.4f}: {resp.text[:200]}")
    resp.raise_for_status()
    feature = resp.json()["features"][0]
    props = feature.get("properties", {})
    out_lat, out_lon = props.get("outlet_lat", lat), props.get("outlet_lng", lon)
    return BoundaryCandidate(
        label=f"Watershed above {out_lat:.4f}, {out_lon:.4f}",
        source="Global Watersheds",
        kind="polygon",
        area_km2=_area(props.get("area_km2")),
        geometry=feature["geometry"],
        ref={"outlet_lat": out_lat, "outlet_lon": out_lon},
        citation=CITATIONS["mghydro"],
    )


def river_outlets(
    candidate: BoundaryCandidate,
    near: tuple[float, float] | None = None,
    log: Callable[[str], None] = print,
    is_canceled: Callable[[], bool] = lambda: False,
) -> list[BoundaryCandidate]:
    """Delineate watersheds at outlets spread along a named river (plus the point nearest the site), largest first."""
    points = _river_points(candidate)
    picks = {points[min(int(f * (len(points) - 1)), len(points) - 1)] for f in OUTLET_FRACTIONS}
    if near is not None:
        lat, lon = near
        picks.add(min(points, key=lambda p: (p[0] - lon) ** 2 + (p[1] - lat) ** 2))
    log(f"Delineating {len(picks)} outlet(s) along {candidate.ref.get('name', candidate.label)} ...")
    results: dict[str, BoundaryCandidate] = {}
    for lon, lat in picks:
        if is_canceled():
            break
        try:
            ws = delineate(lat, lon)
        except Exception as exc:  # noqa: BLE001 (other outlets may still work)
            log(f"  outlet {lat:.4f}, {lon:.4f}: {exc}")
            continue
        key = f"{ws.area_km2}"
        if key in results:
            continue
        ws.label = f"{candidate.ref.get('name', 'River')} above {ws.ref['outlet_lat']:.4f}, {ws.ref['outlet_lon']:.4f}"
        ws.citation = CITATIONS["mghydro"] + " " + CITATIONS["osm"]
        results[key] = ws
        log(f"  outlet {lat:.4f}, {lon:.4f}: {ws.area_km2 or 0:,.0f} km2")
        time.sleep(0.2)
    return sorted(results.values(), key=lambda c: -(c.area_km2 or 0))


def resolve_geometry(candidate: BoundaryCandidate) -> dict:
    """GeoJSON geometry (EPSG:4326) for a polygon candidate, fetching it if needed."""
    if candidate.geometry is None:
        if "wbd_layer" in candidate.ref:
            candidate.geometry = wbd_geometry(candidate)
        elif candidate.ref.get("osm_type"):
            candidate.geometry = osm_polygon(candidate)
        else:
            raise RuntimeError(f"No geometry available for {candidate.label}")
    return candidate.geometry


def rectangle_geometry(west: float, south: float, east: float, north: float) -> dict:
    return {
        "type": "Polygon",
        "coordinates": [[[west, south], [east, south], [east, north], [west, north], [west, south]]],
    }


def _wgs84() -> osr.SpatialReference:
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(4326)
    srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return srs


def projected_rectangle_geometry(epsg: int, bounds: tuple[float, float, float, float], densify: int = 20) -> dict:
    """Rectangle drawn in a projected CRS, returned as a densified EPSG:4326 polygon."""
    xmin, ymin, xmax, ymax = bounds
    src = osr.SpatialReference()
    src.ImportFromEPSG(epsg)
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    ring = ogr.Geometry(ogr.wkbLinearRing)
    for x, y in ((xmin, ymin), (xmax, ymin), (xmax, ymax), (xmin, ymax), (xmin, ymin)):
        ring.AddPoint_2D(x, y)
    poly = ogr.Geometry(ogr.wkbPolygon)
    poly.AddGeometry(ring)
    poly.Segmentize(max(xmax - xmin, ymax - ymin) / densify)
    poly.AssignSpatialReference(src)
    poly.TransformTo(_wgs84())
    return json.loads(poly.ExportToJson())


def file_geometry(path: Path) -> dict:
    """Union of all polygons in a vector file, as EPSG:4326 GeoJSON."""
    ds = ogr.Open(str(path))
    if ds is None:
        raise RuntimeError(f"Cannot open {path}")
    layer = ds.GetLayer(0)
    srs = layer.GetSpatialRef()
    union = None
    for feat in layer:
        geom = feat.GetGeometryRef()
        if geom is None or ogr.GT_Flatten(geom.GetGeometryType()) not in (ogr.wkbPolygon, ogr.wkbMultiPolygon):
            continue
        geom = geom.Clone()
        union = geom if union is None else union.Union(geom)
    if union is None:
        raise RuntimeError(f"{path} contains no polygons")
    if srs is not None:
        src = srs.Clone()
        src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
        union.AssignSpatialReference(src)
        union.TransformTo(_wgs84())
    return json.loads(union.ExportToJson())


def write_boundary(geometry: dict, path: Path, properties: dict) -> Path:
    """Write a one-feature GeoJSON (EPSG:4326) boundary file."""
    path.parent.mkdir(parents=True, exist_ok=True)
    collection = {
        "type": "FeatureCollection",
        "name": path.stem,
        "features": [{"type": "Feature", "properties": properties, "geometry": geometry}],
    }
    path.write_text(json.dumps(collection, indent=2), encoding="utf-8")
    return path


def apply_geometry(project, geometry: dict, mode: str, name: str, source: str, citation: str = "") -> Path:
    """Write ``geometry`` to the project's boundary file and record it on ``project``."""
    path = project.default_boundary_path
    write_boundary(geometry, path, {"name": name, "source": source, "mode": mode, "citation": citation})
    project.boundary_mode = mode
    project.boundary_file = str(path)
    project.boundary_name = name
    project.boundary_source = source
    return path


def apply_candidate(project, candidate: BoundaryCandidate) -> Path:
    geometry = resolve_geometry(candidate)
    source = candidate.source
    if candidate.ref.get("outlet_lat") is not None:
        source += f" (outlet {candidate.ref['outlet_lat']:.4f}, {candidate.ref['outlet_lon']:.4f})"
    return apply_geometry(project, geometry, "watershed", candidate.label, source, candidate.citation)


def read_properties(path: Path) -> dict:
    try:
        data = json.loads(Path(path).read_text(encoding="utf-8"))
        return data["features"][0].get("properties", {}) or {}
    except (OSError, ValueError, KeyError, IndexError, TypeError):
        return {}


def _projected_geometry(path: Path, epsg: int) -> ogr.Geometry:
    ds = ogr.Open(str(path))
    if ds is None:
        raise RuntimeError(f"Cannot open boundary file {path}")
    layer = ds.GetLayer(0)
    srs = layer.GetSpatialRef()
    union = None
    for feat in layer:
        geom = feat.GetGeometryRef()
        if geom is not None:
            union = geom.Clone() if union is None else union.Union(geom)
    if union is None:
        raise RuntimeError(f"Boundary file {path} is empty")
    dst = osr.SpatialReference()
    dst.ImportFromEPSG(epsg)
    dst.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    src = srs.Clone() if srs is not None else _wgs84()
    src.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    union.AssignSpatialReference(src)
    union.TransformTo(dst)
    return union


def boundary_bounds(path: Path, epsg: int) -> tuple[float, float, float, float]:
    xmin, xmax, ymin, ymax = _projected_geometry(path, epsg).GetEnvelope()
    return xmin, ymin, xmax, ymax


def boundary_area_km2(path: Path, epsg: int) -> float:
    return _projected_geometry(path, epsg).GetArea() / 1e6


def boundary_centroid(path: Path) -> tuple[float, float]:
    """(lat, lon) of the boundary centroid."""
    geom = _projected_geometry(path, 4326)
    c = geom.Centroid()
    return c.GetY(), c.GetX()


def boundary_wkt(path: Path, epsg: int) -> str:
    return _projected_geometry(path, epsg).ExportToWkt()


def contains_point(path: Path, lat: float, lon: float) -> bool:
    geom = _projected_geometry(path, 4326)
    pt = ogr.Geometry(ogr.wkbPoint)
    pt.AddPoint_2D(lon, lat)
    return geom.Contains(pt)


def snap_out(value: float, step: float, up: bool) -> float:
    return (math.ceil if up else math.floor)(value / step) * step
