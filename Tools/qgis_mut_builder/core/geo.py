"""Coordinate helpers: UTM zone selection, model grids and lon/lat bounding boxes."""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass

from osgeo import osr

# Above the soft limits a run asks for confirmation (wizard, focus dialog) or logs a warning (headless).
LARGE_GRID_CELLS = 40_000_000
LARGE_FOCUS_CELLS = 50_000_000
# Above the hard limit nothing is downloaded without an explicit override: 250 million cells is 1 GB per float32
# layer, and a run holds several float64 copies (10-15 GB), the practical ceiling of a 16-32 GB workstation.
MAX_GRID_CELLS = 250_000_000
MAX_REGIONAL_RES_M = 1000.0
MAX_FOCUS_RES_M = 30.0


class GridTooLargeError(ValueError):
    """A grid above ``MAX_GRID_CELLS`` was requested without ``allow_large_grid``."""


@dataclass(frozen=True)
class GeoBBox:
    """Geographic bounding box in decimal degrees (WGS 84)."""

    west: float
    south: float
    east: float
    north: float

    def intersects(self, other: "GeoBBox") -> bool:
        return not (
            self.east < other.west
            or self.west > other.east
            or self.north < other.south
            or self.south > other.north
        )

    def as_list(self) -> list[float]:
        return [self.west, self.south, self.east, self.north]


@dataclass(frozen=True)
class Grid:
    """Axis-aligned raster grid in a projected CRS (EPSG code, metres)."""

    epsg: int
    xmin: float
    ymin: float
    xmax: float
    ymax: float
    res: float

    @property
    def width(self) -> int:
        return int(round((self.xmax - self.xmin) / self.res))

    @property
    def height(self) -> int:
        return int(round((self.ymax - self.ymin) / self.res))

    @property
    def bounds(self) -> tuple[float, float, float, float]:
        return (self.xmin, self.ymin, self.xmax, self.ymax)

    @property
    def srs(self) -> str:
        return f"EPSG:{self.epsg}"

    def signature(self) -> dict:
        return asdict(self)

    def geo_bbox(self, densify: int = 20) -> GeoBBox:
        return projected_to_geo_bbox(self.epsg, self.bounds, densify)

    @property
    def cells(self) -> int:
        return self.width * self.height

    def cells_at(self, res: float) -> int:
        """Cells covering this extent at ``res`` (edges rounded outward, as when the grid is snapped)."""
        return math.ceil((self.xmax - self.xmin) / res) * math.ceil((self.ymax - self.ymin) / res)


def _millions(cells: int) -> str:
    return f"{cells / 1e6:,.0f}" if cells >= 10_000_000 else f"{cells / 1e6:,.1f}"


def res_for_cells(grid: Grid, limit: int, step: float) -> float:
    """Smallest cell size, a multiple of ``step``, that keeps ``grid``'s extent at or below ``limit`` cells."""
    res = max(step, math.ceil(grid.res * math.sqrt(grid.cells / limit) / step) * step)
    while grid.cells_at(res) > limit:
        res += step
    return res


def grid_advice(grid: Grid, soft: int, step: float, max_res: float, what: str) -> tuple[str, str]:
    """(level, message) for the size of ``grid``: level is "" (fine), "large" (above ``soft``) or "too_large".

    The message suggests the cell size that brings the grid under ``soft`` (``what`` is "regional" or "focus").
    """
    cells = grid.cells
    if cells <= soft:
        return "", ""
    level = "too_large" if cells > MAX_GRID_CELLS else "large"
    text = (
        f"The {what} grid is {grid.width:,} x {grid.height:,} at {grid.res:g} m: {_millions(cells)} million cells, "
        f"about {cells * 4 / 1e9:,.1f} GB per float32 layer"
    )
    if level == "too_large":
        text += f", above the limit of {_millions(MAX_GRID_CELLS)} million cells."
    else:
        text += f" (above {_millions(soft)} million cells: slow downloads and several GB of memory)."
    area = "outline" if what == "focus" else "boundary"
    res = res_for_cells(grid, soft, step)
    if res <= max_res:
        text += f" A {what} cell size of {res:g} m gives about {_millions(grid.cells_at(res))} million cells."
    else:
        top = grid.cells_at(max_res)
        if top <= MAX_GRID_CELLS:
            text += (
                f" The largest {what} cell size, {max_res:g} m, gives about {_millions(top)} million cells; "
                f"a smaller {area} would be faster."
            )
        else:
            text += (
                f" Even the largest {what} cell size, {max_res:g} m, gives {_millions(top)} million cells: "
                f"draw a smaller {area}."
            )
    return level, text


def check_grid(grid: Grid, soft: int, step: float, max_res: float, what: str, allow_large: bool) -> str:
    """Raise ``GridTooLargeError`` above ``MAX_GRID_CELLS`` unless ``allow_large``; else return the warning (or "")."""
    level, text = grid_advice(grid, soft, step, max_res, what)
    if level == "too_large" and not allow_large:
        raise GridTooLargeError(
            f"{text} Nothing was downloaded. To run anyway use --allow-large-grid (headless) or allow_large_grid=True."
        )
    return text


def utm_epsg(lat: float, lon: float) -> int:
    """WGS 84 / UTM EPSG code for a point, including the Norway and Svalbard exceptions."""
    lon = ((lon + 180.0) % 360.0) - 180.0
    zone = int((lon + 180.0) // 6.0) + 1
    if 56.0 <= lat < 64.0 and 3.0 <= lon < 12.0:
        zone = 32
    if 72.0 <= lat < 84.0:
        if 0.0 <= lon < 9.0:
            zone = 31
        elif 9.0 <= lon < 21.0:
            zone = 33
        elif 21.0 <= lon < 33.0:
            zone = 35
        elif 33.0 <= lon < 42.0:
            zone = 37
    zone = min(max(zone, 1), 60)
    return (32600 if lat >= 0.0 else 32700) + zone


def _srs(epsg: int) -> osr.SpatialReference:
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg)
    srs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    return srs


def transformer(src_epsg: int, dst_epsg: int) -> osr.CoordinateTransformation:
    return osr.CoordinateTransformation(_srs(src_epsg), _srs(dst_epsg))


def lonlat_to_projected(epsg: int, lon: float, lat: float) -> tuple[float, float]:
    x, y, _ = transformer(4326, epsg).TransformPoint(lon, lat)
    return x, y


def projected_to_geo_bbox(epsg: int, bounds: tuple[float, float, float, float], densify: int = 20) -> GeoBBox:
    xmin, ymin, xmax, ymax = bounds
    ct = transformer(epsg, 4326)
    lons: list[float] = []
    lats: list[float] = []
    for i in range(densify + 1):
        f = i / densify
        for x, y in (
            (xmin + f * (xmax - xmin), ymin),
            (xmin + f * (xmax - xmin), ymax),
            (xmin, ymin + f * (ymax - ymin)),
            (xmax, ymin + f * (ymax - ymin)),
        ):
            lon, lat, _ = ct.TransformPoint(x, y)
            lons.append(lon)
            lats.append(lat)
    return GeoBBox(min(lons), min(lats), max(lons), max(lats))


def centred_grid(epsg: int, lat: float, lon: float, half_width_m: float, res: float, snap: float | None = None) -> Grid:
    """Square grid centred on (lat, lon); edges snapped outward to multiples of ``snap`` (default ``res``)."""
    snap = snap or res
    cx, cy = lonlat_to_projected(epsg, lon, lat)
    xmin = math.floor((cx - half_width_m) / snap) * snap
    ymin = math.floor((cy - half_width_m) / snap) * snap
    xmax = math.ceil((cx + half_width_m) / snap) * snap
    ymax = math.ceil((cy + half_width_m) / snap) * snap
    return Grid(epsg, xmin, ymin, xmax, ymax, res)


def pixel_size_m(geotransform, srs: osr.SpatialReference | None, centre_lat: float | None = None) -> float:
    """Approximate pixel size in metres for a raster (degrees converted at the raster centre)."""
    size = abs(geotransform[1])
    if srs is not None and srs.IsGeographic():
        lat = centre_lat if centre_lat is not None else 0.0
        size *= 111_320.0 * max(math.cos(math.radians(lat)), 0.01)
    elif srs is not None and srs.IsProjected():
        size *= srs.GetLinearUnits() or 1.0
    return size
