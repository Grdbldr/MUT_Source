"""GDAL helpers: warp sources onto a model grid, read/write arrays, simple caching."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Callable

import numpy as np
from osgeo import gdal

from .geo import Grid

NODATA = -9999.0
CREATION = ["COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES", "BIGTIFF=IF_SAFER"]
# Target cells per cancelable strip so Cancel can stop within a few seconds during remote /vsicurl/ warps.
_CANCEL_STRIP_CELLS = 2_000_000
_WARP_MEMORY_BYTES = 64 << 20


def vsicurl(url: str) -> str:
    return url if url.startswith("/vsi") else f"/vsicurl/{url}"


def _gdal_callback(is_canceled: Callable[[], bool] | None):
    """GDAL progress callback that aborts the operation (returns 0) once ``is_canceled()`` is true."""
    if is_canceled is None:
        return None
    return lambda _complete, _message, _data: 0 if is_canceled() else 1


def _run_cancelable(operation: Callable[[], object], out: Path | None, is_canceled: Callable[[], bool] | None):
    """Run a GDAL ``operation`` writing ``out``; on cancel or failure delete ``out`` (InterruptedError if canceled)."""
    try:
        ds = operation()
    except Exception:
        if out is not None:
            out.unlink(missing_ok=True)
        if is_canceled is not None and is_canceled():
            raise InterruptedError("Run canceled") from None
        raise
    if is_canceled is not None and is_canceled():
        ds = None
        if out is not None:
            out.unlink(missing_ok=True)
        raise InterruptedError("Run canceled")
    return ds


def _open_sources(sources: list[str], is_canceled: Callable[[], bool] | None):
    datasets = []
    for name in sources:
        # gdal.Warp would open every source (an HTTP request each for /vsicurl/ tiles) before its first callback.
        if is_canceled is not None and is_canceled():
            raise InterruptedError("Run canceled")
        datasets.append(gdal.Open(name))
    return datasets


def _warp_options(
    grid: Grid,
    resample: str,
    src_nodata: float | None,
    src_srs: str | None,
    is_canceled: Callable[[], bool] | None,
    *,
    output_bounds: tuple[float, float, float, float] | None = None,
    width: int | None = None,
    height: int | None = None,
    fmt: str = "GTiff",
    multithread: bool = False,
):
    kwargs: dict = {
        "format": fmt,
        "outputBounds": output_bounds or grid.bounds,
        "xRes": grid.res,
        "yRes": grid.res,
        "dstSRS": grid.srs,
        "srcSRS": src_srs,
        "resampleAlg": resample,
        "srcNodata": src_nodata,
        "dstNodata": NODATA,
        "outputType": gdal.GDT_Float32,
        "multithread": multithread,
        "warpMemoryLimit": _WARP_MEMORY_BYTES,
        "targetAlignedPixels": False,
        "callback": _gdal_callback(is_canceled),
    }
    if fmt == "GTiff":
        kwargs["creationOptions"] = CREATION
    if width is not None:
        kwargs["width"] = width
    if height is not None:
        kwargs["height"] = height
    return gdal.WarpOptions(**kwargs)


def warp_to_grid(
    sources: list[str] | str,
    grid: Grid,
    dst: Path,
    resample: str = "bilinear",
    src_nodata: float | None = None,
    src_srs: str | None = None,
    is_canceled: Callable[[], bool] | None = None,
) -> Path:
    """Reproject and mosaic ``sources`` onto ``grid``; later sources overwrite earlier ones.

    When ``is_canceled()`` becomes true the warp stops, the partial file is removed and InterruptedError is raised.
    Cancelable warps run in horizontal strips (and without GDAL multithread) so a remote DEM read can stop
    within a few seconds instead of waiting for a single multi-minute ``gdal.Warp`` to finish.
    """
    if isinstance(sources, str):
        sources = [sources]
    dst.parent.mkdir(parents=True, exist_ok=True)
    tmp = dst.with_name(dst.stem + ".tmp.tif")
    datasets = _open_sources(sources, is_canceled)
    try:
        if is_canceled is not None and grid.height > 1:
            _warp_cancelable_strips(datasets, grid, tmp, resample, src_nodata, src_srs, is_canceled)
        else:
            options = _warp_options(
                grid, resample, src_nodata, src_srs, is_canceled, fmt="GTiff", multithread=is_canceled is None
            )
            ds = _run_cancelable(lambda: gdal.Warp(str(tmp), datasets, options=options), tmp, is_canceled)
            if ds is None:
                tmp.unlink(missing_ok=True)
                raise RuntimeError(f"gdal.Warp produced no output for {dst.name}")
            ds = None
    finally:
        datasets = None
    tmp.replace(dst)
    return dst


def _warp_cancelable_strips(
    datasets,
    grid: Grid,
    tmp: Path,
    resample: str,
    src_nodata: float | None,
    src_srs: str | None,
    is_canceled: Callable[[], bool],
) -> None:
    """Warp into ``tmp`` one horizontal strip at a time, checking cancel between strips."""
    strip_rows = max(32, min(grid.height, max(1, _CANCEL_STRIP_CELLS // max(1, grid.width))))
    driver = gdal.GetDriverByName("GTiff")
    out = driver.Create(str(tmp), grid.width, grid.height, 1, gdal.GDT_Float32, CREATION)
    if out is None:
        raise RuntimeError(f"Could not create {tmp.name}")
    out.SetGeoTransform((grid.xmin, grid.res, 0.0, grid.ymax, 0.0, -grid.res))
    out.SetProjection(_wkt(grid.epsg))
    band = out.GetRasterBand(1)
    band.SetNoDataValue(NODATA)
    band.Fill(NODATA)
    try:
        for y0 in range(0, grid.height, strip_rows):
            if is_canceled():
                raise InterruptedError("Run canceled")
            rows = min(strip_rows, grid.height - y0)
            ymax = grid.ymax - y0 * grid.res
            ymin = ymax - rows * grid.res
            options = _warp_options(
                grid,
                resample,
                src_nodata,
                src_srs,
                is_canceled,
                output_bounds=(grid.xmin, ymin, grid.xmax, ymax),
                width=grid.width,
                height=rows,
                fmt="MEM",
                multithread=False,
            )
            mem = _run_cancelable(lambda opts=options: gdal.Warp("", datasets, options=opts), None, is_canceled)
            if mem is None:
                raise RuntimeError(f"gdal.Warp produced no output for strip at row {y0}")
            arr = mem.ReadAsArray()
            mem = None
            if arr is None or arr.shape != (rows, grid.width):
                raise RuntimeError(f"gdal.Warp strip shape mismatch at row {y0}")
            band.WriteArray(arr, 0, y0)
    except Exception:
        out = None
        band = None
        tmp.unlink(missing_ok=True)
        raise
    band.ComputeStatistics(False)
    out = None


def read_array(path: Path) -> np.ndarray:
    """Read band 1 as float64 with nodata converted to NaN."""
    ds = gdal.Open(str(path))
    band = ds.GetRasterBand(1)
    arr = band.ReadAsArray().astype("float64")
    nodata = band.GetNoDataValue()
    if nodata is not None:
        arr[arr == nodata] = np.nan
    arr[np.abs(arr) > 1e20] = np.nan
    return arr


def write_array(path: Path, arr: np.ndarray, grid: Grid, description: str = "") -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    driver = gdal.GetDriverByName("GTiff")
    ds = driver.Create(str(path), grid.width, grid.height, 1, gdal.GDT_Float32, CREATION)
    ds.SetGeoTransform((grid.xmin, grid.res, 0.0, grid.ymax, 0.0, -grid.res))
    ds.SetProjection(_wkt(grid.epsg))
    band = ds.GetRasterBand(1)
    band.SetNoDataValue(NODATA)
    if description:
        band.SetDescription(description)
    out = np.where(np.isfinite(arr), arr, NODATA).astype("float32")
    band.WriteArray(out)
    band.ComputeStatistics(False)
    ds = None
    return path


def _wkt(epsg: int) -> str:
    from osgeo import osr

    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg)
    return srs.ExportToWkt()


def fill_small_gaps(
    arr: np.ndarray, grid: Grid, max_distance_px: int, is_canceled: Callable[[], bool] | None = None
) -> np.ndarray:
    """Interpolate NaN gaps up to ``max_distance_px`` pixels wide (GDAL FillNodata)."""
    if not np.isnan(arr).any() or np.isnan(arr).all():
        return arr
    mem = gdal.GetDriverByName("MEM").Create("", grid.width, grid.height, 1, gdal.GDT_Float64)
    band = mem.GetRasterBand(1)
    band.SetNoDataValue(NODATA)
    band.WriteArray(np.where(np.isfinite(arr), arr, NODATA))
    callback = _gdal_callback(is_canceled)
    _run_cancelable(
        lambda: gdal.FillNodata(band, None, maxSearchDist=max_distance_px, smoothingIterations=0, callback=callback),
        None,
        is_canceled,
    )
    out = band.ReadAsArray().astype("float64")
    out[out == NODATA] = np.nan
    return out


def distance_inside(valid: np.ndarray, grid: Grid) -> np.ndarray:
    """Distance (metres) from each valid pixel to the nearest invalid pixel or grid edge."""
    padded = np.zeros((valid.shape[0] + 2, valid.shape[1] + 2), dtype="uint8")
    padded[1:-1, 1:-1] = np.where(valid, 0, 1)
    drv = gdal.GetDriverByName("MEM")
    src = drv.Create("", padded.shape[1], padded.shape[0], 1, gdal.GDT_Byte)
    src.SetGeoTransform((grid.xmin - grid.res, grid.res, 0.0, grid.ymax + grid.res, 0.0, -grid.res))
    src.GetRasterBand(1).WriteArray(padded)
    dst = drv.Create("", padded.shape[1], padded.shape[0], 1, gdal.GDT_Float32)
    dst.SetGeoTransform(src.GetGeoTransform())
    gdal.ComputeProximity(src.GetRasterBand(1), dst.GetRasterBand(1), ["VALUES=1", "DISTUNITS=GEO"])
    return dst.GetRasterBand(1).ReadAsArray()[1:-1, 1:-1].astype("float64")


def polygon_mask(vector: Path, grid: Grid, all_touched: bool = True) -> np.ndarray:
    """True for cells touched by (or, with ``all_touched=False``, centred in) the features in ``vector``."""
    mem = gdal.GetDriverByName("MEM").Create("", grid.width, grid.height, 1, gdal.GDT_Byte)
    mem.SetGeoTransform((grid.xmin, grid.res, 0.0, grid.ymax, 0.0, -grid.res))
    mem.SetProjection(_wkt(grid.epsg))
    src = gdal.OpenEx(str(vector), gdal.OF_VECTOR)
    gdal.Rasterize(mem, src, burnValues=[1], allTouched=all_touched)
    return mem.GetRasterBand(1).ReadAsArray().astype(bool)


def hillshade(src: Path, dst: Path, is_canceled: Callable[[], bool] | None = None) -> Path:
    """Multidirectional hillshade (byte GeoTIFF) for display under the elevation colours."""
    options = gdal.DEMProcessingOptions(
        multiDirectional=True,
        computeEdges=True,
        creationOptions=["COMPRESS=DEFLATE", "TILED=YES"],
        callback=_gdal_callback(is_canceled),
    )
    _run_cancelable(lambda: gdal.DEMProcessing(str(dst), str(src), "hillshade", options=options), dst, is_canceled)
    return dst


def cache_ok(path: Path, signature: dict, refresh: bool) -> bool:
    meta = path.with_suffix(path.suffix + ".json")
    if refresh or not path.exists() or not meta.exists():
        return False
    try:
        return json.loads(meta.read_text(encoding="utf-8")) == signature
    except (OSError, ValueError):
        return False


def clear_cache_meta(path: Path) -> None:
    """Invalidate the cache of ``path`` before it is rebuilt, so an interrupted rebuild is never reused."""
    path.with_suffix(path.suffix + ".json").unlink(missing_ok=True)


def write_cache_meta(path: Path, signature: dict) -> None:
    path.with_suffix(path.suffix + ".json").write_text(json.dumps(signature, indent=1), encoding="utf-8")
