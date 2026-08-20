"""Write per-domain Tecplot layouts (Mesh, Variables, Scatter, Results, Observations, VolumeBudget)."""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from pathlib import Path

from inventory import ArtifactInventory
from parse_mut import MutBuildInfo
from parse_usg import UsgRunInfo
from tecplot_io import (
    ascii_var_absmax,
    is_binary_tecplot,
    mm_per_year_factor,
    nice_abs_limit,
    read_point_xyz,
    read_tecplot_header,
    symmetric_contour_levels,
    var_index,
    var_name,
)


@dataclass
class LayoutFile:
    section_id: str
    page_name: str
    path: Path
    frames: list[str]
    symmetric_legend: bool = False


def _tecplot_path(path: Path) -> str:
    """Absolute POSIX path so Tecplot 2018 does not resolve ../../ against the wrong cwd."""
    return str(path.resolve()).replace("\\", "/")


def _quote(path: str) -> str:
    return f'"{path}"'


def _paper(page_name: str) -> str:
    return f"""$!Page
  Name = '{_escape(page_name)}'
  PaperAttributes
    {{
    BackgroundColor = White
    IsTransparent = No
    OrientPortrait = No
    ShowGrid = No
    ShowRuler = No
    ShowPaper = Yes
    PaperSize = Letter
    }}
$!GlobalFrame
  FrameHeaderHeight = 0.12
  FrameHeaderFormat = '&(FRAMENAME)'
"""


def _escape(text: str) -> str:
    return text.replace("'", "").replace("(", "").replace(")", "")


def _frame_pos(x: float, y: float, w: float, h: float) -> str:
    return f"""$!FrameLayout
  ShowHeader = Yes
  HeaderColor = Black
  XYPos
    {{
    X = {x:.4f}
    Y = {y:.4f}
    }}
  Width = {w:.4f}
  Height = {h:.4f}
"""


def _frame_layout(
    x: float,
    y: float,
    w: float,
    h: float,
    transparent: bool = True,
    show_header: bool = False,
) -> str:
    return f"""$!FrameLayout
  ShowHeader = {"Yes" if show_header else "No"}
  IsTransparent = {"Yes" if transparent else "No"}
  HeaderColor = Black
  XYPos
    {{
    X = {x:.4f}
    Y = {y:.4f}
    }}
  Width = {w:.4f}
  Height = {h:.4f}
"""


def _linking(link_view: bool, link_size: bool = True) -> str:
    bits: list[str] = []
    if link_size:
        bits.append("    LinkFrameSizeAndPosition = Yes")
    if link_view:
        bits.append("    Link3DView = Yes")
    if not bits:
        return ""
    inner = "\n".join(bits) + "\n"
    return f"""$!Linking
  BetweenFrames
    {{
{inner}    }}
"""


def _is_xyz(name: str) -> bool:
    return name.strip().lower() in {"x", "y", "z"}


def _is_zone_name(name: str) -> bool:
    return "zone" in name.lower()


def _non_xyz(variables: list[str]) -> list[str]:
    return [name for name in variables if name and not _is_xyz(name)]


_OBS_KINDS = ("HEAD", "SATURATION", "DEPTH")


def _obs_series(variables: list[str]) -> list[str]:
    return [var for var in variables if var and not var.upper().startswith("TOTAL TIME")]


def _obs_site_and_kind(var: str) -> tuple[str, str]:
    """Split 'Mountain_Top Head' into ('Mountain_Top', 'HEAD')."""
    text = var.strip()
    upper = text.upper()
    for kind in _OBS_KINDS:
        suffix = f" {kind}"
        if upper.endswith(suffix):
            return text[: -len(suffix)].strip(), kind
    return text, ""


def _pretty_units(units: str) -> str:
    text = (units or "").strip()
    if not text:
        return ""
    return text.lower()


def _qty_title(name: str, units: str) -> str:
    """Axis title with auxiliary units in parentheses, e.g. Head (meters)."""
    suffix = _pretty_units(units)
    if suffix:
        return f"{name} ({suffix})"
    return name


def _obs_y_title(kind: str, length_units: str) -> str:
    if kind == "HEAD":
        return _qty_title("Head", length_units or "meters")
    if kind == "DEPTH":
        return _qty_title("Depth", length_units or "meters")
    if kind == "SATURATION":
        return "Saturation (-)"
    return kind.title() if kind else "Value"


def _obs_two_row_slots(n_cols: int) -> tuple[list[tuple[float, float, float, float]], list[tuple[float, float, float, float]]]:
    """Square frames: two rows fill the Letter page height; extra columns run off to the right."""
    if n_cols <= 0:
        return [], []
    page_h = 8.5
    margin = 0.12
    gap = 0.08
    size = (page_h - 2.0 * margin - gap) / 2.0
    left = margin
    bottom = margin
    top: list[tuple[float, float, float, float]] = []
    bot: list[tuple[float, float, float, float]] = []
    for col in range(n_cols):
        x = left + col * (size + gap)
        top.append((x, bottom + size + gap, size, size))
        bot.append((x, bottom, size, size))
    return top, bot


def _obs_path(inv: ArtifactInventory, domain: str) -> Path | None:
    path = inv.obs_tecplot.get(domain) or inv.obs_tecplot.get(domain.upper())
    if path:
        return path
    for key, found in inv.obs_tecplot.items():
        if key.upper() == domain.upper():
            return found
    return None


def _overlap_slot() -> tuple[float, float, float, float]:
    return (0.35, 0.30, 10.30, 7.60)


def _zone_labels(build: MutBuildInfo | None, domain: str) -> list[str]:
    names: dict[int, str] = {}
    if build is not None:
        for assign in build.assignments:
            if assign.domain.upper() != domain.upper() or not assign.zone:
                continue
            try:
                zone = int(float(assign.zone))
            except ValueError:
                continue
            names[zone] = assign.material_name or f"Zone {zone}"
    nmax = max(names) if names else 6
    nmax = max(nmax, 6)
    return [names.get(i, f"Zone {i}") for i in range(1, nmax + 1)]


def _write_custom_labels(path: Path, labels: list[str]) -> None:
    lines = ["CUSTOMLABELS\n"]
    for name in labels:
        safe = name.replace('"', "'")
        lines.append(f'"{safe}",\n')
    path.write_text("".join(lines), encoding="ascii")


def _fieldmap_flood() -> str:
    """Full FieldMap block as written by Tecplot 2018 (Abdul _build.lay)."""
    return """$!FieldMap  [1]
  Mesh
    {
    Color = Black
    }
  Contour
    {
    ContourType = Flood
    Color = Multi
    UseLightingEffect = Yes
    }
  Vector
    {
    Color = Black
    }
  Scatter
    {
    Color = Black
    }
  Shade
    {
    Color = Custom2
    }
  EdgeLayer
    {
    Show = Yes
    Color = Black
    LineThickness = 0.1
    }
  Points
    {
    PointsToPlot = SurfaceNodes
    }
  Surfaces
    {
    SurfacesToPlot = ExposedCellFaces
    IRange
      {
      Max = 1
      }
    }
  VolumeMode
    {
    VolumeObjectsToPlot
      {
      ShowIsosurfaces = No
      ShowSlices = No
      ShowStreamtraces = No
      }
    }
  Effects
    {
    LightingEffect = Gouraud
    }
"""


def _zone_flood(frame_name: str, var_num: int, n_zones: int, show_mesh: bool = True) -> str:
    n_levels = max(n_zones, 1)
    levels = "\n".join(f"{i + 0.5:g}" for i in range(1, n_levels + 1))
    mesh = "Yes" if show_mesh else "No"
    return f"""$!PlotType = Cartesian3D
$!FrameName = '{_escape(frame_name)}'
$!ActiveFieldMaps = [1]
{_threed_axis_block()}$!GlobalContour  1
  Var = {var_num}
  DefNumLevels = {n_levels}
{_contour_labels_format("Zone")}{_legend_top_left()}$!ContourLevels New
  ContourGroup = 1
  RawData
{n_levels}
{levels}
{_fieldmap_flood()}$!FieldLayers
  ShowContour = Yes
  ShowShade = Yes
  ShowMesh = {mesh}
  ShowEdge = No
  UseLightingEffect = Yes
$!View Fit
{_default_3d_finish()}
"""


def _create_frame(x: float, y: float, w: float, h: float) -> str:
    return f"""$!CreateNewFrame
  XYPos
    {{
    X = {x:.4f}
    Y = {y:.4f}
    }}
  Width = {w:.4f}
  Height = {h:.4f}
"""


def _dataset_paths(path: Path) -> list[str]:
    """Single Tecplot dataset file (SZPLT or ASCII)."""
    return [_tecplot_path(path)]


def _read_3d(rel: str | list[str], append: bool = False, reset_style: bool = True) -> str:
    paths = [rel] if isinstance(rel, str) else list(rel)
    extra: list[str] = []
    if append:
        extra.append("  ReadDataOption = Append")
        extra.append("  ResetStyle = No")
    elif not reset_style:
        extra.append("  ResetStyle = No")
    first = paths[0].lower() if paths else ""
    extra_txt = ("\n".join(extra) + "\n") if extra else ""
    if first.endswith(".szplt"):
        # 2018's default ReadDataSet is the ASCII/PLT loader. A bare .szplt path
        # is parsed as ASCII (#!SZPLT is a comment) → "Unexpected End of File
        # in Datafile header". The Subzone loader needs STANDARDSYNTAX.
        n = len(paths)
        quoted_files = " ".join(_quote(p) for p in paths)
        return f"""$!ReadDataSet  '"STANDARDSYNTAX" "1.0" "FILELIST_DATAFILES" "{n}" {quoted_files}'
  DataSetReader = 'Tecplot Subzone Data Loader'
  InitialPlotType = Cartesian3D
{extra_txt}  AssignStrandIDs = Yes
"""
    quoted = " ".join(_quote(p) for p in paths)
    if not first.endswith(".plt"):
        extra.append("  VarLoadMode = ByName")
        extra_txt = ("\n".join(extra) + "\n") if extra else ""
    return f"""$!ReadDataSet  '{quoted}'
  InitialPlotType = Cartesian3D
  IncludeText = No
  IncludeGeom = No
{extra_txt}  AssignStrandIDs = Yes
"""


def _trimmed_ascii_for_layout(src: Path, dest_dir: Path) -> Path:
    """Tecplot 2018 ASCII header buffer dies on Fortran-padded 4000-char records."""
    if not src.is_file() or is_binary_tecplot(src):
        return src
    long_line = False
    with src.open("r", encoding="utf-8", errors="replace") as handle:
        for i, line in enumerate(handle):
            if len(line) > 2048:
                long_line = True
                break
            if i > 40:
                break
    if not long_line:
        return src
    dest = dest_dir / src.name
    dest.write_text(
        "".join(line.rstrip() + "\n" for line in src.open("r", encoding="utf-8", errors="replace")),
        encoding="utf-8",
        newline="\n",
    )
    return dest


def _read_xy(rel: str) -> str:
    return f"""$!ReadDataSet  '{_quote(rel)}'
  InitialPlotType = XYLine
  IncludeText = No
  IncludeGeom = No
  ResetStyle = Yes
  AssignStrandIDs = Yes
  VarLoadMode = ByName
"""


def _contour3d(frame_name: str, var_num: int | None, solution_time: float | None = None) -> str:
    bits = [
        "$!PlotType = Cartesian3D",
        f"$!FrameName = '{_escape(frame_name)}'",
        "$!ActiveFieldMaps = [1]",
        _threed_axis_block().rstrip(),
    ]
    if var_num:
        bits.extend(
            [
                "$!GlobalContour  1",
                f"  Var = {var_num}",
                "  DefNumLevels = 12",
            ]
        )
        bits.append(_contour_labels_format(frame_name).rstrip())
        bits.append(_legend_top_left().rstrip())
        bits.append(_fieldmap_flood().rstrip())
        bits.extend(
            [
                "$!FieldLayers",
                "  ShowContour = Yes",
                "  ShowShade = Yes",
                "  ShowMesh = No",
                "  ShowEdge = No",
                "  UseLightingEffect = Yes",
            ]
        )
    else:
        bits.extend(
            [
                "$!FieldLayers",
                "  ShowMesh = Yes",
                "  ShowEdge = Yes",
                "  ShowShade = Yes",
                "  ShowContour = No",
                "  UseLightingEffect = Yes",
            ]
        )
    if solution_time is not None:
        bits.append(_solution_time(solution_time).rstrip())
    bits.append("$!View Fit")
    bits.append(_default_3d_finish().rstrip())
    return "\n".join(bits) + "\n"


def _fieldmap_scatter(color: str, frame_size: float = 1.0) -> str:
    """Scatter-only FieldMap. CELLS use a smaller sphere (0.5); point sets stay at 1.0."""
    return f"""$!FieldMap  [1]
  Mesh
    {{
    Color = Black
    }}
  Contour
    {{
    ContourType = Flood
    Color = Black
    UseLightingEffect = Yes
    }}
  Vector
    {{
    Color = Black
    }}
  Scatter
    {{
    Show = Yes
    SymbolShape
      {{
      GeomShape = Sphere
      }}
    Color = {color}
    FrameSize = {frame_size:g}
    }}
  Shade
    {{
    Color = Custom2
    }}
  EdgeLayer
    {{
    Show = No
    Color = Black
    LineThickness = 0.1
    }}
  Points
    {{
    PointsToPlot = SurfaceNodes
    }}
  Surfaces
    {{
    SurfacesToPlot = ExposedCellFaces
    IRange
      {{
      Max = 1
      }}
    }}
  VolumeMode
    {{
    VolumeObjectsToPlot
      {{
      ShowIsosurfaces = No
      ShowSlices = No
      ShowStreamtraces = No
      }}
    }}
  Effects
    {{
    LightingEffect = Gouraud
    }}
"""


def _scatter_legend(x: float, y: float) -> str:
    """Scatter-symbol legend. TopLeft so name length does not shift the sphere."""
    return f"""  Legend
    {{
    Show = Yes
    AnchorAlignment = TopLeft
    TextShape
      {{
      FontFamily = 'Helvetica'
      IsBold = Yes
      SizeUnits = Point
      Height = 12
      }}
    Box
      {{
      BoxType = None
      }}
    XYPos
      {{
      X = {x:.3f}
      Y = {y:.3f}
      }}
    }}
"""


def _scatter_legend_xy(frame_size: float, top_index: int) -> tuple[float, float]:
    """Top-left of the frame; shared X so size-1 symbols stack on one vertical line.

    Half-size CELLS spheres get a tiny right nudge so their centres match size-1.
    (The old TopRight-era +4 FrameSize shift pushed CELLS well to the right.)
    """
    x = 2.0 + 0.5 * (1.0 - frame_size)
    y = 98.0 - top_index * 4.0
    return x, y


def _scatter3d(
    frame_name: str,
    color: str = "Red",
    frame_size: float = 1.0,
    legend_xy: tuple[float, float] | None = None,
    show_axes: bool = True,
) -> str:
    legend = (
        _scatter_legend(*legend_xy)
        if legend_xy
        else "  Legend\n    {\n    Show = No\n    }\n"
    )
    return (
        "$!PlotType = Cartesian3D\n"
        f"$!FrameName = '{_escape(frame_name)}'\n"
        "$!ActiveFieldMaps = [1]\n"
        f"{_threed_axis_block(show_axes=show_axes)}"
        "$!GlobalScatter\n"
        "  Var = 3\n"
        "  RelativeSize = 1\n"
        f"{legend}"
        f"{_fieldmap_scatter(color, frame_size)}"
        "$!FieldLayers\n"
        "  ShowMesh = No\n"
        "  ShowContour = No\n"
        "  ShowEdge = No\n"
        "  ShowShade = No\n"
        "  ShowScatter = Yes\n"
        "  UseLightingEffect = Yes\n"
        "$!View Fit\n"
        f"{_default_3d_finish()}"
    )


def _obs_names_for_domain(build: MutBuildInfo | None, domain: str) -> list[str]:
    if build is None:
        return []
    return [
        obs.name.strip()
        for obs in build.observations
        if obs.name and obs.domain.upper() == domain.upper()
    ]


def _scatter_obs_labels(
    points: list[tuple[float, float, float]], names: list[str]
) -> str:
    """Assigned observation names, anchored MidLeft so text sits to the right of each sphere."""
    bits: list[str] = []
    for (x, y, z), name in zip(points, names):
        if not name:
            continue
        bits.append(
            "$!AttachText\n"
            "  PositionCoordSys = Grid3D\n"
            "  AnchorPos\n"
            "    {\n"
            f"    X = {x:.10g}\n"
            f"    Y = {y:.10g}\n"
            f"    Z = {z:.10g}\n"
            "    }\n"
            "  TextShape\n"
            "    {\n"
            "    FontFamily = 'Helvetica'\n"
            "    IsBold = Yes\n"
            "    SizeUnits = Frame\n"
            "    Height = 1.5\n"
            "    }\n"
            "  Anchor = MidLeft\n"
            f"  Text = '  {_escape(name)}'\n"
        )
    return "".join(bits)


def _vectors3d(frame_name: str, u: int, v: int, w: int, mag: int | None, solution_time: float | None) -> str:
    extra = _solution_time(solution_time) if solution_time is not None else ""
    if mag:
        mag_line = (
            "$!GlobalContour  1\n"
            f"  Var = {mag}\n"
            f"{_contour_labels_format(frame_name)}"
            f"{_legend_top_left()}"
            f"{_fieldmap_flood()}"
            "$!FieldLayers\n"
            "  ShowMesh = No\n"
            "  ShowEdge = No\n"
            "  ShowShade = Yes\n"
            "  ShowContour = Yes\n"
            "  ShowVector = Yes\n"
            "  UseLightingEffect = Yes\n"
        )
    else:
        mag_line = (
            "$!FieldLayers\n"
            "  ShowMesh = No\n"
            "  ShowEdge = No\n"
            "  ShowShade = Yes\n"
            "  ShowContour = No\n"
            "  ShowVector = Yes\n"
            "  UseLightingEffect = Yes\n"
        )
    return f"""$!PlotType = Cartesian3D
$!FrameName = '{_escape(frame_name)}'
$!ActiveFieldMaps = [1]
$!GlobalThreeDVector
  UVar = {u}
  VVar = {v}
  WVar = {w}
{extra}{mag_line}{_threed_axis_block()}$!View Fit
{_default_3d_finish()}
"""


def _xy_line_legend() -> str:
    """Line-map legend (2018 $!GlobalLinePlot). Omit SizeUnits; 2018 rejects it here."""
    return """$!GlobalLinePlot
  Legend
    {
    Show = Yes
    ShowText = Yes
    AnchorAlignment = TopRight
    XYPos
      {
      X = 95
      Y = 95
      }
    Box
      {
      BoxType = Filled
      }
    TextShape
      {
      FontFamily = 'Helvetica'
      IsBold = Yes
      Height = 2
      }
    }
"""


def _xy_maps(
    frame_name: str,
    yvars: list[tuple[int, str]],
    x_title: str = "Time",
    y_title: str | None = None,
    show_legend: bool = False,
) -> str:
    colors = ["Red", "Green", "Blue", "Custom1", "Purple", "Black", "Yellow", "Cyan"]
    maps = ["$!DeleteLineMaps\n"]
    active = []
    for i, (yvar, _name) in enumerate(yvars, start=1):
        color = colors[(i - 1) % len(colors)]
        active.append(str(i))
        maps.append(
            f"""$!LineMap  [{i}]
  Name = '&DV&'
  Assign
    {{
    Zone = 1
    XAxisVar = 1
    YAxisVar = {yvar}
    }}
  Lines
    {{
    Color = {color}
    }}
"""
        )
    active_str = ",".join(active) if active else "1"
    legend = _xy_line_legend() if show_legend else ""
    return f"""$!PlotType = XYLine
$!FrameName = '{_escape(frame_name)}'
{"".join(maps)}$!ActiveLineMaps = [{active_str}]
$!XYLineAxis
{_xy_axis_fonts(x_title, y_title)}{legend}$!View Fit
"""


def _grid_slots(n: int) -> list[tuple[float, float, float, float]]:
    """Return (x, y, w, h) for n frames on a landscape Letter page (Y from bottom)."""
    if n <= 0:
        return []
    if n == 1:
        return [_overlap_slot()]
    if n == 2:
        cols, rows = 2, 1
    elif n <= 4:
        cols, rows = 2, 2
    elif n <= 6:
        cols, rows = 3, 2
    elif n <= 9:
        cols, rows = 3, 3
    elif n <= 12:
        cols, rows = 4, 3
    elif n <= 16:
        cols, rows = 4, 4
    else:
        cols = 5
        rows = (n + cols - 1) // cols
    left, bottom = 0.28, 0.22
    page_w, page_h = 11.0, 8.5
    gap = 0.08
    width = page_w - left - 0.22
    height = page_h - bottom - 0.32
    fw = (width - gap * (cols - 1)) / cols
    fh = (height - gap * (rows - 1)) / rows
    slots: list[tuple[float, float, float, float]] = []
    for i in range(n):
        row = i // cols
        col = i % cols
        x = left + col * (fw + gap)
        y = bottom + (rows - 1 - row) * (fh + gap)
        slots.append((x, y, fw, fh))
    return slots


def _contour_var(path: Path, *needles: str) -> int | None:
    header = read_tecplot_header(path)
    return var_index(header.get("variables") or [], *needles)


class _LayoutBuilder:
    def __init__(self, layouts_dir: Path):
        self.layouts_dir = layouts_dir
        self.chunks: list[str] = []
        self.frame_count_on_page = 0

    def start_file(self, page_name: str) -> None:
        self.chunks = [
            "#!MC 1410\n",
            "$!SetStyleBase Factory\n",
            _paper(page_name),
        ]
        self.frame_count_on_page = 0

    def add_background(self, slot: tuple[float, float, float, float]) -> None:
        x, y, w, h = slot
        self.chunks.append(_frame_layout(x, y, w, h, transparent=False))
        self.chunks.append("$!FrameName = '----- background -----'\n")
        self.chunks.append("$!PlotType = Sketch\n")
        self.chunks.append(_linking(link_view=False))
        self.frame_count_on_page += 1

    def add_zone_overlay(
        self,
        mesh: Path,
        frame_name: str,
        slot: tuple[float, float, float, float],
        zone_needles: tuple[str, ...],
        labels_file: Path | None,
        n_zones: int,
    ) -> None:
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(_frame_layout(x, y, w, h, transparent=False))
            self.chunks.append(_read_3d(_dataset_paths(mesh), append=False, reset_style=True))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(_read_3d(_dataset_paths(mesh), append=False, reset_style=False))
        var_num = _contour_var(mesh, *zone_needles) or 5
        self.chunks.append(_zone_flood(frame_name, var_num, n_zones))
        if self.frame_count_on_page > 0:
            self.chunks.append(_linking(link_view=True))
        self.frame_count_on_page += 1

    def add_3d(
        self,
        files: list[Path],
        frame_name: str,
        slot: tuple[float, float, float, float],
        contour_needles: tuple[str, ...] | None,
        solution_time: float | None = None,
        scatter: bool = False,
        vectors: tuple[str, str, str] | None = None,
        custom_labels: Path | None = None,
        n_zones: int = 1,
        link_view: bool = False,
        link_size: bool = False,
        var_num: int | None = None,
    ) -> None:
        if not files:
            return
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(_frame_pos(x, y, w, h))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(_frame_pos(x, y, w, h))
        self.chunks.append(_read_3d(_dataset_paths(files[0]), append=False))
        for extra in files[1:]:
            self.chunks.append(_read_3d(_dataset_paths(extra), append=True))
        if scatter:
            self.chunks.append(_scatter3d(frame_name))
        elif vectors:
            header = read_tecplot_header(files[0])
            variables = header.get("variables") or []
            u = var_index(variables, vectors[0])
            v = var_index(variables, vectors[1])
            wvar = var_index(variables, vectors[2])
            mag = var_index(variables, "GWF Head", "SWF Head", "CLN Head", "Head")
            if is_binary_tecplot(files[0]) and not (u and v and wvar):
                # FULL szplt: X Y Z, Head, Darcy Vx/Vy/Vz
                u, v, wvar, mag = 5, 6, 7, 4
            if u and v and wvar:
                self.chunks.append(_vectors3d(frame_name, u, v, wvar, mag, solution_time))
            else:
                self.chunks.append(_contour3d(frame_name, mag or 4, solution_time))
        elif _is_zone_name(frame_name) or (
            contour_needles and any(_is_zone_name(n) for n in contour_needles)
        ):
            zone_var = var_num or _contour_var(files[0], *(contour_needles or ("Zone",))) or 4
            self.chunks.append(_zone_flood(frame_name, zone_var, n_zones, show_mesh=False))
        else:
            if var_num is None:
                var_num = _contour_var(files[0], *(contour_needles or ()))
            if var_num is None and is_binary_tecplot(files[0]):
                name = files[0].name.lower()
                if "_posto" in name and ".velocity." not in name:
                    var_num = 8  # Head after XYZ + four static cell fields
                else:
                    var_num = 4
            self.chunks.append(_contour3d(frame_name, var_num, solution_time))
        if link_view or link_size:
            self.chunks.append(_linking(link_view=link_view, link_size=link_size))
        self.frame_count_on_page += 1

    def add_scatter(
        self,
        path: Path,
        frame_name: str,
        slot: tuple[float, float, float, float],
        color: str = "Red",
        frame_size: float = 1.0,
        legend_xy: tuple[float, float] | None = None,
        show_header: bool = False,
        show_axes: bool = True,
        transparent: bool = True,
        point_labels: list[str] | None = None,
    ) -> None:
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(
                _frame_layout(x, y, w, h, transparent=transparent, show_header=show_header)
            )
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(
                _frame_layout(x, y, w, h, transparent=transparent, show_header=show_header)
            )
        self.chunks.append(_read_3d(_tecplot_path(path), append=False))
        self.chunks.append(
            _scatter3d(
                frame_name,
                color=color,
                frame_size=frame_size,
                legend_xy=legend_xy,
                show_axes=show_axes,
            )
        )
        if point_labels:
            self.chunks.append(_scatter_obs_labels(read_point_xyz(path), point_labels))
        self.chunks.append(_linking(link_view=True, link_size=True))
        self.frame_count_on_page += 1

    def add_xy(
        self,
        path: Path,
        frame_name: str,
        slot: tuple[float, float, float, float],
        y_needles: list[tuple[str, ...]],
        x_title: str = "Time",
        y_title: str | None = None,
        show_legend: bool = False,
    ) -> None:
        path = _trimmed_ascii_for_layout(path, self.layouts_dir)
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(_frame_pos(x, y, w, h))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
        self.chunks.append(_read_xy(_tecplot_path(path)))
        header = read_tecplot_header(path)
        variables = header.get("variables") or []
        yvars: list[tuple[int, str]] = []
        for needles in y_needles:
            idx = var_index(variables, *needles)
            if idx:
                yvars.append((idx, needles[0]))
        self.chunks.append(
            _xy_maps(frame_name, yvars or [(2, "Y")], x_title, y_title, show_legend)
        )
        self.frame_count_on_page += 1

    def add_custom_3d(
        self,
        path: Path,
        style: str,
        slot: tuple[float, float, float, float] | None = None,
    ) -> None:
        slot = slot or _overlap_slot()
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(_frame_pos(x, y, w, h))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(_frame_pos(x, y, w, h))
        self.chunks.append(_read_3d(_dataset_paths(path), append=False))
        self.chunks.append(style)
        self.frame_count_on_page += 1

    def text(self) -> str:
        return "".join(self.chunks)


def _zone_needles(domain: str) -> tuple[str, ...]:
    return (f"{domain} Zone", "Zone")


def _budget_y_needles(variables: list[str]) -> list[tuple[str, ...]]:
    """Plot all budget rates except time and percent discrepancy (different scale)."""
    needles: list[tuple[str, ...]] = []
    for name in variables:
        if not name:
            continue
        upper = name.upper()
        if upper.startswith("TOTAL TIME"):
            continue
        if "PERCENT DISCREPANCY" in upper:
            continue
        needles.append((name,))
    return needles


def _extra_post_files(inv: ArtifactInventory, domain: str) -> list[tuple[str, Path]]:
    """Post files beyond the main domain FE dataset (not build-time BC scatter points)."""
    skip = {
        domain.upper(),
        f"{domain.upper()}.VELOCITY",
        f"{domain.upper()}_VELOCITY",
        "VOLUMEBUDGET",
    }
    # CELLS / CHD / RCH / … live on {domain}_scatter.lay (build or post point files).
    scatter_suffixes = {s.upper() for s in _SCATTER_ORDER}
    prefix = domain.upper() + "_"
    prefix_dot = domain.upper() + "."
    found: list[tuple[str, Path]] = []
    for key, path in inv.post_tecplot.items():
        ku = key.upper()
        if ku in skip:
            continue
        if ku.startswith(prefix):
            if ku[len(prefix) :] in scatter_suffixes:
                continue
        elif ku.startswith(prefix_dot):
            if ku[len(prefix_dot) :] in scatter_suffixes:
                continue
        else:
            continue
        found.append((key, path))
    return found


_SCATTER_ORDER = ("CELLS", "CHD", "RCH", "SWBC", "DRN", "WEL", "OBS_SCATTER")
_SCATTER_STYLE = {
    "CELLS": ("Custom26", 0.5),
    "CHD": ("Blue", 1.0),
    "RCH": ("Cyan", 1.0),
    "SWBC": ("Custom8", 1.0),
    "DRN": ("Custom11", 1.0),
    "WEL": ("Magenta", 1.0),
    "OBS_SCATTER": ("Red", 1.0),
}


def _scatter_files(inv: ArtifactInventory, domain: str) -> list[tuple[str, Path]]:
    """Build-time point files: _buildo.Modflow.{domain}_CELLS.tecplot.dat and kin."""
    prefix = domain.upper() + "_"
    found: list[tuple[str, Path]] = []
    for key, path in inv.build_tecplot.items():
        if key.upper().startswith(prefix):
            found.append((key, path))

    def sort_key(item: tuple[str, Path]) -> tuple[int, str]:
        suffix = item[0].upper()[len(prefix) :]
        try:
            return (_SCATTER_ORDER.index(suffix), item[0].upper())
        except ValueError:
            return (len(_SCATTER_ORDER), item[0].upper())

    found.sort(key=sort_key)
    return found


def _solution_time_label() -> str:
    """Solution time + zone TimeUnits, centred in the plot below the frame header.

    Frame % (49.833, 93.252) and HeadCenter (Top Center) keep the label in the
    viewport rather than under the header. Zone aux is MUT TimeUnits.
    """
    return """$!AttachText
  AnchorPos
    {
    X = 49.833
    Y = 93.252
    }
  TextShape
    {
    FontFamily = 'Helvetica'
    IsBold = Yes
    SizeUnits = Frame
    Height = 2.5
    }
  Box
    {
    BoxType = Filled
    }
  Anchor = HeadCenter
  Text = '&(solutiontime) &(AUXZONE[1]:TimeUnits)'
"""


def _solution_time(t: float | None) -> str:
    bits: list[str] = []
    if t is not None:
        bits.append(f"$!GlobalTime\n  SolutionTime = {t}\n")
    bits.append(_solution_time_label())
    return "".join(bits)


def _value_blanking(var_num: int | None) -> str:
    if not var_num:
        return ""
    return f"""$!Blanking
  Value
    {{
    Constraint 1
      {{
      Include = Yes
      VarA = {var_num}
      RelOp = LessThanOrEqual
      ValueCutoff = 0
      }}
    }}
"""


def _fieldmap_boundary(*, show_isosurfaces: bool = False, show_slices: bool = False) -> str:
    """Volume outline (boundary faces) so iso-surfaces / slices stay visible."""
    iso = "Yes" if show_isosurfaces else "No"
    slices = "Yes" if show_slices else "No"
    return f"""$!FieldMap  [1]
  Mesh
    {{
    Color = Black
    }}
  Contour
    {{
    ContourType = Flood
    Color = Multi
    UseLightingEffect = Yes
    }}
  Vector
    {{
    Color = Black
    }}
  Scatter
    {{
    Color = Black
    }}
  Shade
    {{
    Color = Custom2
    }}
  EdgeLayer
    {{
    Show = Yes
    Color = Black
    LineThickness = 0.1
    }}
  Points
    {{
    PointsToPlot = SurfaceNodes
    }}
  Surfaces
    {{
    SurfacesToPlot = BoundaryFaces
    IRange
      {{
      Max = 1
      }}
    }}
  VolumeMode
    {{
    VolumeObjectsToPlot
      {{
      ShowIsosurfaces = {iso}
      ShowSlices = {slices}
      ShowStreamtraces = No
      }}
    }}
  Effects
    {{
    LightingEffect = Gouraud
    UseTranslucency = Yes
    SurfaceTranslucency = 80
    }}
"""


def _fieldmap_flood_lines() -> str:
    """Surface flood + contour lines (infiltration map)."""
    return """$!FieldMap  [1]
  Mesh
    {
    Color = Black
    }
  Contour
    {
    ContourType = BothLinesAndFlood
    LineContourGroup = 1
    FloodColoring = Group1
    Color = Black
    UseLightingEffect = Yes
    }
  Vector
    {
    Color = Black
    }
  Scatter
    {
    Color = Black
    }
  Shade
    {
    Color = Custom2
    }
  EdgeLayer
    {
    Show = Yes
    Color = Black
    LineThickness = 0.1
    }
  Points
    {
    PointsToPlot = SurfaceNodes
    }
  Surfaces
    {
    SurfacesToPlot = ExposedCellFaces
    IRange
      {
      Max = 1
      }
    }
  Effects
    {
    LightingEffect = Gouraud
    }
"""


def _axis_scale_fact(z: float = 3.0) -> str:
    """Vertical exaggeration relative to X/Y (applied after View Fit)."""
    return f"""$!GlobalThreeD
  AxisScaleFact
    {{
    X = 1
    Y = 1
    Z = {z:g}
    }}
$!View Fit
"""


def _default_xyz_view() -> str:
    """Tecplot factory 3-D view (psi=60, theta=240), then fit the data."""
    return """$!ThreeDView
  PsiAngle = 60
  ThetaAngle = 240
  AlphaAngle = 0
$!View Fit
"""


def _default_3d_finish() -> str:
    """3× Z exaggeration and factory XYZ view; XY time-series frames omit this."""
    return _axis_scale_fact(3) + _default_xyz_view()


# Axis Details "8" is character height in points (1/72 in), not % of frame.
# "Offset from line (%)" is a separate percentage. Factory SizeUnits is Frame.
_AXIS_POINT_HEIGHT = 8


def _point_text_shape(indent: int = 8, bold: bool = True) -> str:
    pad = " " * indent
    bold_txt = "Yes" if bold else "No"
    return (
        f"{pad}TextShape\n"
        f"{pad}  {{\n"
        f"{pad}  FontFamily = 'Helvetica'\n"
        f"{pad}  IsBold = {bold_txt}\n"
        f"{pad}  SizeUnits = Point\n"
        f"{pad}  Height = {_AXIS_POINT_HEIGHT:g}\n"
        f"{pad}  }}\n"
    )


def _axis_detail_fonts() -> str:
    shape = _point_text_shape(8)
    return (
        "    TickLabel\n"
        "      {\n"
        f"{shape}"
        "      }\n"
        "    Title\n"
        "      {\n"
        f"{shape}"
        "      }\n"
    )


def _threed_axis_block(show_axes: bool = True) -> str:
    """3-D axes: 8 pt Helvetica bold titles and tick labels on X, Y, and Z.

    Overlay frames (scatter, linked mesh) pass show_axes=False so tick labels
    do not hide the back frame.
    """
    if show_axes:
        detail = _axis_detail_fonts()
        extras = ""
    else:
        detail = "    ShowAxis = No\n"
        extras = "  FrameAxis\n    {\n    Show = No\n    }\n"
    return (
        "$!ThreeDAxis\n"
        "  AspectRatioLimit = 25\n"
        "  BoxAspectRatioLimit = 25\n"
        f"{extras}"
        "  XDetail\n"
        "    {\n"
        f"{detail}"
        "    }\n"
        "  YDetail\n"
        "    {\n"
        f"{detail}"
        "    }\n"
        "  ZDetail\n"
        "    {\n"
        f"{detail}"
        "    }\n"
    )


def _xy_axis_fonts(x_title: str = "Time", y_title: str | None = None) -> str:
    """8 pt titles; viewport inset so titles stay inside small observation frames."""
    shape = _point_text_shape(8)
    x_text = (x_title or "Time").replace("'", "")
    y_title_inner = (
        "      TitleMode = UseText\n"
        f"      Text = '{y_title.replace(chr(39), '')}'\n"
        "      Offset = 6\n"
        if y_title
        else "      Offset = 6\n"
    )
    return (
        "  ViewportPosition\n"
        "    {\n"
        "    X1 = 18\n"
        "    Y1 = 16\n"
        "    X2 = 95\n"
        "    Y2 = 90\n"
        "    }\n"
        "  XDetail 1\n"
        "    {\n"
        "    TickLabel\n"
        "      {\n"
        f"{shape}"
        "      }\n"
        "    Title\n"
        "      {\n"
        f"{shape}"
        "      TitleMode = UseText\n"
        f"      Text = '{x_text}'\n"
        "      Offset = 5\n"
        "      }\n"
        "    CoordScale = Log\n"
        "    }\n"
        "  YDetail 1\n"
        "    {\n"
        "    TickLabel\n"
        "      {\n"
        f"{shape}"
        "      }\n"
        "    Title\n"
        "      {\n"
        f"{shape}"
        f"{y_title_inner}"
        "      }\n"
        "    }\n"
    )


def _num_format_kind(name: str | None = None, levels: list[float] | None = None) -> str:
    """integer | fixed2 | best — whole numbers only when the magnitude is readable."""
    if levels:
        finite = [v for v in levels if math.isfinite(v)]
        if finite:
            mag = max(abs(v) for v in finite)
            whole = all(abs(v - round(v)) < 1e-8 * max(1.0, mag) for v in finite)
            if mag < 1e-3 or mag > 1e6:
                return "best"
            if whole and mag >= 1.0:
                return "integer"
            if mag < 10:
                return "fixed2"
            return "best"
    lower = (name or "").lower()
    if any(tok in lower for tok in ("zone", "layer", "ibound")):
        return "integer"
    if "saturation" in lower:
        return "fixed2"
    return "best"


def _contour_labels_format(name: str | None = None, levels: list[float] | None = None) -> str:
    """Legend/contour number format. Integer when values are O(1)–O(1e6) wholes."""
    kind = _num_format_kind(name, levels)
    if kind == "integer":
        inner = (
            "        Formatting = Integer\n"
            "        ShowDecimalsOnWholeNumbers = No\n"
        )
    elif kind == "fixed2":
        inner = (
            "        Formatting = FixedFloat\n"
            "        Precision = 2\n"
            "        ShowDecimalsOnWholeNumbers = No\n"
        )
    else:
        inner = (
            "        Formatting = BestFloat\n"
            "        Precision = 2\n"
            "        ShowDecimalsOnWholeNumbers = No\n"
        )
    return (
        "  Labels\n"
        "    {\n"
        "    NumFormat\n"
        "      {\n"
        f"{inner}"
        "      }\n"
        "    }\n"
    )


def _legend_top_left() -> str:
    """Contour legend top-left, auto-resized to at most 1/4 of the frame height.

    Tecplot 360 EX 2018 uses HeaderTextShape (not Legend.Header). Legend text uses
    frame-percent height so 8 pt SizeUnits=Point is not misread as 8% of the frame.
    """
    return """  Legend
    {
    Show = Yes
    AnchorAlignment = TopLeft
    XYPos
      {
      X = 2
      Y = 88
      }
    AutoResize = Yes
    AutoSizeMaxLimit = 0.25
    OverlayBarGrid = No
    Box
      {
      BoxType = Filled
      }
    HeaderTextShape
      {
      FontFamily = 'Helvetica'
      IsBold = Yes
      SizeUnits = Frame
      Height = 2
      }
    NumberTextShape
      {
      FontFamily = 'Helvetica'
      IsBold = Yes
      SizeUnits = Frame
      Height = 2
      }
    }
"""


def _contour_levels_block(group: int, levels: list[float]) -> str:
    body = "\n".join(f"{value:g}" for value in levels)
    return f"""$!ContourLevels New
  ContourGroup = {group}
  RawData
{len(levels)}
{body}
"""


def _water_table_style(
    head_name: str,
    z_name: str,
    head_var: int,
    pressure_var: int,
    ibound_var: int | None,
    solution_time: float | None,
) -> str:
    return f"""$!PlotType = Cartesian3D
$!FrameName = 'GWF Water Table'
$!ActiveFieldMaps = [1]
{_threed_axis_block()}$!AlterData
  Equation = '{{Pressure Head}} = {{{head_name}}} - {{{z_name}}}'
{_solution_time(solution_time)}{_value_blanking(ibound_var)}$!GlobalContour  1
  Var = {head_var}
  ColorMapName = 'Sequential - Viridis'
  DefNumLevels = 12
{_contour_labels_format(head_name)}{_legend_top_left()}$!GlobalContour  2
  Var = {pressure_var}
  Legend
    {{
    Show = No
    }}
{_fieldmap_boundary(show_isosurfaces=True)}$!FieldLayers
  ShowMesh = No
  ShowContour = No
  ShowShade = Yes
  ShowEdge = Yes
  UseLightingEffect = Yes
  UseTranslucency = Yes
$!IsoSurfaceAttributes  1
  DefinitionContourGroup = 2
  IsoSurfaceSelection = OneSpecificValue
  Isovalue1 = 0
  Contour
    {{
    ContourType = Flood
    FloodColoring = Group1
    }}
  Effects
    {{
    LightingEffect = Gouraud
    }}
$!IsoSurfaceLayers
  Show = Yes
$!View Fit
{_default_3d_finish()}
"""


def _infiltration_style(
    flux_name: str,
    infil_var: int,
    factor: float,
    ibound_var: int | None,
    solution_time: float | None,
    levels: list[float],
    color_limit: float | None,
) -> str:
    # Always Banded (never Continuous); levels carry the zero-centred bands.
    cmap = """  ColorMapFilter
    {
    ColorMapDistribution = Banded
    }
"""
    return f"""$!PlotType = Cartesian3D
$!FrameName = 'SWF Infiltration'
$!ActiveFieldMaps = [1]
{_threed_axis_block()}$!AlterData
  Equation = '{{Infiltration [mm/year]}} = -{{{flux_name}}}*{factor:g}'
{_solution_time(solution_time)}{_value_blanking(ibound_var)}$!GlobalContour  1
  Var = {infil_var}
  ColorMapName = 'Diverging - Blue/Red'
  DefNumLevels = {len(levels)}
{_contour_labels_format("Infiltration", levels)}{_legend_top_left()}{cmap}{_contour_levels_block(1, levels)}{_fieldmap_flood_lines()}$!FieldLayers
  ShowMesh = No
  ShowContour = Yes
  ShowShade = Yes
  ShowEdge = Yes
  UseLightingEffect = Yes
$!View Fit
{_default_3d_finish()}
"""


def _gsf_xyz_range(path: Path) -> tuple[float, float, float, float, float, float] | None:
    """Min/max XYZ from GSF node coordinates (list-directed, possibly wrapped)."""
    try:
        raw = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    lines = [ln.strip() for ln in raw.splitlines() if ln.strip() and not ln.lstrip().startswith("#")]
    if len(lines) < 3:
        return None
    try:
        nnodes = int(float(lines[2].split()[0]))
    except (ValueError, IndexError):
        return None
    need = nnodes * 3
    if need <= 0:
        return None
    vals: list[float] = []
    for line in lines[3:]:
        for tok in line.split():
            try:
                vals.append(float(tok))
            except ValueError:
                continue
            if len(vals) >= need:
                break
        if len(vals) >= need:
            break
    if len(vals) < need:
        return None
    xs = vals[0:need:3]
    ys = vals[1:need:3]
    zs = vals[2:need:3]
    return min(xs), max(xs), min(ys), max(ys), min(zs), max(zs)


def _gwf_x_range(model_dir: Path) -> tuple[float, float] | None:
    matches = sorted(model_dir.glob("*.gwf.gsf"))
    for path in matches:
        rng = _gsf_xyz_range(path)
        if rng is not None:
            return rng[0], rng[1]
    return None


def _slice_x_positions(xmin: float, xmax: float) -> tuple[float, float, float]:
    """Primary at mid-X; start/end inset 10% so fence slices sit inside the volume."""
    span = xmax - xmin
    if not math.isfinite(span) or span <= 0:
        mid = xmin if math.isfinite(xmin) else 0.0
        return mid, mid, mid
    inset = 0.1 * span
    return 0.5 * (xmin + xmax), xmin + inset, xmax - inset


def _slice_position_block(x_primary: float | None, x_start: float | None, x_end: float | None) -> str:
    """Numeric X positions, or Tecplot |MINX|/|MAXX| when the GSF range is unknown."""
    n_slices = 5
    if x_primary is None or x_start is None or x_end is None:
        preamble = (
            "$!VarSet |XSPAN| = (|MAXX|-|MINX|)\n"
            "$!VarSet |XMID| = (|MINX|+0.5*|XSPAN|)\n"
            "$!VarSet |XSTART| = (|MINX|+0.1*|XSPAN|)\n"
            "$!VarSet |XEND| = (|MINX|+0.9*|XSPAN|)\n"
        )
        x_p, x_s, x_e = "|XMID|", "|XSTART|", "|XEND|"
    else:
        preamble = ""
        x_p, x_s, x_e = f"{x_primary:g}", f"{x_start:g}", f"{x_end:g}"
    return f"""{preamble}$!SliceAttributes  1
  SliceSurface = XPlanes
  ShowGroup = Yes
  ShowPrimarySlice = Yes
  PrimaryPosition
    {{
    X = {x_p}
    }}
  ShowStartEndSlice = Yes
  StartPosition
    {{
    X = {x_s}
    }}
  EndPosition
    {{
    X = {x_e}
    }}
  ShowIntermediateSlices = Yes
  NumIntermediateSlices = {n_slices}
"""


def _saturation_slices_style(
    sat_var: int,
    ibound_var: int | None,
    solution_time: float | None,
    x_primary: float | None = None,
    x_start: float | None = None,
    x_end: float | None = None,
) -> str:
    return f"""$!PlotType = Cartesian3D
$!FrameName = 'GWF Saturation Slices'
$!ActiveFieldMaps = [1]
{_threed_axis_block()}{_solution_time(solution_time)}{_value_blanking(ibound_var)}$!GlobalContour  1
  Var = {sat_var}
  ColorMapName = 'Sequential - Viridis'
  ColorMapFilter
    {{
    ReverseColorMap = Yes
    }}
  DefNumLevels = 12
{_contour_labels_format("GWF Saturation")}{_legend_top_left()}{_fieldmap_boundary(show_slices=True)}$!FieldLayers
  ShowMesh = No
  ShowContour = No
  ShowShade = Yes
  ShowEdge = Yes
  UseLightingEffect = Yes
  UseTranslucency = Yes
{_slice_position_block(x_primary, x_start, x_end)}  Contour
    {{
    ContourType = Flood
    FloodColoring = Group1
    }}
  Effects
    {{
    LightingEffect = Gouraud
    SurfaceTranslucency = 10
    }}
$!SliceLayers
  Show = Yes
$!View Fit
{_default_3d_finish()}
"""


def _swf_flux_name(variables: list[str]) -> str | None:
    areal = var_name(variables, "to GWF (areal flux)")
    if areal:
        return areal
    return var_name(variables, "SWF to GWF", "to GWF")


def apply_symmetric_legend_to_layout(path: Path, limit: float) -> None:
    """Rewrite Diverging Blue/Red CMin/CMax and group-1 contour levels to ±limit."""
    if not path.is_file() or limit <= 0 or not math.isfinite(limit):
        return
    text = path.read_text(encoding="utf-8", errors="replace")
    outer = nice_abs_limit(max(abs(limit), 1e-12))
    levels = symmetric_contour_levels(outer)
    filter_block = (
        "  ColorMapFilter\n"
        "    {\n"
        "    ColorMapDistribution = Banded\n"
        "    }\n"
    )
    if "ColorMapName = 'Diverging - Blue/Red'" not in text:
        return
    if "ColorMapFilter" in text:
        text = re.sub(
            r"ColorMapFilter\s*\{.*?\}",
            filter_block.strip(),
            text,
            count=1,
            flags=re.DOTALL,
        )
    else:
        text = text.replace(
            "ColorMapName = 'Diverging - Blue/Red'\n",
            "ColorMapName = 'Diverging - Blue/Red'\n" + filter_block,
            1,
        )
    levels_block = _contour_levels_block(1, levels)
    text, nsub = re.subn(
        r"\$!ContourLevels New\s*ContourGroup = 1\s*RawData\s*\d+(?:\s+[-\d.Ee+]+)+",
        levels_block.rstrip(),
        text,
        count=1,
    )
    if nsub == 0:
        text = text.replace(
            "ColorMapName = 'Diverging - Blue/Red'\n",
            "ColorMapName = 'Diverging - Blue/Red'\n" + levels_block,
            1,
        )
    path.write_text(text, encoding="utf-8")


_DERIVED_LAYOUTS = (
    "GWF_WaterTable.lay",
    "SWF_Infiltration.lay",
    "GWF_SaturationSlices.lay",
    "GWF_VolumeBudget.lay",
)


def _remove_stale_layouts(layouts_dir: Path, keep: set[str]) -> None:
    stale_prefixes = ("04_", "05_", "06_", "07_", "08_")
    for path in layouts_dir.glob("*.lay"):
        if path.name in keep:
            continue
        if (
            path.name == "model_documentation.lay"
            or path.name.startswith(stale_prefixes)
            or path.name in _DERIVED_LAYOUTS
        ):
            path.unlink()


def write_layouts(
    inv: ArtifactInventory,
    usg: UsgRunInfo,
    build: MutBuildInfo | None = None,
) -> list[LayoutFile]:
    inv.layouts_dir.mkdir(parents=True, exist_ok=True)
    produced: list[LayoutFile] = []
    keep: set[str] = set()

    def emit_builder(
        section_id: str,
        page_name: str,
        builder: _LayoutBuilder,
        frames: list[str],
        symmetric_legend: bool = False,
    ) -> None:
        if builder.frame_count_on_page == 0:
            return
        path = inv.layouts_dir / f"{section_id}.lay"
        path.write_text(builder.text(), encoding="utf-8")
        keep.add(path.name)
        produced.append(
            LayoutFile(section_id, page_name, path, frames, symmetric_legend=symmetric_legend)
        )

    def labels_for(domain: str) -> tuple[Path, int]:
        names = _zone_labels(build, domain)
        named_idx = [i for i, lab in enumerate(names, start=1) if not lab.startswith("Zone ")]
        n_zones = max(named_idx) if named_idx else 1
        path = inv.layouts_dir / f"CustomLabels_{domain}.dat"
        _write_custom_labels(path, names)
        keep.add(path.name)
        return path, n_zones

    def fill_mesh(b: _LayoutBuilder, domain: str, mesh: Path) -> list[str]:
        slot = _overlap_slot()
        labels_path, n_zones = labels_for(domain)
        b.add_zone_overlay(mesh, domain, slot, _zone_needles(domain), labels_path, n_zones)
        return [domain]

    def fill_variables(b: _LayoutBuilder, domain: str, mesh: Path) -> list[str]:
        header = read_tecplot_header(mesh)
        variables = _non_xyz(header.get("variables") or [])
        if not variables:
            if is_binary_tecplot(mesh):
                b.add_3d([mesh], f"{domain} variables", _overlap_slot(), None, var_num=4)
                return [f"{domain} variables"]
            return []
        labels_path, n_zones = labels_for(domain)
        slots = _grid_slots(len(variables))
        names: list[str] = []
        for slot, var_name in zip(slots, variables):
            idx = var_index(header.get("variables") or [], var_name)
            zone = _is_zone_name(var_name)
            b.add_3d(
                [mesh],
                var_name,
                slot,
                (var_name,),
                custom_labels=labels_path if zone else None,
                n_zones=n_zones,
                link_view=True,
                var_num=idx,
            )
            names.append(var_name)
        return names

    def fill_scatter(b: _LayoutBuilder, domain: str) -> list[str]:
        files = _scatter_files(inv, domain)
        if not files:
            return []
        slot = _overlap_slot()
        n = len(files)
        names: list[str] = []
        prefix = domain.upper() + "_"
        for i, (key, path) in enumerate(files):
            label = key.replace("_", " ")
            suffix = key.upper()[len(prefix) :] if key.upper().startswith(prefix) else key.upper()
            if suffix == "OBS_SCATTER":
                label = f"{domain} OBS"
            color, frame_size = _SCATTER_STYLE.get(suffix, ("Red", 1.0))
            top_index = n - 1 - i
            labels = _obs_names_for_domain(build, domain) if "OBS" in suffix else None
            b.add_scatter(
                path,
                label,
                slot,
                color=color,
                frame_size=frame_size,
                legend_xy=_scatter_legend_xy(frame_size, top_index),
                show_header=(i == n - 1),
                show_axes=(suffix == "CELLS"),
                transparent=(suffix != "CELLS"),
                point_labels=labels,
            )
            names.append(label)
        return names

    def fill_results(b: _LayoutBuilder, domain: str) -> list[str]:
        items: list[tuple] = []
        t_last = usg.last_times.get(domain)
        post = inv.post_file(domain)
        if post:
            header = read_tecplot_header(post)
            plot_vars = _non_xyz(header.get("variables") or [])
            if plot_vars:
                for var_name in plot_vars:
                    idx = var_index(header.get("variables") or [], var_name)
                    items.append(("contour", post, var_name, (var_name,), t_last, idx, None))
            elif is_binary_tecplot(post):
                items.append(("contour", post, f"{domain} Head", ("Head",), t_last, 8, None))
        vel = inv.post_file(f"{domain}.Velocity", f"{domain}_Velocity")
        if vel:
            items.append(
                ("vector", vel, f"{domain} Velocity", None, t_last, None, ("Darcy Vx", "Darcy Vy", "Darcy Vz"))
            )
        for key, path in _extra_post_files(inv, domain):
            header = read_tecplot_header(path)
            variables = header.get("variables") or []
            if variables and variables[0].upper().startswith("TOTAL TIME"):
                items.append(("xy", path, key, [(v,) for v in variables[1:8]], None, None, None))
                continue
            plot_vars = _non_xyz(variables)
            if not plot_vars:
                if is_binary_tecplot(path):
                    items.append(("contour", path, key, None, t_last, 4, None))
                continue
            idx = var_index(variables, plot_vars[0])
            items.append(("contour", path, key, (plot_vars[0],), t_last, idx, None))

        if not items:
            return []
        slots = _grid_slots(len(items))
        names: list[str] = []
        for slot, item in zip(slots, items):
            kind, path, name, needles, t, idx, vectors = item
            if kind == "xy":
                b.add_xy(
                    path,
                    name,
                    slot,
                    needles or [("Y",)],
                    x_title=_qty_title(
                        "Time",
                        (getattr(build, "time_units", "") if build else "") or "seconds",
                    ),
                )
            elif kind == "vector":
                b.add_3d(
                    [path],
                    name,
                    slot,
                    None,
                    solution_time=t,
                    vectors=vectors,
                    link_view=True,
                )
            else:
                b.add_3d(
                    [path],
                    name,
                    slot,
                    needles,
                    solution_time=t,
                    link_view=True,
                    var_num=idx,
                )
            names.append(name)
        return names

    def fill_observations(b: _LayoutBuilder, domain: str) -> list[str]:
        obs = _obs_path(inv, domain)
        if not obs:
            return []
        header = read_tecplot_header(obs)
        series = _obs_series(header.get("variables") or [])
        if not series:
            return []
        by_site: dict[str, dict[str, str]] = {}
        for var_name in series:
            site, kind = _obs_site_and_kind(var_name)
            if not site:
                continue
            by_site.setdefault(site, {})[kind or var_name] = var_name
        sites = sorted(by_site, key=str.lower)
        if not sites:
            return []
        top_slots, bot_slots = _obs_two_row_slots(len(sites))
        length_u = (
            (getattr(build, "length_units", "") if build else "")
            or header.get("length_units")
            or "meters"
        )
        time_u = (
            (getattr(build, "time_units", "") if build else "")
            or header.get("time_units")
            or "seconds"
        )
        x_title = _qty_title("Time", time_u)
        names: list[str] = []
        for slot, site in zip(top_slots, sites):
            var_name = by_site[site].get("HEAD")
            if var_name:
                b.add_xy(
                    obs,
                    var_name,
                    slot,
                    [(var_name,)],
                    x_title=x_title,
                    y_title=_obs_y_title("HEAD", length_u),
                )
                names.append(var_name)
        for slot, site in zip(bot_slots, sites):
            kinds = by_site[site]
            if kinds.get("SATURATION"):
                kind = "SATURATION"
            elif kinds.get("DEPTH"):
                kind = "DEPTH"
            else:
                continue
            var_name = kinds[kind]
            b.add_xy(
                obs,
                var_name,
                slot,
                [(var_name,)],
                x_title=x_title,
                y_title=_obs_y_title(kind, length_u),
            )
            names.append(var_name)
        return names

    for domain in inv.domains:
        mesh = inv.build_file(domain)
        post = inv.post_file(domain)
        has_vel = inv.post_file(f"{domain}.Velocity", f"{domain}_Velocity")
        has_scatter = bool(_scatter_files(inv, domain))
        if not mesh and not post and not has_vel and not has_scatter:
            continue

        if mesh:
            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file(f"{domain} Mesh")
            frames = fill_mesh(builder, domain, mesh)
            emit_builder(f"{domain}_Mesh", f"{domain} Mesh", builder, frames)
            emit_builder(domain, f"{domain} Mesh", builder, frames)

            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file(f"{domain} Variables")
            frames = fill_variables(builder, domain, mesh)
            emit_builder(f"{domain}_Variables", f"{domain} Variables", builder, frames)

        builder = _LayoutBuilder(inv.layouts_dir)
        builder.start_file(f"{domain} Scatter")
        frames = fill_scatter(builder, domain)
        emit_builder(f"{domain}_scatter", f"{domain} Scatter", builder, frames)

        builder = _LayoutBuilder(inv.layouts_dir)
        builder.start_file(f"{domain} Results")
        frames = fill_results(builder, domain)
        emit_builder(f"{domain}_Results", f"{domain} Results", builder, frames)

        builder = _LayoutBuilder(inv.layouts_dir)
        builder.start_file(f"{domain} Observations")
        frames = fill_observations(builder, domain)
        emit_builder(f"{domain}_Observations", f"{domain} Observations", builder, frames)

    budget = inv.post_file("VolumeBudget")
    if budget:
        header = read_tecplot_header(budget)
        variables = header.get("variables") or []
        needles = _budget_y_needles(variables)
        if needles:
            time_u = (
                (getattr(build, "time_units", "") if build else "")
                or header.get("time_units")
                or "seconds"
            )
            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file("GWF Volume Budget")
            builder.add_xy(
                budget,
                "Volume budget",
                _overlap_slot(),
                needles,
                x_title=_qty_title("Time", time_u),
                y_title="Volumetric rate",
                show_legend=True,
            )
            emit_builder(
                "GWF_VolumeBudget",
                "GWF Volume Budget",
                builder,
                ["Volume budget"],
            )

    gwf_post = inv.post_file("GWF")
    if gwf_post:
        header = read_tecplot_header(gwf_post)
        variables = header.get("variables") or []
        t_gwf = usg.last_times.get("GWF")
        head_idx = var_index(variables, "GWF Head", "Head")
        z_idx = var_index(variables, "GWF z Cell", "z Cell")
        ibound_idx = var_index(variables, "GWF Ibound", "Ibound")
        head_name = var_name(variables, "GWF Head", "Head")
        z_name = var_name(variables, "GWF z Cell", "z Cell")
        if head_idx and z_idx and head_name and z_name:
            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file("GWF Water Table")
            builder.add_custom_3d(
                gwf_post,
                _water_table_style(
                    head_name, z_name, head_idx, len(variables) + 1, ibound_idx, t_gwf
                ),
            )
            emit_builder("GWF_WaterTable", "GWF Water Table", builder, ["Water table isosurface"])
        sat_idx = var_index(variables, "GWF Saturation", "Saturation")
        if sat_idx:
            x_range = _gwf_x_range(inv.model_dir)
            x_primary = x_start = x_end = None
            if x_range is not None:
                x_primary, x_start, x_end = _slice_x_positions(*x_range)
            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file("GWF Saturation Slices")
            builder.add_custom_3d(
                gwf_post,
                _saturation_slices_style(
                    sat_idx, ibound_idx, t_gwf, x_primary, x_start, x_end
                ),
            )
            emit_builder(
                "GWF_SaturationSlices",
                "GWF Saturation Slices",
                builder,
                ["Saturation slices"],
            )

    swf_post = inv.post_file("SWF")
    if swf_post:
        header = read_tecplot_header(swf_post)
        variables = header.get("variables") or []
        flux_name = _swf_flux_name(variables)
        if flux_name:
            t_swf = usg.last_times.get("SWF")
            ibound_idx = var_index(variables, "SWF Ibound", "Ibound")
            factor = mm_per_year_factor(
                getattr(build, "length_units", "") if build else "",
                getattr(build, "time_units", "") if build else "",
            )
            raw_abs = ascii_var_absmax(swf_post, flux_name)
            color_limit: float | None = None
            needs_pass = True
            if raw_abs is not None and raw_abs > 0:
                color_limit = nice_abs_limit(max(raw_abs * factor, 1e-12))
                needs_pass = False
            levels = symmetric_contour_levels(color_limit if color_limit else 30.0)
            builder = _LayoutBuilder(inv.layouts_dir)
            builder.start_file("SWF Infiltration")
            builder.add_custom_3d(
                swf_post,
                _infiltration_style(
                    flux_name,
                    len(variables) + 1,
                    factor,
                    ibound_idx,
                    t_swf,
                    levels,
                    color_limit,
                ),
            )
            emit_builder(
                "SWF_Infiltration",
                "SWF Infiltration",
                builder,
                ["Infiltration [mm/year]"],
                symmetric_legend=needs_pass,
            )

    _remove_stale_layouts(inv.layouts_dir, keep)
    return produced
