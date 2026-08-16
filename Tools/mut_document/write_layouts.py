"""Write per-domain Tecplot layouts (Mesh, Variables, Scatter, Results, Observations)."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from inventory import ArtifactInventory
from parse_mut import MutBuildInfo
from parse_usg import UsgRunInfo
from tecplot_io import read_tecplot_header, var_index


@dataclass
class LayoutFile:
    section_id: str
    page_name: str
    path: Path
    frames: list[str]


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


def _frame_layout(x: float, y: float, w: float, h: float, transparent: bool = True) -> str:
    return f"""$!FrameLayout
  ShowHeader = No
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


def _zone_flood(frame_name: str, var_num: int, n_zones: int) -> str:
    n_levels = max(n_zones, 1)
    levels = "\n".join(f"{i + 0.5:g}" for i in range(1, n_levels + 1))
    return f"""$!PlotType = Cartesian3D
$!FrameName = '{_escape(frame_name)}'
$!ActiveFieldMaps = [1]
$!ThreeDAxis
  AspectRatioLimit = 25
  BoxAspectRatioLimit = 25
$!GlobalContour  1
  Var = {var_num}
  DefNumLevels = {n_levels}
  Labels
    {{
    NumFormat
      {{
      Formatting = Integer
      TimeDateFormat = ''
      }}
    }}
  Legend
    {{
    Show = Yes
    XYPos
      {{
      X = 95
      }}
    OverlayBarGrid = No
    Box
      {{
      BoxType = Filled
      }}
    }}
$!ContourLevels New
  ContourGroup = 1
  RawData
{n_levels}
{levels}
{_fieldmap_flood()}$!FieldLayers
  ShowContour = Yes
  ShowShade = Yes
  ShowMesh = Yes
  ShowEdge = No
  UseLightingEffect = Yes
$!View Fit
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


def _read_3d(rel: str, append: bool = False, reset_style: bool = True) -> str:
    extra: list[str] = []
    if append:
        extra.append("  ReadDataOption = Append")
        extra.append("  ResetStyle = No")
    elif not reset_style:
        extra.append("  ResetStyle = No")
    extra_txt = ("\n".join(extra) + "\n") if extra else ""
    return f"""$!ReadDataSet  '{_quote(rel)}'
  InitialPlotType = Cartesian3D
  IncludeText = No
  IncludeGeom = No
{extra_txt}  AssignStrandIDs = Yes
  VarLoadMode = ByName
"""


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
        "$!ThreeDAxis",
        "  AspectRatioLimit = 25",
        "  BoxAspectRatioLimit = 25",
    ]
    if var_num:
        bits.extend(
            [
                "$!GlobalContour  1",
                f"  Var = {var_num}",
                "  DefNumLevels = 12",
                "  Legend",
                "    {",
                "    Show = Yes",
                "    XYPos",
                "      {",
                "      X = 95",
                "      }",
                "    OverlayBarGrid = No",
                "    Box",
                "      {",
                "      BoxType = Filled",
                "      }",
                "    }",
            ]
        )
        bits.append(_fieldmap_flood().rstrip())
        bits.extend(
            [
                "$!FieldLayers",
                "  ShowContour = Yes",
                "  ShowShade = Yes",
                "  ShowMesh = Yes",
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
        bits.append("$!GlobalTime")
        bits.append(f"  SolutionTime = {solution_time}")
    bits.append("$!View Fit")
    return "\n".join(bits) + "\n"


def _fieldmap_scatter(color: str) -> str:
    """Scatter-only FieldMap: 1.0-size spheres. Color is Multi or a named colour."""
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
    FrameSize = 1
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


def _scatter3d(frame_name: str, color_var: int | None = None) -> str:
    bits = [
        "$!PlotType = Cartesian3D",
        f"$!FrameName = '{_escape(frame_name)}'",
        "$!ActiveFieldMaps = [1]",
        "$!ThreeDAxis",
        "  AspectRatioLimit = 25",
        "  BoxAspectRatioLimit = 25",
    ]
    if color_var:
        bits.extend(
            [
                "$!GlobalContour  1",
                f"  Var = {color_var}",
                "  DefNumLevels = 12",
                "  Legend",
                "    {",
                "    Show = Yes",
                "    XYPos",
                "      {",
                "      X = 95",
                "      }",
                "    OverlayBarGrid = No",
                "    Box",
                "      {",
                "      BoxType = Filled",
                "      }",
                "    }",
                "$!GlobalScatter",
                f"  Var = {color_var}",
                "  RelativeSize = 1",
                "  Legend",
                "    {",
                "    Show = No",
                "    }",
            ]
        )
        color = "Multi"
    else:
        bits.extend(
            [
                "$!GlobalScatter",
                "  Var = 3",
                "  RelativeSize = 1",
            ]
        )
        color = "Red"
    bits.append(_fieldmap_scatter(color).rstrip())
    bits.extend(
        [
            "$!FieldLayers",
            "  ShowMesh = No",
            "  ShowContour = No",
            "  ShowEdge = No",
            "  ShowShade = No",
            "  ShowScatter = Yes",
            "  UseLightingEffect = Yes",
        ]
    )
    bits.append("$!View Fit")
    return "\n".join(bits) + "\n"


def _vectors3d(frame_name: str, u: int, v: int, w: int, mag: int | None, solution_time: float | None) -> str:
    extra = ""
    if solution_time is not None:
        extra = f"$!GlobalTime\n  SolutionTime = {solution_time}\n"
    if mag:
        mag_line = (
            "$!GlobalContour  1\n"
            f"  Var = {mag}\n"
            "  Legend\n"
            "    {\n"
            "    Show = Yes\n"
            "    }\n"
            "$!FieldMap  [1]\n"
            "  Contour\n"
            "    {\n"
            "    ContourType = Flood\n"
            "    Color = Multi\n"
            "    UseLightingEffect = Yes\n"
            "    }\n"
            "$!FieldLayers\n"
            "  ShowMesh = Yes\n"
            "  ShowEdge = No\n"
            "  ShowShade = Yes\n"
            "  ShowContour = Yes\n"
            "  ShowVector = Yes\n"
            "  UseLightingEffect = Yes\n"
        )
    else:
        mag_line = (
            "$!FieldLayers\n"
            "  ShowMesh = Yes\n"
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
{extra}{mag_line}$!ThreeDAxis
  AspectRatioLimit = 25
$!View Fit
"""


def _xy_maps(frame_name: str, yvars: list[tuple[int, str]]) -> str:
    colors = ["Red", "Green", "Blue", "Custom1", "Purple", "Black", "Orange"]
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
    return f"""$!PlotType = XYLine
$!FrameName = '{_escape(frame_name)}'
{"".join(maps)}$!ActiveLineMaps = [{active_str}]
$!XYLineAxis
  XDetail 1
    {{
    CoordScale = Log
    Title
      {{
      Text = 'Time'
      }}
    }}
$!View Fit
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
            self.chunks.append(_read_3d(_tecplot_path(mesh), append=False, reset_style=True))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(_read_3d(_tecplot_path(mesh), append=False, reset_style=False))
        var_num = _contour_var(mesh, *zone_needles) or 4
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
        self.chunks.append(_read_3d(_tecplot_path(files[0]), append=False))
        for extra in files[1:]:
            self.chunks.append(_read_3d(_tecplot_path(extra), append=True))
        if scatter:
            extra_vars = _non_xyz(read_tecplot_header(files[0]).get("variables") or [])
            color_var = var_index(read_tecplot_header(files[0]).get("variables") or [], extra_vars[0]) if extra_vars else None
            self.chunks.append(_scatter3d(frame_name, color_var))
        elif vectors:
            header = read_tecplot_header(files[0])
            variables = header.get("variables") or []
            u = var_index(variables, vectors[0])
            v = var_index(variables, vectors[1])
            wvar = var_index(variables, vectors[2])
            mag = var_index(variables, "GWF Head", "SWF Head", "CLN Head", "Head")
            if u and v and wvar:
                self.chunks.append(_vectors3d(frame_name, u, v, wvar, mag, solution_time))
            else:
                self.chunks.append(_contour3d(frame_name, mag, solution_time))
        elif _is_zone_name(frame_name) or (
            contour_needles and any(_is_zone_name(n) for n in contour_needles)
        ):
            zone_var = var_num or _contour_var(files[0], *(contour_needles or ("Zone",))) or 4
            self.chunks.append(_zone_flood(frame_name, zone_var, n_zones))
        else:
            if var_num is None:
                var_num = _contour_var(files[0], *(contour_needles or ()))
            self.chunks.append(_contour3d(frame_name, var_num, solution_time))
        if link_view or link_size:
            self.chunks.append(_linking(link_view=link_view, link_size=link_size))
        self.frame_count_on_page += 1

    def add_scatter(
        self,
        path: Path,
        frame_name: str,
        slot: tuple[float, float, float, float],
        color_var: int | None,
        link_view: bool = True,
    ) -> None:
        x, y, w, h = slot
        if self.frame_count_on_page == 0:
            self.chunks.append(_frame_pos(x, y, w, h))
        else:
            self.chunks.append(_create_frame(x, y, w, h))
            self.chunks.append(_frame_pos(x, y, w, h))
        self.chunks.append(_read_3d(_tecplot_path(path), append=False))
        self.chunks.append(_scatter3d(frame_name, color_var))
        if link_view:
            self.chunks.append(_linking(link_view=True, link_size=False))
        self.frame_count_on_page += 1

    def add_xy(
        self,
        path: Path,
        frame_name: str,
        slot: tuple[float, float, float, float],
        y_needles: list[tuple[str, ...]],
    ) -> None:
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
        self.chunks.append(_xy_maps(frame_name, yvars or [(2, "Y")]))
        self.frame_count_on_page += 1

    def text(self) -> str:
        return "".join(self.chunks)


def _zone_needles(domain: str) -> tuple[str, ...]:
    return (f"{domain} Zone", "Zone")


def _extra_post_files(inv: ArtifactInventory, domain: str) -> list[tuple[str, Path]]:
    skip = {
        domain.upper(),
        f"{domain.upper()}.VELOCITY",
        f"{domain.upper()}_VELOCITY",
        "VOLUMEBUDGET",
    }
    prefix = domain.upper() + "_"
    prefix_dot = domain.upper() + "."
    found: list[tuple[str, Path]] = []
    for key, path in inv.post_tecplot.items():
        ku = key.upper()
        if ku in skip:
            continue
        if ku.startswith(prefix) or ku.startswith(prefix_dot):
            found.append((key, path))
    return found


_SCATTER_ORDER = ("CELLS", "CHD", "RCH", "SWBC", "DRN", "WEL", "OBS_SCATTER")


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


def _remove_stale_layouts(layouts_dir: Path, keep: set[str]) -> None:
    stale_prefixes = ("04_", "05_", "06_", "07_", "08_")
    for path in layouts_dir.glob("*.lay"):
        if path.name in keep:
            continue
        if path.name == "model_documentation.lay" or path.name.startswith(stale_prefixes):
            path.unlink()


def write_layouts(
    inv: ArtifactInventory,
    usg: UsgRunInfo,
    build: MutBuildInfo | None = None,
) -> list[LayoutFile]:
    inv.layouts_dir.mkdir(parents=True, exist_ok=True)
    produced: list[LayoutFile] = []
    keep: set[str] = set()

    def emit_builder(section_id: str, page_name: str, builder: _LayoutBuilder, frames: list[str]) -> None:
        if builder.frame_count_on_page == 0:
            return
        path = inv.layouts_dir / f"{section_id}.lay"
        path.write_text(builder.text(), encoding="utf-8")
        keep.add(path.name)
        produced.append(LayoutFile(section_id, page_name, path, frames))

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
        slots = _grid_slots(len(files))
        names: list[str] = []
        for slot, (key, path) in zip(slots, files):
            label = key.replace("_", " ")
            header = read_tecplot_header(path)
            extra = _non_xyz(header.get("variables") or [])
            color_var = var_index(header.get("variables") or [], extra[0]) if extra else None
            b.add_scatter(path, label, slot, color_var, link_view=True)
            names.append(label)
        return names

    def fill_results(b: _LayoutBuilder, domain: str) -> list[str]:
        items: list[tuple] = []
        t_last = usg.last_times.get(domain)
        post = inv.post_file(domain)
        if post:
            header = read_tecplot_header(post)
            for var_name in _non_xyz(header.get("variables") or []):
                idx = var_index(header.get("variables") or [], var_name)
                items.append(("contour", post, var_name, (var_name,), t_last, idx, None))
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
                continue
            idx = var_index(variables, plot_vars[0])
            items.append(("contour", path, key, (plot_vars[0],), t_last, idx, None))
        if domain.upper() == "GWF":
            budget = inv.post_file("VolumeBudget")
            if budget:
                header = read_tecplot_header(budget)
                variables = header.get("variables") or []
                needles = [(v,) for v in variables[1:] if v]
                items.append(("xy", budget, "Volume budget", needles[:8], None, None, None))

        if not items:
            return []
        slots = _grid_slots(len(items))
        names: list[str] = []
        for slot, item in zip(slots, items):
            kind, path, name, needles, t, idx, vectors = item
            if kind == "xy":
                b.add_xy(path, name, slot, needles or [("Y",)])
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
        names: list[str] = []
        for slot, site in zip(top_slots, sites):
            var_name = by_site[site].get("HEAD")
            if var_name:
                b.add_xy(obs, var_name, slot, [(var_name,)])
                names.append(var_name)
        for slot, site in zip(bot_slots, sites):
            kinds = by_site[site]
            var_name = kinds.get("SATURATION") or kinds.get("DEPTH")
            if var_name:
                b.add_xy(obs, var_name, slot, [(var_name,)])
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

    _remove_stale_layouts(inv.layouts_dir, keep)
    return produced
