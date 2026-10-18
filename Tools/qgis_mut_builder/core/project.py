"""Project definition stored as ``project.yaml``."""

from __future__ import annotations

import re
from dataclasses import asdict, dataclass, field
from datetime import datetime
from pathlib import Path

import yaml

from .geo import Grid, centred_grid, utm_epsg

BOUNDARY_MODES = ("square", "rectangle", "watershed", "file")
FOCUS_MODES = ("rectangle", "digitized", "file", "feature_watershed", "feature")
DELINEATIONS = ("none", "regional_dem", "global_watersheds")
STREAM_SOURCES = ("auto", "ohn", "nhdplus", "osm", "dem")
LEGACY_KEYS = ("site_buffer_km",)
LEGACY_OPTIONS = ("blend_width_m",)

ROLES = (
    "ground_dem",
    "bedrock_elevation",
    "bedrock_depth_m",
    "bedrock_depth_cm",
    "bathymetry_elevation",
    "bathymetry_depth_m",
    "boundary",
    "ignore",
)


@dataclass
class DataFile:
    path: str
    role: str = "ignore"

    def __post_init__(self) -> None:
        if self.role not in ROLES:
            raise ValueError(f"Unknown data role '{self.role}' for {self.path}; expected one of {ROLES}")


@dataclass
class Options:
    coarse_resolution_m: float = 30.0
    fine_resolution_m: float = 1.0
    fine_threshold_m: float = 5.0
    min_thickness_m: float = 1.0
    lake_bathymetry: bool = True
    lake_min_area_ha: float = 2.0


def slugify(name: str, default: str = "project") -> str:
    return re.sub(r"[^A-Za-z0-9_-]+", "_", name).strip("_") or default


@dataclass
class FocusArea:
    """A named subregion of the regional model that gets its own high-resolution ground surface."""

    name: str
    mode: str = "rectangle"
    boundary_file: str = ""
    source: str = ""
    feature_name: str = ""
    outlet_lat: float | None = None
    outlet_lon: float | None = None
    delineation: str = "none"
    margin_m: float = 200.0
    resolution_m: float = 1.0
    status: str = "defined"
    created: str = field(default_factory=lambda: datetime.now().isoformat(timespec="seconds"))

    def __post_init__(self) -> None:
        if self.mode not in FOCUS_MODES:
            raise ValueError(f"Focus area mode must be one of {FOCUS_MODES}")
        if self.delineation not in DELINEATIONS:
            raise ValueError(f"Focus area delineation must be one of {DELINEATIONS}")
        if self.resolution_m <= 0 or self.margin_m < 0:
            raise ValueError("Focus area resolution must be positive and margin not negative")

    @property
    def slug(self) -> str:
        return slugify(self.name, "focus")


@dataclass
class StreamOptions:
    """Fixed mesh nodes along ranked rivers and streams (level 1 = main river, 2 = its tributaries, ...)."""

    source: str = "auto"
    levels: int = 2
    min_length_km: float = 5.0
    spacing_m: float = 0.0  # 0 = the mesh boundary spacing
    main_river: str = ""
    include_canals: bool = False
    dem_threshold_km2: float = 10.0

    def __post_init__(self) -> None:
        if self.source not in STREAM_SOURCES:
            raise ValueError(f"Stream source must be one of {STREAM_SOURCES}")
        if not 1 <= self.levels <= 6:
            raise ValueError("Stream levels must be between 1 and 6")
        if self.min_length_km < 0 or self.spacing_m < 0 or self.dem_threshold_km2 <= 0:
            raise ValueError("Stream minimum length and spacing must not be negative; DEM threshold must be positive")


@dataclass
class MeshSpec:
    """A 2D Delaunay triangle mesh of the regional model area (boundary and user points fixed, interior free)."""

    name: str
    boundary_spacing_m: float = 300.0
    interior_points: int = 5000
    fixed_points_file: str = ""
    smoothing_iterations: int = 0
    boundary_tolerance_m: float = 0.0
    streams: StreamOptions | None = None
    status: str = "defined"
    created: str = field(default_factory=lambda: datetime.now().isoformat(timespec="seconds"))

    def __post_init__(self) -> None:
        if isinstance(self.streams, dict):
            self.streams = StreamOptions(**self.streams)
        if self.boundary_spacing_m <= 0:
            raise ValueError("Mesh boundary point spacing must be positive")
        if self.boundary_tolerance_m < 0:
            raise ValueError("Mesh boundary simplification tolerance must not be negative")
        if self.interior_points < 1:
            raise ValueError("Mesh interior point count must be at least 1")
        if self.smoothing_iterations < 0:
            raise ValueError("Mesh smoothing iterations must not be negative")

    @property
    def slug(self) -> str:
        return slugify(self.name, "mesh")


@dataclass
class Project:
    name: str
    lat: float
    lon: float
    output_folder: str
    description: str = ""
    data_folder: str = ""
    regional_buffer_km: float = 20.0
    epsg: int = 0
    boundary_mode: str = "square"
    boundary_file: str = ""
    boundary_name: str = ""
    boundary_source: str = ""
    boundary_margin_km: float = 1.0
    providers: list[str] = field(default_factory=list)
    data_files: list[DataFile] = field(default_factory=list)
    options: Options = field(default_factory=Options)
    focus_areas: list[FocusArea] = field(default_factory=list)
    meshes: list[MeshSpec] = field(default_factory=list)
    created: str = field(default_factory=lambda: datetime.now().isoformat(timespec="seconds"))

    def __post_init__(self) -> None:
        if not -90.0 <= self.lat <= 90.0 or not -180.0 <= self.lon <= 180.0:
            raise ValueError(f"Site latitude/longitude out of range: {self.lat}, {self.lon}")
        if self.regional_buffer_km <= 0:
            raise ValueError("The regional half-width must be positive")
        if self.boundary_mode not in BOUNDARY_MODES:
            raise ValueError(f"boundary_mode must be one of {BOUNDARY_MODES}")
        if self.boundary_margin_km < 0:
            raise ValueError("Boundary margin must not be negative")
        if not self.epsg:
            self.epsg = utm_epsg(self.lat, self.lon)

    @property
    def has_boundary(self) -> bool:
        return self.boundary_mode != "square"

    @property
    def default_boundary_path(self) -> Path:
        return self.output_path / "1_data" / "boundary" / "model_boundary.geojson"

    @property
    def slug(self) -> str:
        return slugify(self.name)

    @property
    def output_path(self) -> Path:
        return Path(self.output_folder)

    @property
    def downloads_dir(self) -> Path:
        return self.output_path / "0_incoming" / "downloads"

    @property
    def surfaces_dir(self) -> Path:
        return self.output_path / "1_data" / "surfaces"

    @property
    def yaml_path(self) -> Path:
        return self.output_path / "project.yaml"

    @property
    def qgz_path(self) -> Path:
        return self.output_path / f"{self.slug}.qgz"

    def regional_grid(self) -> Grid:
        """Square around the project location, or the boundary extent plus ``boundary_margin_km``."""
        res = self.options.coarse_resolution_m
        if not self.has_boundary:
            return centred_grid(self.epsg, self.lat, self.lon, self.regional_buffer_km * 1000.0, res)
        path = Path(self.boundary_file)
        if not self.boundary_file or not path.exists():
            raise ValueError(f"Boundary file not found: '{self.boundary_file}'")
        from .boundary import boundary_bounds, snap_out

        xmin, ymin, xmax, ymax = boundary_bounds(path, self.epsg)
        pad = self.boundary_margin_km * 1000.0
        return Grid(
            self.epsg,
            snap_out(xmin - pad, res, False),
            snap_out(ymin - pad, res, False),
            snap_out(xmax + pad, res, True),
            snap_out(ymax + pad, res, True),
            res,
        )

    @property
    def focus_root(self) -> Path:
        return self.output_path / "1_data" / "focus"

    def focus_dir(self, focus: FocusArea) -> Path:
        return self.focus_root / focus.slug

    def get_focus(self, name: str) -> FocusArea | None:
        return next((f for f in self.focus_areas if f.name == name or f.slug == slugify(name, "focus")), None)

    def set_focus(self, focus: FocusArea) -> None:
        """Add ``focus``, replacing any focus area with the same name."""
        self.focus_areas = [f for f in self.focus_areas if f.slug != focus.slug] + [focus]

    @property
    def mesh_root(self) -> Path:
        return self.output_path / "2_mesh"

    def mesh_dir(self, spec: MeshSpec) -> Path:
        return self.mesh_root / spec.slug

    def get_mesh(self, name: str) -> MeshSpec | None:
        return next((m for m in self.meshes if m.name == name or m.slug == slugify(name, "mesh")), None)

    def set_mesh(self, spec: MeshSpec) -> None:
        """Add ``spec``, replacing any mesh with the same name."""
        self.meshes = [m for m in self.meshes if m.slug != spec.slug] + [spec]

    def files_with_role(self, *roles: str) -> list[Path]:
        return [Path(f.path) for f in self.data_files if f.role in roles]

    def to_dict(self) -> dict:
        data = asdict(self)
        ordered = {
            "name": data.pop("name"),
            "description": data.pop("description"),
            "site": {"lat": data.pop("lat"), "lon": data.pop("lon")},
            "crs": f"EPSG:{data.pop('epsg')}",
        }
        ordered.update(data)
        return ordered

    @classmethod
    def from_dict(cls, data: dict) -> "Project":
        data = dict(data)
        site = data.pop("site", {})
        data.setdefault("lat", site.get("lat"))
        data.setdefault("lon", site.get("lon"))
        crs = str(data.pop("crs", "") or "")
        if crs.upper().startswith("EPSG:"):
            data["epsg"] = int(crs.split(":", 1)[1])
        for key in LEGACY_KEYS:
            data.pop(key, None)
        options = dict(data.get("options") or {})
        for key in LEGACY_OPTIONS:
            options.pop(key, None)
        data["options"] = Options(**options)
        data["focus_areas"] = [FocusArea(**f) for f in (data.get("focus_areas") or [])]
        data["meshes"] = [MeshSpec(**m) for m in (data.get("meshes") or [])]
        data["data_files"] = [DataFile(**f) for f in (data.get("data_files") or [])]
        data["providers"] = list(data.get("providers") or [])
        known = set(cls.__dataclass_fields__)
        unknown = set(data) - known
        if unknown:
            raise ValueError(f"Unknown keys in project file: {sorted(unknown)}")
        return cls(**data)

    def save(self, path: Path | None = None) -> Path:
        path = Path(path) if path else self.yaml_path
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8") as handle:
            yaml.safe_dump(self.to_dict(), handle, sort_keys=False, allow_unicode=True)
        return path

    @classmethod
    def load(cls, path: Path) -> "Project":
        path = Path(path)
        with open(path, encoding="utf-8") as handle:
            data = yaml.safe_load(handle) or {}
        project = cls.from_dict(data)
        project.rebase(path.resolve().parent)
        return project

    def rebase(self, folder: Path) -> None:
        """Move paths saved under another ``output_folder`` (a copied or cloned project) onto ``folder``."""
        old = Path(self.output_folder)
        new = Path(folder)
        if not self.output_folder or old == new:
            return

        def moved(value: str) -> str:
            if not value:
                return value
            try:
                return str(new / Path(value).relative_to(old))
            except ValueError:
                return value

        self.data_folder = moved(self.data_folder)
        self.boundary_file = moved(self.boundary_file)
        for focus in self.focus_areas:
            focus.boundary_file = moved(focus.boundary_file)
        for spec in self.meshes:
            spec.fixed_points_file = moved(spec.fixed_points_file)
        for data_file in self.data_files:
            data_file.path = moved(data_file.path)
        self.output_folder = str(new)
