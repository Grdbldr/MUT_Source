"""Discover MUT build, USG run, and post-process artifacts in a model folder."""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path


BUILD_TECPLOT_RE = re.compile(
    r"^_buildo\.(?:Modflow|modflow)\.(.+)\.tecplot(?:\.(?:grid|sol)(?:\.\d+)?)?\.(dat|plt|szplt)$",
    re.IGNORECASE,
)
POST_TECPLOT_RE = re.compile(
    r"^_posto\.(?:Modflow|modflow)\.(.+)\.tecplot(?:\.(?:grid|sol)(?:\.\d+)?)?\.(dat|plt|szplt)$",
    re.IGNORECASE,
)
OBS_TECPLOT_RE = re.compile(
    r"^(?:Modflow|modflow)\.(GWF|SWF|CLN)\.OBS\.tecplot\.dat$", re.IGNORECASE
)


def _first_existing(model_dir: Path, names: list[str]) -> Path | None:
    for name in names:
        path = model_dir / name
        if path.is_file():
            return path
    matches: list[Path] = []
    for name in names:
        matches.extend(model_dir.glob(name))
    return matches[0] if matches else None


def _tecplot_rank(path: Path) -> int:
    """Lower is better. Prefer a single .szplt over ASCII; never prefer leftover grid/sol .plt."""
    name = path.name.lower()
    if name.endswith(".tecplot.szplt"):
        return 0
    if name.endswith(".tecplot.dat"):
        return 1
    if name.endswith(".tecplot.plt"):
        return 2
    if re.search(r"\.tecplot\.sol(?:\.\d+)?\.plt$", name):
        return 3
    if name.endswith(".tecplot.grid.plt"):
        return 4
    return 5


def _iter_tecplot(model_dir: Path) -> list[Path]:
    found: list[Path] = []
    for pattern in (
        "*.tecplot.szplt",
        "*.tecplot.dat",
        "*.tecplot.plt",
        "*.tecplot.sol.plt",
        "*.tecplot.sol.*.plt",
        "*.tecplot.grid.plt",
    ):
        found.extend(model_dir.glob(pattern))
    return found


def _index_tecplot(model_dir: Path, pattern: re.Pattern[str]) -> dict[str, Path]:
    found: dict[str, Path] = {}
    ranks: dict[str, int] = {}
    for path in sorted(_iter_tecplot(model_dir)):
        match = pattern.match(path.name)
        if not match:
            continue
        key = match.group(1)
        rank = _tecplot_rank(path)
        if key not in found or rank < ranks[key]:
            found[key] = path
            ranks[key] = rank
    return found


@dataclass
class ArtifactInventory:
    model_dir: Path
    folder_name: str
    docs_dir: Path
    layouts_dir: Path
    imagery_dir: Path
    imagery_user_dir: Path
    overview_tex: Path | None = None
    build_mut: Path | None = None
    buildo_eco: Path | None = None
    buildo_input: Path | None = None
    post_mut: Path | None = None
    nam: Path | None = None
    lst: Path | None = None
    build_tecplot: dict[str, Path] = field(default_factory=dict)
    post_tecplot: dict[str, Path] = field(default_factory=dict)
    obs_tecplot: dict[str, Path] = field(default_factory=dict)
    user_images: list[Path] = field(default_factory=list)

    @property
    def domains(self) -> list[str]:
        domains: list[str] = []
        for name in ("GWF", "SWF", "CLN"):
            if name in self.build_tecplot or name in self.post_tecplot:
                domains.append(name)
            else:
                for key in list(self.build_tecplot) + list(self.post_tecplot):
                    if key.upper().startswith(name):
                        domains.append(name)
                        break
        # Preserve order, drop duplicates
        seen: set[str] = set()
        ordered: list[str] = []
        for name in domains:
            if name not in seen:
                seen.add(name)
                ordered.append(name)
        return ordered

    @property
    def has_build(self) -> bool:
        return self.build_mut is not None and (
            self.buildo_eco is not None or bool(self.build_tecplot)
        )

    @property
    def has_usgs(self) -> bool:
        return self.lst is not None and self.lst.is_file()

    @property
    def has_post(self) -> bool:
        return bool(self.post_tecplot)

    def tecplot_key(self, *candidates: str) -> Path | None:
        """Return the first Tecplot file matching any candidate key (case-insensitive)."""
        lowered = {k.lower(): p for k, p in {**self.build_tecplot, **self.post_tecplot}.items()}
        for cand in candidates:
            path = lowered.get(cand.lower())
            if path is not None:
                return path
        return None

    def build_file(self, *candidates: str) -> Path | None:
        lowered = {k.lower(): p for k, p in self.build_tecplot.items()}
        for cand in candidates:
            path = lowered.get(cand.lower())
            if path is not None:
                return path
        return None

    def post_file(self, *candidates: str) -> Path | None:
        lowered = {k.lower(): p for k, p in self.post_tecplot.items()}
        for cand in candidates:
            path = lowered.get(cand.lower())
            if path is not None:
                return path
        return None


def scan_model_folder(model_dir: Path) -> ArtifactInventory:
    model_dir = model_dir.resolve()
    docs_dir = model_dir / "Docs"
    inv = ArtifactInventory(
        model_dir=model_dir,
        folder_name=model_dir.name,
        docs_dir=docs_dir,
        layouts_dir=docs_dir / "layouts",
        imagery_dir=docs_dir / "imagery",
        imagery_user_dir=docs_dir / "imagery_user",
    )
    overview = docs_dir / "overview.tex"
    if overview.is_file():
        inv.overview_tex = overview

    inv.build_mut = _first_existing(model_dir, ["_build.mut"])
    inv.buildo_eco = _first_existing(model_dir, ["_buildo.eco"])
    inv.buildo_input = _first_existing(model_dir, ["_buildo.input"])
    inv.post_mut = _first_existing(model_dir, ["_post.mut"])
    inv.nam = _first_existing(model_dir, ["Modflow.nam", "modflow.nam"])
    inv.lst = _first_existing(model_dir, ["Modflow.lst", "modflow.lst"])

    inv.build_tecplot = _index_tecplot(model_dir, BUILD_TECPLOT_RE)
    inv.post_tecplot = _index_tecplot(model_dir, POST_TECPLOT_RE)
    inv.obs_tecplot = _index_tecplot(model_dir, OBS_TECPLOT_RE)

    if inv.imagery_user_dir.is_dir():
        inv.user_images = sorted(
            p for p in inv.imagery_user_dir.iterdir()
            if p.suffix.lower() in {".png", ".jpg", ".jpeg", ".pdf"}
        )
    return inv
