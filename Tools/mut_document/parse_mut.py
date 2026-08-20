"""Parse _build.mut, include files, and _buildo.eco into a structured summary."""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path

from inventory import ArtifactInventory

_BANNER = re.compile(r"^!\s*-{3,}")
_COMMENT = re.compile(r"^\s*!(.*)$")


def _clean(line: str) -> str:
    return line.replace("\x00", "").rstrip()


def _is_blank(line: str) -> bool:
    return not line.strip()


def _whole_line_comment(line: str) -> str | None:
    stripped = line.strip()
    match = _COMMENT.match(stripped)
    if match and not stripped.startswith("!!"):
        return match.group(1).strip()
    return None


def _looks_like_path(text: str) -> bool:
    s = text.strip().strip('"')
    if "!" in s:
        s = s.split("!")[0].strip()
    lower = s.lower()
    if not s or lower in {"gwf", "swf", "cln", "tmplt", "end", "tr", "ss", "include"}:
        return False
    if lower.startswith("include "):
        return False
    if s.startswith("..") or "\\" in s or (len(s) > 2 and s[1] == ":"):
        return True
    return bool(re.fullmatch(r"[\w.\-]+\.(csv|instructions|xyzList|dat|txt|asc|tif|shp)", s, re.I))


def _resolve(model_dir: Path, raw: str, extra_roots: list[Path] | None = None) -> Path | None:
    candidate = Path(raw.strip().strip('"'))
    search = [model_dir]
    if extra_roots:
        search.extend(extra_roots)
    if candidate.is_absolute():
        return candidate if candidate.exists() else candidate
    for root in search:
        path = (root / candidate).resolve()
        if path.exists():
            return path
    return (model_dir / candidate).resolve()


def _read_lines(path: Path) -> list[str]:
    try:
        return [_clean(line) for line in path.read_text(encoding="utf-8", errors="replace").splitlines()]
    except OSError:
        return []


@dataclass
class LayerSpec:
    name: str
    n_sublayers: str | None = None
    elevation: str | None = None
    offset: str | None = None


@dataclass
class MaterialAssign:
    domain: str
    zone: str | None
    material_number: str
    material_name: str | None = None
    n_cells: str | None = None
    properties: list[str] = field(default_factory=list)


@dataclass
class Observation:
    domain: str
    name: str
    cell: str | None = None
    xyz: str | None = None
    distance: str | None = None


@dataclass
class StressPeriod:
    index: int
    type: str | None = None
    duration: str | None = None
    tmaxat: str | None = None


@dataclass
class BcItem:
    text: str
    stress_period: int | None = None


@dataclass
class MutBuildInfo:
    comments: list[str] = field(default_factory=list)
    include_files: list[str] = field(default_factory=list)
    referenced_files: list[tuple[str, str, bool]] = field(default_factory=list)
    mut_version: str = ""
    run_date: str = ""
    time_units: str = ""
    length_units: str = ""
    database_path: str = ""
    mesh_path: str = ""
    mesh_nodes: str = ""
    mesh_elements: str = ""
    mesh_type: str = ""
    layers: list[LayerSpec] = field(default_factory=list)
    materials_db: dict[str, str] = field(default_factory=dict)
    assignments: list[MaterialAssign] = field(default_factory=list)
    observations: list[Observation] = field(default_factory=list)
    stress_periods: list[StressPeriod] = field(default_factory=list)
    ics: list[str] = field(default_factory=list)
    bcs: list[BcItem] = field(default_factory=list)
    recharge: str = ""
    oc_times: list[str] = field(default_factory=list)
    sms_set: str = ""
    instruction_text: str = ""
    include_excerpts: dict[str, str] = field(default_factory=dict)


def _collect_mut_tree(path: Path, model_dir: Path, visited: set[Path] | None = None) -> tuple[list[str], list[Path]]:
    """Return (lines with comments, include paths) walking include files."""
    visited = visited if visited is not None else set()
    path = path.resolve()
    if path in visited or not path.is_file():
        return [], []
    visited.add(path)
    lines = _read_lines(path)
    includes: list[Path] = []
    i = 0
    while i < len(lines):
        stripped = lines[i].strip()
        if stripped.lower() == "include" and i + 1 < len(lines):
            inc = _resolve(path.parent, lines[i + 1], extra_roots=[model_dir])
            if inc is not None:
                includes.append(inc)
                nested_lines, nested_incs = _collect_mut_tree(inc, model_dir, visited)
                # Keep include marker; nested content is stored separately
                includes.extend(nested_incs)
                _ = nested_lines
        i += 1
    return lines, includes


def _parse_build_mut(info: MutBuildInfo, model_dir: Path, build_mut: Path) -> None:
    lines, includes = _collect_mut_tree(build_mut, model_dir)
    info.instruction_text = "\n".join(lines)
    info.include_files = [str(p) for p in includes]
    extra_roots = [model_dir]
    if info.database_path:
        extra_roots.append(model_dir / info.database_path)

    for inc in includes:
        excerpt_lines = _read_lines(inc)
        info.include_excerpts[str(inc)] = "\n".join(excerpt_lines[:80])
        for line in excerpt_lines:
            comment = _whole_line_comment(line)
            if comment and not _BANNER.match("! " + comment):
                if comment and comment not in info.comments:
                    pass  # include comments are not the problem synopsis
            if _looks_like_path(line) and line.strip().lower() not in {"include"}:
                resolved = _resolve(inc.parent, line, extra_roots=extra_roots)
                exists = bool(resolved and resolved.exists())
                rel = line.strip()
                if (rel, str(resolved) if resolved else rel, exists) not in info.referenced_files:
                    info.referenced_files.append((rel, str(resolved) if resolved else rel, exists))

    before_build = True
    i = 0
    while i < len(lines):
        line = lines[i]
        comment = _whole_line_comment(line)
        stripped = line.strip()
        lower = stripped.lower()

        if comment is not None:
            if before_build or not _BANNER.match(stripped):
                if comment and comment not in info.comments:
                    info.comments.append(comment)
            i += 1
            continue

        if lower.startswith("build modflow"):
            before_build = False

        if lower == "include" and i + 1 < len(lines):
            info.include_files.append(lines[i + 1].strip()) if lines[i + 1].strip() not in info.include_files else None
            i += 2
            continue

        if lower == "use local databases" and i + 1 < len(lines):
            info.database_path = lines[i + 1].strip()
            extra_roots.append(_resolve(model_dir, info.database_path) or (model_dir / info.database_path))
            i += 2
            continue

        if lower == "units of time" and i + 1 < len(lines):
            info.time_units = lines[i + 1].strip()
            i += 2
            continue

        if lower == "units of length" and i + 1 < len(lines):
            info.length_units = lines[i + 1].strip()
            i += 2
            continue

        if lower == "read mesh" and i + 1 < len(lines):
            info.mesh_path = lines[i + 1].strip()
            i += 2
            continue

        for domain, verb in (
            ("GWF", "gwf materials database"),
            ("SWF", "swf materials database"),
            ("CLN", "cln materials database"),
        ):
            if lower == verb and i + 1 < len(lines):
                info.materials_db[domain] = lines[i + 1].strip()
                i += 2
                break
        else:
            if _looks_like_path(stripped) and lower not in {"include", "end"}:
                resolved = _resolve(model_dir, stripped, extra_roots=extra_roots)
                exists = bool(resolved and resolved.exists())
                rec = (stripped, str(resolved) if resolved else stripped, exists)
                if rec not in info.referenced_files:
                    info.referenced_files.append(rec)
            i += 1


def _parse_eco(info: MutBuildInfo, eco: Path) -> None:
    lines = _read_lines(eco)
    current_domain = ""
    current_zone: str | None = None
    pending_assign: MaterialAssign | None = None
    in_after_conversion = False
    in_oc = False
    current_obs_domain = ""
    pending_obs: Observation | None = None
    pending_xyz: str | None = None
    sp_index = 0
    current_sp: StressPeriod | None = None
    current_layer: LayerSpec | None = None

    def add_bc(text: str) -> None:
        sp = current_sp.index if current_sp is not None else None
        info.bcs.append(BcItem(text=text, stress_period=sp))

    def flush_assign() -> None:
        nonlocal pending_assign, in_after_conversion
        if pending_assign is not None:
            info.assignments.append(pending_assign)
            pending_assign = None
        in_after_conversion = False

    i = 0
    while i < len(lines):
        raw = lines[i]
        line = raw.strip()
        if not line:
            i += 1
            continue

        if line.startswith("@@  MUT version:"):
            info.mut_version = line.split(":", 1)[1].replace("@", "").strip()
        elif line.startswith("@@  Run date:"):
            info.run_date = line.split(":", 1)[1].replace("@", "").strip()
        elif line.startswith("Units of time:"):
            info.time_units = line.split(":", 1)[1].strip() or info.time_units
        elif "Assumed units of length" in line:
            info.length_units = line.replace("Assumed units of length are", "").strip() or info.length_units
        elif line.startswith("Path to local database files:"):
            info.database_path = line.split(":", 1)[1].strip()
        elif line.startswith("Number of nodes:"):
            info.mesh_nodes = line.split(":", 1)[1].strip()
        elif line.startswith("Number of elements:"):
            info.mesh_elements = line.split(":", 1)[1].strip()
        elif line.startswith("Element type:"):
            info.mesh_type = line.split(":", 1)[1].strip()
        elif "binary mesh file:" in line.lower():
            info.mesh_path = line.split("file:", 1)[1].strip()
        elif line.startswith("Materials file"):
            fname = line.replace("Materials file", "").strip()
            key = current_domain or ("GWF" if "gwf" in fname.lower() else "SWF" if "swf" in fname.lower() else "CLN")
            info.materials_db.setdefault(key, fname)
        elif line.lower() in {"gwf", "swf", "cln", "tmplt"} and i > 0 and lines[i - 1].strip().lower() == "active domain":
            current_domain = line.upper()
            current_obs_domain = current_domain

        elif line.lower() == "layer name" and i + 1 < len(lines):
            current_layer = LayerSpec(name=lines[i + 1].strip())
            i += 1
        elif current_layer is not None and line.lower() == "number of uniform sublayers" or (
            current_layer is not None and line.lower().startswith("number of uniform sublayers")
        ):
            if "sublayers" in line.lower():
                parts = line.split()
                current_layer.n_sublayers = parts[-1]
        elif line.startswith("Number of uniform sublayers") and current_layer is not None:
            current_layer.n_sublayers = line.split()[-1]
        elif line.startswith("Base elevation from") and current_layer is not None:
            current_layer.elevation = line
        elif line.startswith("Layer base elevation") and current_layer is not None:
            current_layer.elevation = line
        elif line.startswith("Offset layer base") and current_layer is not None:
            current_layer.offset = line
        elif line.lower() == "end new layer" and current_layer is not None:
            info.layers.append(current_layer)
            current_layer = None
        elif line.lower() == "top elevation from" or line.startswith("Top elevation from"):
            if "Top elevation from" in line:
                info.ics.append(line) if line not in info.ics else None

        elif line.startswith("Adding zone number:"):
            current_zone = line.split(":")[-1].strip()
        elif line.startswith("Assigning all chosen") and "material" in line.lower():
            flush_assign()
            m = re.search(
                r"Assigning all chosen (\w+).+material\s+(\S+),\s*(.+)$",
                line,
                re.IGNORECASE,
            )
            n_cells = None
            # previous chosen-cells line
            for back in range(1, 8):
                if i - back >= 0 and "Cells chosen:" in lines[i - back]:
                    n_cells = lines[i - back].split(":")[-1].strip()
                    break
            if m:
                pending_assign = MaterialAssign(
                    domain=m.group(1).upper(),
                    zone=current_zone,
                    material_number=m.group(2).rstrip(","),
                    material_name=m.group(3).strip(),
                    n_cells=n_cells,
                )
            in_after_conversion = False
        elif "After Unit Conversion" in line:
            in_after_conversion = True
        elif pending_assign is not None and in_after_conversion:
            if line.startswith("****") or line.startswith("Properties of material"):
                pass
            elif line.startswith("Time Conversion") or line.startswith("Material time") or line.startswith("Modflow time"):
                pass
            elif re.match(r"^[A-Za-z]", line) and ":" in line:
                pending_assign.properties.append(line)
            else:
                if line.lower().startswith("choose") or line.lower().startswith("active") or line.lower().startswith("swf ") or line.lower().startswith("gwf "):
                    flush_assign()

        elif line.lower() == "gwf initial head equals surface elevation":
            info.ics.append("GWF initial head equals surface elevation")
        elif line.startswith("Assigning all chosen SWF cells a starting depth"):
            info.ics.append(line)
        elif line.lower().startswith("cln initial"):
            info.ics.append(line)

        elif line.startswith("Stress period"):
            flush_assign()
            sp_index += 1
            current_sp = StressPeriod(index=sp_index)
            info.stress_periods.append(current_sp)
        elif current_sp is not None and line.startswith("Type:"):
            current_sp.type = line.split(":", 1)[1].strip()
        elif current_sp is not None and line.startswith("Duration:"):
            current_sp.duration = line.split(":", 1)[1].strip()
        elif current_sp is not None and line.startswith("Maximum time step"):
            current_sp.tmaxat = line.split(":", 1)[1].strip()

        elif line.startswith("Assigning SWF recharge:"):
            info.recharge = line.split(":", 1)[1].strip()
            add_bc(line)
        elif line.startswith("Recharge strategy:"):
            add_bc(line)
        elif line.startswith("Option ") and "recharge" in line.lower():
            add_bc(line)
        elif "critical depth" in line.lower() and "define" in line.lower():
            add_bc(line)
        elif line.startswith("Find cell closest to XYZ:") and "critical" in "\n".join(lines[max(0, i - 30):i + 5]).lower():
            add_bc(line)

        elif line.startswith("CSV file :") or line.startswith("CSV file:"):
            info.referenced_files.append((line.split(":", 1)[1].strip(), line.split(":", 1)[1].strip(), True))
        elif line.startswith("Observation point name:"):
            pending_obs = Observation(
                domain=current_obs_domain or current_domain,
                name=line.split(":", 1)[1].strip(),
                xyz=pending_xyz,
            )
        elif line.startswith("GWF observation point name:") or line.startswith("SWF observation point name:") or line.startswith("CLN observation point name:"):
            dom = line.split()[0]
            pending_obs = Observation(domain=dom, name=line.split(":", 1)[1].strip(), xyz=pending_xyz)
        elif pending_obs is not None and line.startswith("Observation point cell:"):
            pending_obs.cell = line.split(":", 1)[1].strip()
        elif line.startswith("Find cell closest to user XYZ:"):
            pending_xyz = line.split(":", 1)[1].strip()
            if pending_obs is not None:
                pending_obs.xyz = pending_xyz
        elif pending_obs is not None and line.startswith("Distance from user XYZ:"):
            pending_obs.distance = line.split(":", 1)[1].strip()
            info.observations.append(pending_obs)
            pending_obs = None
            pending_xyz = None

        elif line.lower() == "generate output control file":
            in_oc = True
        elif in_oc and line.lower().startswith("end generate"):
            in_oc = False
        elif in_oc and re.match(r"^[0-9.eE+-]+$", line):
            info.oc_times.append(line)
        elif line.strip().startswith("#") and "Output time" in line:
            in_oc = False
        elif re.match(r"^\s*\d+\s+[0-9.Ee+-]+\s+DAYS", line):
            parts = line.split()
            if len(parts) >= 2 and parts[1] not in info.oc_times:
                info.oc_times.append(parts[1] + " " + (parts[2] if len(parts) > 2 else ""))

        elif line.startswith("Using SMS parameter set"):
            info.sms_set = line.replace("Using SMS parameter set", "").strip()

        i += 1
    flush_assign()

    # Deduplicate include_files / referenced_files
    seen_inc: set[str] = set()
    uniq_inc: list[str] = []
    for item in info.include_files:
        key = item.replace("/", "\\")
        if key not in seen_inc:
            seen_inc.add(key)
            uniq_inc.append(item)
    info.include_files = uniq_inc


def parse_build(inv: ArtifactInventory) -> MutBuildInfo:
    info = MutBuildInfo()
    if inv.build_mut:
        _parse_build_mut(info, inv.model_dir, inv.build_mut)
    if inv.buildo_eco:
        _parse_eco(info, inv.buildo_eco)
    # Prefer eco mesh path; keep mut path if eco missing
    return info
