"""Parse verification folder lists and Tecplot comparison-layout frames."""

from __future__ import annotations

from pathlib import Path


def parse_folder_list(path: Path) -> list[str]:
    """Return folder names from a one-name-per-line list file.

    Blank lines and comments starting with ``#`` or ``!`` are ignored.
    Only the first token on each remaining line is used.
    """
    if not path.is_file():
        raise FileNotFoundError(f"verification folder list not found: {path}")
    folders: list[str] = []
    for raw in path.read_text(encoding="utf-8", errors="replace").splitlines():
        line = raw.strip()
        if not line or line.startswith("#") or line.startswith("!"):
            continue
        token = line.split()[0]
        if token.startswith("#") or token.startswith("!"):
            continue
        folders.append(token)
    return folders


def parse_layout_frames(layout_path: Path) -> list[tuple[int, str]]:
    """Return (1-based frame number, FrameName) in layout order."""
    if not layout_path.is_file():
        raise FileNotFoundError(f"layout not found: {layout_path}")
    frames: list[tuple[int, str]] = []
    index = 0
    for raw in layout_path.read_text(encoding="utf-8", errors="replace").splitlines():
        line = raw.strip()
        if not line.startswith("$!FrameName"):
            continue
        # $!FrameName  = '10_Forsyth (IN-OUT)'
        if "'" not in line:
            continue
        name = line.split("'", 2)[1]
        index += 1
        frames.append((index, name))
    return frames


def frame_folder_hint(frame_name: str) -> str:
    """Folder-like prefix of a Tecplot frame name (text before ' (')."""
    if " (" in frame_name:
        return frame_name.split(" (", 1)[0].strip()
    return frame_name.strip()


def match_folder(hint: str, folders: list[str]) -> str | None:
    """Case-insensitive match of a frame hint to a listed folder name."""
    low = hint.lower()
    for name in folders:
        if name.lower() == low:
            return name
    return None


def slug(name: str) -> str:
    """Filesystem-safe stem for a PNG / label."""
    out: list[str] = []
    prev_us = False
    for ch in name.strip():
        if ch.isalnum() or ch in "-.":
            out.append(ch)
            prev_us = False
        elif not prev_us:
            out.append("_")
            prev_us = True
    return "".join(out).strip("_")
