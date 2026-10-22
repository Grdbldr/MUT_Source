"""Offline tests for mapping stream polylines onto an existing triangle mesh.

Imports only ``core.stream_map`` pure helpers (numpy/scipy). Does not need QGIS or shapely.

Run from the plugin folder:

    python -m pytest tests/test_stream_map.py -q
    python tests/test_stream_map.py
"""

from __future__ import annotations

import sys
import unittest
from dataclasses import dataclass
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from core.stream_map import map_paths_to_levels, mesh_adjacency, shortest_path  # noqa: E402


@dataclass
class _Path:
    level: int
    name: str
    coords: np.ndarray


def _unit_square():
    """Two triangles on the unit square: nodes (0,0),(1,0),(1,1),(0,1)."""
    xy = np.array([[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]], dtype=float)
    tris = np.array([[0, 1, 2], [0, 2, 3]], dtype=np.int64)
    return xy, tris


class StreamMapCore(unittest.TestCase):
    def test_shortest_path_along_edge(self):
        xy, tris = _unit_square()
        adj = mesh_adjacency(tris, xy)
        path = shortest_path(adj, 0, 2)
        self.assertEqual(path[0], 0)
        self.assertEqual(path[-1], 2)
        self.assertGreaterEqual(len(path), 2)
        edges = {(min(u, v), max(u, v)) for u, nbrs in enumerate(adj) for v, _ in nbrs}
        for a, b in zip(path[:-1], path[1:]):
            self.assertIn((min(a, b), max(a, b)), edges)

    def test_map_diagonal_marks_chain(self):
        xy, tris = _unit_square()
        paths = [_Path(level=1, name="diag", coords=np.array([[0.0, 0.0], [1.0, 1.0]]))]
        levels, warnings, outlets = map_paths_to_levels(paths, xy, tris, ds=0.5, max_snap=0.6)
        self.assertFalse(warnings)
        marked = np.flatnonzero(levels > 0).tolist()
        self.assertEqual(sorted(marked), [0, 2])
        self.assertEqual(int(levels[0]), 1)
        self.assertEqual(int(levels[2]), 1)
        # Downstream end (1,1) is on the mesh boundary.
        self.assertEqual(outlets, [2])

    def test_lower_level_wins(self):
        xy, tris = _unit_square()
        paths = [
            _Path(level=2, name="trib", coords=np.array([[0.0, 0.0], [1.0, 0.0]])),
            _Path(level=1, name="main", coords=np.array([[0.0, 0.0], [1.0, 0.0]])),
        ]
        levels, _, outlets = map_paths_to_levels(paths, xy, tris, ds=0.5, max_snap=0.6)
        self.assertEqual(int(levels[0]), 1)
        self.assertEqual(int(levels[1]), 1)
        self.assertEqual(outlets, [1])

    def test_far_samples_skipped(self):
        xy, tris = _unit_square()
        paths = [_Path(level=1, name="far", coords=np.array([[10.0, 10.0], [11.0, 11.0]]))]
        levels, warnings, outlets = map_paths_to_levels(paths, xy, tris, ds=0.5, max_snap=0.25)
        self.assertEqual(int((levels > 0).sum()), 0)
        self.assertTrue(warnings)
        self.assertEqual(outlets, [])

    def test_level1_outlet_is_downstream_boundary_not_whole_stem(self):
        """Mapped level-1 outlet is the downstream boundary node, not every node on the path."""
        # Strip: nodes 0--1--2 along bottom; triangles above so 0,1,2 are boundary-edge nodes.
        xy = np.array([[0.0, 0.0], [1.0, 0.0], [2.0, 0.0], [1.0, 1.0]], dtype=float)
        tris = np.array([[0, 1, 3], [1, 2, 3]], dtype=np.int64)
        paths = [_Path(level=1, name="main", coords=np.array([[0.0, 0.0], [2.0, 0.0]]))]
        levels, warnings, outlets = map_paths_to_levels(paths, xy, tris, ds=1.0, max_snap=0.6)
        self.assertFalse(warnings)
        self.assertGreaterEqual(int((levels == 1).sum()), 2)
        self.assertEqual(outlets, [2])
        self.assertNotIn(0, outlets)
        # Boundary∩level-1 would wrongly include the upstream end (0) and mid-edge (1).
        on_bnd_l1 = [i for i in range(len(xy)) if levels[i] == 1 and i in (0, 1, 2)]
        self.assertGreater(len(on_bnd_l1), 1)
        self.assertEqual(outlets, [2])


class OutletMaskSource(unittest.TestCase):
    """Static checks: outlet_node_mask must not fall back to all boundary∩level-1."""

    def test_outlet_node_mask_no_broad_fallback(self):
        text = (ROOT / "core" / "mesh.py").read_text(encoding="utf-8")
        start = text.index("def outlet_node_mask")
        end = text.index("\ndef nchos_files", start)
        body = text[start:end]
        self.assertNotIn("(mesh.level == 1) & on_bnd", body)
        self.assertIn("Does **not** flag every level-1", body)

    def test_ensure_outlet_rematches_paths(self):
        text = (ROOT / "core" / "mut_build.py").read_text(encoding="utf-8")
        start = text.index("def _ensure_outlet_nchos")
        end = text.index("\ndef _outlet_nchos", start)
        body = text[start:end]
        self.assertIn("map_paths_to_levels", body)
        self.assertIn("outlet_indices", body)
        self.assertNotIn("mask = outlet_node_mask(mesh)", body)


if __name__ == "__main__":
    unittest.main()
