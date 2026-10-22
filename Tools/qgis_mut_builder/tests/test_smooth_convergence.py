"""Offline checks for automatic smoothing defaults and convergence stop rules.

Does not import ``core.mesh`` (osgeo) or ``core.project`` (yaml). Asserts against
source text and a tiny inlined Laplacian loop that mirrors ``smooth()`` stop logic.

Run from the plugin folder:

    python tests/test_smooth_convergence.py
"""

from __future__ import annotations

import unittest
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[1]
MESH_PY = ROOT / "core" / "mesh.py"
PROJECT_PY = ROOT / "core" / "project.py"


def _laplacian_once(xy: np.ndarray, fixed: np.ndarray, tris: np.ndarray) -> tuple[np.ndarray, float]:
    """One Laplacian pass on free nodes; returns new xy and largest move."""
    n = len(xy)
    e = np.vstack([tris[:, [0, 1]], tris[:, [1, 2]], tris[:, [2, 0]]])
    e = np.vstack([e, e[:, ::-1]])
    e = np.unique(e, axis=0)
    sums = np.zeros((n, 2))
    np.add.at(sums, e[:, 0], xy[e[:, 1]])
    counts = np.bincount(e[:, 0], minlength=n).astype(float)
    free = ~fixed & (counts > 0)
    target = xy.copy()
    target[free] = sums[free] / counts[free, None]
    moved = float(np.hypot(*(target[free] - xy[free]).T).max()) if free.any() else 0.0
    out = xy.copy()
    out[free] = target[free]
    return out, moved


def _smooth_until(xy, fixed, tris, iterations: int, tolerance_m: float) -> dict:
    """Mirror of ``smooth()`` stop rules (no retriangulation)."""
    moved = 0.0
    passes = 0
    converged = False
    for it in range(max(1, iterations)):
        xy, moved = _laplacian_once(xy, fixed, tris)
        passes = it + 1
        if moved < tolerance_m:
            converged = True
            break
    return {"passes": passes, "converged": converged, "last_move_m": moved}


class SourceDefaults(unittest.TestCase):
    def test_mesh_spec_defaults_in_source(self):
        src = PROJECT_PY.read_text(encoding="utf-8")
        self.assertIn("SMOOTHING_MAX_CAP = 10", src)
        self.assertIn("smoothing_iterations: int = 10", src)
        self.assertIn("smoothing_tolerance_m: float = 0.01", src)
        # Legacy low values bump to SMOOTHING_MAX_CAP when loading project.yaml (from_dict).
        self.assertIn("if n < SMOOTHING_MAX_CAP:", src)
        self.assertIn('m["smoothing_iterations"] = SMOOTHING_MAX_CAP', src)
        self.assertIn("min(SMOOTHING_MAX_CAP, max(1, int(self.smoothing_iterations)))", src)

    def test_build_mesh_always_smooths(self):
        src = MESH_PY.read_text(encoding="utf-8")
        self.assertNotIn("if spec.smoothing_iterations > 0:", src)
        self.assertIn("tolerance_m=spec.smoothing_tolerance_m", src)
        self.assertIn("result.stats[\"smoothing\"] = smooth_info", src)
        # Absolute tolerance, not spacing-relative early exit.
        self.assertNotIn("moved < 1e-3 * spacing", src)
        self.assertIn("if moved < tolerance_m:", src)


class ConvergenceRules(unittest.TestCase):
    def _square_with_offset_free(self):
        xy = np.array(
            [
                [0.0, 0.0],
                [10.0, 0.0],
                [10.0, 10.0],
                [0.0, 10.0],
                [2.0, 3.0],
            ],
            dtype=float,
        )
        fixed = np.array([True, True, True, True, False])
        tris = np.array([[0, 1, 4], [1, 2, 4], [2, 3, 4], [3, 0, 4]], dtype=np.int64)
        return xy, fixed, tris

    def test_large_tolerance_stops_in_one_pass(self):
        xy, fixed, tris = self._square_with_offset_free()
        info = _smooth_until(xy, fixed, tris, iterations=10, tolerance_m=1e6)
        self.assertEqual(info["passes"], 1)
        self.assertTrue(info["converged"])

    def test_tiny_tolerance_hits_max(self):
        xy, fixed, tris = self._square_with_offset_free()
        # Reset free node after each pass so moves never fall below a tiny tol.
        moved_pass = []

        def sticky(xy0, fixed0, tris0, iterations, tolerance_m):
            xy = xy0.copy()
            for it in range(max(1, iterations)):
                xy, moved = _laplacian_once(xy, fixed0, tris0)
                moved_pass.append(moved)
                xy[4] = [2.0, 3.0]  # keep away from equilibrium
                if moved < tolerance_m:
                    return {"passes": it + 1, "converged": True, "last_move_m": moved}
            return {"passes": iterations, "converged": False, "last_move_m": moved_pass[-1]}

        info = sticky(xy, fixed, tris, iterations=2, tolerance_m=1e-12)
        self.assertEqual(info["passes"], 2)
        self.assertFalse(info["converged"])
        self.assertGreater(info["last_move_m"], 1e-12)


if __name__ == "__main__":
    unittest.main()
