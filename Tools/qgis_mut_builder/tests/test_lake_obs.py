"""Offline unit tests for SWF lake-level observation point helpers.

Run from the plugin folder (plain Python with numpy):

    python -m pytest tests/test_lake_obs.py -q
    python tests/test_lake_obs.py
"""

from __future__ import annotations

import sys
import tempfile
import types
import unittest
from pathlib import Path
from unittest.mock import MagicMock

import numpy as np

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

# Offline tests must not require QGIS / GDAL Python bindings.
if "osgeo" not in sys.modules:
    osgeo = types.ModuleType("osgeo")
    osgeo.osr = MagicMock()
    osgeo.ogr = MagicMock()
    osgeo.gdal = MagicMock()
    sys.modules["osgeo"] = osgeo
    sys.modules["osgeo.osr"] = osgeo.osr
    sys.modules["osgeo.ogr"] = osgeo.ogr
    sys.modules["osgeo.gdal"] = osgeo.gdal

from core.geo import Grid  # noqa: E402
from core.lake_obs import (  # noqa: E402
    INSTRUCTIONS_NAME,
    MAX_OBS,
    LakeObsPoint,
    deepest_cell,
    instructions_text,
    uniquify_names,
    write_swf_lake_level_obs_instructions,
)


class LakeObsHelpers(unittest.TestCase):
    def test_uniquify_names(self):
        self.assertEqual(
            uniquify_names(["A", "B", "A", "(unnamed)", "(unnamed)", ""]),
            ["A", "B", "A (2)", "(unnamed)", "(unnamed) (2)", "(unnamed) (3)"],
        )

    def test_deepest_cell(self):
        grid = Grid(32617, 0.0, 0.0, 30.0, 20.0, 10.0)
        depth = np.array(
            [
                [np.nan, 1.0, 2.0],
                [0.5, 5.0, 1.0],
            ],
            dtype=float,
        )
        mask = np.array(
            [
                [False, True, True],
                [True, True, False],
            ]
        )
        hit = deepest_cell(depth, mask, grid, water_level_m=100.0)
        self.assertIsNotNone(hit)
        x, y, z, d = hit
        # cell (1,1): centre x=15, y=5; bottom = 100 - 5
        self.assertAlmostEqual(x, 15.0)
        self.assertAlmostEqual(y, 5.0)
        self.assertAlmostEqual(z, 95.0)
        self.assertAlmostEqual(d, 5.0)

    def test_deepest_cell_empty_mask(self):
        grid = Grid(32617, 0.0, 0.0, 20.0, 20.0, 10.0)
        depth = np.ones((2, 2))
        mask = np.zeros((2, 2), dtype=bool)
        self.assertIsNone(deepest_cell(depth, mask, grid, 50.0))

    def test_instructions_text_preserves_spaces(self):
        points = [
            LakeObsPoint("Conestogo Lake", 100.0, 200.0, 50.0, max_depth_m=3.0, method="estimated"),
            LakeObsPoint("Belwood Lake", 110.0, 210.0, 48.0, max_depth_m=5.0, method="measured"),
        ]
        text = instructions_text(points)
        self.assertIn("active domain", text)
        self.assertIn("swf", text)
        self.assertIn("observation point", text)
        self.assertIn("Conestogo Lake", text)
        self.assertIn("Belwood Lake", text)
        self.assertIn("100.000000 200.000000 50.000000", text)
        self.assertNotIn("observation points from csv file", text)

    def test_write_instructions_file(self):
        points = [LakeObsPoint("Lake A", 1.0, 2.0, 3.0)]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / INSTRUCTIONS_NAME
            out = write_swf_lake_level_obs_instructions(path, points)
            self.assertEqual(out, path)
            body = path.read_text(encoding="utf-8")
            self.assertIn("Lake A", body)
            self.assertTrue(body.endswith("\n") or "observation point" in body)

    def test_max_obs_constant(self):
        self.assertEqual(MAX_OBS, 1000)


if __name__ == "__main__":
    unittest.main()
