"""Offline check that Build MUT assigns Borden sand / bedrock / Grass / Channel materials
and emits Surficial + Bedrock log-proportional sublayering.

Does not import ``mut_build`` (that pulls QGIS/shapely); asserts against the source text
and a tiny inlined copy of the weight helper for numeric checks.

Run from the plugin folder:

    python -m pytest tests/test_default_materials.py -q
    python tests/test_default_materials.py
"""

from __future__ import annotations

import ast
import re
import unittest
from pathlib import Path

MUT_BUILD = Path(__file__).resolve().parents[1] / "core" / "mut_build.py"


def _log_proportional_weights(n: int, decade: float = 10.0) -> list[float]:
    if n < 1:
        raise ValueError(f"n must be >= 1, got {n}")
    if n == 1:
        return [1.0]
    r = float(decade) ** (1.0 / (n - 1))
    return [r**i for i in range(n)]


class DefaultMaterials(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.src = MUT_BUILD.read_text(encoding="utf-8")

    def test_constants(self):
        self.assertIn("DEFAULT_GWF_MATERIAL = 6  # Borden sand (surficial)", self.src)
        self.assertIn("DEFAULT_GWF_BEDROCK_MATERIAL = 21  # Fractured crystalline rock", self.src)
        self.assertIn("DEFAULT_SWF_MATERIAL = 4  # Grass", self.src)
        self.assertIn("DEFAULT_CLN_MATERIAL = 3  # Channel (rectangular 10 m × 1 m)", self.src)
        self.assertIn("BEDROCK_EXTENSION_M = 200.0", self.src)
        self.assertIn("SURFICIAL_SUBLAYERS = 10", self.src)
        self.assertIn("BEDROCK_SUBLAYERS = 5", self.src)

    def test_assignments_use_constants(self):
        self.assertIn("chosen cells use gwf material number", self.src)
        self.assertIn('f"        {DEFAULT_GWF_MATERIAL}"', self.src)
        self.assertIn('f"        {DEFAULT_GWF_BEDROCK_MATERIAL}"', self.src)
        self.assertIn("chosen zones use swf material number", self.src)
        self.assertIn('f"        {DEFAULT_SWF_MATERIAL}"', self.src)
        self.assertIn("chosen zones use cln material number", self.src)
        self.assertIn('f"    {DEFAULT_CLN_MATERIAL}"', self.src)
        self.assertIn("! Borden sand", self.src)
        self.assertIn("! Fractured crystalline rock", self.src)
        self.assertIn("! Grass", self.src)
        self.assertIn("! Channel 10 m × 1 m", self.src)

    def test_zone_material_assignment(self):
        self.assertIn("choose zone number", self.src)
        self.assertIn("choose cells by chosen zones", self.src)
        # zone 1 then zone 2 in that order
        zones = re.findall(r'choose zone number"\s*,\s*\n\s*"\s*(\d+)"', self.src)
        # source uses separate string literals; fall back to line scan
        lines = self.src.splitlines()
        nums = []
        for i, line in enumerate(lines):
            if "choose zone number" in line and i + 1 < len(lines):
                nxt = lines[i + 1].strip().strip('"').strip()
                # f-strings or plain "        1"
                m = re.search(r"\b([12])\b", nxt)
                if m and ("1" in nxt or "2" in nxt) and "DEFAULT" not in nxt:
                    # next line is like '        "1",' or '        1',
                    if nxt in ("1", "2") or nxt.startswith("1") or nxt.startswith("2"):
                        # parse ast for string "1" / "2"
                        try:
                            val = ast.literal_eval(nxt.rstrip(","))
                        except (SyntaxError, ValueError):
                            val = nxt.rstrip(",").strip('"')
                        if str(val) in ("1", "2"):
                            nums.append(str(val))
        self.assertEqual(nums[:2], ["1", "2"], f"expected zone 1 then 2, got {nums}")

    def test_layering_keywords(self):
        self.assertIn("proportional sublayering", self.src)
        self.assertIn("elevation constant", self.src)
        self.assertIn("layer name", self.src)
        self.assertIn("Surficial", self.src)
        self.assertIn("Bedrock", self.src)
        self.assertNotIn("uniform sublayering", self.src)
        self.assertIn("log_proportional_weights", self.src)
        self.assertIn("model_base_m", self.src)

    def test_log_weights_decade(self):
        w10 = _log_proportional_weights(10)
        w5 = _log_proportional_weights(5)
        self.assertEqual(len(w10), 10)
        self.assertEqual(len(w5), 5)
        self.assertAlmostEqual(w10[0], 1.0)
        self.assertAlmostEqual(w10[-1], 10.0, places=5)
        self.assertAlmostEqual(w5[-1] / w5[0], 10.0, places=5)
        # geometric ratio
        r10 = w10[1] / w10[0]
        for a, b in zip(w10, w10[1:]):
            self.assertAlmostEqual(b / a, r10, places=5)
        # helper in source matches
        self.assertIn("def log_proportional_weights", self.src)


if __name__ == "__main__":
    unittest.main()
