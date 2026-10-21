"""Offline unit tests for climate aggregation, Hargreaves PET, gauge pick and MUT forcing text.

Run from the plugin folder (plain Python with numpy):

    python -m pytest tests/test_climate_flows.py -q
    python tests/test_climate_flows.py
"""

from __future__ import annotations

import sys
import tempfile
import types
import unittest
from datetime import date
from pathlib import Path
from unittest.mock import MagicMock

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

from core.climate import (  # noqa: E402
    aggregate_monthly,
    hargreaves_pet_mm,
    lattice_points,
    long_term_average,
    monthly_climatology,
    net_recharge_m_per_d,
    record_window,
    write_climate_forcing_instructions,
    write_constant_arcascii,
    write_precipitation_rts,
)
from core.geo import GeoBBox  # noqa: E402


class ClimateHelpers(unittest.TestCase):
    def test_parse_daymet_headers_with_units(self):
        from core.climate import _parse_daymet

        sample = (
            "Daymet Single Pixel Extraction\n"
            "year,yday,prcp (mm/day),tmax (deg c),tmin (deg c)\n"
            "2020,1,1.5,2.0,-3.0\n"
            "2020,2,0.0,3.0,-2.0\n"
        )
        rows = _parse_daymet(sample)
        self.assertEqual(len(rows), 2)
        self.assertEqual(rows[0]["date"], date(2020, 1, 1))
        self.assertAlmostEqual(rows[0]["prcp_mm"], 1.5)
        self.assertAlmostEqual(rows[1]["tmax"], 3.0)

    def test_record_window_ten_complete_years(self):
        start, end = record_window(date(2026, 10, 18))
        self.assertEqual(start, date(2016, 1, 1))
        self.assertEqual(end, date(2025, 12, 31))

    def test_lattice_respects_limit(self):
        pts = lattice_points(GeoBBox(-81.0, 43.0, -79.5, 44.0), step=0.25, limit=9)
        self.assertLessEqual(len(pts), 9)
        self.assertGreaterEqual(len(pts), 1)

    def test_hargreaves_positive_summer(self):
        pet = hargreaves_pet_mm(28.0, 14.0, 43.5, 180)
        self.assertTrue(pet > 1.0)
        self.assertTrue(pet < 15.0)

    def test_hargreaves_zero_when_tmax_equals_tmin(self):
        pet = hargreaves_pet_mm(10.0, 10.0, 43.5, 100)
        self.assertEqual(pet, 0.0)

    def test_aggregate_and_lta(self):
        daily = []
        for i in range(31):
            daily.append({
                "date": date(2020, 1, i + 1),
                "P_m_per_d": 0.002,
                "T_C": 0.0,
                "PET_m_per_d": 0.001,
                "source": "test",
            })
        for i in range(29):
            daily.append({
                "date": date(2020, 2, i + 1),
                "P_m_per_d": 0.004,
                "T_C": 2.0,
                "PET_m_per_d": 0.002,
                "source": "test",
            })
        monthly = aggregate_monthly(daily)
        self.assertEqual(len(monthly), 2)
        self.assertAlmostEqual(monthly[0]["P_m_per_d"], 0.002)
        self.assertAlmostEqual(monthly[1]["P_m_per_month"], 0.004 * 29, places=6)
        lta = long_term_average(daily)
        self.assertAlmostEqual(lta["P_m_per_d"], (0.002 * 31 + 0.004 * 29) / 60, places=6)
        clim = monthly_climatology(daily)
        self.assertIn("1", clim)
        self.assertIn("2", clim)


class MutForcing(unittest.TestCase):
    def test_net_recharge_clamp(self):
        net, clamped = net_recharge_m_per_d({"P_m_per_d": 0.001, "PET_m_per_d": 0.002})
        self.assertEqual(net, 0.0)
        self.assertTrue(clamped)
        net, clamped = net_recharge_m_per_d({"P_m_per_d": 0.0025, "PET_m_per_d": 0.0018})
        self.assertAlmostEqual(net, 0.0007)
        self.assertFalse(clamped)

    def test_constant_arcascii(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = write_constant_arcascii(
                Path(tmp) / "NetRecharge.asc",
                xmin=0.0,
                ymin=0.0,
                xmax=1000.0,
                ymax=500.0,
                value=0.0007,
            )
            text = path.read_text(encoding="utf-8")
            self.assertIn("ncols 2", text)
            self.assertIn("0.00070000", text)

    def test_average_instructions(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ClimateForcing.instructions"
            # P=0.0025, PET=0.0018 → net=0.0007; with CLN footprint reduce SWF rate
            a_mesh = 1.0e8
            a_cln = 1.0e6
            swf = 0.0007 * (a_mesh - a_cln) / a_mesh
            write_climate_forcing_instructions(
                path,
                mode="average",
                monthly=[],
                lta={"P_m_per_d": 0.0025, "PET_m_per_d": 0.0018},
                include_swf=True,
                outlet_rel="..\\..\\2_mesh\\R\\R.nchos.Stream level 1",
                observed_notes=["Observed outlet Q: ECCC 02GA003"],
                include_cln=True,
                net_rate=0.0007,
                swf_rate=swf,
                net_asc_rel="NetRecharge.asc",
                a_mesh_m2=a_mesh,
                a_cln_m2=a_cln,
            )
            text = path.read_text(encoding="utf-8")
            self.assertIn("stress period", text)
            self.assertIn("TR", text)
            self.assertIn("3000.0", text)
            self.assertNotIn("\n    SS\n", text)
            self.assertIn("cln gstr", text)
            self.assertIn("Rain_CLN", text)
            self.assertIn("NetRecharge.asc", text)
            self.assertIn("swf recharge", text)
            self.assertIn(f"{swf:.8f}", text)
            self.assertNotIn("gwf evt", text)
            # SWF RCH must be the reduced rate, not full P (P may still appear in comments)
            self.assertRegex(text, r"swf recharge\n\s+0\.00069300\n")
            self.assertIn("swf critical depth", text)
            self.assertIn("Outlet", text)
            self.assertIn("Observed outlet Q", text)

    def test_average_swf_only(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ClimateForcing.instructions"
            write_climate_forcing_instructions(
                path,
                mode="average",
                monthly=[],
                lta={"P_m_per_d": 0.0025, "PET_m_per_d": 0.0018},
                include_swf=True,
                outlet_rel=None,
                observed_notes=[],
                include_cln=False,
                net_rate=0.0007,
                swf_rate=0.0007,
            )
            text = path.read_text(encoding="utf-8")
            self.assertIn("0.00070000", text)
            self.assertNotIn("cln gstr", text)
            self.assertNotIn("gwf evt", text)

    def test_transient_monthly_instructions(self):
        monthly = [
            {"year": 2020, "month": 1, "year_month": "2020-01", "days": 31, "P_m_per_d": 0.001, "PET_m_per_d": 0.0005, "T_C": 0.0, "source": "t"},
            {"year": 2020, "month": 2, "year_month": "2020-02", "days": 29, "P_m_per_d": 0.002, "PET_m_per_d": 0.0008, "T_C": 1.0, "source": "t"},
        ]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ClimateForcing.instructions"
            write_climate_forcing_instructions(
                path,
                mode="transient",
                monthly=monthly,
                lta={},
                include_swf=True,
                outlet_rel=None,
                observed_notes=[],
            )
            text = path.read_text(encoding="utf-8")
            self.assertEqual(text.count("end stress period"), 2)
            self.assertIn("TR", text)
            self.assertIn("31.0", text)
            self.assertIn("29.0", text)
            self.assertIn("0.00100000", text)
            self.assertIn("0.00200000", text)

    def test_precipitation_rts(self):
        daily = [
            {"date": date(2020, 1, 1), "P_m_per_d": 0.0, "T_C": 0.0, "PET_m_per_d": 0.0, "source": "t"},
            {"date": date(2020, 1, 2), "P_m_per_d": 0.01, "T_C": 0.0, "PET_m_per_d": 0.0, "source": "t"},
        ]
        with tempfile.TemporaryDirectory() as tmp:
            path = write_precipitation_rts(Path(tmp) / "Precipitation.rts", daily)
            lines = path.read_text(encoding="utf-8").strip().splitlines()
            self.assertEqual(len(lines), 2)
            self.assertTrue(lines[1].endswith("0.01000000"))


class FlowsHelpers(unittest.TestCase):
    def test_nearest_gauge(self):
        from core.flows import Outlet, _nearest

        outlet = Outlet("test", -80.3, 43.1)
        stations = [
            {"id": "A", "name": "far", "lon": -81.0, "lat": 44.0},
            {"id": "B", "name": "near", "lon": -80.31, "lat": 43.11},
        ]
        match = _nearest(outlet, stations, max_km=25.0)
        self.assertIsNotNone(match)
        self.assertEqual(match["id"], "B")
        self.assertLess(match["distance_km"], 5.0)


if __name__ == "__main__":
    unittest.main()
