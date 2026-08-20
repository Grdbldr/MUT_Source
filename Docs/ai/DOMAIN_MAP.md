# MUT — Domain map

Navigation for agents. User-facing prose lives in TeX; this table only points at owners.

## Entry points

| Role | File | Notes |
|------|------|--------|
| Program | `pre.f90` | `program PreMUT` → `OpenMUT` / `ProcessMUT` / `CloseMUT` |
| Orchestrator | `TGModule.f90` (`MUT`) | Build vs post-process commands |
| Facade | `Modflow_USG.f90` (`MUSG`) | Large module; still the writer/coordinator |
| Core types | `MUSG_Core.f90` | `cell`, `ModflowDomain`, `ModflowProject`, `GSTRInstance` |
| Instruction DSL | `MUSG_InstructionParser.f90` | Routes `_build.mut` verbs to domain modules |

## Feature owners

| Topic | Fortran | USG / output | User's Guide | Notes |
|-------|---------|--------------|--------------|-------|
| Instruction DSL | `MUSG_InstructionParser.f90` | — | `ModelBuild.tex` | `_build.mut` verbs |
| Mesh / GSF | `MUSG_Mesh.f90`, `MeshGeneration.f90`, `NumericalMesh.f90`, `GrdBldr.f90` | GSF | `TemplateMesh.tex` | Grid Builder + template mesh |
| Cell selection | `MUSG_Selection.f90` | — | `ModelBuild.tex` | Chosen-cell bit flags |
| Material DBs | `MUSG_Database.f90`, `Materials.f90` | SMS, GWF/CLN/SWF/ET CSV | `Excel.tex` | Sparse IDs allowed |
| Material assign | `MUSG_MaterialProperties.f90` | LPF/BCF, CLN, SWF props | `GWF.tex`, `CLN.tex`, `SWF.tex` | GWF porosity for ALV; CLN `InfillPorosity` |
| BCs | `MUSG_BoundaryConditions.f90` | CHD, DRN, RCH/RTS, WEL, … | `GWF.tex`, `Recharge.tex` | RTS zone merge; `chd zone name` |
| GSTR | `MUSG_Core.f90` (`GSTRInstance`), `raster.f90` | GSTR in USG-Beta | `GSTR.tex` | ArcASCII snapshots; named instances on GWF/CLN/SWF |
| ICs | `MUSG_InitialConditions.f90` | STRT | domain chapters | |
| Stress periods | `MUSG_StressPeriods.f90` | DIS / TDIS | `StressPeriods.tex` | |
| OC / VEL | `MUSG_OutputControl.f90` | OC, NAM `VEL`; USG-Beta `glo2velu1.f` | `OutputControl.tex`, `SWF.tex` | `SAVE DARCY/LINEAR VELOCITY`; optional VEL `ORIGINAL_SWF_VELOCITY` |
| Observations | `MUSG_ObservationPoints.f90` | OBS | `ObservationPoints.tex` | |
| CLN intersection | `CLNIntersection.f90` | CLN structure / `.xyzList` | `CLN.tex` | Split CLN at mesh faces |
| Tecplot | `Tecplot.f90` | `.szplt` / `.dat` | `ModelExecution.tex`, `VisualizeBuild.tex` | Sequential 1-D `Vx`, `Vy`, `Vz`; default TecIO SZL (one full file; post shares later zones from zone 1) |
| Model dossier PDF | `Tools/mut_document/` | `Docs/<folder>.pdf`, `Docs/layouts/*.lay` | `ModelBuild.tex`, `ModelExecution.tex` | Default after `mut _build` / `_post` (`--skip-export`); `no model documentation` skips that run; optional `GWF_VolumeBudget` / `GWF_WaterTable` / `SWF_Infiltration` / `GWF_SaturationSlices` layouts |
| Release verification | `Tools/mut_verify/`, `Tools/verify_release.ps1` | `C:\Work\Examples-Release` vs `Examples-Base` | `VerifyRelease.tex`, `ReleaseComparison.tex` (appendix) | `VerificationFolder.List` only; publish selected inputs with `ToRepos.bat`; never copy MUT_Examples → Examples-Release |
| Rasters | `raster.f90` | ArcASCII | `QGIS_Useage.tex`, `GSTR.tex` | DEM / GSTR snapshots |
| Version / I/O | `GeneralRoutines.f90` | — | `Modifications.tex` | `MUTVersion` |
| QGIS | — (workflow, not a MUT package) | shapefiles, rasters | `QGIS_Useage.tex` | Appendix; MUT consumes rasters/shapes |

## Typical test-case layout (often outside this repo)

```
_model/
  _build.mut          ← MUT instructions
  _buildo.*           ← generated MODFLOW-USG inputs
  mut _post           ← Tecplot from USG binaries
```

Examples may live under `C:\Work\Examples-Release` (working tree), `C:\_repo\Grdbldr\MUT_Examples` (published subset via `ToRepos.bat`), or project trees such as `KURT_Model`.
