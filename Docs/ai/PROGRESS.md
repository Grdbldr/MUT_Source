# MUT — Current progress

**Last updated:** 2026-08-20
**Version in code:** 2025.026 (`GeneralRoutines.f90`)
**Version in User's Guide:** 2025.026 (title page and `Modifications.tex`)
**Active plan:** none

## Done recently

- Dropped `MUT_Batch` / `Run.MUTBatch`. Verification folders are listed in `VerificationFolder.List` (Examples-Release). `mut_verify` runs `mut _build` / `usgs_1` / `mut _post` per listed folder. User's Guide Appendix D how-to is durable `VerifyRelease.tex`; `ReleaseComparison.tex` remains generated results.
- `{domain}_scatter.lay` overlay frames (OBS scatter, CHD, …) hide 3-D axes (`ShowAxis = No`) so tick labels do not cover the back `{domain} CELLS` frame, which is the only scatter frame that keeps axes on.
- `{domain}_Variables.lay` zone frames (e.g. GWF Zone) hide the mesh (`ShowMesh = No`); contour flood and shade stay on. The dedicated `{domain}_Mesh.lay` overlay still shows the mesh.
- Volume budget is `GWF_VolumeBudget.lay` (full-page XY, line-map legend, all rate terms except percent discrepancy), not a frame on `GWF_Results.lay`. Dossier figure and User's Guide list the dedicated layout.
- `SWF_Infiltration.lay`: flux converted to **mm/year** (365.25-day year); colour map distribution is always **Banded** (Diverging Blue/Red, zero-centred levels).
- `GWF_SaturationSlices.lay` reverses Sequential-Viridis (`ColorMapFilter ReverseColorMap = Yes`) so high saturation is purple and low is yellow.
- Contour legend header uses 2018 `HeaderTextShape` (not `Legend.Header`, which 2018 R2 rejects as an invalid macro option). 8 pt Helvetica bold axis titles/labels (`SizeUnits = Point`) on X/Y/Z and XY; legend numbers Integer when readable wholes, two decimals for saturation, else BestFloat precision 2. 3-D frames still use 3× Z exaggeration and factory XYZ view.
- SWF post schema fallback (`tecplot_io._post_variables`) now includes `SWF to GWF` and `SWF to GWF (areal flux)` so `SWF_Infiltration.lay` is still written when the SZL name scrape fails. Abdul SZL already carries those names in the file tail; dossier layouts from an older 9-name schema omitted the infiltration map.
- `GWF_SaturationSlices.lay`: `ShowSlices` / `ShowGroup` / `ShowPrimarySlice` on; primary at mid-$X$; start/end inset 10% from GSF $X$ extents; five intermediate slices (Tecplot factory start/end of 0.05/0.95 clustered all planes at $X{\approx}0$ on real models). Factory XYZ view (`PsiAngle=60`, `ThetaAngle=240`) and contour legend top-left.
- `GWF_WaterTable.lay`: iso-surface is contour group 2 = Pressure Head at 0 (new variable index = nvars+1 after AlterData). SZL variable names are CR-separated in the file tail (`GWF to STORAGE` is var 10 on Abdul; a 9-name schema had bound the iso-surface to that CBB field). Zone `ShowIsosurfaces = Yes`; 3× vertical exaggeration; factory XYZ view and contour legend top-left.
- SZPLT replace while Tecplot has the file mapped: `TecIO_DeleteIfExists` no longer skips a locked `.tecplot.szplt` (that path leaked the Fortran unit and surfaced as `tecend142: TecIO error -1` / `Cannot write to file`). It now stops with a message to close the dataset in Tecplot 360. Reproduced on Abdul `_build` with `tec360` holding `_buildo.Modflow.GWF.tecplot.szplt`.
- Default-on model documentation: after a successful `mut _build` or `mut _post`, MUT runs `%USERBIN%\mut_document\mut_document.py --skip-export` unless that MUT file contains `no model documentation` (build only / post only / both / neither). Missing Python, the script, or pdflatex is a `WarnMsg`; MUT still ends with `Normal exit`. Manual `python %USERBIN%\mut_document\mut_document.py` remains for PNG export.
- Release-verification appendix: `Tools/verify_release.ps1` runs folders listed in `VerificationFolder.List` in `C:\Work\Examples-Release` against `C:\Work\Examples-Base`, exports `BatchComparisonPlots.lay` frames, writes `ReleaseComparison.tex` (User's Guide Appendix D results), then publishes selected inputs with `ToRepos.bat`. Never copies MUT_Examples → Examples-Release. Versus 2025.012: Abdul matches bit-for-bit once IN–OUT is compared index-wise (duplicate TOTAL TIME dumps at SP boundaries were a false positive). `3_0_SWF_CHD` and `3_1_CLN_for_SWF` differ because 2025.014 writes CLN/SWF CHD as start→end head ramps; `3_SWF` (critical depth) is unchanged.
- Default FE Tecplot is one TecIO SZL `.tecplot.szplt` per dataset (`FileFormat=1`). `_post` writes a single file with all output times (later zones share XYZ, four static cell fields, and connectivity from zone 1; `tecflush142` retains zone 1 via a by-reference wrapper because vendor `tecio.f90` incorrectly marks the first argument `VALUE`). Velocity is `_posto.*.Velocity.tecplot.szplt` sharing XYZ+connectivity. Opt-in `write ascii tecplot output` restores `.tecplot.dat`. Classic PLT grid+sol writers removed.
- Binary Tecplot `_post` (Abdul prism): 2018 R2 solution files may contain only one solution time — superseded by SZPLT multi-time files in 2025.024
- Velocity Tecplot (`DomainVelocityBinaryToTecplot`): under `nodal control volumes`, do not mark Head/Vx/Vy/Vz as `CELLCENTERED` (values are nodal; E is FE element count) — fixes Tecplot “Bad or Missing FE Cell Indices”
- Original SWF velocity option: MUT instruction `original swf velocity calculation` writes VEL `OPTIONS` / `ORIGINAL_SWF_VELOCITY`; usgs_1 default face-flux and HGS Manning paths both use `SWF_THIK` (smoothed flow depth), not depth minus depression/obstruction
- GWF_Results.lay "Unexpected End of File in Datafile header": Tecplot 2018 `$!ReadDataSet '"file.szplt"'` uses the ASCII/PLT loader (`#!SZPLT` is treated as a comment). Layouts now load SZL with `STANDARDSYNTAX` / `FILELIST_DATAFILES` and `DataSetReader = 'Tecplot Subzone Data Loader'`. Volume-budget ASCII still needs trimmed records (4000-char Fortran padding).
- Mesh page (`04_mesh.lay`): overlapping linked GWF/SWF frames, zone flood + `CustomLabels_*.dat` legends (Abdul `_build.lay` pattern)
- Per-domain Tecplot layouts: `{domain}_Mesh.lay`, `{domain}_Variables.lay`, `{domain}_scatter.lay`, `{domain}_Results.lay`, `{domain}_Observations.lay` (2018 EX cannot use `$!PAGECONTROL`; scatter layouts use 1.0-size spheres, coloured by the optional column; observations: square frames, Head top / Saturation-or-Depth bottom, sites A–Z, extra columns off-view)
- `Tools/mut_document/` — stdlib Python dossier generator (`mut_document.py`) writes `Docs/<folder>.pdf` plus sectioned Tecplot layouts after `mut _build`; optional `tec360` PNG export and inclusion of `usgs_1`/`mut _post` mass-balance results
- User's Guide documents velocity vectors (`ModelExecution.tex` §Velocity vectors): separate `_posto.*.Velocity.tecplot.szplt`, `Darcy Vx/Vy/Vz` and `Average Linear Vx/Vy/Vz`, 2018 `$!GlobalThreeDVector`, `{domain}_Results.lay` frame. Dossier Velocity section lists those files and no longer captions `GWF_Results.png` as "Darcy velocity".
- `mut_document` derived 3-D views: `GWF_WaterTable.lay` (Pressure Head = Head − z Cell, iso-surface at 0), `SWF_Infiltration.lay` (SWF-to-GWF flux in mm/hour, diverging Blue/Red legend centred on zero), `GWF_SaturationSlices.lay` when those post fields exist. Dossier chapter *Simulation results* has matching subsections; PNG export may SaveLayout-pass SZL files to set ±L.
- Agent Markdown layout added: `AGENTS.md`, `Docs/ai/*`, `.cursor/rules/mut-fortran.mdc`

## In progress

- None tracked here

## Next up

- `verify_release.ps1` does not call `mut_document` itself; default-on MUT will now write `Docs/` in Examples-Release (`--skip-export`). Add `no model documentation` to example mut files only if those folders should stay without `Docs/`
- Prefer `Save to Workspace` for new Cursor plans so they land in `.cursor/plans/`

## Blockers / external

- Velocity post (`mut _post` `Vx/Vy/Vz`) needs a USG-Transport build that recognizes NAM file type `VEL`
- Original SWF velocity option needs a usgs_1 build that reads VEL `ORIGINAL_SWF_VELOCITY`
- GSTR runtime is in USG-Beta; MUT only writes inputs and Tecplot of applied rates
- Model-dossier PNG figures need `tec360` on PATH; PDF still builds without it
- Release-verification PNGs need `tec360` on PATH; appendix TeX still writes without figures

## Test case to use

- Model documentation: `C:\_repo\Work_v2\KURT_Model\2_models\1_Regional_Boundary_Average_Rainfall`
- GSTR: `C:\_repo\Work_v2\KURT_Model\` (look for `4_GSTR` / `_build.mut` under transient-rainfall models)
- Deployed MUT: `C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\mut.exe`
- Release verification: `C:\Work\Examples-Release` vs `C:\Work\Examples-Base`
