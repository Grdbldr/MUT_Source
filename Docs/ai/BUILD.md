# MUT — Build and deploy

## Fortran executable

- Solution: `Modflow-User-Tools.sln` (Intel Fortran in Visual Studio)
- Project: `Modflow-User-Tools.vfproj`
- Configurations: `x64 Debug` | `x64 Release` (Win32 configs also exist)
- Output: `x64\Debug\Modflow-User-Tools.exe` or `x64\Release\Modflow-User-Tools.exe`
- Libraries: Intel Fortran (`ifport`, `ifwin`); Tecplot `tecio.lib` via `TEC360INCLUDE` / `TEC360LIB` (classic 142 API; SZL `FileFormat=1`)

## Deploy

| Config | Script | Destinations |
|--------|--------|----------------|
| Debug | `post_build.bat` | `%USERBIN%\mut.exe` and `C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\mut.exe` |
| Release | `post_buildR.bat` | same |

## User's Guide PDF

- Script: `Docs/build_mut_guide.bat`
- Main file: `Docs/User's Guide/MUT User's Guide.tex`
- Toolchain: MiKTeX — `pdflatex`, `makeindex`, then `pdflatex` twice more
- Changelog: `Docs/User's Guide/Modifications.tex`
- Cover version/date: title page in `MUT User's Guide.tex`

Do not treat generated `.aux` / `.toc` / `.idx` / `.pdf` as source of truth.

## Release verification appendix

- Command (from MUT_Source root, after deploying `mut.exe`): `.\Tools\verify_release.ps1`
- Working tree: `C:\Work\Examples-Release` (`VerificationFolder.List` folders only; extra problem-set subfolders are ignored)
- Baseline: `C:\Work\Examples-Base`
- **Never** copy MUT_Examples → Examples-Release. After a successful run the script calls `ToRepos.bat` so Robocopy publishes selected inputs into `C:\_repo\GrdBldr\MUT_Examples`
- Flags: `-SkipBatch`, `-SkipExport`, `-SkipPdf`, `-SkipToRepos`
- Appendix: `Docs/User's Guide/VerifyRelease.tex` (how to run) plus generated `ReleaseComparison.tex` and PNGs under `Imagery/verification/`
- Commit MUT_Source (appendix) and MUT_Examples (published inputs) separately; do not auto-push. Do not auto-promote Examples-Release → Examples-Base

## USG simulator (external)

- Solution: `c:\_repo\Sorab\USG-Beta\MF2K5_USGs_1\USGs_1\USGs_1.sln` (project `USGs_1.vfproj`)
- Configurations: `x64 Debug` → `post_build.bat`; `x64 Release` → `post_buildR.bat`
- Deploy: `%USERBIN%\USGS_1.exe` and `C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\USGS_1.exe` (same destinations as `mut.exe`)
- Package changes (GSTR, VEL, …) land in USG-Beta first
- MUT writes NAM/OC lines; USG must recognize new file types (e.g. NAM `VEL`)
- `mut _post` reads USG velocity binaries and writes Tecplot arrays; it does not compute Darcy velocity itself

## Tecplot

- Module: `Tecplot.f90` (TecIO `tecini142` / `teczne142` / `tecdatd142` / `tecnode142` / `tecflush142`)
- Default one full SZL `.tecplot.szplt` per FE dataset; instruction `write ascii tecplot output` writes `.tecplot.dat`
- Classic PLT grid+sol writers removed in 2025.024; SZPLT is the default FE Tecplot format
- Post-process FE domains: one `.tecplot.szplt` with all output times (later zones share XYZ, static cell fields, and connectivity from zone 1); velocity is a separate `.Velocity.tecplot.szplt`
- Convention: sequential 1-D arrays `Vx`, `Vy`, `Vz` per domain
- Build-time visualization: `VisualizeBuild.tex`
- Post-process: `ModelExecution.tex` / `_post` path

## QGIS

- Documented in `Docs/User's Guide/QGIS_Useage.tex` — not built by MUT
- MUT side: ArcASCII rasters (GSTR, elevations) and shape-derived selections
