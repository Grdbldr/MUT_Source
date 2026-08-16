# MUT — Build and deploy

## Fortran executable

- Solution: `Modflow-User-Tools.sln` (Intel Fortran in Visual Studio)
- Project: `Modflow-User-Tools.vfproj`
- Configurations: `x64 Debug` | `x64 Release` (Win32 configs also exist)
- Output: `x64\Debug\Modflow-User-Tools.exe` or `x64\Release\Modflow-User-Tools.exe`
- Libraries: Intel Fortran (`ifport`, `ifwin`); Tecplot `tecio.lib` via `TEC360INCLUDE` / `TEC360LIB`

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

## USG simulator (external)

- Solution: `c:\_repo\Sorab\USG-Beta\MF2K5_USGs_1\USGs_1\USGs_1.sln` (project `USGs_1.vfproj`)
- Configurations: `x64 Debug` → `post_build.bat`; `x64 Release` → `post_buildR.bat`
- Deploy: `%USERBIN%\USGS_1.exe` and `C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\USGS_1.exe` (same destinations as `mut.exe`)
- Package changes (GSTR, VEL, …) land in USG-Beta first
- MUT writes NAM/OC lines; USG must recognize new file types (e.g. NAM `VEL`)
- `mut _post` reads USG velocity binaries and writes Tecplot arrays; it does not compute Darcy velocity itself

## Tecplot

- Module: `Tecplot.f90`
- Convention: sequential 1-D arrays `Vx`, `Vy`, `Vz` per domain
- Build-time visualization: `VisualizeBuild.tex`
- Post-process: `ModelExecution.tex` / `_post` path

## QGIS

- Documented in `Docs/User's Guide/QGIS_Useage.tex` — not built by MUT
- MUT side: ArcASCII rasters (GSTR, elevations) and shape-derived selections
