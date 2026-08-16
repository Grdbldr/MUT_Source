# MUT — Current progress

**Last updated:** 2026-08-16
**Version in code:** 2025.022 (`GeneralRoutines.f90`)
**Version in User's Guide:** 2025.022 (title page and `Modifications.tex`)
**Active plan:** none (2025.020/2025.021 appendix ownership locked; OC sample synced to 2025.022)

## Done recently

- Velocity Tecplot (`DomainVelocityBinaryToTecplot`): under `nodal control volumes`, do not mark Head/Vx/Vy/Vz as `CELLCENTERED` (values are nodal; E is FE element count) — fixes Tecplot “Bad or Missing FE Cell Indices”
- Original SWF velocity option: MUT instruction `original swf velocity calculation` writes VEL `OPTIONS` / `ORIGINAL_SWF_VELOCITY`; usgs_1 default face-flux and HGS Manning paths both use `SWF_THIK` (smoothed flow depth), not depth minus depression/obstruction
- `Tools/mut_document/` — stdlib Python dossier generator (`mut_document.py`) writes `Docs/<folder>.pdf` plus sectioned Tecplot layouts; deployed via `post_build.bat` / `post_buildR.bat`
- Mesh page (`04_mesh.lay`): overlapping linked GWF/SWF frames, zone flood + `CustomLabels_*.dat` legends (Abdul `_build.lay` pattern)
- Per-domain Tecplot layouts: `{domain}_Mesh.lay`, `{domain}_Variables.lay`, `{domain}_scatter.lay`, `{domain}_Results.lay`, `{domain}_Observations.lay` (2018 EX cannot use `$!PAGECONTROL`; scatter layouts use 1.0-size spheres, coloured by the optional column; observations: square frames, Head top / Saturation-or-Depth bottom, sites A–Z, extra columns off-view)
- User's Guide: `ModelExecution.tex` documentation section; `Modifications.tex` / title page aligned to **2025.022**
- Agent Markdown layout added: `AGENTS.md`, `Docs/ai/*`, `.cursor/rules/mut-fortran.mdc`

## In progress

- None tracked here

## Next up

- Prefer `Save to Workspace` for new Cursor plans so they land in `.cursor/plans/`

## Blockers / external

- Velocity post (`mut _post` `Vx/Vy/Vz`) needs a USG-Transport build that recognizes NAM file type `VEL`
- Original SWF velocity option needs a usgs_1 build that reads VEL `ORIGINAL_SWF_VELOCITY`
- GSTR runtime is in USG-Beta; MUT only writes inputs and Tecplot of applied rates
- Model-dossier PNG figures need `tec360` on PATH; PDF still builds without it

## Test case to use

- Model documentation: `C:\_repo\Work_v2\KURT_Model\2_models\1_Regional_Boundary_Average_Rainfall`
- GSTR: `C:\_repo\Work_v2\KURT_Model\` (look for `4_GSTR` / `_build.mut` under transient-rainfall models)
- Deployed MUT: `C:\_repo\Grdbldr\MUT_Examples\_MUT_USERBIN\mut.exe`
