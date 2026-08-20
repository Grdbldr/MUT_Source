# MUT — Agent Instructions

## What this repo is

Fortran preprocessor for MODFLOW-USG. It reads `_build.mut` instructions, writes NAM/GSF and package files, and post-processes USG binaries (Tecplot, budgets). The executable is `mut.exe`.

## Read first

- [CODE_STANDARDS.md](CODE_STANDARDS.md) — Fortran style (`t_` types, `KindParameters`, error handling)
- [Docs/ai/DOMAIN_MAP.md](Docs/ai/DOMAIN_MAP.md) — which module owns GWF/CLN/SWF/GSTR/VEL/Tecplot
- [Docs/ai/BUILD.md](Docs/ai/BUILD.md) — Visual Studio build, deploy, User's Guide PDF
- [Docs/ai/PROGRESS.md](Docs/ai/PROGRESS.md) — current task and next steps

Do not treat [CODE_ANALYSIS.md](CODE_ANALYSIS.md) as current architecture; prefer `DOMAIN_MAP.md` and the `MUSG_*.f90` modules.

## Version discipline

- Source of truth: `MUTVersion` in [GeneralRoutines.f90](GeneralRoutines.f90) (DEBUG/RELEASE)
- User-facing changelog: [Docs/User's Guide/Modifications.tex](Docs/User's Guide/Modifications.tex) and the title page in [Docs/User's Guide/MUT User's Guide.tex](Docs/User's Guide/MUT User's Guide.tex)
- Keep these aligned on every release; do not bump Fortran without updating TeX

## Local commits

- When the user asks to commit, run [Tools/commit_local.ps1](Tools/commit_local.ps1) from the repo root
- Pass `-Message "…"` with a one-sentence summary of intent when the auto path-based summary is too vague
- The script appends ` - Version YYYY.nnn` from `MUTVersion` (does not bump the version)
- Local commit only; do not push unless the user explicitly asks
- Before a release commit, keep `MUTVersion`, title page, and Appendix C aligned

```powershell
.\Tools\commit_local.ps1
.\Tools\commit_local.ps1 -Message "Add GSTR and default VEL package wiring"
```

## Documentation policy

- User instructions → edit TeX under `Docs/User's Guide/` (`GWF.tex`, `CLN.tex`, `SWF.tex`, `GSTR.tex`, …)
- Rebuild PDF: [Docs/build_mut_guide.bat](Docs/build_mut_guide.bat) (pdflatex ×3 + makeindex, MiKTeX)
- Release verification appendix: [Tools/verify_release.ps1](Tools/verify_release.ps1) — from MUT_Source; `C:\Work\Examples-Release` (`VerificationFolder.List` only); never copy MUT_Examples → Examples-Release; publish selected inputs with `ToRepos.bat`. How-to: `Docs/User's Guide/VerifyRelease.tex`
- Do not duplicate User's Guide prose in Markdown

## External dependencies

- Simulator: USG-Beta (separate repo) — new packages (GSTR, VEL) are implemented there first
- Deploy: [post_build.bat](post_build.bat) / [post_buildR.bat](post_buildR.bat) → `%USERBIN%\mut.exe` and `MUT_Examples\_MUT_USERBIN`
- Test cases: work in `C:\Work\Examples-Release`; published subset is `MUT_Examples` (via `ToRepos.bat`). Other trees such as `KURT_Model` are outside this repo.

## Domain packages (MODFLOW-USG)

| Domain | MUT focus | User's Guide |
|--------|-----------|--------------|
| GWF | LPF/BCF, porosity, RCH/RTS/GSTR | `GWF.tex`, `GSTR.tex` |
| CLN | structure file, infill porosity | `CLN.tex` |
| SWF | RTS zones; optional `original swf velocity calculation` (VEL `ORIGINAL_SWF_VELOCITY`; both paths use `SWF_THIK`) | `SWF.tex` |
| Post | Default SZL `.tecplot.szplt` (`Vx/Vy/Vz`, GSTR rates); opt-in `write ascii tecplot output`; default `Docs/` dossier unless `no model documentation` | `OutputControl.tex`, `ModelExecution.tex` |
| GIS | shapefile/raster workflows | `QGIS_Useage.tex` (appendix) |

## Agent behavior

- Prefer minimal diffs; match existing `MUSG_*` module patterns
- Use `KindParameters` (`i4`, `dp`); `implicit none` everywhere
- Use `WarnMsg` / `ErrMsg` / `HandleError`, not bare `STOP`
- New MUSG features: parser entry in `MUSG_InstructionParser.f90`, writer in `Modflow_USG.f90` or the domain module
- When implementing a planned feature: follow `.cursor/plans/*.plan.md`; do not edit the plan file during execution unless explicitly revising the plan
- After substantive work: update [Docs/ai/PROGRESS.md](Docs/ai/PROGRESS.md)
