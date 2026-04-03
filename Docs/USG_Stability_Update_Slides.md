# USG-TRANSPORT Robustness Update

Version label: `USG-TRANSPORT VERSION 2.7.0rgm`  
Codebase: `c:\_repo\Sorab\USG-Beta`  
Date: 2026-04-02

---

## Slide 1 - Title

**USG-TRANSPORT Robustness Update**  
`USG-Beta` stabilization summary (`2.7.0rgm`)

- Scope: CLN/SWF/GW coupling and solver stability
- Basis: current `git` working-tree deltas
- Outcome: target case now runs successfully

**Speaker notes**
- This update addresses a cluster of runtime failures observed during startup and first time-step processing.
- Changes are concentrated in sparse matrix assembly, SWF indexing, and CLN/SWF budget/output routines.

---

## Slide 2 - Starting Symptoms

- Invalid CSR / `IA`-`NJA` structural failure
- SWF out-of-bounds (`ASWFNDS` indexing)
- CLN/SWF budget writer aborts
- Zero-size `BUFF` runtime bounds fault

**Speaker notes**
- These were independent-looking errors but tied to a few common structural assumptions in matrix/connectivity and output code paths.

---

## Slide 3 - Root-Cause Themes

- Sparse rows could be empty or zero-capacity
- SWF loops used full `NEQS` in mixed-equation contexts
- Budget routines assumed one adjacency orientation only
- Output code dereferenced arrays even when node counts were zero

**Speaker notes**
- Fix strategy focused on safe defaults, bounds enforcement, and dual-orientation lookup for connectivity.

---

## Slide 4 - Fix Group 1: CSR Hardening

- Enforce `ROWMAXNNZ >= 1`
- Add `ensure_no_empty_rows()` with diagonal insertion
- Call empty-row normalization before `FILLIAJA`
- Add row/column bounds checks in sparse insert path

Files: `glo2basu1.f`, `sparse.f`

**Speaker notes**
- This directly prevents malformed CSR and downstream solver setup crashes.

---

## Slide 5 - Fix Group 2: SWF Bounds Corrections

- Correct SWF local index: `N-NODES-NCLNNDS`
- Restrict SWF loops to SWF equation block
- Add defensive checks around `NJA/NJAS` mapped indices

File: `glo2sms-u1.f` (plus safety guards in `swf2basu1.f`)

**Speaker notes**
- Prevents accessing `ASWFNDS` and matrix mappings outside valid bounds in mixed GW/CLN/SWF/GNC systems.

---

## Slide 6 - Fix Group 3: Budget Writer Stability

- If `(ND1,N)` not found, fallback to `(N,ND1)`
- Preserve sign consistency in fallback rate assignment
- Normalize GW-side inactive checks via `IBOUND(N)`
- Correct SWF routine error text label

Files: `gwf2bcf-lpf-u1.f`, `cln2basu1.f`, `swf2basu1.f`

**Speaker notes**
- This removes false fatal exits caused by connectivity orientation differences.

---

## Slide 7 - Fix Group 4: Zero-Node Output Guards

- Early return when `NCLNNDS<=0` in CLN H/D/F/IB output routines
- Early return when `NSWFNDS<=0` in SWF H/D/F/IB output routines
- Eliminates `BUFF(1)` on zero-length array cases

Files: `cln2basu1.f`, `swf2basu1.f`

**Speaker notes**
- This is the direct fix for the observed Fortran bounds exception in output handling.

---

## Slide 8 - Additional Changes

- `xmd.f`: strict `xmdcheck` bypassed for edge reduced-map behavior
- `gwf2lak7u1.f`: `G10.5 -> G12.5` format-width fix
- `mfusg.f`: version string now `USG-TRANSPORT VERSION 2.7.0rgm`

**Speaker notes**
- These are lower-risk but useful cleanup and compatibility improvements.

---

## Slide 9 - Changed Files and Size

Major deltas:

- `glo2sms-u1.f` `+59/-11`
- `swf2basu1.f` `+41/-7`
- `gwf2bcf-lpf-u1.f` `+30/-7`
- `sparse.f` `+23/-1`
- `cln2basu1.f` `+18/-3`
- Plus: `glo2basu1.f`, `xmd.f`, `gwf2lak7u1.f`, `mfusg.f`

**Speaker notes**
- Most functional impact is in the top four files by delta size.

---

## Slide 10 - Validation and Next Steps

- Target model now runs:  
  `c:\_repo\Work_v2\KURT_Model\2_models\1TEST_Regional_Boundary_Average_Rainfall_SHAFT`
- Recommended regression:
  - GW-only, GW+CLN, GW+SWF, GW+CLN+SWF
  - With and without GNC
  - Zero-node edge-case package configs
- Prepare commit and tagged release note

**Speaker notes**
- Suggest turning this into a patch release with explicit stability-focused messaging.

---

## Optional Conversion to PPTX

If Pandoc is installed, convert this file to PowerPoint:

`pandoc "Docs/USG_Stability_Update_Slides.md" -t pptx -o "Docs/USG_Stability_Update_Slides.pptx"`
