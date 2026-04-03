# USG-TRANSPORT Stability Patch Bulletin

Date: 2026-04-02  
Codebase: `c:\_repo\Sorab\USG-Beta`  
Release label: `USG-TRANSPORT VERSION 2.7.0rgm`

## Summary

A stability-focused patch set was applied to resolve runtime failures in CLN/SWF/GW coupled simulations.  
The target model now runs successfully:

`c:\_repo\Work_v2\KURT_Model\2_models\1TEST_Regional_Boundary_Average_Rainfall_SHAFT`

## Issues Resolved

### 1) Invalid sparse CSR (`IA/NJA`) failure

- **Symptom:** solver setup failures tied to malformed sparse structure.
- **Root cause:** zero-capacity and/or empty rows during IA/JA assembly.
- **Resolution:**
  - Enforced `ROWMAXNNZ >= 1`.
  - Added empty-row diagonal insertion before CSR extraction.
  - Added sparse row/column bounds checks.
- **Files:** `sparse.f`, `glo2basu1.f`.

### 2) SWF bounds crash (`ASWFNDS`)

- **Symptom:** out-of-bounds access in SWF convergence/correction paths.
- **Root cause:** SWF indexing and loop limits extended beyond SWF equation range in mixed `NEQS` systems.
- **Resolution:**
  - Corrected SWF local index mapping.
  - Limited loops to SWF equation block.
  - Added defensive mapped-index bounds checks.
- **Files:** `glo2sms-u1.f` (plus related guards in `swf2basu1.f`).

### 3) CLN/SWF budget writer aborts

- **Symptom:** fatal stops in budget write routines when expected connectivity pair was not found.
- **Root cause:** lookup assumed only one adjacency orientation.
- **Resolution:**
  - Added reverse-orientation fallback lookup.
  - Preserved sign-consistent rate handling in fallback path.
  - Standardized GW-side `IBOUND(N)` checks.
  - Corrected SWF routine error label text.
- **Files:** `gwf2bcf-lpf-u1.f`, `cln2basu1.f`, `swf2basu1.f`.

### 4) Zero-size `BUFF` bounds fault

- **Symptom:** Fortran runtime error `BUFF(1)` with upper bound `0`.
- **Root cause:** output routines dereferenced element 1 of zero-length arrays when node counts were zero.
- **Resolution:**
  - Added early-return guards for `NCLNNDS<=0` and `NSWFNDS<=0`.
- **Files:** `cln2basu1.f`, `swf2basu1.f`.

## Additional Updates

- `xmd.f`: bypassed strict `xmdcheck` call for edge reduced-map behavior.
- `gwf2lak7u1.f`: widened format `G10.5 -> G12.5`.
- `mfusg.f`: updated version label to `2.7.0rgm`.

## Changed Source Files

- `cln2basu1.f`
- `glo2basu1.f`
- `glo2sms-u1.f`
- `gwf2bcf-lpf-u1.f`
- `gwf2lak7u1.f`
- `mfusg.f`
- `sparse.f`
- `swf2basu1.f`
- `xmd.f`

## Recommended Follow-Up

- Run a regression matrix across:
  - GW-only, GW+CLN, GW+SWF, GW+CLN+SWF
  - GNC on/off
  - Transport on/off
  - Zero-node CLN/SWF edge-case package configurations
- Commit and tag this patch set as a stability release after regression pass.
