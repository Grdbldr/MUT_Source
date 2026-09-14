# KAERI Modelling Approach — Direction & 2026 Development

Style: Appendix J report slides (4:3, prose + figures)  
Version: MUT `2025.026`  
Date: 11 September 2026

Image path keys used by `Tools/build_kaeri_direction_pptx.py`.

---

## Slide 1 — Title

**KAERI Modelling Approach**  
Direction and 2026 development summary

Supporting lines:
- Integrated groundwater, surface-water and conduit flow for KURT
- MUT · MODFLOW-USG · QGIS · Tecplot
- Current MUT release: 2025.026

Layout: title

---

## Slide 2 — Purpose of the modelling stack

**Title:** Purpose — Supporting KURT engineering assessment

**Prose:**
The KAERI/KURT assessment needs a reproducible path from site GIS into a coupled flow model that can represent regional groundwater, overland response to rainfall, and engineered conduits or shafts. QGIS organises DEMs and spatial layers; MUT translates those into MODFLOW-USG inputs; usgs_1 solves the flow system; Tecplot and the model dossier communicate water levels, exchange fluxes and budgets for engineering review.

**Figure:** `UG/QGIS_ProjectDEM.png`  
**Caption:** Example project DEM prepared in QGIS for regional model construction.

Layout: left_text_right_fig

---

## Slide 3 — Coupled flow domains

**Title:** Coupled domains — Groundwater, surface water and conduits

**Prose:**
Engineering questions at KURT are rarely single-domain. Rainfall first wets the surface (SWF), infiltrates to the porous medium (GWF), and may interact with discrete conduits or shafts (CLN). The 2026 toolchain builds these domains on a shared mesh so exchange fluxes, heads and budgets can be examined together rather than as separate models stitched after the fact.

**Figure:** `UG/3_26_SWF_GWF.png`  
**Caption:** SWF surface cells overlying the GWF mesh — dual-domain exchange geometry.

Layout: left_text_right_fig

---

## Slide 4 — Rainfall and surface forcing

**Title:** Rainfall and surface forcing — Zones, transient recharge and controls

**Prose:**
Surface hydrology for engineering design needs spatially varying rainfall and controllable boundary heads. MUT now supports multi-zone SWF transient recharge (merged RTS columns and matching RCH zone maps), reports whether each stress period uses RCH or RTS, and writes CLN/SWF constant-head boundaries as start-to-end head ramps so engineered water levels can be imposed consistently through a simulation.

**Figure:** `UG/3_35_SWFRecharge.png`  
**Caption:** Applied SWF recharge mapped on the surface domain for review in Tecplot.

Layout: left_text_right_fig

---

## Slide 5 — Spatio-temporal recharge (GSTR)

**Title:** Spatio-temporal recharge — Raster snapshots for transient rain

**Prose:**
For storms and spatially migrating rainfall fields, General Spatio-Temporal Recharge (GSTR) assigns ArcASCII raster snapshots to named GWF, SWF or CLN instances and interpolates rates in time. Existing RCH/RTS packages remain available; when both are active the fluxes add. A KURT demonstration lives under the transient-rainfall GSTR test model. The figure shows applied recharge from the related RTS workflow used to evaluate surface response.

**Figure:** `KURT_RTS/1_RTS_Applied_RCH_with_legend.png`  
**Caption:** Applied recharge field from the KURT transient-rainfall RTS review (proxy for GSTR-style spatial forcing).

Layout: left_text_right_fig

---

## Slide 6 — KURT 4_GSTR example output

**Title:** KURT 4_GSTR example - Circular storm applied to SWF

**Prose:**
The TestTransientRainfall/4_GSTR model drives SWF recharge with named GSTR instance StormEW: ArcASCII storm rasters are interpolated in time and applied on the surface domain. Post-processing writes cell-by-cell applied rates (`_posto.modflow.SWF_GSTR.tecplot.dat`) and a domain-mean time series. The maps below show the peak applied-rate field (t = 0.375 d) and the short storm pulse in mean GSTR before rates return to zero for the remainder of the run.

**Figures:**
- `KURT_GSTR/GSTR_AppliedRate_Map.png` — Applied SWF GSTR rate at peak mean (StormEW, t = 0.375 d)
- `KURT_GSTR/GSTR_MeanRate_vs_Time.png` — Domain-mean applied GSTR rate vs time (first 2 days)

Layout: top_text_two_figs

---

## Slide 7 — Evapotranspiration in MODFLOW-USG

**Title:** Evapotranspiration in MODFLOW-USG - Head-dependent plant and soil water loss

**Prose:**
usgs_1 (MODFLOW-USG / USG-Transport) already provides classical groundwater ET packages that remove water from the saturated zone as a function of water-table position. The EVT package applies a maximum ET rate that declines linearly to zero over an extinction depth below a specified ET surface. The ETS package extends this with a segmented (piecewise) ET-versus-depth curve for non-linear plant uptake. EVT can also take time-varying maximum rates (ETS keyword / time-series option) so seasonal potential ET can be imposed. These packages act on GWF nodes (top layer, specified nodes, or uppermost active cells) and appear as budget terms when activated - they are the practical path today for regional ET sinks alongside rainfall recharge.

**Figure:** `KURT/GWF_WaterTable.png`  
**Caption:** Regional water-table surface - ET demand in EVT/ETS is controlled by how close the water table sits to the land surface / extinction depth.

Layout: left_text_right_fig

---

## Slide 8 — Evapotranspiration in MUT

**Title:** Evapotranspiration in MUT - Database scaffolding, writers still to come

**Prose:**
MUT can load an ET parameter database (`et database`) whose fields mirror an HGS-style plant/soil ET concept: evaporation and root depths, LAI tables, wilting point and field capacity, oxic/anoxic limits, and canopy interception storage. Sparse ET material IDs are supported alongside GWF/CLN/SWF databases. However, the User's Guide still treats ET.xlsx / LAI.xlsx as placeholders for future development: MUT does not yet assign ET zones to cells or write EVT/ETS package files. Name-file recognition of EVT/EVS/ETS exists for reading external datasets, but the engineering workflow for KURT today is rainfall and infiltration without an automated MUT-driven ET sink. Closing that gap means mapping the ET database (or a simpler EVT/ETS instruction set) onto usgs_1 package writers and exposing ET in volume-budget and Tecplot review.

**Figure:** `KURT/GWF_VolumeBudget.png`  
**Caption:** Current KURT volume-budget dossier frame - recharge, storage and boundary fluxes are present; ET package terms appear only when EVT/ETS are active in usgs_1.

Layout: left_text_right_fig

---

## Slide 9 — CLN conduits and shafts

**Title:** Conduits and shafts — Building CLN from the mesh

**Prose:**
Engineered openings and discrete pathways are represented with the Connected Linear Network (CLN). Development this year focused on intersecting conduit polylines with GWF/SWF element faces, preserving network connectivity, and connecting CLN nodes to the groundwater mesh for simple and shaft-style configurations. Conduits can also be rebuilt from a structure file with a companion coordinate list for geometry checks.

**Figure:** `UG/3_36_CLN.png`  
**Caption:** CLN conduits shown with the surrounding porous-medium mesh.

Layout: left_text_right_fig

---

## Slide 10 — CLN hydraulic detailing

**Title:** CLN hydraulic detailing — Skin, geometry and initial heads

**Prose:**
Once the conduit geometry exists, hydraulic behaviour must match the engineering concept. Per-cell skin conductivity (FSKIN) defaults from connected GWF horizontal conductivity, general tabular cross-sections support non-circular conduits, and initial CLN heads can be set from the land surface via column tops — a practical starting condition for shafts that daylight at topography.

**Figure:** `UG/MUT_InitialCLNHead.png`  
**Caption:** CLN initial heads assigned from surface elevation / GWF column tops.

Layout: left_text_right_fig

---

## Slide 11 — Velocity and flow pathways

**Title:** Velocity and pathways — Darcy and average-linear flow

**Prose:**
Flow interpretation and future solute transport both need velocities, not only heads. Builds now emit a default VEL package for active GWF, CLN and SWF domains. Post-processing writes Darcy and average-linear velocity components for Tecplot; porosity (and CLN infill porosity where used) converts Darcy flux to pore velocity. An optional original SWF Manning velocity path remains available for continuity with HGS-style surface flow.

**Figure:** `KURT/GWF_WaterTable.png`  
**Caption:** Regional water-table iso-surface from the KURT average-rainfall model dossier.

Layout: left_text_right_fig

---

## Slide 12 — Communicating results

**Title:** Communicating results — Tecplot SZL and model dossiers

**Prose:**
Engineering review depends on clear maps of saturation, infiltration, water table and volume budgets. Finite-element results default to compact TecIO SZL files with all output times. After build or post-processing, mut_document writes a model dossier PDF and sectioned Tecplot layouts — including dedicated views for volume budget, water table, SWF-to-GWF infiltration and saturation slices — so reviewers see the same figures the modeller used.

**Figure:** `UG/4_16_Infiltration.png`  
**Caption:** Example SWF-to-GWF infiltration map used in post-processed hillslope review.

Layout: left_text_right_fig

---

## Slide 13 — Regional KURT illustration

**Title:** Regional KURT illustration — Mesh, water table and infiltration

**Prose:**
The regional average-rainfall KURT model exercises the full stack on site-scale geometry: dual-domain mesh, transient surface response, and groundwater storage. Side-by-side dossier frames below illustrate how mesh structure, water-table position and infiltration patterns are delivered together for engineering discussion.

**Figures:**
- `KURT/GWF_Mesh.png` — GWF mesh / domain overview
- `KURT/GWF_WaterTable.png` — Water-table surface
- `KURT/SWF_Infiltration.png` — SWF-to-GWF infiltration

Layout: top_text_three_figs

---

## Slide 14 — Verification practice

**Title:** Verification practice — Confidence from known problems

**Prose:**
Before relying on regional forecasts, the same toolchain is checked against published and in-house flow problems (Abdul hillslope, SWF critical-depth and CLN-for-SWF cases). Outflow and depth comparisons against accepted solutions provide engineering confidence that surface–subsurface coupling and conduit representations behave as intended when rainfall, outlets and materials are known.

**Figures:**
- `EX/Abdul_Outflow_Comparison.png` — Abdul outlet hydrograph comparison
- `EX/CLN_Outflow_Comparison.png` — CLN-for-SWF outflow comparison

Layout: top_text_two_figs

---

## Slide 15 — Future capability: DFN medium

**Title:** Future capability - Discrete Fracture Network (DFN) medium

**Prose:**
KURT fracture zones are naturally represented as discrete 2D flow surfaces embedded in 3D porous rock, not as continuum smearing alone and not as 1D pipes. A new medium, DFN, would sit alongside GWF, CLN and SWF: planar fracture elements with aperture and transmissivity, exchanging water (and later solute) with the surrounding GWF matrix. CLN remains the right tool for shafts and tubular conduits; DFN targets sheet-like fractures imported from site STL/GIS fracture surfaces (as in the KURT incoming fracture set).

**Figure:** `KURT_FRAC/fractures.png`  
**Caption:** KURT fracture surfaces (incoming STL set) - candidate geometry for a DFN medium.

Layout: left_text_right_fig

---

## Slide 16 — Implementing DFN

**Title:** Implementing DFN - Steps for 2D fracture flow and transport

**Prose:**
1. Design - Equations, aperture/T properties, DFN-GWF exchange; NAM type and IDOMAIN code  
2. usgs_1 flow - Domain arrays, matrix assembly, budgets, binary heads/fluxes (patterned on CLN/SWF)  
3. Geometry - Import fracture surfaces; intersect/mesh onto GWF; write DFN GSF connectivity  
4. MUT - Domain, DFN materials DB, `_build.mut` verbs, package writers, Tecplot/dossier views  
5. Transport - BCT (or equivalent) on DFN nodes; matrix exchange; CON save and observations  
6. Verify - Single-fracture flow then transport; multi-fracture KURT case against a known solution  

**Figure:** `UG/3_38_CLN_GWF_Cells.png`  
**Caption:** Analogue: CLN-GWF coupling today - DFN would add planar fracture-matrix exchange in the same multi-domain stack.

Layout: left_text_right_fig

---

## Slide 17 — Gaps and next direction

**Title:** Gaps and next direction - Aligning solvers, ET writers and transport

**Prose:**
The flow modelling stack is usable today at MUT 2025.026, but a few pieces still need aligned USG builds: velocity post-processing requires a usgs_1 that recognises the VEL name-file type, and GSTR rates must be solved by a USG-Beta build that includes the GSTR package. MUT ET package writers (EVT/ETS from the existing ET database or a simpler instruction set) are still future work. Further ahead: SWF/GWF solute transport (Abdul path) and a new DFN medium for 2D fracture flow and transport on KURT fracture surfaces.

**Figure:** `UG/4_5_Post_Abdul.png`  
**Caption:** Abdul hillslope post-processed results - candidate platform for transport verification.

Layout: left_text_right_fig

---

## Slide 18 — Discussion

**Title:** Discussion - Priorities for the next phase

**Prose:**
With the 2026 flow capabilities in place, the meeting should set priorities for KURT delivery:

1. Deepen rainfall and GSTR workflows for regional transient forcing  
2. Add MUT writers for MODFLOW EVT/ETS (or fuller HGS-style ET) so regional water balances include plant/soil water loss  
3. Extend shaft / CLN engineering detail on the regional mesh  
4. Advance surface-subsurface transport (Abdul verification path)  
5. Scope DFN (2D discrete fracture flow and transport) against KURT fracture geometry  
6. Agree which usgs_1 build is the project standard for VEL and GSTR  

**Figure:** `KURT/GWF_Results.png`  
**Caption:** Example regional GWF results frame from the KURT model dossier.

Layout: left_text_right_fig
