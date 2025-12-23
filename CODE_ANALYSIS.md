# MUT_Source Code Analysis

## Project Overview
**Modflow User Tool (MUT)** - A Fortran-based preprocessor for MODFLOW-USG (Unstructured Grid) groundwater modeling. The project processes input files and generates MODFLOW-USG compatible output files.

## Architecture

### Main Program
- **`pre.f90`**: Entry point - `program PreMUT`
  - Calls: `OpenMUT`, `ProcessMUT`, `CloseMUT` (from MUT module)

### Core Modules

#### 1. **KindParameters.f90**
- **Purpose**: Defines kind parameters for type declarations
- **Key Parameters**:
  - `sp` = single precision (6 digits, 37 exponent range)
- `dp` = double precision (15 digits, 307 exponent range)
  - `i2` = 2-byte integer
  - `i4` = 4-byte integer

#### 2. **GeneralRoutines.f90**
- **Purpose**: Core utility routines and global constants
- **Key Features**:
  - Version info: `MUTVersion='2025.1 RELEASE'` or `'2025.1 DEBUG'`
  - Constants: PI, small values, string lengths
  - File I/O management (unit numbers, file existence checks)
  - Bit-setting constants for cell/node flags (Chosen, ConstantHead, Drain, Recharge, etc.)
  - Raster data structures (ArcAscii type)
  - Stopwatch/timing utilities
  - Date/time handling
- **Dependencies**: `ifport`, `ifwin` (Intel Fortran libraries)

#### 3. **BasicTypes.f90**
- **Purpose**: Fundamental geometric data types
- **Key Types**:
  - `t_point`: 3D point (x, y, z) with name and id
  - `t_segment`: Two points forming a line segment
  - `t_triangle`: Three points forming a triangle
  - `t_line`: Polyline/polygon with allocatable points
  - `t_pointset`: Collection of points (e.g., wells)
- **Functions**:
  - `LineLength()`: Calculate line length
  - `distance2Points()`: Calculate distance between points
  - `LineFrom_ID_X_Y_File()`: Read line from file
  - `WellsFrom_ID_X_Y_File()`: Read wells from file
- **Dependencies**: `KindParameters`, `GeneralRoutines`

#### 4. **NumericalMesh.f90**
- **Purpose**: Numerical mesh data structures and operations
- **Dependencies**: Used by multiple modules

#### 5. **MUSGModules.f90**
- **Purpose**: MODFLOW-USG global variables and data structures
- **Key Modules**:
  - `GLOBAL`: Core MODFLOW-USG variables (NCOL, NROW, NLAY, NODES, etc.)
  - `NAMEFILEMODULE`: File name management
  - `PARAMMODULE`: Parameter definitions
- **Key Variables**:
  - Grid dimensions: `NCOL`, `NROW`, `NLAY`, `NODES`
  - Connectivity: `IA`, `JA`, `JAS` (sparse matrix connectivity)
  - Solution arrays: `HNEW`, `HOLD`, `RHS`, `AMAT`
  - Material properties: `TOP`, `BOT`, `AREA`, `PGF`

#### 6. **Modflow_USG.f90** (module MUSG)
- **Purpose**: Main MODFLOW-USG processing module
- **Key Features**:
  - Instruction parsing and command recognition
  - Database management (SMS, GWF, CLN, SWF materials)
  - Mesh generation commands
  - Domain selection and cell/node operations
  - Initial condition assignment
  - Boundary condition setup
  - Stress period definitions
  - Output control
- **Size**: Very large module (~18,000+ lines)
- **Dependencies**: `GeneralRoutines`, `NumericalMesh`, `materials`, `MeshGen`, `tecplot`, `global`, `CLN1MODULE`, `SWF1MODULE`

#### 7. **GrdBldr.f90**
- **Purpose**: Grid builder module for mesh generation
- **Key Modules**:
  - `error_param`: Error handling
  - `gb`: Grid builder data structures
- **Key Features**:
  - Node and element coordinate arrays
  - Boundary node management
  - Area definitions with cut lines
  - Well locations and element sizing
  - Grid extents (xmin, xmax, ymin, ymax)
- **Dependencies**: `GeneralRoutines`, `NumericalMesh`, `BasicTypes`, `MeshGen`

#### 8. **Materials.f90**
- **Purpose**: Material property definitions
- **Dependencies**: Used by MUSG module

#### 9. **MeshGeneration.f90**
- **Purpose**: Mesh generation algorithms
- **Dependencies**: Referenced as `MeshGen` module

#### 10. **Tecplot.f90**
- **Purpose**: Tecplot output file generation
- **Dependencies**: Used for visualization output

#### 11. **TGModule.f90** (module MUT)
- **Purpose**: Main MUT module providing `OpenMUT`, `ProcessMUT`, `CloseMUT`
- **Dependencies**: Likely orchestrates other modules

#### 12. **GenericList.f90**
- **Purpose**: Generic list data structure
- **Modules**: `list`, `data`

#### 13. **raster.f90**
- **Purpose**: Raster data processing (likely for DEM/elevation data)

## Build System

### Visual Studio Project
- **File**: `Modflow-User-Tools.vfproj`
- **Compiler**: Intel Fortran (ifort/ifx)
- **Configurations**:
  - Debug|Win32, Debug|x64
  - Release|Win32, Release|x64
- **External Libraries**:
  - `tecio.lib` (Tecplot library)
  - Environment variables: `$(TEC360INCLUDE)`, `$(TEC360LIB)`
- **Post-build**: `post_build.bat` / `post_buildR.bat`
  - Copies executable to `%USERBIN%\mut.exe`

### Source File Compilation Order
Based on dependencies, suggested order:
1. `KindParameters.f90` (no dependencies)
2. `GeneralRoutines.f90` (depends on KindParameters)
3. `BasicTypes.f90` (depends on KindParameters, GeneralRoutines)
4. `NumericalMesh.f90`
5. `GenericList.f90`
6. `raster.f90`
7. `Materials.f90`
8. `MeshGeneration.f90`
9. `Tecplot.f90`
10. `MUSGModules.f90` (depends on KindParameters)
11. `GrdBldr.f90` (depends on multiple)
12. `Modflow_USG.f90` (depends on many)
13. `TGModule.f90` (MUT module)
14. `pre.f90` (main program, depends on MUT module)

## Key Features

### 1. Instruction-Based Processing
The system uses text-based instructions (e.g., "build modflow usg", "choose all cells", "gwf initial head") parsed from input files.

### 2. Multiple Domain Support
- **GWF**: Groundwater Flow domain
- **SWF**: Surface Water Flow domain  
- **CLN**: Conduit/Linear Network domain

### 3. Mesh Generation Options
- From Grid Builder (GB)
- From Groundwater Vistas
- Uniform/variable rectangles
- Layered 3D from 2D mesh

### 4. Output Formats
- MODFLOW-USG native format
- Tecplot (visualization)
- QGIS (geospatial)

### 5. Material Databases
CSV-based material property databases for:
- GWF materials
- CLN materials
- SWF materials
- SMS solver parameters
- ET (evapotranspiration)

## Dependencies

### External Libraries
- **Intel Fortran Runtime**: `ifport`, `ifwin`
- **Tecplot Library**: `tecio.lib` (for visualization output)

### Internal Module Dependencies
```
KindParameters (base, no dependencies)
    ↓
GeneralRoutines (uses KindParameters, ifport, ifwin)
    ↓
BasicTypes (uses KindParameters, GeneralRoutines)
    ↓
NumericalMesh (uses BasicTypes, GeneralRoutines)
    ↓
    ├─→ Materials (uses GeneralRoutines)
    ├─→ MeshGen (MeshGeneration.f90)
    ├─→ Tecplot (uses iso_c_binding, GeneralRoutines)
    └─→ raster (uses GeneralRoutines)
    ↓
GrdBldr (uses GeneralRoutines, NumericalMesh, BasicTypes, MeshGen)
    ↓
MUSGModules (uses KindParameters)
    ├─→ GLOBAL
    ├─→ NAMEFILEMODULE
    └─→ PARAMMODULE
    ↓
Modflow_USG (module MUSG)
    (uses GeneralRoutines, NumericalMesh, materials, MeshGen, 
     tecplot, global, CLN1MODULE, SWF1MODULE)
    ↓
TGModule (module MUT)
    (uses GeneralRoutines, error_param, MeshGen, gb, MUSG, 
     tecplot, NumericalMesh)
    ↓
pre.f90 (main program, uses MUT)
```

**Note**: Some modules (CLN1MODULE, SWF1MODULE, GWFBASMODULE, etc.) appear to be from MODFLOW-USG source code and may be external dependencies.

## Code Quality Observations

### Strengths
1. **Modular Design**: Well-separated concerns
2. **Type Safety**: Uses kind parameters consistently
3. **Modern Fortran**: Uses allocatable arrays, derived types
4. **Error Handling**: Includes allocation checks (`AllocChk`)

### Areas for Improvement
1. **Large Modules**: `Modflow_USG.f90` is very large (~18K lines)
2. **Global State**: Heavy use of module-level variables
3. **Mixed Naming**: Some inconsistencies (e.g., `BasicTypes` vs `BasicType`)
4. **Documentation**: Limited inline documentation in some modules

## File Structure
```
MUT_Source/
├── *.f90              # Fortran source files
├── *.vfproj           # Visual Studio project
├── *.sln              # Visual Studio solution
├── *.bat              # Build scripts
├── openspec.inc       # Include file
└── Docs/              # Documentation
    ├── User's Guide/  # LaTeX user documentation
    └── Slides/        # Presentation materials
```

## Version Information
- **Current Version**: 2025.1 (RELEASE/DEBUG)
- **Last Updated**: December 2025 (based on file timestamps)

