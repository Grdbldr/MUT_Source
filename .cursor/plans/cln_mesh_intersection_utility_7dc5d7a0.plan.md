---
name: CLN Mesh Intersection Utility
overview: Create a Fortran utility that reads MODFLOW CLN structures (linked line segments), finds intersection points with finite-element mesh faces, and generates a new CLN structure with cells split at intersection points.
todos:
  - id: locate_xyzFromList
    content: Locate and examine xyzFromList subroutine to understand CLN coordinate reading format
    status: completed
  - id: design_data_structures
    content: Design data structures for CLN cells (linked segments), mesh faces, and intersection points
    status: completed
    dependencies:
      - locate_xyzFromList
  - id: implement_cln_reader
    content: Implement ReadCLNStructure subroutine using xyzFromList format
    status: completed
    dependencies:
      - locate_xyzFromList
      - design_data_structures
  - id: implement_mesh_reader
    content: Implement ReadFEMesh subroutine for finite-element mesh input
    status: completed
    dependencies:
      - design_data_structures
  - id: implement_intersection
    content: Implement line-plane intersection and point-in-face test algorithms
    status: completed
    dependencies:
      - design_data_structures
  - id: implement_cell_splitting
    content: Implement SplitCLNCells subroutine to create new cells at intersection points
    status: completed
    dependencies:
      - implement_intersection
  - id: implement_cln_writer
    content: Implement WriteCLNStructure subroutine to output new CLN structure
    status: completed
    dependencies:
      - implement_cell_splitting
  - id: create_main_interface
    content: Create main program or module interface for the utility
    status: completed
    dependencies:
      - implement_cln_reader
      - implement_mesh_reader
      - implement_cln_writer
---

# CLN-Mesh

Intersection Utility

## Overview

Create a new Fortran utility module that processes MODFLOW CLN (Conduit Flow Network) structures by:

1. Reading CLN cell coordinates from linked line segments

2. Computing intersections with finite-element mesh faces

3. Generating a new CLN structure with cells split at intersection points

## Implementation Plan

### 1. Create New Module: `CLNIntersection.f90`

- **Purpose**: Main module for CLN-mesh intersection processing
- **Key Components**:

    - CLN structure data types (cells as linked line segments)

    - Mesh face data structures

    - Intersection computation routines

    - Cell splitting logic

    - I/O routines for reading/writing CLN structures

### 2. Data Structures

- **CLN Cell Type**: 

    - Cell ID, start point (x1,y1,z1), end point (x2,y2,z2)

    - Link to next cell in network

    - Material properties

- **Intersection Point Type**:

    - xyz coordinates

    - Reference to intersecting cell ID and face ID
- **Mesh Face Type**:

    - Face vertices (3 or 4 points for triangular/quadrilateral faces)

    - Face normal vector

    - Element ID

### 3. Core Algorithms

#### 3.1 Read CLN Structure

- **Function**: `ReadCLNStructure(filename)`

- **Format**: Match `xyzFromList` subroutine format (to be confirmed)

- **Input**: File containing CLN cell definitions as linked line segments

- **Output**: Linked list/array of CLN cells with xyz coordinates

#### 3.2 Read Finite-Element Mesh

- **Function**: Use subroutine ReadMeshBIN in file NumericalMesh.f90

- **Input**: Mesh file 

- **Output**: Array of elements with face definitions

#### 3.3 Compute Intersections

- **Function**: `FindCLN_MeshIntersections(cln_cells, mesh_faces)`

- **Algorithm**: 
    - For each CLN cell (line segment):

    - For each mesh face:
        - Compute line-plane intersection

        - Check if intersection point lies within face bounds

        - Store intersection point with references

- **Output**: Array of intersection points with metadata

#### 3.4 Split CLN Cells

- **Function**: `SplitCLNCells(cln_cells, intersections)`

- **Algorithm**:

    - Sort intersections along each CLN cell by distance from start

    - For each cell with intersections:

    - Create new cells: [start → intersection1], [intersection1 → intersection2], ..., [intersectionN → end]

    - Preserve material properties and network connectivity

- **Output**: New CLN structure with split cells

#### 3.5 Write New CLN Structure

- **Function**: `WriteCLNStructure(filename, new_cln_cells)`

- **Format**: Match input format or MODFLOW CLN package format

### 4. Integration Points

- **Dependencies**: 
    - `KindParameters.f90` (for precision types)

    - `GeneralRoutines.f90` (for file I/O utilities)

    - `BasicTypes.f90` (for point/segment types if compatible)

    - `NumericalMesh.f90` (for mesh data structures if available)

- **Coordinate System**: Ensure consistent xyz coordinate system between CLN and mesh

### 5. Main Program/Interface

- **Option B**: Fortran module with callable subroutines
    - Can be integrated into existing MUT workflow

### 6. Computational Geometry Routines

- **Line-Plane Intersection**: 

    - Given line segment (P1, P2) and plane (point + normal)

    - Compute intersection point if it exists
- **Point-in-Face Test**:
    - Barycentric coordinates for triangular faces

    - Point-in-polygon for quadrilateral faces

- **Distance Calculations**:

    - Along-line distance for sorting intersections

### 7. Error Handling

- Validate CLN cell connectivity

- Check mesh face validity

- Handle degenerate cases (line parallel to face, zero-length cells)

- Report intersection statistics

## File Structure

```javascript
MUT_Source/
├── CLNIntersection.f90    # New module (main implementation)
├── CLNGeometry.f90         # Optional: geometric computation routines
└── (existing files...)
```



## Testing Considerations

- Test with simple cases (single cell, single intersection)

- Test with complex networks (multiple branches, multiple intersections per cell)

- Validate output CLN structure maintains connectivity
- Verify intersection point accuracy

## Open Questions

1. **xyzFromList format**: Need to locate and examine this subroutine to match I/O format

2. **Mesh format**: Determine input format for finite-element mesh

3. **Output format**: Confirm if output should match input format or MODFLOW CLN package format 