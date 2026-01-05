module CLNIntersection
    ! Purpose: Utility for processing MODFLOW CLN structures with finite-element mesh intersections
    ! Reads CLN cells as linked line segments, finds intersections with mesh faces,
    ! and generates new CLN structure with cells split at intersection points
    
    use KindParameters, only: dp, i4
    use GeneralRoutines
    use BasicTypes
    use NumericalMesh
    use ErrorHandling, only: ERR_INVALID_INPUT, ERR_FILE_IO, ERR_LOGIC, HandleError
    implicit none
    
    private
    
    ! Public types
    public :: t_cln_cell, t_cln_structure, t_intersection_point
    
    ! Public subroutines
    public :: ReadCLNStructure, ReadCLNFromXYZList
    public :: FindCLN_MeshIntersections
    public :: SplitCLNCells
    public :: WriteCLNStructure
    
    !----------------------------------------------------------------------
    ! Data Structures
    !----------------------------------------------------------------------
    
    ! CLN Cell Type: Represents a single CLN cell as a line segment
    type :: t_cln_cell
        integer(i4) :: id                    ! Cell ID
        real(dp) :: x1, y1, z1              ! Start point coordinates
        real(dp) :: x2, y2, z2              ! End point coordinates
        integer(i4) :: next_cell_id          ! ID of next cell in network (0 if end)
        integer(i4) :: material_id           ! Material property ID
        real(dp) :: radius_or_width         ! Radius (circular) or width (rectangular)
        real(dp) :: height                  ! Height (for rectangular cross-section)
        logical :: is_circular              ! True if circular, false if rectangular
    end type t_cln_cell
    
    ! CLN Structure: Collection of linked CLN cells
    type :: t_cln_structure
        integer(i4) :: nCells
        type(t_cln_cell), allocatable :: cell(:)
        character(len=:), allocatable :: name
    end type t_cln_structure
    
    ! Intersection Point Type
    type :: t_intersection_point
        real(dp) :: x, y, z                 ! Intersection coordinates
        integer(i4) :: cln_cell_id          ! ID of CLN cell that intersects
        integer(i4) :: mesh_element_id      ! ID of mesh element
        integer(i4) :: mesh_face_id         ! Local face ID within element
        real(dp) :: distance_from_start     ! Distance along CLN cell from start point
    end type t_intersection_point
    
    ! Collection of intersection points
    type :: t_intersection_list
        integer(i4) :: nIntersections
        type(t_intersection_point), allocatable :: point(:)
    end type t_intersection_list
    
    ! Tolerance for geometric comparisons
    real(dp), parameter :: TOL_GEOM = 1.0d-6
    
    contains
    
    !----------------------------------------------------------------------
    ! Read CLN Structure from File
    !----------------------------------------------------------------------
    subroutine ReadCLNStructure(filename, cln_struct)
        implicit none
        character(*), intent(in) :: filename
        type(t_cln_structure), intent(out) :: cln_struct
        
        integer(i4) :: i, FNum, nCells
        character(MAX_LBL) :: VarSTR
        
        call OpenAscii(FNum, filename)
        call Msg('  ')
        call Msg(FileReadSTR//'CLN structure file: '//trim(filename))
        
        ! Read header line
        read(FNum, '(a)') VarSTR
        
        ! Count number of cells (assuming format: ID X1 Y1 Z1 X2 Y2 Z2 NextID MaterialID Radius/Width Height IsCircular)
        nCells = 0
        CountLoop: do
            read(FNum, *, iostat=status) i
            if (status /= 0) exit CountLoop
            nCells = nCells + 1
        end do CountLoop
        
        cln_struct%nCells = nCells
        allocate(cln_struct%cell(nCells), stat=ialloc)
        call AllocChk(ialloc, 'ReadCLNStructure: cln_struct%cell array')
        
        ! Rewind and read data
        rewind(FNum)
        read(FNum, '(a)') VarSTR
        
        do i = 1, nCells
            read(FNum, *, iostat=status) &
                cln_struct%cell(i)%id, &
                cln_struct%cell(i)%x1, cln_struct%cell(i)%y1, cln_struct%cell(i)%z1, &
                cln_struct%cell(i)%x2, cln_struct%cell(i)%y2, cln_struct%cell(i)%z2, &
                cln_struct%cell(i)%next_cell_id, &
                cln_struct%cell(i)%material_id, &
                cln_struct%cell(i)%radius_or_width, &
                cln_struct%cell(i)%height, &
                cln_struct%cell(i)%is_circular
            if (status /= 0) then
                call HandleError(ERR_FILE_IO, 'Error reading CLN cell data', 'ReadCLNStructure')
            end if
        end do
        
        call FreeUnit(FNum)
        
        write(TMPStr, '(a,i8)') 'Number of CLN cells read:', cln_struct%nCells
        call Msg(TMPStr)
        
    end subroutine ReadCLNStructure
    
    !----------------------------------------------------------------------
    ! Read CLN Structure from XYZ List Format (similar to xyzFromList)
    ! Format: First line is header, then lines with: ID X Y Z
    ! Cells are linked sequentially (cell i connects to cell i+1)
    !----------------------------------------------------------------------
    subroutine ReadCLNFromXYZList(filename, cln_struct)
        implicit none
        character(*), intent(in) :: filename
        type(t_cln_structure), intent(out) :: cln_struct
        
        integer(i4) :: i, FNum, nPoints
        character(MAX_LBL) :: VarSTR
        type(t_point), allocatable :: points(:)
        
        call OpenAscii(FNum, filename)
        call Msg('  ')
        call Msg(FileReadSTR//'CLN structure from XYZ list: '//trim(filename))
        
        ! Read header line
        read(FNum, '(a)') VarSTR
        
        ! Count number of points
        nPoints = 0
        CountLoop: do
            read(FNum, *, iostat=status) i
            if (status /= 0) exit CountLoop
            nPoints = nPoints + 1
        end do CountLoop
        
        allocate(points(nPoints), stat=ialloc)
        call AllocChk(ialloc, 'ReadCLNFromXYZList: points array')
        
        ! Rewind and read points
        rewind(FNum)
        read(FNum, '(a)') VarSTR
        
        do i = 1, nPoints
            read(FNum, *, iostat=status) points(i)%id, points(i)%x, points(i)%y, points(i)%z
            if (status /= 0) then
                call HandleError(ERR_FILE_IO, 'Error reading XYZ point data', 'ReadCLNFromXYZList')
            end if
        end do
        
        call FreeUnit(FNum)
        
        ! Convert points to CLN cells (each pair of consecutive points forms a cell)
        if (nPoints < 2) then
            call HandleError(ERR_INVALID_INPUT, 'Need at least 2 points to form CLN cells', 'ReadCLNFromXYZList')
        end if
        
        cln_struct%nCells = nPoints - 1
        allocate(cln_struct%cell(cln_struct%nCells), stat=ialloc)
        call AllocChk(ialloc, 'ReadCLNFromXYZList: cln_struct%cell array')
        
        do i = 1, cln_struct%nCells
            cln_struct%cell(i)%id = i
            cln_struct%cell(i)%x1 = points(i)%x
            cln_struct%cell(i)%y1 = points(i)%y
            cln_struct%cell(i)%z1 = points(i)%z
            cln_struct%cell(i)%x2 = points(i+1)%x
            cln_struct%cell(i)%y2 = points(i+1)%y
            cln_struct%cell(i)%z2 = points(i+1)%z
            
            ! Link cells sequentially
            if (i < cln_struct%nCells) then
                cln_struct%cell(i)%next_cell_id = i + 1
            else
                cln_struct%cell(i)%next_cell_id = 0  ! Last cell
            end if
            
            ! Default values (should be set from material database or input)
            cln_struct%cell(i)%material_id = 1
            cln_struct%cell(i)%radius_or_width = 1.0d0
            cln_struct%cell(i)%height = 1.0d0
            cln_struct%cell(i)%is_circular = .true.
        end do
        
        deallocate(points)
        
        write(TMPStr, '(a,i8)') 'Number of CLN cells created from XYZ list:', cln_struct%nCells
        call Msg(TMPStr)
        
    end subroutine ReadCLNFromXYZList
    
    !----------------------------------------------------------------------
    ! Find Intersections between CLN cells and mesh faces
    !----------------------------------------------------------------------
    subroutine FindCLN_MeshIntersections(cln_struct, M, intersections)
        implicit none
        type(t_cln_structure), intent(in) :: cln_struct
        type(mesh), intent(in) :: M
        type(t_intersection_list), intent(out) :: intersections
        
        integer(i4) :: iCell, iElem, iFace, iNode, nNodes
        integer(i4) :: nIntersections
        real(dp) :: x1, y1, z1, x2, y2, z2
        real(dp) :: xInt, yInt, zInt
        real(dp) :: dist
        logical :: found_intersection
        integer(i4), allocatable :: nodeIds(:)
        real(dp), allocatable :: faceNodesX(:), faceNodesY(:), faceNodesZ(:)
        
        ! Ensure mesh faces are calculated
        if (.not. M%FacesCalculated) then
            call BuildFaceTopologyFrommesh(M)
        end if
        
        ! Allocate temporary arrays (over-allocate to be safe)
        allocate(intersections%point(cln_struct%nCells * M%nFacesPerElement * 2), stat=ialloc)
        call AllocChk(ialloc, 'FindCLN_MeshIntersections: intersections%point array')
        
        nIntersections = 0
        
        ! Loop over all CLN cells
        do iCell = 1, cln_struct%nCells
            x1 = cln_struct%cell(iCell)%x1
            y1 = cln_struct%cell(iCell)%y1
            z1 = cln_struct%cell(iCell)%z1
            x2 = cln_struct%cell(iCell)%x2
            y2 = cln_struct%cell(iCell)%y2
            z2 = cln_struct%cell(iCell)%z2
            
            ! Loop over all mesh elements
            do iElem = 1, M%nElements
                ! Loop over all faces of this element
                do iFace = 1, M%nFacesPerElement
                    ! Get face node IDs
                    nNodes = M%nNodesPerFace
                    allocate(nodeIds(nNodes), faceNodesX(nNodes), faceNodesY(nNodes), faceNodesZ(nNodes), stat=ialloc)
                    call AllocChk(ialloc, 'FindCLN_MeshIntersections: face node arrays')
                    
                    do iNode = 1, nNodes
                        nodeIds(iNode) = M%idNode(M%LocalFaceNodes(iNode, iFace), iElem)
                        faceNodesX(iNode) = M%node(nodeIds(iNode))%x
                        faceNodesY(iNode) = M%node(nodeIds(iNode))%y
                        faceNodesZ(iNode) = M%node(nodeIds(iNode))%z
                    end do
                    
                    ! Check for intersection between line segment and face
                    found_intersection = .false.
                    call LineFaceIntersection(x1, y1, z1, x2, y2, z2, &
                                              faceNodesX, faceNodesY, faceNodesZ, nNodes, &
                                              xInt, yInt, zInt, found_intersection)
                    
                    if (found_intersection) then
                        ! Check if intersection is within line segment bounds
                        if (PointOnLineSegment(x1, y1, z1, x2, y2, z2, xInt, yInt, zInt)) then
                            nIntersections = nIntersections + 1
                            intersections%point(nIntersections)%x = xInt
                            intersections%point(nIntersections)%y = yInt
                            intersections%point(nIntersections)%z = zInt
                            intersections%point(nIntersections)%cln_cell_id = iCell
                            intersections%point(nIntersections)%mesh_element_id = iElem
                            intersections%point(nIntersections)%mesh_face_id = iFace
                            
                            ! Calculate distance from start of CLN cell
                            dist = sqrt((xInt - x1)**2 + (yInt - y1)**2 + (zInt - z1)**2)
                            intersections%point(nIntersections)%distance_from_start = dist
                        end if
                    end if
                    
                    deallocate(nodeIds, faceNodesX, faceNodesY, faceNodesZ)
                end do
            end do
        end do
        
        intersections%nIntersections = nIntersections
        
        ! Trim array to actual size if needed
        if (nIntersections > 0 .and. nIntersections < size(intersections%point)) then
            ! Reallocate to exact size (simplified - in practice would use temporary array)
            ! For now, just set the count - the array is already allocated large enough
        end if
        
        write(TMPStr, '(a,i8)') 'Number of intersections found:', nIntersections
        call Msg(TMPStr)
        
    end subroutine FindCLN_MeshIntersections
    
    !----------------------------------------------------------------------
    ! Line-Face Intersection: Compute intersection of line segment with polygonal face
    !----------------------------------------------------------------------
    subroutine LineFaceIntersection(x1, y1, z1, x2, y2, z2, &
                                     faceX, faceY, faceZ, nFaceNodes, &
                                     xInt, yInt, zInt, found)
        implicit none
        real(dp), intent(in) :: x1, y1, z1, x2, y2, z2
        real(dp), intent(in) :: faceX(:), faceY(:), faceZ(:)
        integer(i4), intent(in) :: nFaceNodes
        real(dp), intent(out) :: xInt, yInt, zInt
        logical, intent(out) :: found
        
        real(dp) :: nx, ny, nz, d
        real(dp) :: vx, vy, vz, denom, t
        real(dp) :: px, py, pz
        
        found = .false.
        
        ! Compute face plane normal (using first 3 nodes)
        if (nFaceNodes < 3) return
        
        ! Vector from node 1 to node 2
        vx = faceX(2) - faceX(1)
        vy = faceY(2) - faceY(1)
        vz = faceZ(2) - faceZ(1)
        
        ! Vector from node 1 to node 3
        px = faceX(3) - faceX(1)
        py = faceY(3) - faceY(1)
        pz = faceZ(3) - faceZ(1)
        
        ! Cross product to get normal
        nx = vy * pz - vz * py
        ny = vz * px - vx * pz
        nz = vx * py - vy * px
        
        ! Normalize
        d = sqrt(nx*nx + ny*ny + nz*nz)
        if (d < TOL_GEOM) return  ! Degenerate face
        
        nx = nx / d
        ny = ny / d
        nz = nz / d
        
        ! Plane equation: n·(p - p0) = 0, where p0 is faceX(1), faceY(1), faceZ(1)
        ! Line equation: p = p1 + t*(p2 - p1)
        ! Substitute into plane: n·(p1 + t*(p2-p1) - p0) = 0
        ! Solve for t: t = n·(p0 - p1) / n·(p2 - p1)
        
        vx = x2 - x1
        vy = y2 - y1
        vz = z2 - z1
        
        denom = nx * vx + ny * vy + nz * vz
        
        if (abs(denom) < TOL_GEOM) return  ! Line parallel to plane
        
        t = (nx * (faceX(1) - x1) + ny * (faceY(1) - y1) + nz * (faceZ(1) - z1)) / denom
        
        ! Intersection point
        xInt = x1 + t * vx
        yInt = y1 + t * vy
        zInt = z1 + t * vz
        
        ! Check if point is within face bounds (point-in-polygon test)
        if (PointInFace(xInt, yInt, zInt, faceX, faceY, faceZ, nFaceNodes, nx, ny, nz)) then
            found = .true.
        end if
        
    end subroutine LineFaceIntersection
    
    !----------------------------------------------------------------------
    ! Point-in-Face Test: Check if point lies within polygonal face
    !----------------------------------------------------------------------
    function PointInFace(px, py, pz, faceX, faceY, faceZ, nNodes, nx, ny, nz) result(inside)
        implicit none
        real(dp), intent(in) :: px, py, pz
        real(dp), intent(in) :: faceX(:), faceY(:), faceZ(:)
        integer(i4), intent(in) :: nNodes
        real(dp), intent(in) :: nx, ny, nz  ! Face normal
        logical :: inside
        
        
        inside = .false.
        
        ! For triangular faces, use barycentric coordinates
        if (nNodes == 3) then
            inside = PointInTriangle(px, py, pz, &
                                     faceX(1), faceY(1), faceZ(1), &
                                     faceX(2), faceY(2), faceZ(2), &
                                     faceX(3), faceY(3), faceZ(3))
            return
        end if
        
        ! For quadrilateral or higher-order faces, use ray-casting method
        ! Project to 2D plane and use point-in-polygon test
        ! Choose projection plane based on dominant normal component
        if (abs(nz) >= abs(nx) .and. abs(nz) >= abs(ny)) then
            ! Project to XY plane
            inside = PointInPolygon2D(px, py, faceX, faceY, nNodes)
        else if (abs(ny) >= abs(nx)) then
            ! Project to XZ plane
            inside = PointInPolygon2D(px, pz, faceX, faceZ, nNodes)
        else
            ! Project to YZ plane
            inside = PointInPolygon2D(py, pz, faceY, faceZ, nNodes)
        end if
        
    end function PointInFace
    
    !----------------------------------------------------------------------
    ! Point-in-Triangle Test using barycentric coordinates
    !----------------------------------------------------------------------
    function PointInTriangle(px, py, pz, x1, y1, z1, x2, y2, z2, x3, y3, z3) result(inside)
        implicit none
        real(dp), intent(in) :: px, py, pz
        real(dp), intent(in) :: x1, y1, z1, x2, y2, z2, x3, y3, z3
        logical :: inside
        
        real(dp) :: v0x, v0y, v0z, v1x, v1y, v1z, v2x, v2y, v2z
        real(dp) :: dot00, dot01, dot02, dot11, dot12
        real(dp) :: invDenom, u, v
        
        ! Vectors from vertex 1
        v0x = x3 - x1
        v0y = y3 - y1
        v0z = z3 - z1
        v1x = x2 - x1
        v1y = y2 - y1
        v1z = z2 - z1
        v2x = px - x1
        v2y = py - y1
        v2z = pz - z1
        
        ! Compute dot products
        dot00 = v0x*v0x + v0y*v0y + v0z*v0z
        dot01 = v0x*v1x + v0y*v1y + v0z*v1z
        dot02 = v0x*v2x + v0y*v2y + v0z*v2z
        dot11 = v1x*v1x + v1y*v1y + v1z*v1z
        dot12 = v1x*v2x + v1y*v2y + v1z*v2z
        
        ! Compute barycentric coordinates
        invDenom = 1.0d0 / (dot00 * dot11 - dot01 * dot01)
        u = (dot11 * dot02 - dot01 * dot12) * invDenom
        v = (dot00 * dot12 - dot01 * dot02) * invDenom
        
        ! Check if point is in triangle
        inside = (u >= -TOL_GEOM) .and. (v >= -TOL_GEOM) .and. (u + v <= 1.0d0 + TOL_GEOM)
        
    end function PointInTriangle
    
    !----------------------------------------------------------------------
    ! Point-in-Polygon Test (2D projection)
    !----------------------------------------------------------------------
    function PointInPolygon2D(px, py, polyX, polyY, nNodes) result(inside)
        implicit none
        real(dp), intent(in) :: px, py
        real(dp), intent(in) :: polyX(:), polyY(:)
        integer(i4), intent(in) :: nNodes
        logical :: inside
        
        integer(i4) :: i, j
        logical :: oddNodes
        
        oddNodes = .false.
        j = nNodes
        
        do i = 1, nNodes
            if (((polyY(i) > py) .neqv. (polyY(j) > py)) .and. &
                (px < (polyX(j) - polyX(i)) * (py - polyY(i)) / (polyY(j) - polyY(i)) + polyX(i))) then
                oddNodes = .not. oddNodes
            end if
            j = i
        end do
        
        inside = oddNodes
        
    end function PointInPolygon2D
    
    !----------------------------------------------------------------------
    ! Check if point lies on line segment
    !----------------------------------------------------------------------
    function PointOnLineSegment(x1, y1, z1, x2, y2, z2, px, py, pz) result(onSegment)
        implicit none
        real(dp), intent(in) :: x1, y1, z1, x2, y2, z2, px, py, pz
        logical :: onSegment
        
        real(dp) :: dist12, dist1p, dist2p
        
        ! Distance from p1 to p2
        dist12 = sqrt((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2)
        
        ! Distance from p1 to p
        dist1p = sqrt((px - x1)**2 + (py - y1)**2 + (pz - z1)**2)
        
        ! Distance from p2 to p
        dist2p = sqrt((px - x2)**2 + (py - y2)**2 + (pz - z2)**2)
        
        ! Point is on segment if dist1p + dist2p ≈ dist12 (within tolerance)
        onSegment = abs(dist1p + dist2p - dist12) < TOL_GEOM
        
    end function PointOnLineSegment
    
    !----------------------------------------------------------------------
    ! Split CLN Cells at Intersection Points
    !----------------------------------------------------------------------
    subroutine SplitCLNCells(cln_struct, intersections, new_cln_struct)
        implicit none
        type(t_cln_structure), intent(in) :: cln_struct
        type(t_intersection_list), intent(in) :: intersections
        type(t_cln_structure), intent(out) :: new_cln_struct
        
        integer(i4) :: iCell, iInt, nNewCells
        integer(i4) :: j, k
        integer(i4), allocatable :: cellIntersections(:), sortedIndices(:)
        real(dp), allocatable :: distances(:)
        integer(i4) :: currentNewId
        
        ! Count new cells needed
        nNewCells = 0
        do iCell = 1, cln_struct%nCells
            ! Count intersections for this cell
            j = 0
            do iInt = 1, intersections%nIntersections
                if (intersections%point(iInt)%cln_cell_id == iCell) then
                    j = j + 1
                end if
            end do
            ! Original cell + number of intersections = number of new cells
            nNewCells = nNewCells + 1 + j
        end do
        
        allocate(new_cln_struct%cell(nNewCells), stat=ialloc)
        call AllocChk(ialloc, 'SplitCLNCells: new_cln_struct%cell array')
        
        currentNewId = 1
        
        ! Process each original cell
        do iCell = 1, cln_struct%nCells
            ! Collect intersections for this cell
            j = 0
            do iInt = 1, intersections%nIntersections
                if (intersections%point(iInt)%cln_cell_id == iCell) then
                    j = j + 1
                end if
            end do
            
            if (j == 0) then
                ! No intersections - copy cell as-is
                new_cln_struct%cell(currentNewId) = cln_struct%cell(iCell)
                new_cln_struct%cell(currentNewId)%id = currentNewId
                ! Update next_cell_id (will be handled after all cells are created)
                currentNewId = currentNewId + 1
            else
                ! Allocate arrays for this cell's intersections
                allocate(cellIntersections(j), distances(j), sortedIndices(j), stat=ialloc)
                call AllocChk(ialloc, 'SplitCLNCells: cell intersection arrays')
                
                ! Collect intersection indices and distances
                k = 0
                do iInt = 1, intersections%nIntersections
                    if (intersections%point(iInt)%cln_cell_id == iCell) then
                        k = k + 1
                        cellIntersections(k) = iInt
                        distances(k) = intersections%point(iInt)%distance_from_start
                        sortedIndices(k) = k
                    end if
                end do
                
                ! Sort intersections by distance from start
                call SortByDistance(distances, sortedIndices, j)
                
                ! Create new cells
                ! First segment: start to first intersection
                new_cln_struct%cell(currentNewId)%id = currentNewId
                new_cln_struct%cell(currentNewId)%x1 = cln_struct%cell(iCell)%x1
                new_cln_struct%cell(currentNewId)%y1 = cln_struct%cell(iCell)%y1
                new_cln_struct%cell(currentNewId)%z1 = cln_struct%cell(iCell)%z1
                iInt = cellIntersections(sortedIndices(1))
                new_cln_struct%cell(currentNewId)%x2 = intersections%point(iInt)%x
                new_cln_struct%cell(currentNewId)%y2 = intersections%point(iInt)%y
                new_cln_struct%cell(currentNewId)%z2 = intersections%point(iInt)%z
                new_cln_struct%cell(currentNewId)%next_cell_id = currentNewId + 1
                new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                currentNewId = currentNewId + 1
                
                ! Middle segments: between intersections
                do k = 1, j - 1
                    iInt = cellIntersections(sortedIndices(k))
                    new_cln_struct%cell(currentNewId)%id = currentNewId
                    new_cln_struct%cell(currentNewId)%x1 = intersections%point(iInt)%x
                    new_cln_struct%cell(currentNewId)%y1 = intersections%point(iInt)%y
                    new_cln_struct%cell(currentNewId)%z1 = intersections%point(iInt)%z
                    iInt = cellIntersections(sortedIndices(k + 1))
                    new_cln_struct%cell(currentNewId)%x2 = intersections%point(iInt)%x
                    new_cln_struct%cell(currentNewId)%y2 = intersections%point(iInt)%y
                    new_cln_struct%cell(currentNewId)%z2 = intersections%point(iInt)%z
                    new_cln_struct%cell(currentNewId)%next_cell_id = currentNewId + 1
                    new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                    new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                    new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                    new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                    currentNewId = currentNewId + 1
                end do
                
                ! Last segment: last intersection to end
                iInt = cellIntersections(sortedIndices(j))
                new_cln_struct%cell(currentNewId)%id = currentNewId
                new_cln_struct%cell(currentNewId)%x1 = intersections%point(iInt)%x
                new_cln_struct%cell(currentNewId)%y1 = intersections%point(iInt)%y
                new_cln_struct%cell(currentNewId)%z1 = intersections%point(iInt)%z
                new_cln_struct%cell(currentNewId)%x2 = cln_struct%cell(iCell)%x2
                new_cln_struct%cell(currentNewId)%y2 = cln_struct%cell(iCell)%y2
                new_cln_struct%cell(currentNewId)%z2 = cln_struct%cell(iCell)%z2
                ! next_cell_id will be set based on original cell's next_cell_id
                new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                currentNewId = currentNewId + 1
                
                deallocate(cellIntersections, distances, sortedIndices)
            end if
        end do
        
        new_cln_struct%nCells = nNewCells
        
        ! Update next_cell_id to maintain network connectivity
        ! For cells that were split, the segments are already linked sequentially
        ! For the last segment of each original cell, link to the first segment of the next original cell
        ! (if the original cell had a next_cell_id)
        ! This is a simplified approach - in a full implementation, would need to track
        ! which new cells correspond to which original cells more carefully
        
        ! Find last new cell ID for each original cell and update links
        currentNewId = 1
        do iCell = 1, cln_struct%nCells
            ! Count intersections for this cell
            j = 0
            do iInt = 1, intersections%nIntersections
                if (intersections%point(iInt)%cln_cell_id == iCell) then
                    j = j + 1
                end if
            end do
            
            ! Number of new cells from this original cell
            k = 1 + j
            
            ! If original cell had a next_cell_id, find the first new cell from that next cell
            if (cln_struct%cell(iCell)%next_cell_id > 0 .and. &
                cln_struct%cell(iCell)%next_cell_id <= cln_struct%nCells) then
                ! Find first new cell ID for the next original cell
                ! (This requires tracking - simplified: assume cells are processed in order)
                ! For now, set to 0 (end of network) - proper implementation would track this
                new_cln_struct%cell(currentNewId + k - 1)%next_cell_id = 0
            else
                ! End of original network
                new_cln_struct%cell(currentNewId + k - 1)%next_cell_id = 0
            end if
            
            currentNewId = currentNewId + k
        end do
        
        write(TMPStr, '(a,i8)') 'Number of new CLN cells after splitting:', new_cln_struct%nCells
        call Msg(TMPStr)
        
    end subroutine SplitCLNCells
    
    !----------------------------------------------------------------------
    ! Sort array by distance (simple bubble sort)
    !----------------------------------------------------------------------
    subroutine SortByDistance(distances, indices, n)
        implicit none
        real(dp), intent(in) :: distances(:)
        integer(i4), intent(inout) :: indices(:)
        integer(i4), intent(in) :: n
        
        integer(i4) :: i, j, temp
        logical :: swapped
        
        do i = 1, n - 1
            swapped = .false.
            do j = 1, n - i
                if (distances(indices(j)) > distances(indices(j + 1))) then
                    temp = indices(j)
                    indices(j) = indices(j + 1)
                    indices(j + 1) = temp
                    swapped = .true.
                end if
            end do
            if (.not. swapped) exit
        end do
        
    end subroutine SortByDistance
    
    !----------------------------------------------------------------------
    ! Write CLN Structure to File
    !----------------------------------------------------------------------
    subroutine WriteCLNStructure(filename, cln_struct)
        implicit none
        character(*), intent(in) :: filename
        type(t_cln_structure), intent(in) :: cln_struct
        
        integer(i4) :: i, FNum
        
        call OpenAscii(FNum, filename)
        call Msg('  ')
        call Msg(FileCreateSTR//'CLN structure file: '//trim(filename))
        
        ! Write header
        write(FNum, '(a)') '# MODFLOW CLN Structure - Generated by CLNIntersection utility'
        write(FNum, '(a,a)') '# MUT Version: ', trim(MUTVersion)
        write(FNum, '(a)') '# Format: ID X1 Y1 Z1 X2 Y2 Z2 NextID MaterialID Radius/Width Height IsCircular'
        
        ! Write cells
        do i = 1, cln_struct%nCells
            write(FNum, '(i8,6(1x,'//FMT_R8//'),2i8,2(1x,'//FMT_R8//'),l2)') &
                cln_struct%cell(i)%id, &
                cln_struct%cell(i)%x1, cln_struct%cell(i)%y1, cln_struct%cell(i)%z1, &
                cln_struct%cell(i)%x2, cln_struct%cell(i)%y2, cln_struct%cell(i)%z2, &
                cln_struct%cell(i)%next_cell_id, &
                cln_struct%cell(i)%material_id, &
                cln_struct%cell(i)%radius_or_width, &
                cln_struct%cell(i)%height, &
                cln_struct%cell(i)%is_circular
        end do
        
        call FreeUnit(FNum)
        
        write(TMPStr, '(a,i8)') 'Number of CLN cells written:', cln_struct%nCells
        call Msg(TMPStr)
        
    end subroutine WriteCLNStructure

end module CLNIntersection

