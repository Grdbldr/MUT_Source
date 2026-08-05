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
    public :: t_cln_cell, t_cln_structure, t_intersection_point, t_intersection_list
    
    ! Public subroutines
    public :: ReadCLNStructure, ReadCLNFromXYZList
    public :: FindCLN_MeshIntersections
    public :: SplitCLNCells
    public :: WriteCLNStructure
    public :: RunCLNMeshIntersectionWithDomains
    
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
        
        ! Count number of cells (skip blank/comment lines starting with #)
        nCells = 0
        CountLoop: do
            read(FNum, '(a)', iostat=status) VarSTR
            if (status /= 0) exit CountLoop
            if (len_trim(VarSTR) == 0) cycle CountLoop
            if (VarSTR(1:1) == '#') cycle CountLoop
            nCells = nCells + 1
        end do CountLoop
        
        cln_struct%nCells = nCells
        allocate(cln_struct%cell(nCells), stat=ialloc)
        call AllocChk(ialloc, 'ReadCLNStructure: cln_struct%cell array')
        
        ! Rewind and read data (skip blank/comment lines) using free-format file reads
        rewind(FNum)
        i = 0
        ReadLoop: do
            if (i >= nCells) exit ReadLoop
            read(FNum, '(a)', iostat=status) VarSTR
            if (status /= 0) then
                call HandleError(ERR_FILE_IO, 'Error reading CLN cell data', 'ReadCLNStructure')
            end if
            if (len_trim(VarSTR) == 0) cycle ReadLoop
            if (adjustl(VarSTR(1:1)) == '#') cycle ReadLoop
            ! Backspace and re-read the data line with list-directed I/O
            backspace(FNum)
            i = i + 1
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
        end do ReadLoop
        
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
        class(mesh), intent(inout) :: M
        type(t_intersection_list), intent(out) :: intersections
        
        integer(i4) :: iCell, iElem, iFace, iNode, nNodes
        integer(i4) :: nIntersections
        real(dp) :: x1, y1, z1, x2, y2, z2
        real(dp) :: xInt, yInt, zInt
        real(dp) :: dist
        logical :: found_intersection
        integer(i4), allocatable :: nodeIds(:)
        real(dp), allocatable :: faceNodesX(:), faceNodesY(:), faceNodesZ(:)
        
        call Msg('  ')
        call Msg('FindCLN_MeshIntersections: mesh domain = '//trim(M%name))
        
        ! Ensure mesh faces are calculated
        if (.not. M%FacesCalculated) then
            call BuildFaceTopologyFrommesh(M)
        end if
        
        ! Allocate intersection array: each CLN cell can cross up to 2 faces per element traversed
        allocate(intersections%point(cln_struct%nCells * M%nElements * 2), stat=ialloc)
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
                    ! Get face node IDs (skip zero-padded entries for mixed-face elements like prisms)
                    allocate(nodeIds(M%nNodesPerFace), faceNodesX(M%nNodesPerFace), &
                             faceNodesY(M%nNodesPerFace), faceNodesZ(M%nNodesPerFace), stat=ialloc)
                    call AllocChk(ialloc, 'FindCLN_MeshIntersections: face node arrays')
                    
                    nNodes = 0
                    do iNode = 1, M%nNodesPerFace
                        if (M%LocalFaceNodes(iNode, iFace) == 0) cycle
                        nNodes = nNodes + 1
                        nodeIds(nNodes) = M%idNode(M%LocalFaceNodes(iNode, iFace), iElem)
                        faceNodesX(nNodes) = M%node(nodeIds(nNodes))%x
                        faceNodesY(nNodes) = M%node(nodeIds(nNodes))%y
                        faceNodesZ(nNodes) = M%node(nodeIds(nNodes))%z
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
    ! Duplicate intersection distances (within TOL_GEOM) are collapsed so
    ! each GWF/SWF face crossing becomes a single CLN node.
    !----------------------------------------------------------------------
    subroutine SplitCLNCells(cln_struct, intersections, new_cln_struct)
        implicit none
        type(t_cln_structure), intent(in) :: cln_struct
        type(t_intersection_list), intent(in) :: intersections
        type(t_cln_structure), intent(out) :: new_cln_struct
        
        integer(i4) :: iCell, iInt, nNewCells, nUnique, nRaw
        integer(i4) :: j, k, currentNewId
        integer(i4), allocatable :: cellIntersections(:), sortedIndices(:)
        integer(i4), allocatable :: firstNewId(:), lastNewId(:)
        real(dp), allocatable :: distances(:), ux(:), uy(:), uz(:), udist(:)
        real(dp) :: dprev, segLen
        
        allocate(firstNewId(cln_struct%nCells), lastNewId(cln_struct%nCells), stat=ialloc)
        call AllocChk(ialloc, 'SplitCLNCells: first/last new id arrays')
        
        ! First pass: count unique split points per cell
        nNewCells = 0
        do iCell = 1, cln_struct%nCells
            nRaw = 0
            do iInt = 1, intersections%nIntersections
                if (intersections%point(iInt)%cln_cell_id == iCell) nRaw = nRaw + 1
            end do
            if (nRaw == 0) then
                nNewCells = nNewCells + 1
            else
                allocate(distances(nRaw), sortedIndices(nRaw), cellIntersections(nRaw), stat=ialloc)
                call AllocChk(ialloc, 'SplitCLNCells: count pass arrays')
                k = 0
                do iInt = 1, intersections%nIntersections
                    if (intersections%point(iInt)%cln_cell_id == iCell) then
                        k = k + 1
                        cellIntersections(k) = iInt
                        distances(k) = intersections%point(iInt)%distance_from_start
                        sortedIndices(k) = k
                    end if
                end do
                call SortByDistance(distances, sortedIndices, nRaw)
                nUnique = 1
                dprev = distances(sortedIndices(1))
                do k = 2, nRaw
                    if (abs(distances(sortedIndices(k)) - dprev) > TOL_GEOM) then
                        nUnique = nUnique + 1
                        dprev = distances(sortedIndices(k))
                    end if
                end do
                nNewCells = nNewCells + nUnique + 1
                deallocate(distances, sortedIndices, cellIntersections)
            end if
        end do
        
        allocate(new_cln_struct%cell(nNewCells), stat=ialloc)
        call AllocChk(ialloc, 'SplitCLNCells: new_cln_struct%cell array')
        
        currentNewId = 1
        do iCell = 1, cln_struct%nCells
            firstNewId(iCell) = currentNewId
            
            nRaw = 0
            do iInt = 1, intersections%nIntersections
                if (intersections%point(iInt)%cln_cell_id == iCell) nRaw = nRaw + 1
            end do
            
            if (nRaw == 0) then
                new_cln_struct%cell(currentNewId) = cln_struct%cell(iCell)
                new_cln_struct%cell(currentNewId)%id = currentNewId
                lastNewId(iCell) = currentNewId
                currentNewId = currentNewId + 1
            else
                allocate(distances(nRaw), sortedIndices(nRaw), cellIntersections(nRaw), stat=ialloc)
                call AllocChk(ialloc, 'SplitCLNCells: split pass arrays')
                k = 0
                do iInt = 1, intersections%nIntersections
                    if (intersections%point(iInt)%cln_cell_id == iCell) then
                        k = k + 1
                        cellIntersections(k) = iInt
                        distances(k) = intersections%point(iInt)%distance_from_start
                        sortedIndices(k) = k
                    end if
                end do
                call SortByDistance(distances, sortedIndices, nRaw)
                
                ! Build unique intersection coordinates ordered along the cell
                allocate(ux(nRaw), uy(nRaw), uz(nRaw), udist(nRaw), stat=ialloc)
                call AllocChk(ialloc, 'SplitCLNCells: unique intersection arrays')
                nUnique = 0
                dprev = -1.0d30
                do k = 1, nRaw
                    iInt = cellIntersections(sortedIndices(k))
                    if (nUnique == 0 .or. abs(distances(sortedIndices(k)) - dprev) > TOL_GEOM) then
                        nUnique = nUnique + 1
                        ux(nUnique) = intersections%point(iInt)%x
                        uy(nUnique) = intersections%point(iInt)%y
                        uz(nUnique) = intersections%point(iInt)%z
                        udist(nUnique) = distances(sortedIndices(k))
                        dprev = distances(sortedIndices(k))
                    end if
                end do
                
                ! Segment: start -> first unique intersection
                new_cln_struct%cell(currentNewId)%id = currentNewId
                new_cln_struct%cell(currentNewId)%x1 = cln_struct%cell(iCell)%x1
                new_cln_struct%cell(currentNewId)%y1 = cln_struct%cell(iCell)%y1
                new_cln_struct%cell(currentNewId)%z1 = cln_struct%cell(iCell)%z1
                new_cln_struct%cell(currentNewId)%x2 = ux(1)
                new_cln_struct%cell(currentNewId)%y2 = uy(1)
                new_cln_struct%cell(currentNewId)%z2 = uz(1)
                new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                new_cln_struct%cell(currentNewId)%next_cell_id = currentNewId + 1
                currentNewId = currentNewId + 1
                
                ! Middle segments between unique intersections
                do k = 1, nUnique - 1
                    new_cln_struct%cell(currentNewId)%id = currentNewId
                    new_cln_struct%cell(currentNewId)%x1 = ux(k)
                    new_cln_struct%cell(currentNewId)%y1 = uy(k)
                    new_cln_struct%cell(currentNewId)%z1 = uz(k)
                    new_cln_struct%cell(currentNewId)%x2 = ux(k + 1)
                    new_cln_struct%cell(currentNewId)%y2 = uy(k + 1)
                    new_cln_struct%cell(currentNewId)%z2 = uz(k + 1)
                    new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                    new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                    new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                    new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                    new_cln_struct%cell(currentNewId)%next_cell_id = currentNewId + 1
                    currentNewId = currentNewId + 1
                end do
                
                ! Last segment: last unique intersection -> end
                new_cln_struct%cell(currentNewId)%id = currentNewId
                new_cln_struct%cell(currentNewId)%x1 = ux(nUnique)
                new_cln_struct%cell(currentNewId)%y1 = uy(nUnique)
                new_cln_struct%cell(currentNewId)%z1 = uz(nUnique)
                new_cln_struct%cell(currentNewId)%x2 = cln_struct%cell(iCell)%x2
                new_cln_struct%cell(currentNewId)%y2 = cln_struct%cell(iCell)%y2
                new_cln_struct%cell(currentNewId)%z2 = cln_struct%cell(iCell)%z2
                new_cln_struct%cell(currentNewId)%material_id = cln_struct%cell(iCell)%material_id
                new_cln_struct%cell(currentNewId)%radius_or_width = cln_struct%cell(iCell)%radius_or_width
                new_cln_struct%cell(currentNewId)%height = cln_struct%cell(iCell)%height
                new_cln_struct%cell(currentNewId)%is_circular = cln_struct%cell(iCell)%is_circular
                new_cln_struct%cell(currentNewId)%next_cell_id = 0
                lastNewId(iCell) = currentNewId
                currentNewId = currentNewId + 1
                
                deallocate(distances, sortedIndices, cellIntersections, ux, uy, uz, udist)
            end if
        end do
        
        new_cln_struct%nCells = nNewCells
        
        ! Link last segment of each original cell to the first segment of its next cell
        do iCell = 1, cln_struct%nCells
            if (cln_struct%cell(iCell)%next_cell_id > 0 .and. &
                cln_struct%cell(iCell)%next_cell_id <= cln_struct%nCells) then
                new_cln_struct%cell(lastNewId(iCell))%next_cell_id = firstNewId(cln_struct%cell(iCell)%next_cell_id)
            else
                new_cln_struct%cell(lastNewId(iCell))%next_cell_id = 0
            end if
        end do
        
        ! Drop any accidental zero-length segments by reconnecting neighbors
        j = 0
        do iCell = 1, new_cln_struct%nCells
            segLen = sqrt( &
                (new_cln_struct%cell(iCell)%x2 - new_cln_struct%cell(iCell)%x1)**2 + &
                (new_cln_struct%cell(iCell)%y2 - new_cln_struct%cell(iCell)%y1)**2 + &
                (new_cln_struct%cell(iCell)%z2 - new_cln_struct%cell(iCell)%z1)**2)
            if (segLen > TOL_GEOM) j = j + 1
        end do
        if (j < new_cln_struct%nCells) then
            call CompactCLNStructure(new_cln_struct, TOL_GEOM)
        end if
        
        deallocate(firstNewId, lastNewId)
        
        write(TMPStr, '(a,i8)') 'Number of new CLN cells after splitting:', new_cln_struct%nCells
        call Msg(TMPStr)
        
    end subroutine SplitCLNCells
    
    !----------------------------------------------------------------------
    ! Remove zero-length CLN segments and renumber sequential connectivity
    !----------------------------------------------------------------------
    subroutine CompactCLNStructure(cln_struct, tol)
        implicit none
        type(t_cln_structure), intent(inout) :: cln_struct
        real(dp), intent(in) :: tol
        
        type(t_cln_cell), allocatable :: kept(:)
        integer(i4) :: i, nKeep
        real(dp) :: segLen
        
        allocate(kept(cln_struct%nCells), stat=ialloc)
        call AllocChk(ialloc, 'CompactCLNStructure: kept cells')
        nKeep = 0
        do i = 1, cln_struct%nCells
            segLen = sqrt( &
                (cln_struct%cell(i)%x2 - cln_struct%cell(i)%x1)**2 + &
                (cln_struct%cell(i)%y2 - cln_struct%cell(i)%y1)**2 + &
                (cln_struct%cell(i)%z2 - cln_struct%cell(i)%z1)**2)
            if (segLen > tol) then
                nKeep = nKeep + 1
                kept(nKeep) = cln_struct%cell(i)
                kept(nKeep)%id = nKeep
                if (nKeep > 1) kept(nKeep - 1)%next_cell_id = nKeep
            end if
        end do
        if (nKeep > 0) kept(nKeep)%next_cell_id = 0
        deallocate(cln_struct%cell)
        allocate(cln_struct%cell(nKeep), stat=ialloc)
        call AllocChk(ialloc, 'CompactCLNStructure: compacted cells')
        cln_struct%cell(1:nKeep) = kept(1:nKeep)
        cln_struct%nCells = nKeep
        deallocate(kept)
    end subroutine CompactCLNStructure
    
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
    
    !----------------------------------------------------------------------
    ! Merge two intersection lists into one (order preserved; SplitCLNCells sorts by distance per cell)
    !----------------------------------------------------------------------
    subroutine MergeIntersectionLists(list1, list2, merged)
        implicit none
        type(t_intersection_list), intent(in) :: list1, list2
        type(t_intersection_list), intent(out) :: merged
        
        integer(i4) :: n1, n2
        
        n1 = list1%nIntersections
        n2 = list2%nIntersections
        merged%nIntersections = n1 + n2
        if (merged%nIntersections <= 0) return
        allocate(merged%point(max(1, merged%nIntersections)), stat=ialloc)
        call AllocChk(ialloc, 'MergeIntersectionLists: merged%point')
        if (n1 > 0) merged%point(1:n1) = list1%point(1:n1)
        if (n2 > 0) merged%point(n1+1:n1+n2) = list2%point(1:n2)
    end subroutine MergeIntersectionLists

    !----------------------------------------------------------------------
    ! Run CLN-mesh intersection using in-memory GWF (and optionally SWF) domains.
    ! CLN input is always read from a user-defined XYZ file.
    ! Builds face topology, finds intersections for each domain, merges lists, splits cells, writes output.
    !----------------------------------------------------------------------
    subroutine RunCLNMeshIntersectionWithDomains(cln_filename, output_cln_filename, &
            GWF_domain, SWF_domain)
        implicit none
        character(*), intent(in) :: cln_filename
        character(*), intent(in) :: output_cln_filename
        class(mesh), intent(inout) :: GWF_domain
        class(mesh), intent(inout), optional :: SWF_domain
        
        type(t_cln_structure) :: cln_struct
        type(t_cln_structure) :: new_cln_struct
        type(t_intersection_list) :: intersections_gwf
        type(t_intersection_list) :: intersections_swf
        type(t_intersection_list) :: intersections_merged
        character(MAX_STR) :: xyz_out
        
        ! Read CLN input from XYZ file
        call ReadCLNFromXYZList(trim(cln_filename), cln_struct)
        
        ! Clear stale 2D face topology inherited from the template mesh copy
        ! so BuildFaceTopologyFrommesh rebuilds it correctly for the 3D mesh
        call ClearMeshFaceTopology(GWF_domain)
        if (GWF_domain%nNodesPerElement == 6) then
            GWF_domain%TecplotTyp = 'feprism'
        else if (GWF_domain%nNodesPerElement == 8) then
            GWF_domain%TecplotTyp = 'febrick'
        end if
        
        ! Build face topology and find intersections for GWF
        call BuildFaceTopologyFrommesh(GWF_domain)
        call FindCLN_MeshIntersections(cln_struct, GWF_domain, intersections_gwf)

        ! Restore Tecplot-compatible zonetype (connectivity is written as febrick)
        if (GWF_domain%nNodesPerElement == 6) then
            GWF_domain%TecplotTyp = 'febrick'
        end if
        
        if (present(SWF_domain)) then
            call ClearMeshFaceTopology(SWF_domain)
            ! Build face topology and find intersections for SWF
            call BuildFaceTopologyFrommesh(SWF_domain)
            call FindCLN_MeshIntersections(cln_struct, SWF_domain, intersections_swf)
            ! Merge and split
            call MergeIntersectionLists(intersections_gwf, intersections_swf, intersections_merged)
            call SplitCLNCells(cln_struct, intersections_merged, new_cln_struct)
        else
            call SplitCLNCells(cln_struct, intersections_gwf, new_cln_struct)
        end if
        
        call WriteCLNStructure(trim(output_cln_filename), new_cln_struct)
        
        ! Also write an XYZ polyline list that can be used by "cln from xyz list"
        xyz_out = trim(output_cln_filename)
        if (len_trim(xyz_out) > 4) then
            if (xyz_out(len_trim(xyz_out)-3:len_trim(xyz_out)) == '.dat') then
                xyz_out = xyz_out(1:len_trim(xyz_out)-4)//'.xyzList'
            else
                xyz_out = trim(xyz_out)//'.xyzList'
            end if
        else
            xyz_out = trim(xyz_out)//'.xyzList'
        end if
        call WriteCLNXYZList(trim(xyz_out), new_cln_struct)
        
        call Msg('CLN-mesh intersection utility (GWF/SWF domains) completed.')
        
    end subroutine RunCLNMeshIntersectionWithDomains
    
    !----------------------------------------------------------------------
    subroutine ClearMeshFaceTopology(M)
        implicit none
        class(mesh), intent(inout) :: M
        
        if (allocated(M%FaceHost)) deallocate(M%FaceHost)
        if (allocated(M%FaceNeighbour)) deallocate(M%FaceNeighbour)
        if (allocated(M%FaceCentroidX)) deallocate(M%FaceCentroidX)
        if (allocated(M%FaceCentroidY)) deallocate(M%FaceCentroidY)
        if (allocated(M%FaceCentroidZ)) deallocate(M%FaceCentroidZ)
        if (allocated(M%LocalFaceNodes)) deallocate(M%LocalFaceNodes)
        M%FacesCalculated = .false.
        M%nFaces = 0
    end subroutine ClearMeshFaceTopology
    
    !----------------------------------------------------------------------
    ! Write sequential XYZ polyline list from a CLN structure
    !----------------------------------------------------------------------
    subroutine WriteCLNXYZList(filename, cln_struct)
        implicit none
        character(*), intent(in) :: filename
        type(t_cln_structure), intent(in) :: cln_struct
        
        integer(i4) :: i, FNum, nPoints
        
        if (cln_struct%nCells <= 0) return
        
        call OpenAscii(FNum, filename)
        call Msg('  ')
        call Msg(FileCreateSTR//'CLN XYZ list file: '//trim(filename))
        
        write(FNum, '(a)') 'ID X Y Z'
        write(FNum, '(i8,3(1x,'//FMT_R8//'))') 1, &
            cln_struct%cell(1)%x1, cln_struct%cell(1)%y1, cln_struct%cell(1)%z1
        nPoints = 1
        do i = 1, cln_struct%nCells
            nPoints = nPoints + 1
            write(FNum, '(i8,3(1x,'//FMT_R8//'))') nPoints, &
                cln_struct%cell(i)%x2, cln_struct%cell(i)%y2, cln_struct%cell(i)%z2
        end do
        
        call FreeUnit(FNum)
        write(TMPStr, '(a,i8)') 'Number of XYZ points written:', nPoints
        call Msg(TMPStr)
    end subroutine WriteCLNXYZList

end module CLNIntersection

