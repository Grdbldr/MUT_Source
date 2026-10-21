! Domain mesh cache: DomainBIN + .reuse stamps so later mut _build runs can skip
! Generate*/BuildModflow*Domain when the template and generate-block inputs match.
module MUSG_DomainCache
    use KindParameters, only: dp, i4
    use GeneralRoutines
    use NumericalMesh
    use MUSG_Core, only: ModflowDomain, NodalControlVolume
    use CLNIntersection, only: MeshReuseChecksum, FileSizeAndMtime, FileContentChecksum, &
        ClearMeshFaceTopology, MarkCLNFacesReady
    implicit none
    private

    public :: SaveDomainBIN, ReadDomainBIN
    public :: DomainCachePaths
    public :: TryReuseDomain, SaveDomainCacheAfterBuild
    public :: PeekGenerateBlockFingerprint
    public :: ConsumeGenerateBlock
    public :: DomainReuseChecksum
    public :: InitSWFZoneDefaultsAfterLoad, InitCLNZoneDefaultsAfterLoad
    public :: TryReuseCLNGWFConnections, SaveCLNGWFConnectionsCache

    integer(i4), parameter :: DOMAINBIN_MAGIC = 20250341
    integer(i4), parameter :: CLNGWFCONN_MAGIC = 20250351
    integer(i4), parameter :: MAX_PEEK_LINES = 2000

contains

    !----------------------------------------------------------------------
    subroutine DomainCachePaths(prefix, domain_tag, bin_path, reuse_path)
        implicit none
        character(*), intent(in) :: prefix, domain_tag
        character(*), intent(out) :: bin_path, reuse_path
        bin_path = trim(prefix)//'.'//trim(domain_tag)//'.DomainBIN'
        reuse_path = trim(prefix)//'.'//trim(domain_tag)//'.DomainBIN.reuse'
    end subroutine DomainCachePaths

    !----------------------------------------------------------------------
    subroutine DomainReuseChecksum(D, checksum)
        implicit none
        type(ModflowDomain), intent(in) :: D
        integer(kind=8), intent(out) :: checksum
        integer(i4) :: i

        call MeshReuseChecksum(D, checksum)
        checksum = checksum + int(D%nCells, kind=8) * 10000019_8
        checksum = checksum + int(D%nodelay, kind=8) * 10000079_8
        if (.not. allocated(D%cell)) return
        do i = 1, D%nCells
            checksum = checksum + nint(D%cell(i)%x * 1.0d3, kind=8)
            checksum = checksum + nint(D%cell(i)%y * 1.0d3, kind=8)
            checksum = checksum + nint(D%cell(i)%z * 1.0d3, kind=8)
            checksum = checksum + nint(D%cell(i)%Top * 1.0d3, kind=8)
            checksum = checksum + nint(D%cell(i)%Bottom * 1.0d3, kind=8)
        end do
    end subroutine DomainReuseChecksum

    !----------------------------------------------------------------------
    ! Peek generate-block lines (nested end-aware), fingerprint, then backspace.
    !----------------------------------------------------------------------
    subroutine PeekGenerateBlockFingerprint(FNumMUT, block_cs, files_cs, ok)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        integer(kind=8), intent(out) :: block_cs, files_cs
        logical, intent(out) :: ok
        character(MAX_INST) :: line, lwr, prev
        integer(i4) :: nlines, depth, i, ios
        integer(kind=8) :: fsize, fmtime, fcs
        logical :: file_ok
        character(MAX_INST) :: lines(MAX_PEEK_LINES)

        block_cs = 0_8
        files_cs = 0_8
        ok = .false.
        nlines = 0
        depth = 1
        prev = ' '

        do while (depth > 0)
            read(FNumMUT, '(a)', iostat=ios) line
            if (ios /= 0) exit
            nlines = nlines + 1
            if (nlines > MAX_PEEK_LINES) then
                do i = 1, nlines
                    backspace(FNumMUT)
                end do
                call WarnMsg('Domain cache: generate block too long to fingerprint')
                return
            end if
            lines(nlines) = line
            lwr = line
            call LwrCse(lwr)

            ! Check 'end' first so "end top elevation" does not look like an opener
            if (index(lwr, 'end') /= 0) then
                depth = depth - 1
            else if (index(lwr, 'top elevation') /= 0 .or. &
                     index(lwr, 'new layer') /= 0 .or. &
                     index(lwr, 'elevation from xz pairs') /= 0) then
                depth = depth + 1
            end if

            ! File path on the line after a file-bearing instruction
            if (index(prev, 'elevation from list file') /= 0 .or. &
                index(prev, 'elevation from gb file') /= 0 .or. &
                index(prev, 'cln from xyz list') /= 0 .or. &
                index(prev, 'cln from structure file') /= 0) then
                if (len_trim(line) > 0) then
                    call FileSizeAndMtime(trim(adjustl(line)), fsize, fmtime, file_ok)
                    if (file_ok) then
                        call FileContentChecksum(trim(adjustl(line)), fcs, file_ok)
                        if (file_ok) then
                            files_cs = files_cs + fsize + fcs
                        end if
                    end if
                end if
            end if
            prev = lwr
        end do

        do i = 1, nlines
            do ios = 1, len_trim(lines(i))
                block_cs = block_cs + iachar(lines(i)(ios:ios)) * int(i + ios, kind=8)
            end do
        end do

        do i = 1, nlines
            backspace(FNumMUT)
        end do
        ok = (nlines > 0 .and. depth == 0)
        if (.not. ok) then
            call WarnMsg('Domain cache: generate-block fingerprint incomplete')
        end if
    end subroutine PeekGenerateBlockFingerprint

    !----------------------------------------------------------------------
    ! Advance past a generate block without building (after DomainBIN hit).
    !----------------------------------------------------------------------
    subroutine ConsumeGenerateBlock(FNumMUT)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        character(MAX_INST) :: line, lwr
        integer(i4) :: depth, ios

        depth = 1
        do while (depth > 0)
            read(FNumMUT, '(a)', iostat=ios) line
            if (ios /= 0) exit
            lwr = line
            call LwrCse(lwr)
            call Msg(trim(line))
            if (index(lwr, 'end') /= 0) then
                depth = depth - 1
            else if (index(lwr, 'top elevation') /= 0 .or. &
                     index(lwr, 'new layer') /= 0 .or. &
                     index(lwr, 'elevation from xz pairs') /= 0) then
                depth = depth + 1
            end if
        end do
    end subroutine ConsumeGenerateBlock

    !----------------------------------------------------------------------
    subroutine WriteDomainReuseStamp(reuse_path, domain_tag, TMPLT, D, &
            block_cs, files_cs)
        implicit none
        character(*), intent(in) :: reuse_path, domain_tag
        type(mesh), intent(in) :: TMPLT
        type(ModflowDomain), intent(in) :: D
        integer(kind=8), intent(in) :: block_cs, files_cs
        integer(i4) :: FNum, ncv_flag
        integer(kind=8) :: tmplt_cs, domain_cs

        call MeshReuseChecksum(TMPLT, tmplt_cs)
        call DomainReuseChecksum(D, domain_cs)
        ncv_flag = 0
        if (NodalControlVolume) ncv_flag = 1

        call GetUnit(FNum)
        open(FNum, file=trim(reuse_path), form='formatted', status='replace', iostat=status)
        if (status /= 0) then
            call FreeUnit(FNum)
            call WarnMsg('Could not write domain reuse stamp: '//trim(reuse_path))
            return
        end if
        write(FNum, '(a)') 'mut='//trim(adjustl(MUTVersion))
        write(FNum, '(a,i0)') 'ncv=', ncv_flag
        write(FNum, '(a)') 'domain='//trim(domain_tag)
        write(FNum, '(a,i0)') 'tmplt_checksum=', tmplt_cs
        write(FNum, '(a,i0)') 'block_checksum=', block_cs
        write(FNum, '(a,i0)') 'files_checksum=', files_cs
        write(FNum, '(a,i0)') 'domain_checksum=', domain_cs
        write(FNum, '(a,i0)') 'nnodes=', D%nNodes
        write(FNum, '(a,i0)') 'nelements=', D%nElements
        write(FNum, '(a,i0)') 'ncells=', D%nCells
        call FreeUnit(FNum)
    end subroutine WriteDomainReuseStamp

    !----------------------------------------------------------------------
    logical function DomainReuseStampMatches(reuse_path, domain_tag, TMPLT, &
            block_cs, files_cs)
        implicit none
        character(*), intent(in) :: reuse_path, domain_tag
        type(mesh), intent(in) :: TMPLT
        integer(kind=8), intent(in) :: block_cs, files_cs
        character(MAX_STR) :: expected(12), actual
        integer(i4) :: FNum, nexp, i, ios, ncv_flag
        integer(kind=8) :: tmplt_cs
        logical :: exists

        DomainReuseStampMatches = .false.
        inquire(file=trim(reuse_path), exist=exists)
        if (.not. exists) then
            call Msg('Domain cache: no stamp '//trim(reuse_path))
            return
        end if
        call MeshReuseChecksum(TMPLT, tmplt_cs)
        ncv_flag = 0
        if (NodalControlVolume) ncv_flag = 1

        nexp = 0
        nexp = nexp + 1; write(expected(nexp), '(a)') 'mut='//trim(adjustl(MUTVersion))
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'ncv=', ncv_flag
        nexp = nexp + 1; write(expected(nexp), '(a)') 'domain='//trim(domain_tag)
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'tmplt_checksum=', tmplt_cs
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'block_checksum=', block_cs
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'files_checksum=', files_cs
        ! domain_checksum / sizes checked after load

        call OpenAscii(FNum, reuse_path)
        do i = 1, nexp
            read(FNum, '(a)', iostat=ios) actual
            if (ios /= 0) then
                call FreeUnit(FNum)
                call Msg('Domain cache miss: short stamp at '//trim(expected(i)))
                return
            end if
            if (trim(actual) /= trim(expected(i))) then
                call FreeUnit(FNum)
                call Msg('Domain cache miss: '//trim(expected(i))// &
                    ' (stamp had '//trim(actual)//')')
                return
            end if
        end do
        call FreeUnit(FNum)
        DomainReuseStampMatches = .true.
    end function DomainReuseStampMatches

    !----------------------------------------------------------------------
    subroutine SaveDomainBIN(bin_path, D)
        implicit none
        character(*), intent(in) :: bin_path
        type(ModflowDomain), intent(inout) :: D
        integer(i4) :: FNum, i, j, nConn, hasTF, hasFace, hasCL, hasLaybcd
        integer(i4) :: nFaceHostElems, nTF

        ! Do not persist stale 2D faces on extruded GWF
        if (D%nNodesPerElement >= 6) then
            call ClearMeshFaceTopology(D)
        end if

        call OpenBinary(FNum, bin_path)
        call Msg('  ')
        call Msg(FileCreateSTR//'DomainBIN file: '//trim(bin_path))

        write(FNum) DOMAINBIN_MAGIC
        write(FNum) D%name
        write(FNum) D%MeshType
        write(FNum) D%TecplotTyp
        write(FNum) D%nNodes, D%nElements, D%nNodesPerElement, D%nLayers, D%nZones
        write(FNum) D%nCells, D%nNodesPerCell, D%nodelay, D%njag
        write(FNum) D%xMin, D%xMax, D%yMin, D%yMax, D%zMin, D%zMax

        do i = 1, D%nNodes
            write(FNum) D%node(i)%id, D%node(i)%x, D%node(i)%y, D%node(i)%z, D%node(i)%is
        end do

        write(FNum) D%idNode
        do i = 1, D%nElements
            write(FNum) D%element(i)%id, D%element(i)%typ, D%element(i)%idZone, D%element(i)%iLayer
            write(FNum) D%element(i)%x, D%element(i)%y, D%element(i)%z, D%element(i)%is
            write(FNum) D%element(i)%area, D%element(i)%xyArea
            write(FNum) D%element(i)%xCircle, D%element(i)%yCircle, D%element(i)%zCircle, D%element(i)%rCircle
            do j = 1, min(4, D%nNodesPerElement)
                write(FNum) D%element(i)%xSide(j), D%element(i)%ySide(j), D%element(i)%SideLength(j)
            end do
        end do

        do i = 1, D%nZones
            write(FNum) D%zone(i)%name
            write(FNum) D%zone(i)%id, D%zone(i)%is
        end do

        nConn = 0
        if (allocated(D%ia)) nConn = size(D%ia)
        write(FNum) nConn
        if (nConn > 0) then
            write(FNum) D%ia(1:nConn)
            do i = 1, nConn
                write(FNum) D%ConnectionList(:, i)
            end do
        end if

        hasTF = 0
        nTF = 0
        if (allocated(D%ThroughFace)) then
            hasTF = 1
            nTF = size(D%ThroughFace, 2)
        end if
        write(FNum) hasTF, nTF
        if (hasTF == 1) then
            do i = 1, nTF
                write(FNum) D%ThroughFace(:, i)
            end do
        end if

        hasFace = 0
        nFaceHostElems = 0
        if (allocated(D%FaceHost) .and. D%FacesCalculated) then
            hasFace = 1
            nFaceHostElems = size(D%FaceHost, 2)
        end if
        write(FNum) hasFace, D%nFaces, D%nNodesPerFace, D%nFacesPerElement, nFaceHostElems
        if (hasFace == 1) then
            write(FNum) D%LocalFaceNodes
            do j = 1, nFaceHostElems
                write(FNum) (D%FaceHost(i, j), i = 1, D%nFacesPerElement)
                write(FNum) (D%FaceNeighbour(i, j), i = 1, D%nFacesPerElement)
                write(FNum) (D%FaceCentroidX(i, j), i = 1, D%nFacesPerElement)
                write(FNum) (D%FaceCentroidY(i, j), i = 1, D%nFacesPerElement)
                write(FNum) (D%FaceCentroidZ(i, j), i = 1, D%nFacesPerElement)
            end do
        end if

        do i = 1, D%nCells
            write(FNum) D%cell(i)%x, D%cell(i)%y, D%cell(i)%z
            write(FNum) D%cell(i)%xyArea, D%cell(i)%Top, D%cell(i)%Bottom
            write(FNum) D%cell(i)%iLayer, D%cell(i)%idZone, D%cell(i)%id
            write(FNum) D%cell(i)%Length, D%cell(i)%LowestElevation, D%cell(i)%SlopeAngle
        end do

        hasCL = 0
        if (allocated(D%ConnectionLength) .and. allocated(D%PerpendicularArea)) hasCL = 1
        write(FNum) hasCL
        if (hasCL == 1) then
            write(FNum) size(D%ConnectionLength, 2)
            do i = 1, size(D%ConnectionLength, 2)
                write(FNum) D%ConnectionLength(:, i)
                write(FNum) D%PerpendicularArea(:, i)
            end do
        end if

        hasLaybcd = 0
        if (allocated(D%Laybcd)) hasLaybcd = 1
        write(FNum) hasLaybcd
        if (hasLaybcd == 1) then
            write(FNum) size(D%Laybcd)
            write(FNum) D%Laybcd
        end if

        call FreeUnit(FNum)
    end subroutine SaveDomainBIN

    !----------------------------------------------------------------------
    subroutine ReadDomainBIN(bin_path, D, ok)
        implicit none
        character(*), intent(in) :: bin_path
        type(ModflowDomain), intent(inout) :: D
        logical, intent(out) :: ok
        integer(i4) :: FNum, i, j, nConn, hasTF, hasFace, hasCL, hasLaybcd
        integer(i4) :: magic, nFaceHostElems, nCL, nLay, nTF
        logical :: exists

        ok = .false.
        inquire(file=trim(bin_path), exist=exists)
        if (.not. exists) return

        call OpenBinary(FNum, bin_path)
        call Msg('  ')
        call Msg(FileReadSTR//'DomainBIN file: '//trim(bin_path))

        read(FNum, iostat=status) magic
        if (status /= 0 .or. magic /= DOMAINBIN_MAGIC) then
            call FreeUnit(FNum)
            call WarnMsg('DomainBIN magic mismatch: '//trim(bin_path))
            return
        end if

        read(FNum) D%name
        read(FNum) D%MeshType
        read(FNum) D%TecplotTyp
        read(FNum) D%nNodes, D%nElements, D%nNodesPerElement, D%nLayers, D%nZones
        read(FNum) D%nCells, D%nNodesPerCell, D%nodelay, D%njag
        read(FNum) D%xMin, D%xMax, D%yMin, D%yMax, D%zMin, D%zMax

        if (allocated(D%Node)) deallocate(D%Node)
        allocate(D%Node(D%nNodes), stat=ialloc)
        call AllocChk(ialloc, 'ReadDomainBIN: Node')
        do i = 1, D%nNodes
            read(FNum) D%node(i)%id, D%node(i)%x, D%node(i)%y, D%node(i)%z, D%node(i)%is
            D%node(i)%is = 0
        end do

        if (allocated(D%Element)) deallocate(D%Element)
        if (allocated(D%idNode)) deallocate(D%idNode)
        allocate(D%Element(D%nElements), D%idNode(D%nNodesPerElement, D%nElements), stat=ialloc)
        call AllocChk(ialloc, 'ReadDomainBIN: Element/idNode')
        read(FNum) D%idNode
        do i = 1, D%nElements
            read(FNum) D%element(i)%id, D%element(i)%typ, D%element(i)%idZone, D%element(i)%iLayer
            read(FNum) D%element(i)%x, D%element(i)%y, D%element(i)%z, D%element(i)%is
            read(FNum) D%element(i)%area, D%element(i)%xyArea
            read(FNum) D%element(i)%xCircle, D%element(i)%yCircle, D%element(i)%zCircle, D%element(i)%rCircle
            do j = 1, min(4, D%nNodesPerElement)
                read(FNum) D%element(i)%xSide(j), D%element(i)%ySide(j), D%element(i)%SideLength(j)
            end do
            D%element(i)%is = 0
        end do

        if (allocated(D%Zone)) deallocate(D%Zone)
        allocate(D%Zone(max(1, D%nZones)), stat=ialloc)
        call AllocChk(ialloc, 'ReadDomainBIN: Zone')
        do i = 1, D%nZones
            read(FNum) D%zone(i)%name
            read(FNum) D%zone(i)%id, D%zone(i)%is
            D%zone(i)%is = 0
        end do

        read(FNum) nConn
        if (allocated(D%ia)) deallocate(D%ia)
        if (allocated(D%ConnectionList)) deallocate(D%ConnectionList)
        if (nConn > 0) then
            allocate(D%ia(nConn), D%ConnectionList(MAX_CNCTS, nConn), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: ia/ConnectionList')
            read(FNum) D%ia(1:nConn)
            do i = 1, nConn
                read(FNum) D%ConnectionList(:, i)
            end do
        end if

        read(FNum) hasTF, nTF
        if (allocated(D%ThroughFace)) deallocate(D%ThroughFace)
        if (hasTF == 1 .and. nTF > 0) then
            allocate(D%ThroughFace(MAX_CNCTS, nTF), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: ThroughFace')
            do i = 1, nTF
                read(FNum) D%ThroughFace(:, i)
            end do
        end if

        read(FNum) hasFace, D%nFaces, D%nNodesPerFace, D%nFacesPerElement, nFaceHostElems
        D%FacesCalculated = .false.
        if (allocated(D%FaceHost)) deallocate(D%FaceHost)
        if (allocated(D%FaceNeighbour)) deallocate(D%FaceNeighbour)
        if (allocated(D%FaceCentroidX)) deallocate(D%FaceCentroidX)
        if (allocated(D%FaceCentroidY)) deallocate(D%FaceCentroidY)
        if (allocated(D%FaceCentroidZ)) deallocate(D%FaceCentroidZ)
        if (allocated(D%LocalFaceNodes)) deallocate(D%LocalFaceNodes)
        if (hasFace == 1 .and. nFaceHostElems > 0) then
            allocate(D%LocalFaceNodes(D%nNodesPerFace, D%nFacesPerElement), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: LocalFaceNodes')
            read(FNum) D%LocalFaceNodes
            allocate(D%FaceHost(D%nFacesPerElement, nFaceHostElems), &
                     D%FaceNeighbour(D%nFacesPerElement, nFaceHostElems), &
                     D%FaceCentroidX(D%nFacesPerElement, nFaceHostElems), &
                     D%FaceCentroidY(D%nFacesPerElement, nFaceHostElems), &
                     D%FaceCentroidZ(D%nFacesPerElement, nFaceHostElems), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: Face arrays')
            do j = 1, nFaceHostElems
                read(FNum) (D%FaceHost(i, j), i = 1, D%nFacesPerElement)
                read(FNum) (D%FaceNeighbour(i, j), i = 1, D%nFacesPerElement)
                read(FNum) (D%FaceCentroidX(i, j), i = 1, D%nFacesPerElement)
                read(FNum) (D%FaceCentroidY(i, j), i = 1, D%nFacesPerElement)
                read(FNum) (D%FaceCentroidZ(i, j), i = 1, D%nFacesPerElement)
            end do
            D%FacesCalculated = .true.
        end if

        if (allocated(D%cell)) deallocate(D%cell)
        allocate(D%cell(D%nCells), stat=ialloc)
        call AllocChk(ialloc, 'ReadDomainBIN: cell')
        do i = 1, D%nCells
            read(FNum) D%cell(i)%x, D%cell(i)%y, D%cell(i)%z
            read(FNum) D%cell(i)%xyArea, D%cell(i)%Top, D%cell(i)%Bottom
            read(FNum) D%cell(i)%iLayer, D%cell(i)%idZone, D%cell(i)%id
            read(FNum) D%cell(i)%Length, D%cell(i)%LowestElevation, D%cell(i)%SlopeAngle
            D%cell(i)%is = 0
            D%cell(i)%Kh = -999.0
            D%cell(i)%Kv = -999.0
            D%cell(i)%Ss = -999.0
            D%cell(i)%Sy = -999.0
            D%cell(i)%Alpha = -999.0
            D%cell(i)%Beta = -999.0
            D%cell(i)%Sr = -999.0
            D%cell(i)%Brooks = -999.0
            D%cell(i)%Porosity = 1.0
            D%cell(i)%StartingHeads = -999.0d0
            D%cell(i)%Sgcl = 0.001
            D%cell(i)%CriticalDepthLength = 0.0d0
            D%cell(i)%FSKIN = -999.0
        end do

        read(FNum) hasCL
        if (allocated(D%ConnectionLength)) deallocate(D%ConnectionLength)
        if (allocated(D%PerpendicularArea)) deallocate(D%PerpendicularArea)
        if (hasCL == 1) then
            read(FNum) nCL
            allocate(D%ConnectionLength(MAX_CNCTS, nCL), &
                     D%PerpendicularArea(MAX_CNCTS, nCL), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: CL/FAHL')
            do i = 1, nCL
                read(FNum) D%ConnectionLength(:, i)
                read(FNum) D%PerpendicularArea(:, i)
            end do
        end if

        read(FNum) hasLaybcd
        if (allocated(D%Laybcd)) deallocate(D%Laybcd)
        if (hasLaybcd == 1) then
            read(FNum) nLay
            allocate(D%Laybcd(nLay), stat=ialloc)
            call AllocChk(ialloc, 'ReadDomainBIN: Laybcd')
            read(FNum) D%Laybcd
        end if

        call FreeUnit(FNum)
        D%IsDefined = .true.
        ok = .true.
    end subroutine ReadDomainBIN

    !----------------------------------------------------------------------
    ! Try to load DomainBIN when stamp matches; consume generate block on hit.
    !----------------------------------------------------------------------
    logical function TryReuseDomain(FNumMUT, prefix, domain_tag, TMPLT, D)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        character(*), intent(in) :: prefix, domain_tag
        type(mesh), intent(in) :: TMPLT
        type(ModflowDomain), intent(inout) :: D
        character(MAX_STR) :: bin_path, reuse_path
        integer(kind=8) :: block_cs, files_cs
        logical :: peek_ok, bin_ok, exists

        TryReuseDomain = .false.
        call DomainCachePaths(prefix, domain_tag, bin_path, reuse_path)
        inquire(file=trim(bin_path), exist=exists)
        if (.not. exists) return

        call PeekGenerateBlockFingerprint(FNumMUT, block_cs, files_cs, peek_ok)
        if (.not. peek_ok) return
        if (.not. DomainReuseStampMatches(reuse_path, domain_tag, TMPLT, block_cs, files_cs)) return

        call ReadDomainBIN(bin_path, D, bin_ok)
        if (.not. bin_ok) return

        ! BIN may store a blank name; restore from domain_tag for name-based checks
        if (trim(domain_tag) == 'gwf') then
            D%name = 'GWF'
        else if (trim(domain_tag) == 'swf') then
            D%name = 'SWF'
        else if (trim(domain_tag) == 'cln') then
            D%name = 'CLN'
        end if

        call ConsumeGenerateBlock(FNumMUT)
        call Msg('Domain cache: reusing '//trim(bin_path)// &
            ' (inputs unchanged; delete DomainBIN/.reuse to force rebuild)')

        ! SWF 2D faces may be restored; GWF 3D faces were cleared on save
        if (allocated(D%FaceHost) .and. D%FacesCalculated .and. D%nNodesPerElement < 6) then
            if (trim(domain_tag) == 'swf') then
                call MarkCLNFacesReady(.false.)
            end if
        end if
        TryReuseDomain = .true.
    end function TryReuseDomain

    !----------------------------------------------------------------------
    subroutine SaveDomainCacheAfterBuild(prefix, domain_tag, TMPLT, D, &
            block_cs, files_cs)
        implicit none
        character(*), intent(in) :: prefix, domain_tag
        type(mesh), intent(in) :: TMPLT
        type(ModflowDomain), intent(inout) :: D
        integer(kind=8), intent(in) :: block_cs, files_cs
        character(MAX_STR) :: bin_path, reuse_path

        call DomainCachePaths(prefix, domain_tag, bin_path, reuse_path)
        call SaveDomainBIN(bin_path, D)
        call WriteDomainReuseStamp(reuse_path, domain_tag, TMPLT, D, block_cs, files_cs)
    end subroutine SaveDomainCacheAfterBuild

    !----------------------------------------------------------------------
    subroutine InitSWFZoneDefaultsAfterLoad(D)
        implicit none
        type(ModflowDomain), intent(inout) :: D
        if (allocated(D%Manning)) return
        allocate(D%Manning(D%nZones), &
                 D%DepressionStorageHeight(D%nZones), &
                 D%ObstructionStorageHeight(D%nZones), &
                 D%H1DepthForSmoothing(D%nZones), &
                 D%H2DepthForSmoothing(D%nZones), stat=ialloc)
        call AllocChk(ialloc, 'InitSWFZoneDefaultsAfterLoad')
        D%Manning(:) = -999.0
        D%DepressionStorageHeight(:) = -999.0
        D%ObstructionStorageHeight(:) = -999.0
        D%H1DepthForSmoothing(:) = -999.0
        D%H2DepthForSmoothing(:) = -999.0
        D%node%is = 0
    end subroutine InitSWFZoneDefaultsAfterLoad

    !----------------------------------------------------------------------
    subroutine InitCLNZoneDefaultsAfterLoad(D)
        implicit none
        type(ModflowDomain), intent(inout) :: D
        if (allocated(D%Geometry)) return
        allocate(D%Geometry(D%nZones), &
                 D%Direction(D%nZones), &
                 D%CircularRadius(D%nZones), &
                 D%RectangularWidth(D%nZones), &
                 D%RectangularHeight(D%nZones), &
                 D%LongitudinalK(D%nZones), &
                 D%InfillPorosity(D%nZones), &
                 D%FlowTreatment(D%nZones), stat=ialloc)
        call AllocChk(ialloc, 'InitCLNZoneDefaultsAfterLoad')
        D%NCONDUITYP = 0
        D%NRECTYP = 0
        D%NGENSHPTYP = 0
        D%NGENTABROWS = 0
        D%Geometry(:) = -999
        D%Direction(:) = -999
        D%CircularRadius(:) = -999.0
        D%RectangularWidth(:) = -999.0
        D%RectangularHeight(:) = -999.0
        D%LongitudinalK(:) = -999.0
        D%InfillPorosity(:) = 1.0
        D%FlowTreatment(:) = -999
    end subroutine InitCLNZoneDefaultsAfterLoad

    !----------------------------------------------------------------------
    ! Cache CLN–GWF nearest-cell IDs + intersection-based FLENGW connections
    ! so WriteCLNFiles can skip FindCLN_MeshIntersections on later builds.
    !----------------------------------------------------------------------
    subroutine CLNGWFConnPaths(prefix, bin_path, reuse_path)
        implicit none
        character(*), intent(in) :: prefix
        character(*), intent(out) :: bin_path, reuse_path
        bin_path = trim(prefix)//'.cln.gwfconn.bin'
        reuse_path = trim(prefix)//'.cln.gwfconn.reuse'
    end subroutine CLNGWFConnPaths

    !----------------------------------------------------------------------
    logical function TryReuseCLNGWFConnections(prefix, GWF, CLN)
        implicit none
        character(*), intent(in) :: prefix
        type(ModflowDomain), intent(in) :: GWF
        type(ModflowDomain), intent(inout) :: CLN
        character(MAX_STR) :: bin_path, reuse_path, expected(8), actual
        integer(i4) :: FNum, i, ios, nexp, magic, ncln, ngwf, nconn
        integer(kind=8) :: gwf_cs, cln_cs
        logical :: exists

        TryReuseCLNGWFConnections = .false.
        call CLNGWFConnPaths(prefix, bin_path, reuse_path)
        inquire(file=trim(bin_path), exist=exists)
        if (.not. exists) return
        inquire(file=trim(reuse_path), exist=exists)
        if (.not. exists) return

        call DomainReuseChecksum(GWF, gwf_cs)
        call DomainReuseChecksum(CLN, cln_cs)

        nexp = 0
        nexp = nexp + 1; write(expected(nexp), '(a)') 'mut='//trim(adjustl(MUTVersion))
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'gwf_checksum=', gwf_cs
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'cln_checksum=', cln_cs
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'ncln=', CLN%nCells
        nexp = nexp + 1; write(expected(nexp), '(a,i0)') 'ngwf=', GWF%nCells

        call OpenAscii(FNum, reuse_path)
        do i = 1, nexp
            read(FNum, '(a)', iostat=ios) actual
            if (ios /= 0 .or. trim(actual) /= trim(expected(i))) then
                call FreeUnit(FNum)
                call Msg('CLN-GWF connections reuse miss: '//trim(expected(i)))
                return
            end if
        end do
        call FreeUnit(FNum)

        call OpenBinary(FNum, bin_path)
        read(FNum, iostat=ios) magic, ncln, ngwf, nconn
        if (ios /= 0 .or. magic /= CLNGWFCONN_MAGIC .or. &
            ncln /= CLN%nCells .or. ngwf /= GWF%nCells) then
            call FreeUnit(FNum)
            call WarnMsg('CLN-GWF connections cache invalid: '//trim(bin_path))
            return
        end if

        if (allocated(CLN%CLNGWFCellID)) deallocate(CLN%CLNGWFCellID)
        allocate(CLN%CLNGWFCellID(ncln), stat=ialloc)
        call AllocChk(ialloc, 'TryReuseCLNGWFConnections: CLNGWFCellID')
        read(FNum) CLN%CLNGWFCellID

        CLN%NCLNGWC = nconn
        if (allocated(CLN%CLNGWFConnCLNCell)) deallocate(CLN%CLNGWFConnCLNCell)
        if (allocated(CLN%CLNGWFConnGWFCell)) deallocate(CLN%CLNGWFConnGWFCell)
        if (allocated(CLN%CLNGWFConnFLENGW)) deallocate(CLN%CLNGWFConnFLENGW)
        if (nconn > 0) then
            allocate(CLN%CLNGWFConnCLNCell(nconn), CLN%CLNGWFConnGWFCell(nconn), &
                     CLN%CLNGWFConnFLENGW(nconn), stat=ialloc)
            call AllocChk(ialloc, 'TryReuseCLNGWFConnections: conn arrays')
            read(FNum) CLN%CLNGWFConnCLNCell
            read(FNum) CLN%CLNGWFConnGWFCell
            read(FNum) CLN%CLNGWFConnFLENGW
        end if
        call FreeUnit(FNum)

        call Msg('CLN-GWF connections: reusing '//trim(bin_path)// &
            ' (delete *.gwfconn.bin/.reuse to force rebuild)')
        write(TmpSTR, '(a,i8)') 'Total CLN-GWF connections: ', CLN%NCLNGWC
        call Msg(TmpSTR)
        TryReuseCLNGWFConnections = .true.
    end function TryReuseCLNGWFConnections

    !----------------------------------------------------------------------
    subroutine SaveCLNGWFConnectionsCache(prefix, GWF, CLN)
        implicit none
        character(*), intent(in) :: prefix
        type(ModflowDomain), intent(in) :: GWF, CLN
        character(MAX_STR) :: bin_path, reuse_path
        integer(i4) :: FNum
        integer(kind=8) :: gwf_cs, cln_cs

        if (.not. allocated(CLN%CLNGWFCellID)) return
        call CLNGWFConnPaths(prefix, bin_path, reuse_path)
        call DomainReuseChecksum(GWF, gwf_cs)
        call DomainReuseChecksum(CLN, cln_cs)

        call OpenBinary(FNum, bin_path)
        call Msg(FileCreateSTR//'CLN-GWF connections cache: '//trim(bin_path))
        write(FNum) CLNGWFCONN_MAGIC, CLN%nCells, GWF%nCells, CLN%NCLNGWC
        write(FNum) CLN%CLNGWFCellID
        if (CLN%NCLNGWC > 0 .and. allocated(CLN%CLNGWFConnCLNCell)) then
            write(FNum) CLN%CLNGWFConnCLNCell
            write(FNum) CLN%CLNGWFConnGWFCell
            write(FNum) CLN%CLNGWFConnFLENGW
        end if
        call FreeUnit(FNum)

        call GetUnit(FNum)
        open(FNum, file=trim(reuse_path), form='formatted', status='replace', iostat=status)
        if (status /= 0) then
            call FreeUnit(FNum)
            call WarnMsg('Could not write CLN-GWF connections reuse stamp: '//trim(reuse_path))
            return
        end if
        write(FNum, '(a)') 'mut='//trim(adjustl(MUTVersion))
        write(FNum, '(a,i0)') 'gwf_checksum=', gwf_cs
        write(FNum, '(a,i0)') 'cln_checksum=', cln_cs
        write(FNum, '(a,i0)') 'ncln=', CLN%nCells
        write(FNum, '(a,i0)') 'ngwf=', GWF%nCells
        write(FNum, '(a,i0)') 'nconn=', CLN%NCLNGWC
        call FreeUnit(FNum)
    end subroutine SaveCLNGWFConnectionsCache

end module MUSG_DomainCache
