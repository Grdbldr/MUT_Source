! Vendor tecio.f90 marks tecflush142(numZonesToRetain) as VALUE, but TECIO.h is
! INTEGER4 const* (pass by reference). Calling the VALUE interface AVs in SZL.
module TecIO_SzlFlush
    use iso_c_binding
    implicit none
    interface
        integer(c_int32_t) function TecFlush142ByRef(numZonesToRetain, zonesToRetain) &
            bind(c, name="tecflush142")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: numZonesToRetain
            integer(c_int32_t), intent(in) :: zonesToRetain(*)
        end function TecFlush142ByRef
    end interface
end module TecIO_SzlFlush


module Tecplot !
    use iso_c_binding
    use TecIO_SzlFlush, only: TecFlush142ByRef
    use GeneralRoutines
    use NumericalMesh, only: node
    use ErrorHandling, only: ERR_FILE_IO, ERR_INVALID_INPUT, HandleError
    implicit none

    include "tecio.f90"
    integer(i4) :: nVar
    character(MAX_STR) :: VarSTR
    character(MAX_STR) :: ZoneSTR
    character(MAX_STR) :: CellCenteredSTR    
    
    type TecplotDomain
        ! common to all types of domains: GWF, CLN, SWF, ...
        character(MAX_LBL) :: STR_LengthUnit
        
        logical :: IsDefined=.false.      ! this type of domain has been defined 
        character(128) :: MeshType      ! structured or unstructured?

        character(11) :: Name='none'
            
    end type TecplotDomain


    ! General parameters 
	integer(i4) :: ieco = 0
	integer(i4) :: nln
	integer(i4) :: len
	integer(i4) :: nfile
    real(dp)	:: xc, yc, zc
    integer(i4) :: nx, ny, nz

    integer(i4), parameter :: TEC_FILETYPE_FULL = 0
    integer(c_int32_t), parameter :: TEC_LOC_CELL = 0
    integer(c_int32_t), parameter :: TEC_LOC_NODAL = 1
    
    contains
    

    subroutine Line3DSegment_Tecplot(FNum,x1,y1,z1,x2,y2,z2)
        implicit none
        integer(i4) :: FNum
        real(dp) :: x1, y1, z1
        real(dp) :: x2, y2, z2
        write(FNum,'(a)') 'GEOMETRY T=LINE3D' !, C=CUST3, LT=0.1'
        write(FNum,'(i5)') 1
        write(FNum,'(i5)') 2
        write(FNum,'(3('//FMT_R8//'))')x1,y1,z1
        write(FNum,'(3('//FMT_R8//'))')x2,y2,z2
    end subroutine Line3DSegment_Tecplot


    function TecIO_FileName(asciiName) result(fname)
        implicit none
        character(*), intent(in) :: asciiName
        character(MAX_STR) :: fname
        integer(i4) :: n

        n = len_trim(asciiName)
        if (n >= 14) then
            if (asciiName(n-13:n) == '.tecplot.szplt') then
                fname = asciiName(1:n)
                return
            end if
        end if
        if (n >= 12) then
            if (asciiName(n-11:n) == '.tecplot.dat') then
                fname = asciiName(1:n-12)//'.tecplot.szplt'
                return
            end if
        end if
        fname = asciiName(1:n)//'.tecplot.szplt'
    end function TecIO_FileName


    function TecIO_VarsFromHeader(header) result(tecvars)
        implicit none
        character(*), intent(in) :: header
        character(4000) :: tecvars
        integer(i4) :: i, j, n
        character(4000) :: token
        logical :: inQuote, first

        ! TECINI142: if the string contains a comma, comma is the separator
        ! (names may contain spaces). Otherwise the separator is a space.
        ! Quotes are not grouping tokens, so do not emit space-separated names.
        tecvars = ' '
        first = .true.
        inQuote = .false.
        n = 0
        token = ' '
        do i = 1, len_trim(header)
            if (header(i:i) == '"') then
                if (inQuote) then
                    if (n > 0) then
                        if (.not. first) tecvars = trim(tecvars)//','
                        tecvars = trim(tecvars)//token(1:n)
                        first = .false.
                    end if
                    n = 0
                    token = ' '
                    inQuote = .false.
                else
                    inQuote = .true.
                    n = 0
                    token = ' '
                end if
            else if (inQuote) then
                n = n + 1
                token(n:n) = header(i:i)
            end if
        end do
        if (len_trim(tecvars) == 0) then
            tecvars = header
            j = index(tecvars, '=')
            if (j > 0) tecvars = adjustl(tecvars(j+1:))
        end if
    end function TecIO_VarsFromHeader


    subroutine TecIO_Check(ierr, where)
        implicit none
        integer(c_int32_t), intent(in) :: ierr
        character(*), intent(in) :: where
        if (ierr /= 0) then
            write(TmpSTR,'(a,i0)') 'TecIO error ', ierr
            call HandleError(ERR_FILE_IO, trim(TmpSTR), where)
        end if
    end subroutine TecIO_Check


    integer(c_int32_t) function TecIO_ZoneType(tecplotTyp, nNodesPerCell)
        implicit none
        character(*), intent(in) :: tecplotTyp
        integer(i4), intent(in) :: nNodesPerCell
        character(40) :: typ

        select case (nNodesPerCell)
        case (2)
            TecIO_ZoneType = 1  ! FELINESEG
            return
        case (3)
            TecIO_ZoneType = 2  ! FETRIANGLE
            return
        case (4)
            TecIO_ZoneType = 3  ! FEQUADRILATERAL
            return
        case (6, 8)
            TecIO_ZoneType = 5  ! FEBRICK (6-node prisms as degenerate bricks)
            return
        end select

        typ = tecplotTyp
        call LwrCse(typ)
        if (index(typ, 'line') > 0) then
            TecIO_ZoneType = 1
        else if (index(typ, 'tri') > 0) then
            TecIO_ZoneType = 2
        else if (index(typ, 'quad') > 0) then
            TecIO_ZoneType = 3
        else if (index(typ, 'tet') > 0) then
            TecIO_ZoneType = 4
        else
            TecIO_ZoneType = 5
        end if
    end function TecIO_ZoneType


    subroutine TecIO_Open(title, variables, fname)
        implicit none
        character(*), intent(in) :: title
        character(*), intent(in) :: variables
        character(*), intent(in) :: fname
        integer(c_int32_t) :: fileFormat, fType, debug, visDouble, ierr

        fileFormat = 1  ! SZL (.szplt)
        fType = int(TEC_FILETYPE_FULL, kind=c_int32_t)
        debug = 0
        visDouble = 1
        ierr = tecini142(trim(title)//c_null_char, &
                         trim(variables)//c_null_char, &
                         trim(fname)//c_null_char, &
                         '.'//c_null_char, &
                         fileFormat, fType, debug, visDouble)
        call TecIO_Check(ierr, 'tecini142 '//trim(fname))
        call Msg(FileCreateSTR//'Tecplot SZPLT file: '//trim(fname))
    end subroutine TecIO_Open


    subroutine TecIO_ZoneFE(zoneTitle, tecplotTyp, nNodesPerCell, nNodes, nElements, &
                           nVarIn, nNodal, shareFromZone1, solutionTime, strandID, &
                           shareConnFromZone)
        implicit none
        character(*), intent(in) :: zoneTitle
        character(*), intent(in) :: tecplotTyp
        integer(i4), intent(in) :: nNodesPerCell
        integer(i4), intent(in) :: nNodes
        integer(i4), intent(in) :: nElements
        integer(i4), intent(in) :: nVarIn
        integer(i4), intent(in) :: nNodal
        integer(i4), intent(in) :: shareFromZone1
        real(dp), intent(in) :: solutionTime
        integer(i4), intent(in) :: strandID
        integer(i4), intent(in), optional :: shareConnFromZone

        integer(c_int32_t) :: zoneType, nPts, nEls, nFaces
        integer(c_int32_t) :: iCellMax, jCellMax, kCellMax
        integer(c_int32_t) :: strand, parentZn, isBlock
        integer(c_int32_t) :: nFConns, fNMode, shrConn, ierr
        integer(c_int32_t) :: nFaceNodes, nBFaces, nBConns
        integer(c_int32_t), allocatable :: valueLocation(:)
        integer(c_int32_t), allocatable :: shareVar(:)
        ! Cray pointer NULL, as in Tecplot 360 EX Fortran examples (pyramid.F90).
        ! BIND(C) ShareVarFromZone(*) is a C int*; a zero-filled array is not NULL.
        pointer(nullPtr, nullInts)
        integer(c_int32_t) :: nullInts(*)
        integer(i4) :: i, nV
        real(c_double) :: solTime

        nV = max(nVarIn, 1)
        allocate(valueLocation(nV))
        if (nNodal <= 0) then
            valueLocation = TEC_LOC_CELL
        else if (nNodal >= nV) then
            valueLocation = TEC_LOC_NODAL
        else
            valueLocation(1:nNodal) = TEC_LOC_NODAL
            valueLocation(nNodal+1:nV) = TEC_LOC_CELL
        end if

        ! TecIO: NULL ShareVarFromZone means "do not share". A zero-filled array is
        ! not NULL; solution files then fail with "First zone cannot share variables".
        nullPtr = 0

        zoneType = TecIO_ZoneType(tecplotTyp, nNodesPerCell)
        nPts = int(nNodes, kind=c_int32_t)
        nEls = int(nElements, kind=c_int32_t)
        nFaces = 0
        iCellMax = 0
        jCellMax = 0
        kCellMax = 0
        solTime = real(solutionTime, kind=c_double)
        strand = int(strandID, kind=c_int32_t)
        parentZn = 0
        isBlock = 1
        nFConns = 0
        fNMode = 0
        nFaceNodes = 0
        nBFaces = 0
        nBConns = 0
        shrConn = 0
        if (present(shareConnFromZone)) then
            if (shareConnFromZone > 0) shrConn = int(shareConnFromZone, kind=c_int32_t)
        end if

        if (shareFromZone1 > 0) then
            allocate(shareVar(nV))
            shareVar = 0
            do i = 1, min(shareFromZone1, nV)
                shareVar(i) = 1
            end do
            ierr = teczne142(trim(zoneTitle)//c_null_char, &
                             zoneType, nPts, nEls, nFaces, &
                             iCellMax, jCellMax, kCellMax, &
                             solTime, strand, parentZn, isBlock, &
                             nFConns, fNMode, nFaceNodes, nBFaces, nBConns, &
                             nullInts, valueLocation(1), shareVar(1), shrConn)
            deallocate(shareVar)
        else
            ierr = teczne142(trim(zoneTitle)//c_null_char, &
                             zoneType, nPts, nEls, nFaces, &
                             iCellMax, jCellMax, kCellMax, &
                             solTime, strand, parentZn, isBlock, &
                             nFConns, fNMode, nFaceNodes, nBFaces, nBConns, &
                             nullInts, valueLocation(1), nullInts, shrConn)
        end if
        call TecIO_Check(ierr, 'teczne142 '//trim(zoneTitle))
        deallocate(valueLocation)
    end subroutine TecIO_ZoneFE


    subroutine TecIO_WriteD(n, vals)
        implicit none
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: vals(*)
        integer(c_int32_t) :: n32, ierr
        real(c_double), allocatable :: buf(:)
        integer(i4) :: i

        if (n <= 0) return
        allocate(buf(n))
        do i = 1, n
            buf(i) = real(vals(i), kind=c_double)
        end do
        n32 = int(n, kind=c_int32_t)
        ierr = tecdatd142(n32, buf)
        call TecIO_Check(ierr, 'tecdatd142')
        deallocate(buf)
    end subroutine TecIO_WriteD


    subroutine TecIO_WriteR4(n, vals)
        implicit none
        integer(i4), intent(in) :: n
        real(sp), intent(in) :: vals(*)
        integer(c_int32_t) :: n32, ierr
        real(c_double), allocatable :: buf(:)
        integer(i4) :: i

        if (n <= 0) return
        allocate(buf(n))
        do i = 1, n
            buf(i) = real(vals(i), kind=c_double)
        end do
        n32 = int(n, kind=c_int32_t)
        ierr = tecdatd142(n32, buf)
        call TecIO_Check(ierr, 'tecdatd142 r4')
        deallocate(buf)
    end subroutine TecIO_WriteR4


    subroutine TecIO_WriteI(n, ivals)
        implicit none
        integer(i4), intent(in) :: n
        integer(i4), intent(in) :: ivals(*)
        integer(c_int32_t) :: n32, ierr
        real(c_double), allocatable :: buf(:)
        integer(i4) :: i

        if (n <= 0) return
        allocate(buf(n))
        do i = 1, n
            buf(i) = real(ivals(i), kind=c_double)
        end do
        n32 = int(n, kind=c_int32_t)
        ierr = tecdatd142(n32, buf)
        call TecIO_Check(ierr, 'tecdatd142 int')
        deallocate(buf)
    end subroutine TecIO_WriteI


    subroutine TecIO_WriteXYZ(nodes, nNodes)
        implicit none
        type(node), intent(in) :: nodes(*)
        integer(i4), intent(in) :: nNodes
        real(dp), allocatable :: buf(:)
        integer(i4) :: i

        if (nNodes <= 0) return
        allocate(buf(nNodes))
        do i = 1, nNodes
            buf(i) = nodes(i)%x
        end do
        call TecIO_WriteD(nNodes, buf)
        do i = 1, nNodes
            buf(i) = nodes(i)%y
        end do
        call TecIO_WriteD(nNodes, buf)
        do i = 1, nNodes
            buf(i) = nodes(i)%z
        end do
        call TecIO_WriteD(nNodes, buf)
        deallocate(buf)
    end subroutine TecIO_WriteXYZ


    subroutine TecIO_WriteNodes(idNode, nNodesPerCell, nElements)
        implicit none
        integer(i4), intent(in) :: idNode(:,:)
        integer(i4), intent(in) :: nNodesPerCell
        integer(i4), intent(in) :: nElements
        integer(c_int32_t), allocatable :: conn(:)
        integer(c_int32_t) :: nOut, n32, ierr
        integer(i4) :: i, j, k, nWrite

        if (nElements <= 0 .or. nNodesPerCell <= 0) return
        if (nNodesPerCell == 6) then
            nWrite = 8
        else
            nWrite = nNodesPerCell
        end if
        nOut = int(nElements * nWrite, kind=c_int32_t)
        allocate(conn(nOut))
        k = 0
        do i = 1, nElements
            if (nNodesPerCell == 6) then
                conn(k+1) = int(idNode(1,i), kind=c_int32_t)
                conn(k+2) = int(idNode(2,i), kind=c_int32_t)
                conn(k+3) = int(idNode(3,i), kind=c_int32_t)
                conn(k+4) = int(idNode(3,i), kind=c_int32_t)
                conn(k+5) = int(idNode(4,i), kind=c_int32_t)
                conn(k+6) = int(idNode(5,i), kind=c_int32_t)
                conn(k+7) = int(idNode(6,i), kind=c_int32_t)
                conn(k+8) = int(idNode(6,i), kind=c_int32_t)
                k = k + 8
            else if (nNodesPerCell == 4) then
                conn(k+1) = int(idNode(1,i), kind=c_int32_t)
                conn(k+2) = int(idNode(2,i), kind=c_int32_t)
                conn(k+3) = int(idNode(3,i), kind=c_int32_t)
                if (idNode(4,i) > 0) then
                    conn(k+4) = int(idNode(4,i), kind=c_int32_t)
                else
                    conn(k+4) = int(idNode(3,i), kind=c_int32_t)
                end if
                k = k + 4
            else
                do j = 1, nNodesPerCell
                    k = k + 1
                    conn(k) = int(idNode(j,i), kind=c_int32_t)
                end do
            end if
        end do
        n32 = nOut
        ierr = tecnode142(n32, conn)
        call TecIO_Check(ierr, 'tecnode142')
        deallocate(conn)
    end subroutine TecIO_WriteNodes


    subroutine TecIO_ZoneAux(timeUnits, lengthUnits, version)
        implicit none
        character(*), intent(in) :: timeUnits
        character(*), intent(in) :: lengthUnits
        character(*), intent(in) :: version
        integer(c_int32_t) :: ierr

        ierr = teczauxstr142('TimeUnits'//c_null_char, trim(timeUnits)//c_null_char)
        call TecIO_Check(ierr, 'teczauxstr142 TimeUnits')
        ierr = teczauxstr142('LengthUnits'//c_null_char, trim(lengthUnits)//c_null_char)
        call TecIO_Check(ierr, 'teczauxstr142 LengthUnits')
        ierr = teczauxstr142('MUTVersion'//c_null_char, trim(version)//c_null_char)
        call TecIO_Check(ierr, 'teczauxstr142 MUTVersion')
    end subroutine TecIO_ZoneAux


    subroutine TecIO_DeleteIfExists(fname)
        implicit none
        character(*), intent(in) :: fname
        integer(i4) :: u, ios
        logical :: exists

        inquire(file=trim(fname), exist=exists)
        if (.not. exists) return
        open(newunit=u, file=trim(fname), status='old', action='readwrite', iostat=ios)
        if (ios /= 0) then
            call HandleError(ERR_FILE_IO, &
                'Cannot replace '//trim(fname)// &
                ' because it is open in another program. Close that dataset in Tecplot 360 (or exit tec360) and re-run.', &
                'TecIO_DeleteIfExists')
        end if
        close(u, status='delete', iostat=ios)
        if (ios /= 0) then
            close(u, iostat=ios)
            call HandleError(ERR_FILE_IO, &
                'Cannot replace '//trim(fname)// &
                ' because it is open in another program. Close that dataset in Tecplot 360 (or exit tec360) and re-run.', &
                'TecIO_DeleteIfExists')
        end if
    end subroutine TecIO_DeleteIfExists


    subroutine TecIO_FlushRetainZone1(zone1)
        implicit none
        integer(i4), intent(in) :: zone1
        integer(c_int32_t) :: ierr, nRetain, zonesToRetain(1)

        nRetain = 1
        zonesToRetain(1) = int(zone1, kind=c_int32_t)
        ierr = TecFlush142ByRef(nRetain, zonesToRetain)
        call TecIO_Check(ierr, 'tecflush142')
    end subroutine TecIO_FlushRetainZone1


    subroutine TecIO_DeleteStalePlt(szpltName)
        implicit none
        character(*), intent(in) :: szpltName
        character(MAX_STR) :: stem, stale
        integer(i4) :: n, i
        character(16) :: tstr

        n = len_trim(szpltName)
        if (n >= 14) then
            if (szpltName(n-13:n) == '.tecplot.szplt') then
                stem = szpltName(1:n-14)
            else
                stem = szpltName(1:n)
            end if
        else
            stem = szpltName(1:n)
        end if

        call TecIO_DeleteIfExists(trim(stem)//'.tecplot.plt')
        call TecIO_DeleteIfExists(trim(stem)//'.tecplot.grid.plt')
        call TecIO_DeleteIfExists(trim(stem)//'.tecplot.sol.plt')
        do i = 1, 99
            write(tstr,'(i3.3)') i
            stale = trim(stem)//'.tecplot.sol.'//trim(tstr)//'.plt'
            call TecIO_DeleteIfExists(stale)
        end do
    end subroutine TecIO_DeleteStalePlt


    subroutine TecIO_Close()
        implicit none
        integer(c_int32_t) :: ierr
        ierr = tecend142()
        call TecIO_Check(ierr, 'tecend142')
    end subroutine TecIO_Close

end module Tecplot !
