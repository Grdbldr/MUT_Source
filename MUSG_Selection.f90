module MUSG_Selection
    !### Cell/node/zone selection routines for MODFLOW-USG
    ! Handles selection and clearing of cells, nodes, and zones in domains
    ! Includes selection by coordinates, layers, files, zones, and GB elements/nodes
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, Msg, ErrMsg, TmpSTR, FMT_R8, UnitsOfLength
    use GeneralRoutines, only: bcheck, chosen, set, clear, getunit, freeunit, status, small, TAB
    use ErrorHandling, only: ERR_INVALID_INPUT, ERR_FILE_IO, ERR_LOGIC, HandleError
    use MUSG_Core, only: ModflowDomain
    use NumericalMesh, only: mesh
    use MeshGen, only: xyzFromList
    
    implicit none
    private
    
    public :: ChooseAllZones, ChooseZoneNumber, ChooseAllNodes, ChooseGBNodes
    public :: ChooseGBNodesTemplate, ChooseAllCells, ChooseNodeAtXYZTemplate, ChooseNodeAtXYZ
    public :: ChooseCellsByLayer, ChooseCellAtXYZ, ChooseCellsFromXYZList
    public :: ChooseCellbyXYZ_LayerRange, ChooseCellsFromFile, ChooseCellsByChosenZones
    public :: ChooseCellsFromGBElements, ChooseCellsFromGBElementsTemplate
    public :: ChooseCellsFromGBNodes, ChooseCellsFromGBNodesTemplate
    public :: ClearAllCells, ClearAllNodes, ClearAllZones
    
    contains
    
    !----------------------------------------------------------------------
    subroutine ChooseAllZones(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount

        ncount=0
        do i=1,domain%nZones
            call set(domain%zone(i)%is,chosen)
            ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' zone numbers currently chosen: '
        call Msg(trim(TmpSTR))
        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                write(TmpSTR,'(a,i5)') TAB,i
                call Msg(trim(TmpSTR))
            endif
        end do

        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Zones chosen', 'ChooseAllZones')
        
    end subroutine ChooseAllZones
    
    !----------------------------------------------------------------------
    subroutine ChooseZoneNumber(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: number

        read(FNumMUT,*) number
        write(TmpSTR,'(a,i8)') 'Adding zone number: ',number
        call Msg(trim(TmpSTR))
        
        if(number <= 0 .or. number > domain%nZones) then
            write(TmpSTR,'(a,i8)') 'Number must be between 1 and ',domain%nZones
            call HandleError(ERR_INVALID_INPUT, trim(TmpSTR), 'ChooseZoneNumber')
        end if

        call set(domain%zone(number)%is,chosen)

        write(TmpSTR,'(a,i10)') trim(domain%name)//' zone numbers currently chosen: '
        call Msg(trim(TmpSTR))
        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                write(TmpSTR,'(a,i5)') TAB,i
                call Msg(trim(TmpSTR))
            endif
        end do
                
    
    end subroutine ChooseZoneNumber
    
    !----------------------------------------------------------------------
    subroutine ChooseAllNodes(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount
        
        ncount=0
        do i=1,domain%nNodes
            call set(domain%node(i)%is,chosen)
            ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') 'Nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No nodes chosen', 'ChooseAllNodes')
        
    end subroutine ChooseAllNodes

    !----------------------------------------------------------------------
    subroutine ChooseGBNodes(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, j
        integer(i4) :: nLayer_bot, nLayer_top, ncount, iNode

        character(MAX_STR) :: FName
        character*80 :: dummy
        logical :: togon(domain%nNodes)
        integer(i4) :: itmp

        read(FNumMUT,'(a)') fname
        call Msg('Choose nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,domain%nNodes)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseGBNodes')
        end if
        
        if(domain%name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,domain%nLayers)
            nLayer_top=min(nLayer_top,domain%nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
            call Msg('From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
            call Msg('To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain%nNodes
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iNode=(j-1)*domain%nNodes+i
                        call set(domain%node(iNode)%is,chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
       
        else
            ncount=0
            do i=1,domain%nNodes
                if(togon(i)) then
                    call set(domain%node(i)%is,chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') trim(domain%name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No nodes chosen', 'ChooseAllNodes')
        
        call freeunit(itmp)


    end subroutine ChooseGBNodes
    
    !----------------------------------------------------------------------
    subroutine ChooseGBNodesTemplate(FNumMUT,TMPLT) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: TMPLT

        integer(i4) :: i
        integer(i4) :: ncount
        integer(i4) :: itmp

        character(MAX_STR) :: FName
        character*80 :: dummy
        logical :: togon(TMPLT%nNodes)

        read(FNumMUT,'(a)') fname
        call Msg('Choose nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT%nNodes)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseGBNodesTemplate')
        end if
        
        ncount=0
        do i=1,TMPLT%nNodes
            if(togon(i)) then
                call set(TMPLT%node(i)%is,chosen)
                ncount=ncount+1
            end if
        end do

        write(TmpSTR,'(a,i10)') trim(TMPLT%name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No nodes chosen', 'ChooseAllNodes')
        
        call freeunit(itmp)


    end subroutine ChooseGBNodesTemplate
    
    !----------------------------------------------------------------------
    subroutine ChooseAllCells(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount


        ncount=0
        do i=1,domain%nCells
            call set(domain%cell(i)%is,chosen)
            ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')

    end subroutine ChooseAllCells
    
    !----------------------------------------------------------------------
    subroutine ChooseNodeAtXYZTemplate(FNumMut,Domain)
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: Domain

        integer(i4) :: i,iNode
        real(dp) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) 'Find node closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
        do i=1,domain%nNodes
            f1=sqrt((x1-domain%node(i)%x)**2+((y1-domain%node(i)%y))**2+((z1-domain%node(i)%z))**2)
            if(f1.lt.dist_min) then
                inode=i
                dist_min=f1
            endif
        end do
        call set(domain%node(iNode)%is,chosen)
        
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Found x, y, z  ',domain%node(iNode)%x,domain%node(iNode)%y,domain%node(iNode)%z,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Delta x, y, z  ',domain%node(iNode)%x-x1,domain%node(iNode)%y-y1,domain%node(iNode)%z-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)

    end subroutine ChooseNodeAtXYZTemplate
    
    !----------------------------------------------------------------------
    subroutine ChooseNodeAtXYZ(FNumMut,Domain)
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i,iNode
        real(dp) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) 'Find node closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
        do i=1,domain%nNodes
            f1=sqrt((x1-domain%node(i)%x)**2+((y1-domain%node(i)%y))**2+((z1-domain%node(i)%z))**2)
            if(f1.lt.dist_min) then
                inode=i
                dist_min=f1
            endif
        end do
        call set(domain%node(iNode)%is,chosen)
        
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Found x, y, z  ',domain%node(iNode)%x,domain%node(iNode)%y,domain%node(iNode)%z,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Delta x, y, z  ',domain%node(iNode)%x-x1,domain%node(iNode)%y-y1,domain%node(iNode)%z-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)

    end subroutine ChooseNodeAtXYZ
    
    !----------------------------------------------------------------------
    subroutine ChooseCellsByLayer(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, iLyr
        integer(i4) :: ncount

        read(FNumMut,*) iLyr
        write(TMPStr,*) 'Choose cells in layer: ',iLyr
        call Msg(TMPStr)

        ncount=0
        do i=1,domain%nCells
            if(domain%cell(i)%iLayer == iLyr) then
                call set(domain%cell(i)%is,chosen)
                ncount=ncount+1
            endif
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen in layer: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')

    end subroutine ChooseCellsByLayer
    
    !----------------------------------------------------------------------
    subroutine ChooseCellAtXYZ(FNumMut,Domain)
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i,iCell
        real(dp) :: x1,y1,z1,dist_min,f1

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,'(a,3f12.2)') 'Find cell closest to XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
        do i=1,domain%nCells
            f1=sqrt((x1-domain%cell(i)%x)**2+((y1-domain%cell(i)%y))**2+((z1-domain%cell(i)%z))**2)
            if(f1.lt.dist_min) then
                iCell=i
                dist_min=f1
            endif
        end do
        call set(domain%cell(iCell)%is,chosen)
        
        write(tmpSTR,'(a14,i8)') 'Found cell  ',iCell
        call Msg(tmpSTR)
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Found x, y, z  ',domain%cell(iCell)%x,domain%cell(iCell)%y,domain%cell(iCell)%z,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
        write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'Delta x, y, z  ',domain%cell(iCell)%x-x1,domain%cell(iCell)%y-y1,domain%cell(iCell)%z-z1,'     '//TRIM(UnitsOfLength)
        call Msg(tmpSTR)
        
    end subroutine ChooseCellAtXYZ
    
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromXYZList(FNum,Domain)
        implicit none
        integer(i4), intent(in) :: FNum
        
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, j, iCell
        real(dp) :: x1,y1,z1,dist_min,f1
        real(sp), allocatable :: xi(:), yi(:), zi(:)
        integer(i4) :: nPoints

        call xyzFromList(FNum,xi,yi,zi,nPoints)

        
        do i=1,nPoints
            x1=xi(i)
            y1=yi(i)
            z1=zi(i)
            write(TMPStr,'(a,3f12.2)') 'Find cell closest to XYZ: ',x1, y1, z1
            call Msg(TMPStr)
            dist_min=1.0e20
            do j=1,domain%nCells
                f1=sqrt((x1-domain%cell(j)%x)**2+((y1-domain%cell(j)%y))**2+((z1-domain%cell(j)%z))**2)
                if(f1.lt.dist_min) then
                    iCell=j
                    dist_min=f1
                endif
            end do
            call set(domain%cell(iCell)%is,chosen)
            
            write(tmpSTR,'(a14,i8)') 'Found cell  ',iCell
            call Msg(tmpSTR)
            write(tmpSTR,'(a14,3('//FMT_R8//'),a)') 'At x, y, z  ',domain%cell(iCell)%x,domain%cell(iCell)%y,domain%cell(iCell)%z,'     '//TRIM(UnitsOfLength)
            call Msg(tmpSTR)
        end do
        
    end subroutine ChooseCellsFromXYZList

    !----------------------------------------------------------------------
    subroutine ChooseCellbyXYZ_LayerRange(FNumMut,Domain)
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        real(dp) :: x1,x2
        real(dp) :: y1,y2
        real(dp) :: z1,z2
        integer(i4) :: ltop,lbot,ntemp,ielmin,ielmax
        integer(i4) :: ncount

        call Msg('Find cells whose centroids are in the range defined by: ')
        read(FNumMut,*) x1,x2
        write(TMPStr,*) 'X range: ',x1, x2
        call Msg(TMPStr)

        read(FNumMut,*) y1,y2
        write(TMPStr,*) 'Y range: ',y1, y2
        call Msg(TMPStr)

        read(FNumMut,*) z1,z2
        write(TMPStr,*) 'Z range: ',z1, z2
        call Msg(TMPStr)

        read(FNumMut,*) ltop,lbot
        write(TMPStr,*) 'Layer range: ',ltop, lbot
        call Msg(TMPStr)

        if(ltop.gt.lbot) then
            ntemp=ltop
            ltop=lbot
            lbot=ntemp
        end if
        ielmin=(ltop-1)*domain%nodelay
        ielmax=lbot*domain%nodelay + 1

        x1=x1-small
        x2=x2+small
        y1=y1-small
        y2=y2+small
        z1=z1-small
        z2=z2+small

        ncount=0
        do i=1,domain%nCells
            if(domain%cell(i)%x.ge.x1 .and. domain%cell(i)%x.le.x2 .and. domain%cell(i)%y.ge.y1 .and. domain%cell(i)%y.le.y2 .and. domain%cell(i)%z.ge.z1 .and. domain%cell(i)%z.le.z2) then
                if(i .gt. ielmin .and. i .lt. ielmax) then
                    call set(domain%cell(i)%is,chosen)
                    ncount=ncount+1
                end if
            end if
        end do
    
        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')
        
    end subroutine ChooseCellbyXYZ_LayerRange

    !----------------------------------------------------------------------
    subroutine ChooseCellsFromFile(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount, iCell,status2
        integer(i4) :: itmp

        character*80 fname
        logical togon(domain%nCells)

        read(FNumMUT,'(a)') fname
        call Msg('Choose Cells from ascii file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown')
        togon(:)=.false.
        do
            read(itmp,*,iostat=status2) iCell
            if(status2/=0) exit
            togon(iCell)=.true.
        enddo
        
        ncount=0
        do i=1,domain%nCells
            if(togon(i)) then
                call set(domain%cell(i)%is,chosen)
                ncount=ncount+1
            end if
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')
        
        call freeunit(itmp)


    end subroutine ChooseCellsFromFile

    !----------------------------------------------------------------------
    subroutine ChooseCellsByChosenZones(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, j
        integer(i4) :: ncount


       
        ncount=0
        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) then
                do j=1,domain%nCells
                    if(domain%cell(j)%idZone == i) then
                        call set(domain%cell(j)%is,chosen)
                        ncount=ncount+1
                    end if
                end do
            end if
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')
        


    end subroutine ChooseCellsByChosenZones

    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBElements(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, j
        integer(i4) :: nLayer_bot, nLayer_top, ncount, iCell
        integer(i4) :: itmp

        character*80 fname
        character*80 dummy
        logical togon(domain%nElements)

        read(FNumMUT,'(a)') fname
        call Msg('Choose Cells from gb chosen elements file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,domain%nElements)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseCellsFromGBElements')
        end if
        
        if(domain%name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,domain%nLayers)
            nLayer_top=min(nLayer_top,domain%nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
            call Msg('From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
            call Msg('To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain%nCells
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iCell=(j-1)*domain%nElements+i
                        call set(domain%cell(iCell)%is,chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,domain%nCells
                if(togon(i)) then
                    call set(domain%cell(i)%is,chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')
        
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBElements
    
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBElementsTemplate(FNumMUT,TMPLT) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: TMPLT

        integer(i4) :: i, j
        integer(i4) :: nLayer_bot, nLayer_top, ncount, iElement
        integer(i4) :: itmp

        character*80 fname
        character*80 dummy
        logical togon(TMPLT%nElements)

        read(FNumMUT,'(a)') fname
        call Msg('Choose Elements from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT%nElements)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseCellsFromGBElementsTemplate')
        end if
        
        if(TMPLT%name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,TMPLT%nLayers)
            nLayer_top=min(nLayer_top,TMPLT%nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
            call Msg('From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
            call Msg('To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,TMPLT%nElements
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iElement=(j-1)*TMPLT%nElements+i
                        call set(TMPLT%element(iElement)%is,chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,TMPLT%nElements
                if(togon(i)) then
                    call set(TMPLT%element(i)%is,chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') trim(TMPLT%name)//' Elements chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Elements chosen', 'ChooseCellsFromGBElementsTemplate')
        
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBElementsTemplate
   
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBNodes(FNumMUT,domain) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i, j
        integer(i4) :: nLayer_bot, nLayer_top, ncount, iCell
        integer(i4) :: itmp

        character*80 fname
        character*80 dummy
        logical togon(domain%nNodes)

        read(FNumMUT,'(a)') fname
        call Msg('Choose Cells from GB chosen nodes file '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,domain%nNodes)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseCellsFromGBNodes')
        end if
        
        if(domain%name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,domain%nLayers)
            nLayer_top=min(nLayer_top,domain%nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
            call Msg('From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
            call Msg('To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,domain%nCells
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iCell=(j-1)*domain%nNodes+i
                        call set(domain%cell(iCell)%is,chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,domain%nCells
                if(togon(i)) then
                    call set(domain%cell(i)%is,chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Cells chosen', 'ChooseAllCells')
        
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBNodes
    
    !----------------------------------------------------------------------
    subroutine ChooseCellsFromGBNodesTemplate(FNumMUT,TMPLT) 
        implicit none

        integer(i4), intent(in) :: FNumMUT
        type(mesh), intent(inout) :: TMPLT

        integer(i4) :: i, j
        integer(i4) :: nLayer_bot, nLayer_top, ncount, iElement
        integer(i4) :: itmp

        character*80 fname
        character*80 dummy
        logical togon(TMPLT%nNodes)

        read(FNumMUT,'(a)') fname
        call Msg('Choose Nodes from '//trim(fname))

        call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')
        read(itmp) dummy
        read(itmp,iostat=status) (togon(i),i=1,TMPLT%nNodes)
        if(status /= 0) then
            call HandleError(ERR_FILE_IO, 'While reading: '//trim(fname), 'ChooseCellsFromGBNodesTemplate')
        end if
        
        if(TMPLT%name == 'GWF') then

            read(FNumMUT,*) nLayer_top,nLayer_bot
        
            nLayer_bot=max(nLayer_bot,1)
            nLayer_bot=min(nLayer_bot,TMPLT%nLayers)
            nLayer_top=min(nLayer_top,TMPLT%nLayers)
            nLayer_top=max(nLayer_top,1)
        
            write(TmpSTR,'(i5)') nLayer_top
            call Msg('From Layer: '//trim(TmpSTR))
            write(TmpSTR,'(i5)') nLayer_bot
            call Msg('To Layer:   '//trim(TmpSTR))

            ncount=0
            do i=1,TMPLT%nNodes
                if(togon(i)) then
                    do j=nLayer_top,nLayer_bot
                        iElement=(j-1)*TMPLT%nNodes+i
                        call set(TMPLT%element(iElement)%is,chosen)
                        ncount=ncount+1
                    end do
                end if
            end do
        else
            ncount=0
            do i=1,TMPLT%nNodes
                if(togon(i)) then
                    call set(TMPLT%element(i)%is,chosen)
                    ncount=ncount+1
                end if
            end do
        end if

        write(TmpSTR,'(a,i10)') trim(TMPLT%name)//' Nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        if(ncount == 0) call HandleError(ERR_INVALID_INPUT, 'No Nodes chosen', 'ChooseCellsFromGBNodesTemplate')
        
        call freeunit(itmp)


    end subroutine ChooseCellsFromGBNodesTemplate
    
    !----------------------------------------------------------------------
    subroutine ClearAllCells(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount


        do i=1,domain%nCells
            call clear(domain%cell(i)%is,chosen)
        end do
        
        ncount=0
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Cells chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount /= 0) call HandleError(ERR_LOGIC, 'Some Cells chosen', 'ClearAllCells')

    end subroutine ClearAllCells
    
    !----------------------------------------------------------------------
    subroutine ClearAllNodes(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount

        do i=1,domain%nNodes
            call clear(domain%node(i)%is,chosen)
        end do
        
        ncount=0
        do i=1,domain%nNodes
            if(bcheck(domain%node(i)%is,chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' nodes chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount /= 0) call HandleError(ERR_LOGIC, 'Some nodes chosen', 'ClearAllNodes')


    end subroutine ClearAllNodes
    
    !----------------------------------------------------------------------
    subroutine ClearAllZones(domain) 
        implicit none

        type(ModflowDomain), intent(inout) :: Domain

        integer(i4) :: i
        integer(i4) :: ncount

        do i=1,domain%nZones
            call clear(domain%zone(i)%is,chosen)
        end do
        
        ncount=0
        do i=1,domain%nZones
            if(bcheck(domain%zone(i)%is,chosen)) ncount=ncount+1
        end do

        write(TmpSTR,'(a,i10)') trim(domain%name)//' Zones chosen: ',ncount
        call Msg(trim(TmpSTR))
        
        if(ncount /= 0) call HandleError(ERR_LOGIC, 'Some Zones chosen', 'ClearAllZones')

    end subroutine ClearAllZones

end module MUSG_Selection

