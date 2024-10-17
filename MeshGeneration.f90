Module MeshGeneration
    use GeneralRoutines
    use Tecplot
    implicit none

    integer :: user_nz
    integer :: user_maxnlayer=50
    real(dr), allocatable  :: x(:)
    real(dr), allocatable  :: y(:)
    real(dr), allocatable  :: z(:)
    integer, allocatable  :: in(:,:)
    integer, allocatable  :: iprp(:)
    integer, allocatable  :: ilyr(:)
    real(dr), allocatable  :: zi(:)
    real(dr), allocatable  :: base_elev(:)
    character(MAX_LBL), allocatable  :: layer_name(:)
    integer, allocatable  :: nsublayer(:)
        
    integer :: nsheet
    integer :: nlayers

    real(dr), allocatable  :: top_elev(:)

    integer :: nn_temp, ne_temp
        
    logical :: layer_defined=.false.
    logical :: zone_by_template=.false.

    contains

    !----------------------------------------------------------------------
    subroutine MeshFromGb(FNumMUT,TMPLT)
        implicit none
        
        integer :: FNumMUT
        type (TecplotDomain) TMPLT
        
        character(128) :: GBPrefix

        integer :: i,j
        real*8 :: x(3),y(3)
        real*8 :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3)
        
        
       TMPLT.name='TMPLT'

        !rgm oct-95  added this so only grid builder prefix needed
        !     prefix of grid files
        read(FNumMut,'(a80)') GBPrefix

        inquire(file=trim(GBprefix)//'.grd',exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('File not found: '//trim(GBprefix)//'.grd')
        end if

        TMPLT.nNodesPerElement=3
        TMPLT.ElementType='fetriangle'
        
        !     NODE COORDINATES
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.xyc',form='unformatted')
        read(itmp)TMPLT.nNodes
        allocate(TMPLT.x(TMPLT.nNodes),TMPLT.y(TMPLT.nNodes),TMPLT.z(TMPLT.nNodes),stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d node arrays')
        TMPLT.x = 0 ! automatic initialization
        TMPLT.y = 0 ! automatic initialization
        TMPLT.z = 0 ! automatic initialization
        read(itmp) (TMPLT.x(i),TMPLT.y(i),i=1,TMPLT.nNodes)
	    call freeunit(itmp)

        !     ELEMENT INCIDENCES
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.in3',form='unformatted')
        read(itmp)TMPLT.nElements
        allocate(TMPLT.iZone(TMPLT.nElements),TMPLT.iNode(TMPLT.nNodesPerElement,TMPLT.nElements),&
           TMPLT.iLayer(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d element arrays')
        TMPLT.iZone = 0 ! automatic initialization
        TMPLT.iNode = 0 ! automatic initialization
        TMPLT.iLayer = 1 ! automatic initialization
        read(itmp) (TMPLT.iNode(1,i),TMPLT.iNode(2,i),TMPLT.iNode(3,i),i=1,TMPLT.nElements)
	    call freeunit(itmp)

        !     Element area numbers
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.ean',form='unformatted')
        read(itmp) (TMPLT.iZone(i),i=1,TMPLT.nElements)
	    call freeunit(itmp)
        
        allocate(TMPLT.ElementArea(TMPLT.nElements),TMPLT.rCircle(TMPLT.nElements),TMPLT.xCircle(TMPLT.nElements),&
            TMPLT.yCircle(TMPLT.nElements),TMPLT.xElement(TMPLT.nElements), TMPLT.yElement(TMPLT.nElements),&
            TMPLT.zElement(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'GB Inner circle arrays')
        
        allocate(TMPLT.SideLength(TMPLT.nNodesPerElement,TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'GB SideLlength array')

        do i=1,TMPLT.nElements
            ! xc and yc from circumcircles
            if(TMPLT.nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
            do j=1,TMPLT.nNodesPerElement
                x(j)=TMPLT.x(TMPLT.iNode(j,i))
                y(j)=TMPLT.y(TMPLT.iNode(j,i))
            end do
            call InnerCircle(x,y,TMPLT.ElementArea(i),xc,yc,TMPLT.rCircle(i),lseg,aseg,dseg)
            
            TMPLT.SideLength(1,i)=lseg(1,2)
            TMPLT.SideLength(2,i)=lseg(2,3)
            TMPLT.SideLength(3,i)=lseg(3,1)
           
            TMPLT.xCircle(i)=xc
            TMPLT.yCircle(i)=yc
                
                
            ! zc from centroid of the iNode array coordinates
            zc=0.0
            do j=1,3
                zc=zc+TMPLT.z(TMPLT.iNode(j,i))
            end do
                
            TMPLT.xElement(i)=xc
            TMPLT.yElement(i)=yc
            TMPLT.zElement(i)=zc/3
           
        end do
                    
        
        TMPLT.InnerCircles=.true.
        TMPLT.IsDefined=.true.
        allocate(TMPLT.Element_Is(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'TMPLT Element_Is array')            
        TMPLT.Element_Is(:)=0

        write(TmpSTR,'(a,i8)') TAB//'Number of nodes:       ',TMPLT.nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)') TAB//'Number of elements:    ',TMPLT.nElements
        call Msg(TmpSTR)
        
        TMPLT.STR_LengthUnit=UnitsOfLength
        write(TmpSTR,'(a)') TAB//'Assumed length Units:  '//trim(UnitsOfLength)
        call Msg(TmpSTR)

        return
    end subroutine MeshFromGb
    !----------------------------------------------------------------------
    subroutine Quadtree2DMeshFromGWV(FNumMUT,TMPLT)
        implicit none
    
        integer :: FNumMUT
        type (TecplotDomain) TMPLT
        
        character(MAX_STR) :: line
        character(MAX_STR) :: FName
        
        integer :: i,j, i1, i2
        real(dr) :: r1, r2, r3
        
        
        TMPLT.name='TMPLT'

		read(FNumMUT,'(a)') FName
		call Msg(TAB//TAB//'Quadtree file '//trim(FName))

        call OpenAscii(itmp,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))
        
        ! read initial comment lines beginning with #
        do 
            read(itmp,'(a)') line
            if(line(1:1).eq.'#') then
                write(*,'(a)') line
                cycle
            end if
            backspace(itmp)
            exit
        end do

        read(itmp,*) TMPLT.meshtype
        read(itmp,*) TMPLT.nElements, TMPLT.nLayers, i1, i2
        read(itmp,*) TMPLT.nNodes
        allocate(TMPLT.x(TMPLT.nNodes),TMPLT.y(TMPLT.nNodes),TMPLT.z(TMPLT.nNodes), stat=ialloc)
        call AllocChk(ialloc,'SWF node coordinate arrays')
        TMPLT.x = 0 ! automatic initialization
        TMPLT.y = 0 ! automatic initialization
        TMPLT.z = 0 ! automatic initialization
        
        read(itmp,*) (TMPLT.x(i),TMPLT.y(i),TMPLT.z(i),i=1,TMPLT.nNodes)
        
        ! determine the number of nodes per Element (TMPLT.nNodesPerElement)
        read(itmp,*) i1,r1,r2,r3,i2,TMPLT.nNodesPerElement
        backspace(itmp)
        allocate(TMPLT.iNode(TMPLT.nNodesPerElement,TMPLT.nElements),stat=ialloc)
        allocate(TMPLT.xElement(TMPLT.nElements),TMPLT.yElement(TMPLT.nElements),TMPLT.zElement(TMPLT.nElements),TMPLT.iLayer(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'SWF iNode, xyzElement arrays')
        
        TMPLT.iNode = 0 ! automatic initialization
        do i=1,TMPLT.nElements
            read(itmp,*) i1,TMPLT.xElement(i),TMPLT.yElement(i),TMPLT.zElement(i),TMPLT.iLayer(i),i2,(TMPLT.iNode(j,i),j=1,TMPLT.nNodesPerElement)
        end do
	    call freeunit(itmp)
        
        TMPLT.IsDefined=.true.

        write(TmpSTR,'(i10)') TMPLT.nElements 
        call Msg('nElements: '//TmpSTR)
        allocate(TMPLT.Element_Is(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'TMPLT Element_Is array')            
        TMPLT.Element_Is(:)=0


        TMPLT.ElementType='fequadrilateral'
        
        allocate(TMPLT.iZone(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'TMPLT iZone array')
        TMPLT.iZone(:) = 1 ! automatic initialization
        
        allocate(TMPLT.ElementArea(TMPLT.nElements),stat=ialloc)
        call AllocChk(ialloc,'TMPLT ElementArea array')
        
        !allocate(TMPLT.SideLength(TMPLT.nNodesPerElement,TMPLT.nElements),stat=ialloc)
        !call AllocChk(ialloc,'GB SideLlength array')
        
        do i=1,TMPLT.nElements
            ! xc and yc quadtree element side lengths
            TMPLT.ElementArea(i)=(TMPLT.x(TMPLT.iNode(4,i))-TMPLT.x(TMPLT.iNode(1,i))) * &
                                 (TMPLT.y(TMPLT.iNode(2,i))-TMPLT.y(TMPLT.iNode(1,i)))
            !do j=1,TMPLT.nNodesPerElement
            !    x(j)=TMPLT.x(TMPLT.iNode(j,i))
            !    y(j)=TMPLT.y(TMPLT.iNode(j,i))
            !end do
            !call InnerCircle(x,y,TMPLT.ElementArea(i),xc,yc,TMPLT.rCircle(i),lseg,aseg,dseg)
            !
            !TMPLT.SideLength(1,i)=lseg(1,2)
            !TMPLT.SideLength(2,i)=lseg(2,3)
            !TMPLT.SideLength(3,i)=lseg(3,1)
            !
            !TMPLT.xCircle(i)=xc
            !TMPLT.yCircle(i)=yc
            !    
            !    
            !! zc from centroid of the iNode array coordinates
            !zc=0.0
            !do j=1,3
            !    zc=zc+TMPLT.z(TMPLT.iNode(j,i))
            !end do
            !    
            !TMPLT.xElement(i)=xc
            !TMPLT.yElement(i)=yc
            !TMPLT.zElement(i)=zc/3
            !
        end do
                    
        
        !TMPLT.InnerCircles=.true.
        TMPLT.IsDefined=.true.
    
        write(TmpSTR,'(a,i8)') '        Number of nodes               ',TMPLT.nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)') '        Number of elements               ',TMPLT.nElements
        call Msg(TmpSTR)

        return
    end subroutine Quadtree2DMeshFromGWV
    !----------------------------------------------------------------------
    subroutine list_file_elevation(fname,nprop,maxnnp)
        implicit none

        integer :: i
        integer :: maxnnp

        character*(*) fname
        character*11 file_type
        real*8 :: nprop(maxnnp)
	    character(80) :: dtitle

        inquire(file=fname,exist=FileExists,form=file_type)
        if(.not. FileExists) then
            call ErrMsg(' File not found: '//fname)
        end if


	    call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='formatted')

	    read(itmp,'(a)') dtitle
	    read(itmp,*) (nprop(i),i=1,maxnnp)

	    call freeunit(itmp)

    end subroutine list_file_elevation
    
    !----------------------------------------------------------------------
    subroutine GenerateCLNDomain(FNum,TMPLT_CLN)
        implicit none
        integer :: FNum
        type(TecplotDomain) TMPLT_CLN
        
        character(MAX_INST) :: Instruction
        character(MAX_INST) :: CLNFromXYZPair_cmd		=   'cln from xyz pair'
        character(MAX_INST) :: CLNFromListFile_cmd		=   'cln from list file'
        
        real, allocatable :: xi(:), yi(:), zi(:)  ! xyz coordinate list defining CLN to be read
        integer :: nPoints  ! number of points in list
        
	    ! Build a single tecplot file which can have multiple CLN's
        TMPLT_CLN.name='TMPLT_CLN'
        TMPLT_CLN.meshtype='UNSTRUCTURED'
        TMPLT_CLN.nZones=0
        TMPLT_CLN.nNodesPerElement=2
        TMPLT_CLN.ElementType='felineseg'

        read_Instructions: do
            read(FNum,'(a60)',iostat=status) Instruction
            if(status /= 0) exit

		    call LwrCse(instruction)

            if(index(Instruction,'end') /= 0) then
                call Msg(TAB//'end generate cln domain')
                exit read_Instructions
            else
                call Msg('')
                call Msg(TAB//Instruction)
            end if
                

            if(index(Instruction, CLNFromListFile_cmd)  /= 0) then
                call xyzFromListFile(FNum,xi,yi,zi,nPoints)
              
            else if(index(Instruction, CLNFromXYZPair_cmd)  /= 0) then
                call CLNFromXYZPair(FNum,TMPLT_CLN)
                
            else
			    call ErrMsg(TAB//'Unrecognized instruction: generate cln domain')
            end if

        end do read_Instructions
        
        TMPLT_CLN.IsDefined=.true.
        
        allocate(TMPLT_CLN.Element_Is(TMPLT_CLN.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_CLN.name)//' Element_Is array')            
        TMPLT_CLN.Element_Is(:)=0
        
        continue
        
    end subroutine GenerateCLNDomain
    !----------------------------------------------------------------------
    subroutine GenerateLayeredGWFDomain(FNumMUT,TMPLT,TMPLT_GWF)
        implicit none

        integer :: FNumMUT
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_GWF
        
        character(MAX_INST) :: instruction
        character(MAX_INST) :: zone_by_template_cmd			=   'zone by template'
        character(MAX_INST) :: top_elevation_cmd			    =   'top elevation'
        character(MAX_INST) :: new_layer_cmd				    =   'new layer'
        

	    ! Given a 2D mesh, define top elevation, layer bottoms and sublayering interactively.

        integer :: i, j
        
        if(TMPLT.nNodes < 1000) then
            user_nz=1000
        else
            user_nz=60
        endif

        nn_temp=user_nz*TMPLT.nNodes
        ne_temp=(user_nz-1)*TMPLT.nElements
        
        ! NOTE: Option exists to search GB .grd file for string "T  ! treat as rectangles" then set up as 4-node rectangular elements 
        !blockel=.false.
        nln=TMPLT.nNodesPerElement*2

        ! Set up arrays to store grid
	    allocate(x(nn_temp),y(nn_temp),z(nn_temp),in(nln,ne_temp),iprp(ne_temp),ilyr(ne_temp),zi(user_nz),stat=ialloc)
        call AllocChk(ialloc,'3d temp grid arrays')
        x(:) = 0.0d0
        y(:) = 0.0d0
        z(:) = 0.0d0
        in(:,:) = 0
        iprp(:) = 0
        ilyr(:) = 0
        zi(:) = 0.0d0

        if(.not. allocated(top_elev)) then  ! could have been allocated if SWF TMPLT_GWF defined
            allocate(base_elev(TMPLT.nNodes),top_elev(TMPLT.nNodes),stat=ialloc)
        else
            allocate(base_elev(TMPLT.nNodes),stat=ialloc)
        end if
        call AllocChk(ialloc,'Slice2lyr base/top arrays')

	    ! set default base and top elevation for problem
        base_elev(: ) = 0.0d0
        top_elev(: ) = 0.0d0

	    ! Assign xyz coordinates for top
        do j=1,TMPLT.nNodes
            x(j)=TMPLT.x(j)
            y(j)=TMPLT.y(j)
            z(j)=top_elev(j)
        end do

        TMPLT_GWF.nZones=1
        nsheet=1

	    ! Define mesh layers and sublayers
        allocate(layer_name(user_maxnlayer),nsublayer(user_maxnlayer),stat=ialloc)
        call AllocChk(ialloc,'slice2lyr_interactive layer arrays')
        layer_name(: ) = ''
        nsublayer(: ) = 0

	    ! Process slice to layer instructions

        read_slice2lyr: do
            read(FNumMUT,'(a60)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg('end generate layered gwf domain')
                exit read_slice2lyr
            else
                call Msg('')
                call Msg(TAB//instruction)
            end if
                

            if(index(instruction, zone_by_template_cmd)  /= 0) then
			    if(layer_defined) then
                    call ErrMsg('   Use this instruction before defining any new layers')
			    end if
                zone_by_template=.true.

            else if(index(instruction, top_elevation_cmd)  /= 0) then
			    if(layer_defined) then
                    call ErrMsg('   Use this instruction before defining any new layers')
			    end if
                call top_elevation(FNumMUT,TMPLT)
			    do j=1,TMPLT.nNodes
				    z(j)=top_elev(j)
			    end do

            else if(index(instruction, new_layer_cmd)  /= 0) then
                call new_layer(FNumMUT,TMPLT,zone_by_template)
			    layer_defined=.true.

            else
			    call ErrMsg(TAB//'Unrecognized instruction: generate gwf domain')
            end if

        end do read_slice2lyr
        
        if(.not. layer_defined) call ErrMsg('You must define at least 1 layer') 
        
        
        ! Copy the local and TMPLT data to the TMPLT_GWF data structure
        TMPLT_GWF.name='TMPLT_GWF'
        TMPLT_GWF.meshtype='UNSTRUCTURED'
        TMPLT_GWF.nNodes=TMPLT.nNodes*nsheet
        allocate(TMPLT_GWF.x(TMPLT_GWF.nNodes),TMPLT_GWF.y(TMPLT_GWF.nNodes),TMPLT_GWF.z(TMPLT_GWF.nNodes), stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' node coordinate arrays')
        do i=1,TMPLT_GWF.nNodes
            TMPLT_GWF.x(i)= x(i)
            TMPLT_GWF.y(i)= y(i)
            TMPLT_GWF.z(i)= z(i)
        end do
        
        TMPLT_GWF.nLayers=nsheet-1
        
        TMPLT_GWF.nNodesPerElement=TMPLT.nNodesPerElement*2
        TMPLT_GWF.nElements=TMPLT.nElements*(nsheet-1)
        
        ! Just define these for now
        !TMPLT_GWF.ic=0
        
        
        ! Cell node list
        allocate(TMPLT_GWF.iNode(TMPLT_GWF.nNodesPerElement,TMPLT_GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' Element node list array')
        do i=1,TMPLT_GWF.nElements
            do j=1,TMPLT_GWF.nNodesPerElement
                TMPLT_GWF.iNode(j,i) = in(j,i) 
            end do
        end do
        
        ! Element layer number
        allocate(TMPLT_GWF.iLayer(TMPLT_GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' Element layer number array')
        do i=1,TMPLT_GWF.nElements
            TMPLT_GWF.iLayer(i) = ilyr(i) 
        end do
        

        ! Element zone number
        allocate(TMPLT_GWF.iZone(TMPLT_GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' iZone arrays')
        do i=1,TMPLT_GWF.nElements
            TMPLT_GWF.iZone(i) = iprp(i) 
        end do
        if(zone_by_template) then
            TMPLT_GWF.nZones=TMPLT.nZones
        else
            TMPLT_GWF.nZones=nlayers
        end if
        
        TMPLT_GWF.ElementType='febrick'

        nz=nsheet
        zi(nsheet)=z((nsheet-1)*TMPLT.nNodes+1)
        
        TMPLT_GWF.IsDefined=.true.

        allocate(TMPLT_GWF.Element_Is(TMPLT_GWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_GWF.name)//' Element_Is array')            
        TMPLT_GWF.Element_Is(:)=0
    
    end subroutine GenerateLayeredGWFDomain
    
    !----------------------------------------------------------------------
    subroutine GenerateSWFDomain(FNumMUT,TMPLT,TMPLT_SWF)
        implicit none
        
        character(MAX_INST) :: instruction
        character(MAX_INST) :: top_elevation_cmd			    =   'top elevation'
    
        integer :: FNumMUT
        type (TecplotDomain) TMPLT
        type (TecplotDomain) TMPLT_SWF

	    ! Given the template (i.e. a 2D mesh), define SWF TMPLT_SWF 

        integer :: i, j
        
        ! Option exists to search GB .grd for string "T  ! treat as rectangles" then set up as 4-node rectangular elements 

        ! Copy the template data to the Modflow SWF data structure
        TMPLT_SWF.name='TMPLT_SWF'
        TMPLT_SWF.meshtype='UNSTRUCTURED'
        
        TMPLT_SWF.nNodes=TMPLT.nNodes
        allocate(TMPLT_SWF.x(TMPLT_SWF.nNodes),TMPLT_SWF.y(TMPLT_SWF.nNodes),TMPLT_SWF.z(TMPLT_SWF.nNodes), stat=ialloc)
        call AllocChk(ialloc,'SWF node coordinate arrays')
        TMPLT_SWF.x(:)= TMPLT.x(:)
        TMPLT_SWF.y(:)= TMPLT.y(:)

        ! Define elevation (z coordinate) of SWF TMPLT_SWF
        allocate(top_elev(TMPLT.nNodes),stat=ialloc)
        call AllocChk(ialloc,'Template top elevation arrays')

	    ! Process slice to layer instructions
        read_slice2lyr: do
            read(FNumMUT,'(a60)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg(TAB//'end generate swf domain instructions')
                exit read_slice2lyr
            else
                call Msg('')
                call Msg(TAB//instruction)
            end if
                

            if(index(instruction, top_elevation_cmd)  /= 0) then
                call top_elevation(FNumMUT,TMPLT)
			    do j=1,TMPLT.nNodes
				    TMPLT_SWF.z(j)=top_elev(j)
			    end do


            else
			    call ErrMsg(TAB//'Unrecognized instruction: generate swf domain')
            end if

        end do read_slice2lyr
        
        
        
        TMPLT_SWF.nLayers=1
        !TMPLT_SWF.iz=0
        !TMPLT_SWF.nodelay=TMPLT.nElements   ! number of modflow Elements per layer

        
        TMPLT_SWF.nNodesPerElement=TMPLT.nNodesPerElement
        TMPLT_SWF.ElementType=TMPLT.ElementType
        TMPLT_SWF.nElements=TMPLT.nElements
        
        ! Just define these for now
        !TMPLT_SWF.ic=0
        
        ! Element node list
        allocate(TMPLT_SWF.iNode(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_SWF.name)//' Element node list array')
        do i=1,TMPLT_SWF.nElements
            do j=1,TMPLT_SWF.nNodesPerElement
                TMPLT_SWF.iNode(j,i) = TMPLT.iNode(j,i)
            end do
        end do
        
        ! Element side lengths
        allocate(TMPLT_SWF.SideLength(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_SWF.name)//' Element SideLength array')
        do i=1,TMPLT_SWF.nElements
            do j=1,TMPLT_SWF.nNodesPerElement
                TMPLT_SWF.SideLength(j,i) = TMPLT.SideLength(j,i)
            end do
        end do
        
        ! Element layer number
        allocate(TMPLT_SWF.iLayer(TMPLT_SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_SWF.name)//' Element layer number array')
        do i=1,TMPLT_SWF.nElements
            TMPLT_SWF.iLayer(i) = 1
        end do
        

        ! Element zone number
        allocate(TMPLT_SWF.iZone(TMPLT_SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_SWF.name)//' iZone arrays')
        do i=1,TMPLT_SWF.nElements
            TMPLT_SWF.iZone(i) = TMPLT.iZone(i) 
            if(TMPLT.iZone(j).gt.TMPLT_SWF.nZones) TMPLT_SWF.nZones=TMPLT.iZone(j)
        end do

        TMPLT_SWF.IsDefined=.true.
        
        allocate(TMPLT_SWF.Element_Is(TMPLT_SWF.nElements),stat=ialloc)
        call AllocChk(ialloc,trim(TMPLT_SWF.name)//' Element_Is array')            
        TMPLT_SWF.Element_Is(:)=0
    
    end subroutine GenerateSWFDomain

    !----------------------------------------------------------------------
    subroutine GenerateUniformRectangles(FNum,TMPLT)
        implicit none
        integer :: FNum
        type(TecplotDomain) TMPLT


        integer :: i, j, k
	    integer :: nbx, nby, nn2d, ne2d
        real(dr) :: xl, yl, delx, dely
        
        real, allocatable :: xi(:)
        real, allocatable :: yi(:)

        TMPLT.name='TMPLT'

        !     generate uniform rectangles
        TMPLT.nNodesPerElement=4
        TMPLT.ElementType='fequadrilateral'


        !     xl, yl are grid lengths in x- and y-directions
        read(FNum,*) xl, nbx
        write(TMPStr,'(a,g15.5,a)') TAB//'Mesh length in X        ',xl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')    TAB//'Number of elements in X ',nbx
        call Msg(TMPStr)

        read(FNum,*) yl, nby
        write(TMPStr,'(a,g15.5,a)') TAB//'Mesh length in Y        ',yl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')    TAB//'Number of elements in Y ',nby
        call Msg(TMPStr)

        nx=nbx+1
        ny=nby+1

        allocate(xi(nx),yi(ny),stat=ialloc)
        call AllocChk(ialloc,'Gen_u_rects xi,yi arrays')
        xi = 0.0d0 
        yi = 0.0d0 

        delx=xl/float(nx-1)
        dely=yl/float(ny-1)
        do i=1,nx
            xi(i)=float(i-1)*delx
        end do
        do i=1,ny
            yi(i)=float(i-1)*dely
        end do

        !     generate 2D slice first
        TMPLT.nNodes=nx*ny
        allocate(TMPLT.x(TMPLT.nNodes),TMPLT.y(TMPLT.nNodes),TMPLT.z(TMPLT.nNodes),stat=ialloc)
        call AllocChk(ialloc,'Gen_u_rects node arrays')
        TMPLT.x = 0.0d0 
        TMPLT.y = 0.0d0 
        TMPLT.z = 0.0d0 

        TMPLT.nElements=(nx-1)*(ny-1)
        allocate(TMPLT.iZone(TMPLT.nElements), &
            TMPLT.iNode(TMPLT.nNodesPerElement,TMPLT.nElements), &
            TMPLT.iLayer(TMPLT.nElements), &
            TMPLT.xElement(TMPLT.nElements), &
            TMPLT.yElement(TMPLT.nElements), &
            TMPLT.zElement(TMPLT.nElements), &
            TMPLT.ElementArea(TMPLT.nElements), &
            TMPLT.SideLength(TMPLT.nNodesPerElement,TMPLT.nElements), &
            stat=ialloc)
        call AllocChk(ialloc,'Gen_u_rects element arrays')
        TMPLT.iZone = 0 
        TMPLT.iNode = 0 
        TMPLT.iLayer = 0
        TMPLT.xElement=0.0d0
        TMPLT.yElement=0.0d0
        TMPLT.zElement=0.0d0
        TMPLT.ElementArea=0.0d0
        
        !     generate 2D node coordinates
        nn2d=0
        do i=1,ny
            do j=1,nx
                nn2d=nn2d+1
                TMPLT.x(nn2d)=xi(j)
                TMPLT.y(nn2d)=yi(i)
            end do
        end do

        !     generate 2D rectangular element incidences
        ne2d=0
        do i=1,ny-1
            k=1+(i-1)*nx
            do j=1,nx-1
                ne2d=ne2d+1
                TMPLT.iNode(1,ne2d)=k
                TMPLT.iNode(2,ne2d)=k+1
                TMPLT.iNode(3,ne2d)=k+nx+1
                TMPLT.iNode(4,ne2d)=k+nx
                TMPLT.iZone(ne2d) = 1
                TMPLT.iLayer(ne2d) = 1
                k=k+1
            end do
        end do

        do i=1,TMPLT.nElements
            TMPLT.ElementArea(i)=(TMPLT.x(TMPLT.iNode(2,i))-TMPLT.x(TMPLT.iNode(1,i))) * &
                                 (TMPLT.y(TMPLT.iNode(3,i))-TMPLT.y(TMPLT.iNode(1,i)))
            TMPLT.xElement(i)=(TMPLT.x(TMPLT.iNode(2,i)) + TMPLT.x(TMPLT.iNode(1,i)))/2.0d0
            TMPLT.yElement(i)=(TMPLT.y(TMPLT.iNode(3,i)) + TMPLT.y(TMPLT.iNode(1,i)))/2.0d0
            
            TMPLT.SideLength(1,i)=abs(TMPLT.x(TMPLT.iNode(2,i)) - TMPLT.x(TMPLT.iNode(1,i)))
            TMPLT.SideLength(2,i)=abs(TMPLT.y(TMPLT.iNode(3,i)) - TMPLT.y(TMPLT.iNode(2,i)))
            TMPLT.SideLength(3,i)=abs(TMPLT.x(TMPLT.iNode(4,i)) - TMPLT.x(TMPLT.iNode(3,i)))
            TMPLT.SideLength(4,i)=abs(TMPLT.y(TMPLT.iNode(1,i)) - TMPLT.y(TMPLT.iNode(4,i)))


        end do
                    
        TMPLT.IsDefined=.true.
    
        call Msg(' ')
        write(TmpSTR,'(a,i8)')    TAB//'Number of nodes         ',TMPLT.nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    TAB//'Number of elements      ',TMPLT.nElements
        call Msg(TmpSTR)


        return
    end subroutine GenerateUniformRectangles
    

    !----------------------------------------------------------------------
    subroutine CLNFromXYZPair(FNum,TMPLT_CLN)
        implicit none
        integer :: FNum
        type(TecplotDomain) TMPLT_CLN

        integer :: i
        integer :: nSizeInit, nNodesInit, nElementsInit
        real(dr), allocatable :: xiTMP(:), yiTMP(:), ziTMP(:)  ! temporary xyz arrays
        integer :: nCells
                
        real(dr) :: TotalLength
        real(dr) :: dx, dy, dz
        real(dr) :: cx, cy, cz

	    real(dr) :: xp(2)
	    real(dr) :: yp(2)
	    real(dr) :: zp(2)
	    xp(:) = 0
	    yp(:) = 0
	    zp(:) = 0
        
        
        nNodesInit=TMPLT_CLN.nNodes
        nElementsInit=TMPLT_CLN.nElements
        TMPLT_CLN.nZones=TMPLT_CLN.nZones+1

        nSizeInit=max(2,TMPLT_CLN.nNodes)
	    allocate(xiTMP(nSizeInit*2),yiTMP(nSizeInit*2),ziTMP(nSizeInit*2),stat=ialloc)
	    call AllocChk(ialloc,'xyzTMP arrays')
	    xiTMP(:) = -999.0d0
	    yiTMP(:) = -999.0d0
	    ziTMP(:) = -999.0d0
        
        if(.not. allocated(TMPLT_CLN.x)) then  
            allocate(TMPLT_CLN.x(nSizeInit),TMPLT_CLN.y(nSizeInit),TMPLT_CLN.z(nSizeInit),stat=ialloc)
	        call AllocChk(ialloc,'TMPLT_CLN.xyz arrays')
	        TMPLT_CLN.x(:) = -999.0d0
	        TMPLT_CLN.y(:) = -999.0d0
	        TMPLT_CLN.z(:) = -999.0d0
        endif

        call Msg(TAB//'                X                Y                Z')

	    do i=1,2
			read(FNum,*) xp(i),yp(i),zp(i)
            write(TmpSTR,'(i8,2x,3g15.5,a)') i,xp(i),yp(i),zp(i),'     '//TRIM(UnitsOfLength)
            call Msg(TAB//trim(TmpSTR))
        end do 
        
 		read(FNum,*) nCells
        write(TmpSTR,'(a, i8)') 'Number of new CLN cells: ',nCells 
        call Msg(TAB//trim(TmpSTR))
        
        TotalLength=sqrt((xp(1) - xp(2))**2 + (yp(1) - yp(2))**2 + (zp(1) - zp(2))**2)
        write(TmpSTR,'(a, g15.5)') 'Total Length of new CLN: ',TotalLength 
        call Msg(TAB//trim(TmpSTR))
        if(TotalLength < 0.0001) then
            call Msg(TAB//'NOTE: Total Length of new CLN is less than 0.0001')
        else if(TotalLength < 1e-10) then   
            call ErrMsg(TAB//'Total Length of new CLN is essentially zero')
        endif
        
        dx=(xp(2) - xp(1))/nCells
        dy=(yp(2) - yp(1))/nCells
        dz=(zp(2) - zp(1))/nCells
        cx=xp(1)
        cy=yp(1)
        cz=zp(1)
        do i=1,nCells+1
            TMPLT_CLN.nNodes=TMPLT_CLN.nNodes+1
            if(TMPLT_CLN.nNodes > nSizeInit) then
                xiTMP (1:nSizeInit) = TMPLT_CLN.x 
                call move_alloc (xiTMP, TMPLT_CLN.x)
                yiTMP (1:nSizeInit) = TMPLT_CLN.y 
                call move_alloc (yiTMP, TMPLT_CLN.y)
                ziTMP (1:nSizeInit) = TMPLT_CLN.z 
                call move_alloc (ziTMP, TMPLT_CLN.z)
                
                nSizeInit=nSizeInit*2
                allocate(xiTMP(nSizeInit*2),yiTMP(nSizeInit*2),ziTMP(nSizeInit*2),stat=ialloc)
	            call AllocChk(ialloc,'xyzTMP points arrays')
	            xiTMP(:) = -999.0d0
	            yiTMP(:) = -999.0d0
	            ziTMP(:) = -999.0d0

            endif
            TMPLT_CLN.x(TMPLT_CLN.nNodes)=cx
            TMPLT_CLN.y(TMPLT_CLN.nNodes)=cy
            TMPLT_CLN.z(TMPLT_CLN.nNodes)=cz
            cx=cx+dx
            cy=cy+dy
            cz=cz+dz
        end do
        
        ! Trim CLN xyz to final size
        nSizeInit=TMPLT_CLN.nNodes
        deallocate(xiTMP,yiTMP,ziTMP)
        allocate(xiTMP(nSizeInit),yiTMP(nSizeInit),ziTMP(nSizeInit),stat=ialloc)
	    call AllocChk(ialloc,'xyzTMP points arrays')
	    xiTMP(:) = -999.0d0
	    yiTMP(:) = -999.0d0
	    ziTMP(:) = -999.0d0
        
        xiTMP (1:nSizeInit) = TMPLT_CLN.x 
        call move_alloc (xiTMP, TMPLT_CLN.x)
        yiTMP (1:nSizeInit) = TMPLT_CLN.y 
        call move_alloc (yiTMP, TMPLT_CLN.y)
        ziTMP (1:nSizeInit) = TMPLT_CLN.z 
        call move_alloc (ziTMP, TMPLT_CLN.z)
        
        if(.not. allocated(TMPLT_CLN.iZone)) then  
            TMPLT_CLN.nElements=TMPLT_CLN.nNodes-1
            allocate(TMPLT_CLN.iZone(TMPLT_CLN.nElements), &
                TMPLT_CLN.iNode(TMPLT_CLN.nNodesPerElement,TMPLT_CLN.nElements), &
                TMPLT_CLN.iLayer(TMPLT_CLN.nElements), &
                TMPLT_CLN.xElement(TMPLT_CLN.nElements), &
                TMPLT_CLN.yElement(TMPLT_CLN.nElements), &
                TMPLT_CLN.zElement(TMPLT_CLN.nElements), &
                TMPLT_CLN.ElementArea(TMPLT_CLN.nElements), &
                TMPLT_CLN.Length(TMPLT_CLN.nElements), &
                TMPLT_CLN.LowestElevation(TMPLT_CLN.nElements), &
                TMPLT_CLN.SlopeAngle(TMPLT_CLN.nElements), &
                stat=ialloc)
            call AllocChk(ialloc,'CLN element arrays')
            TMPLT_CLN.iZone = -999 
            TMPLT_CLN.iNode = -999 
            TMPLT_CLN.iLayer = -999 
            TMPLT_CLN.xElement=-999.0d0
            TMPLT_CLN.yElement=-999.0d0
            TMPLT_CLN.zElement=-999.0d0
            TMPLT_CLN.ElementArea=-999.0d0
            TMPLT_CLN.Length=-999.0d0
            TMPLT_CLN.LowestElevation=-999.0d0
            TMPLT_CLN.SlopeAngle=-999.0d0
        else
            nSizeInit=TMPLT_CLN.nElements
            TMPLT_CLN.nElements=TMPLT_CLN.nNodes-1
            call growInteger2dArray(TMPLT_CLN.iNode,2,nSizeInit,TMPLT_CLN.nElements)
            call growIntegerArray(TMPLT_CLN.iZone,nSizeInit,TMPLT_CLN.nElements)
            call growIntegerArray(TMPLT_CLN.iLayer,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.xElement,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.yElement,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.zElement,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.ElementArea,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.Length,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.LowestElevation,nSizeInit,TMPLT_CLN.nElements)
            call growRealArray(TMPLT_CLN.SlopeAngle,nSizeInit,TMPLT_CLN.nElements)
        end if

        ! generate line element incidences
        do i=nElementsInit+1,TMPLT_CLN.nElements
            TMPLT_CLN.iZone(i)=TMPLT_CLN.nZones
            TMPLT_CLN.iNode(1,i)=i
            TMPLT_CLN.iNode(2,i)=i+1
            TMPLT_CLN.xElement(i)=(TMPLT_CLN.x(TMPLT_CLN.iNode(2,i)) + TMPLT_CLN.x(TMPLT_CLN.iNode(1,i)))/2.0d0
            TMPLT_CLN.yElement(i)=(TMPLT_CLN.y(TMPLT_CLN.iNode(2,i)) + TMPLT_CLN.y(TMPLT_CLN.iNode(1,i)))/2.0d0
            TMPLT_CLN.zElement(i)=(TMPLT_CLN.z(TMPLT_CLN.iNode(2,i)) + TMPLT_CLN.z(TMPLT_CLN.iNode(1,i)))/2.0d0
            TMPLT_CLN.Length(i)=sqrt( (TMPLT_CLN.x(TMPLT_CLN.iNode(2,i)) - TMPLT_CLN.x(TMPLT_CLN.iNode(1,i)))**2 + & 
                                        (TMPLT_CLN.y(TMPLT_CLN.iNode(2,i)) - TMPLT_CLN.y(TMPLT_CLN.iNode(1,i)))**2 + & 
                                        (TMPLT_CLN.z(TMPLT_CLN.iNode(2,i)) - TMPLT_CLN.z(TMPLT_CLN.iNode(1,i)))**2) 
            TMPLT_CLN.LowestElevation(i)=min(TMPLT_CLN.z(TMPLT_CLN.iNode(2,i)),TMPLT_CLN.z(TMPLT_CLN.iNode(1,i)))
            TMPLT_CLN.SlopeAngle(i)=asin(abs(TMPLT_CLN.z(TMPLT_CLN.iNode(2,i))-TMPLT_CLN.z(TMPLT_CLN.iNode(1,i))))* 180.0d0 * pi
        end do
                    
        TMPLT_CLN.IsDefined=.true.
    
        call Msg(' ')
        write(TmpSTR,'(a,i8)')    TAB//'Number of nodes         ',TMPLT_CLN.nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    TAB//'Number of elements      ',TMPLT_CLN.nElements
        call Msg(TmpSTR)


        continue 
    end subroutine CLNFromXYZPair
    


    !----------------------------------------------------------------------
    subroutine new_layer(FNumMUT,TMPLT,zone_by_template)
        implicit none

        character(MAX_INST) :: instruction
        character(MAX_INST) :: layer_name_cmd				    =   'layer name'
        character(MAX_INST) :: minimum_layer_thickness_cmd    =   'minimum layer thickness'
        character(MAX_INST) :: offset_base_cmd			    =   'offset base'
        character(MAX_INST) :: uniform_sublayers_cmd		    =   'uniform sublayering'
        character(MAX_INST) :: proportional_sublayers_cmd	    =   'proportional sublayering'
        character(MAX_INST) :: constant_elevation_cmd		    =   'elevation constant'
        character(MAX_INST) :: gb_file_elevation_cmd		    =   'elevation from gb file'
        character(MAX_INST) :: list_file_elevation_cmd		=   'elevation from list file'
        character(MAX_INST) :: xz_pairs_elevation_cmd			=   'elevation from xz pairs'
        !character(MAX_INST) :: gms_file_elevation_cmd		    =   'elevation from gms file'
        !character(MAX_INST) :: raster_file_elevation_cmd		=   'elevation from raster file'
        !character(MAX_INST) :: bilinear_function_elevation_cmd=   'elevation from bilinear function in xy'
        !character(MAX_INST) :: sine_function_elevation_cmd	=   'elevation from sine function in xy'
        !character(MAX_INST) :: cosine_function_elevation_cmd	=   'elevation from cosine function in xy'
        
        integer :: FNumMUT
        type (TecplotDomain)  TMPLT
        

        integer :: j,k
        logical :: zone_by_template
        character(120) :: basefile

        real(dr), allocatable :: sub_thick(:)
        !real(dr) :: zelev
        real(dr) :: z_added
        !real(dr) :: zelev_proportional
        real(dr) :: sub_thick_frac
	    real(dr) :: tot_thick
        real(dr) :: base_offset

	    integer :: node_fixed
	    integer :: node3d
	    integer :: nel3d

        logical :: minimum_layer_thickness
        logical :: offset_base
	    logical :: proportional_sublayering

	    integer :: nz_temp=0	

        nlayers=nlayers+1

	    proportional_sublayering=.false.
	    minimum_layer_thickness = .false.
	    offset_base = .false.
	    base_offset=0.0
	    nsublayer(nlayers)=1  ! default - no sublayering

        write(layer_name(nlayers),'(a,i5)') 'Layer ',nlayers ! default name


        read_layer_instructions: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            
            if(index(instruction,'end') /=0) then
                call Msg(TAB//'end new layer')
                exit read_layer_instructions
            else
                call Msg(TAB//TAB//instruction)
            end if

            if(index(instruction,layer_name_cmd) /=0) then
                read(FNumMUT,'(a)') layer_name(nlayers)
                call Msg(TAB//TAB//layer_name(nlayers))

            elseif(index(instruction,minimum_layer_thickness_cmd) /=0) then
			    minimum_layer_thickness = .true.
			    read(FNumMUT,*) z_added
			    write(TmpSTR,'(g15.5,a)') z_added,'     '//TRIM(UnitsOfLength)
			    call Msg(TAB//TAB//'Enforce minimum layer thickness of '//trim(TmpSTR))

            elseif(index(instruction,offset_base_cmd) /=0) then
			    offset_base = .true.
			    read(FNumMUT,*) base_offset
			    write(TmpSTR,'(g15.5,a)') base_offset,'     '//TRIM(UnitsOfLength)
			    call Msg(TAB//TAB//'Offset layer base by '//trim(TmpSTR))

            elseif(index(instruction,uniform_sublayers_cmd) /=0) then
			    read(FNumMUT,*) nsublayer(nlayers)
			    nz_temp = nz_temp + nsublayer(nlayers) 		
			    !call user_size_check(nz_temp,user_nz,user_nz_str)
			    write(TmpSTR,'(i4)') nsublayer(nlayers)
			    call Msg(TAB//TAB//'Number of uniform sublayers '//trim(TmpSTR))

            elseif(index(instruction,proportional_sublayers_cmd) /=0) then
			    proportional_sublayering=.true.
			    read(FNumMUT,*) nsublayer(nlayers)
			    nz_temp = nz_temp + nsublayer(nlayers) 		
			    !call user_size_check(nz_temp,user_nz,user_nz_str)
			    write(TmpSTR,'(i4)') nsublayer(nlayers)
			    call Msg(TAB//TAB//'Number of proportional sublayers '//trim(TmpSTR))

                allocate(sub_thick(nsublayer(nlayers)),stat=ialloc)
                call AllocChk(ialloc,'new_layer proportional sublayering array')
                sub_thick(: )= 0.0d0
                tot_thick=0.0
                do j=1,nsublayer(nlayers)
                    read(FNumMUT,*) sub_thick(j)
                    tot_thick=tot_thick+sub_thick(j)
                end do
                call Msg(TAB//TAB//' Sub#   Thickness       Fraction')

                do j=1,nsublayer(nlayers)
                    sub_thick(j)=sub_thick(j)/tot_thick
			        write(TmpSTR,'(i4,2g15.5)') j,sub_thick(j)*tot_thick,sub_thick(j)
                    call Msg(TAB//TAB//trim(TmpSTR))
                end do

            elseif(index(instruction,constant_elevation_cmd) /=0) then
			    read(FNumMUT,*) base_elev(1)
			    write(TmpSTR,'(g15.5,a)') base_elev(1),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//TAB//'Layer base elevation '//TRIM(TmpSTR))
			    do j=2,TMPLT.nNodes
				    base_elev(j)=base_elev(1)
			    end do


            elseif(index(instruction,gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg(TAB//TAB//'Base elevation from '//trim(basefile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(basefile,base_elev,TMPLT.nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg(TAB//TAB//'Base elevation from '//trim(basefile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call list_file_elevation(basefile,base_elev,TMPLT.nNodes)
                
                

            elseif(index(instruction,xz_pairs_elevation_cmd) /=0) then
                call xz_pairs_elevation(FNumMUT,base_elev,TMPLT)
            
       !     elseif(index(instruction,raster_file_elevation_cmd) /=0) then
			    !read(FNumMUT,'(a)') topfile
			    !write(ieco,*) 'Layer top elevation from gb file  ',topfile
       !         call read_raster_to_mesh_elev(topfile,top_elev)
       !     
       !     elseif(index(instruction,bilinear_function_elevation_cmd) /=0) then
       !         call bilinear_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)
       !     
       !     elseif(index(instruction,sine_function_elevation_cmd) /=0) then
       !         call sine_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)
       !     
       !     elseif(index(instruction,cosine_function_elevation_cmd) /=0) then
       !         call cosine_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)
       !     

            else
			    call ErrMsg(TAB//'Unrecognized instruction: new layer')
            end if

        end do read_layer_instructions

	    if(offset_base) then
            do j=1,TMPLT.nNodes
                base_elev(j)=base_elev(j)+base_offset
		    end do

	    end if

	    sub_thick_frac=0.0
        do k=1,nsublayer(nlayers)
            if(proportional_sublayering) sub_thick_frac=sub_thick_frac+sub_thick(k)

           !         new nodes
            node_fixed=0
            do j=1,TMPLT.nNodes
                if(base_elev(j) >= top_elev(j)) then
                    !rt-jun01
                    if(.not. minimum_layer_thickness) then
                        write(ieco,*) ' Error: Base of layer ',nlayers,' >= top'
                        write(ieco,*) ' At x: ',TMPLT.x(j),' y: ',TMPLT.y(j)
                        write(ieco,*) ' Base elev= ', base_elev(j),' top elev= ',top_elev(j)

                        write(*,*) ' Error: Base of layer ',nlayers,' >= top'
                        write(*,*) ' At x: ',TMPLT.x(j),' y: ',TMPLT.y(j)
                        write(*,*) ' Base elev= ', base_elev(j),' top elev= ',top_elev(j)
                        call ErrMsg('Base elevation > top. See above.')
                    else
                        base_elev(j) = top_elev(j) - z_added
                        node_fixed = node_fixed + 1
                    end if
                else if(minimum_layer_thickness .and. (base_elev(j) + top_elev(j) > z_added)) then
                    base_elev(j) = top_elev(j) - z_added
                    node_fixed = node_fixed + 1
                end if
                node3d=nsheet*TMPLT.nNodes+j
                x(node3d)=TMPLT.x(j)
                y(node3d)=TMPLT.y(j)
                if(proportional_sublayering) then
                    z(node3d)=zelev_proportional(top_elev(j),base_elev(j),sub_thick_frac,1.0d0)
                else
                    z(node3d)=zelev(top_elev(j),base_elev(j),k,nsublayer(nlayers))
                end if
            end do

            !         new elements
            do j=1,TMPLT.nElements
                nel3d=(nsheet-1)*TMPLT.nElements+j
                ilyr(nel3d)=nsheet
                if(nln==6) then ! prisms from triangles
                    in(1,nel3d)=TMPLT.iNode(1,j)+nsheet*TMPLT.nNodes
                    in(2,nel3d)=TMPLT.iNode(2,j)+nsheet*TMPLT.nNodes
                    in(3,nel3d)=TMPLT.iNode(3,j)+nsheet*TMPLT.nNodes
                    in(4,nel3d)=TMPLT.iNode(1,j)+(nsheet-1)*TMPLT.nNodes
                    in(5,nel3d)=TMPLT.iNode(2,j)+(nsheet-1)*TMPLT.nNodes
                    in(6,nel3d)=TMPLT.iNode(3,j)+(nsheet-1)*TMPLT.nNodes
                    if(zone_by_template) then
                        iprp(nel3d)=TMPLT.iZone(j)
                    else
                        iprp(nel3d)=nlayers
                    end if
                else ! blocks from rectangles, not currently supported
                    in(1,nel3d)=TMPLT.iNode(1,j)+nsheet*TMPLT.nNodes
                    in(2,nel3d)=TMPLT.iNode(2,j)+nsheet*TMPLT.nNodes
                    in(3,nel3d)=TMPLT.iNode(3,j)+nsheet*TMPLT.nNodes
                    in(4,nel3d)=TMPLT.iNode(4,j)+nsheet*TMPLT.nNodes
				    in(5,nel3d)=TMPLT.iNode(1,j)+(nsheet-1)*TMPLT.nNodes
                    in(6,nel3d)=TMPLT.iNode(2,j)+(nsheet-1)*TMPLT.nNodes
                    in(7,nel3d)=TMPLT.iNode(3,j)+(nsheet-1)*TMPLT.nNodes
                    in(8,nel3d)=TMPLT.iNode(4,j)+(nsheet-1)*TMPLT.nNodes
        
                    if(zone_by_template) then
                        iprp(nel3d)=TMPLT.iZone(j)
                    else
                        iprp(nel3d)=nlayers
                    end if
    
                end if
            end do


            zi(nsheet)=z((nsheet-1)*TMPLT.nNodes+1)
            nsheet=nsheet+1
            !call user_size_check(nsheet+1,user_nz,user_nz_str)
        end do

        if(minimum_layer_thickness) then
            write(ieco,*) ' Number of nodes for layer ',nlayers
            write(ieco,*) '  that have had their base elevation modified'
            write(ieco,*) ' by subtracting ',z_added,' is equal to',node_fixed
        end if

	    ! initialize base elevation for (potential) next layer
        do j=1,TMPLT.nNodes
            top_elev(j)=base_elev(j)
        end do

        if(allocated(sub_thick)) deallocate(sub_thick)
        

    end subroutine new_layer
    
    !----------------------------------------------------------------------
    subroutine read_gb_nprop(fname,nprop,maxnnp)
        implicit none

        integer :: i
        integer :: maxnnp

        character*(*) fname
        character*11 file_type
        real*8 :: nprop(maxnnp)
        real*4 :: nprop_gb(maxnnp)
	    character(80) :: dtitle

        inquire(file=fname,exist=FileExists,form=file_type)
        if(.not. FileExists) then
            call ErrMsg(' File not found: '//fname)
        end if


	    call getunit(itmp)
        open(itmp,file=fname,status='unknown',form='unformatted')

	    read(itmp) dtitle
	    read(itmp) (nprop_gb(i),i=1,maxnnp)
	    nprop(:) =nprop_gb(:)

	    call freeunit(itmp)

    end subroutine read_gb_nprop
    !----------------------------------------------------------------------
    subroutine top_elevation(FNumMUT,TMPLT)
        implicit none
        character(MAX_INST) :: instruction

        character(MAX_INST) :: offset_top_cmd				    =   'offset top'
        character(MAX_INST) :: constant_elevation_cmd		    =   'elevation constant'
        character(MAX_INST) :: gb_file_elevation_cmd		    =   'elevation from gb file'
        character(MAX_INST) :: list_file_elevation_cmd		    =   'elevation from list file'
        character(MAX_INST) :: xz_pairs_elevation_cmd			=   'elevation from xz pairs'
        !character(MAX_INST) :: gms_file_elevation_cmd		=   'elevation from gms file'
        !character(MAX_INST) :: raster_file_elevation_cmd		=   'elevation from raster file'
        !character(MAX_INST) :: bilinear_function_elevation_cmd=   'elevation from bilinear function in xy'
        !character(MAX_INST) :: sine_function_elevation_cmd	=   'elevation from sine function in xy'
        !character(MAX_INST) :: cosine_function_elevation_cmd	=   'elevation from cosine function in xy'
        
        integer :: FNumMUT
        type (TecplotDomain) TMPLT

        integer :: j
        character(120) :: topfile
        real(dr) :: top_offset
        logical :: offset_top

	    offset_top = .false.
	    top_offset=0.0

	    ! Change default behaviours and top elevation
        read_top: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            if(index(instruction, 'end') /=0) then
                call Msg(TAB//'end top elevation')
                exit read_top
            else
                call LwrCse(instruction)
                call Msg(TAB//instruction)
            end if    

            if(index(instruction,constant_elevation_cmd) /=0) then
                read(FNumMUT,*) top_elev(1)
                write(TmpSTR,'(2g15.5,a)') top_elev(1),'     '//UnitsOfLength
                call Msg(TAB//trim(TmpSTR))
                do j=2,TMPLT.nNodes
	                top_elev(j)=top_elev(1)
                end do

            elseif(index(instruction, offset_top_cmd) /=0) then
                offset_top = .true.
                read(FNumMUT,*) top_offset
			    write(TmpSTR,'(2g15.5)') top_offset
                call Msg(TAB//trim(TmpSTR))

            elseif(index(instruction, gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg(TAB//TAB//'Top elevation from '//trim(topfile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(topfile,top_elev,TMPLT.nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg(TAB//TAB//'Top elevation from '//trim(topfile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call list_file_elevation(topfile,top_elev,TMPLT.nNodes)
                
            elseif(index(instruction, xz_pairs_elevation_cmd) /=0) then
                call xz_pairs_elevation(FNumMUT,top_elev,TMPLT)
                
       !     elseif(instruction .eq. raster_file_elevation_cmd) /=0) then
			    !read(FNumMUT,'(a)') topfile
			    !write(ieco,*) 'System top from file ',topfile
       !         call read_raster_to_mesh_elev(topfile,top_elev)

       !     elseif(index(instruction, bilinear_function_elevation_cmd) /=0) then
       !         call bilinear_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)
       !
       !     elseif(index(instruction, sine_function_elevation_cmd) /=0) then
       !         call sine_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)
       !
       !     elseif(index(instruction, cosine_function_elevation_cmd) /=0) then
       !         call cosine_function_in_xy(x2d,y2d,top_elev,nn2d,FNumMUT,ieco)


            else
			    call ErrMsg(TAB//'Unrecognized instruction: top elevation')
            end if

        end do read_top
    
	    if(offset_top) then
            do j=1,TMPLT.nNodes
                top_elev(j)=top_elev(j)+top_offset
		    end do

	    end if
    end subroutine top_elevation
    
    !----------------------------------------------------------------------
    subroutine xyzFromListFile(FNum,xi,yi,zi,nPoints)
        implicit none
        integer :: FNum
        
        integer :: nSizeInit=2
        real, allocatable :: xi(:), yi(:), zi(:)  ! xyz coordinate list defining CLN to be read
        real, allocatable :: xiTMP(:), yiTMP(:), ziTMP(:)  ! temporary xyz arrays
        integer :: nPoints  ! number of points in list

        character(256) :: instruction

	    allocate(xi(nSizeInit),yi(nSizeInit),zi(nSizeInit),stat=ialloc)
	    call AllocChk(ialloc,'xyz points arrays')
	    xi(:) = -999.0d0
	    yi(:) = -999.0d0
	    zi(:) = -999.0d0
	    allocate(xiTMP(nSizeInit*2),yiTMP(nSizeInit*2),ziTMP(nSizeInit*2),stat=ialloc)
	    call AllocChk(ialloc,'xyzTMP points arrays')
	    xiTMP(:) = -999.0d0
	    yiTMP(:) = -999.0d0
	    ziTMP(:) = -999.0d0


        call Msg(TAB//'                X         Y        Z')

	    nPoints=0
	    do
		    read(FNum,'(a)',iostat=status) instruction
		    if(status /= 0) exit

		    call LwrCse(instruction)

		    if(index(instruction,'end') > 0) then
                call Msg(TAB//'end xyz list of points')
			    exit
		    else
			    nPoints=nPoints+1
                if(nPoints>nSizeInit) then
                    xiTMP (1:nSizeInit) = xi 
                    call move_alloc (xiTMP, xi)
                    yiTMP (1:nSizeInit) = yi 
                    call move_alloc (yiTMP, yi)
                    ziTMP (1:nSizeInit) = zi 
                    call move_alloc (ziTMP, zi)
                    
                    nSizeInit=nSizeInit*2
                    allocate(xiTMP(nSizeInit*2),yiTMP(nSizeInit*2),ziTMP(nSizeInit*2),stat=ialloc)
	                call AllocChk(ialloc,'xyzTMP points arrays')
	                xiTMP(:) = -999.0d0
	                yiTMP(:) = -999.0d0
	                ziTMP(:) = -999.0d0

                endif
                
			    read(instruction,*,iostat=status) xi(nPoints),yi(nPoints),zi(nPoints)

			    if(status /= 0) then
				    call ErrMsg('Bad xyz triple')
                endif
                
                write(TmpSTR,'(i8,2x,3g15.5,a)') nPoints,xi(nPoints),yi(nPoints),zi(nPoints),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//trim(TmpSTR))

		    endif
        end do
        
       
        continue

    end subroutine xyzFromListFile

    !----------------------------------------------------------------------
    subroutine xz_pairs_elevation(FNum,nprop,TMPLT)
        implicit none
        integer :: FNum
        type(TecplotDomain) TMPLT

        integer :: i, j
	    integer :: npairs
        real(dr) :: nprop(TMPLT.nNodes)
	    real(dr) :: t
                
        character(256) :: instruction

	    real(dr) :: xp(1000)
	    real(dr) :: zp(1000)
	    xp(:) = 0
	    zp(:) = 0


        call Msg(TAB//'                X                Z')

	    npairs=0
	    read_xz_pairs:do
		    read(FNum,'(a)',iostat=status) instruction
		    if(status /= 0) exit

		    len=len_trim(instruction)
            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg(TAB//'end elevation from xz pairs')
                exit read_xz_pairs
		    else
			    npairs=npairs+1
			    read(instruction,*,iostat=status) xp(npairs),zp(npairs)

			    if(npairs > 1) then
			        if(xp(npairs) <= xp(npairs-1)) then
				        call ErrMsg('X values must be entered in ascending order')
				    endif
			    endif

			    if(status /= 0) then
				    call ErrMsg('Bad xz pair')
                endif
                
                write(TmpSTR,'(i8,2x,2g15.5,a)') npairs,xp(npairs),zp(npairs),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//trim(TmpSTR))

		    endif
	    end do read_xz_pairs


        do i=1,TMPLT.nNodes
		    do j=1,npairs-1
			    if(TMPLT.x(i) >= xp(j) .and. TMPLT.x(i) <= xp(j+1)) then  ! interpolate
	                t=(TMPLT.x(i)-xp(j))/(xp(j+1)-xp(j))
				    nprop(i)=(1.0-t)*zp(j)+t*zp(j+1)
			    end if
		    end do
        end do



    end subroutine xz_pairs_elevation
    !----------------------------------------------------------------------
    function zelev(top,base,k,nk)
        implicit none

        real(dr) :: zelev
	    integer :: k, nk
	    real(dr) :: base, top

        zelev=top-(top-base)*k/(nk)

    end function zelev

    !----------------------------------------------------------------------
    function zelev_proportional(top,base,rsub,rsubtot)
        implicit none

	    real(dr) :: zelev_proportional
	
	    real(dr) :: base, top, rsub, rsubtot

        zelev_proportional=top-(top-base)*rsub/(rsubtot)

        return
    end function zelev_proportional


end Module MeshGeneration
