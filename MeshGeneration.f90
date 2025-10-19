Module MeshGen
    use GeneralRoutines
    use NumericalMesh
    use Tecplot
    implicit none
        
    logical :: NeedGBName=.false. ! flag to indicate if GridBuilder name is needed


    integer(i4) :: user_nz
    integer(i4) :: user_maxnlayer=50
    real(dp), allocatable  :: x(:)
    real(dp), allocatable  :: y(:)
    real(dp), allocatable  :: z(:)
    integer(i4), allocatable  :: in(:,:)
    integer(i4), allocatable  :: iprp(:)
    integer(i4), allocatable  :: ilyr(:)
    real(dp), allocatable  :: zi(:)
    real(dp), allocatable  :: base_elev(:)
    character(MAX_LBL), allocatable  :: layer_name(:)
    integer(i4), allocatable  :: nsublayer(:)
        
    integer(i4) :: nsheet
    integer(i4) :: nlayers

    real(dp), allocatable  :: top_elev(:)

    integer(i4) :: nn_temp, ne_temp
        
    logical :: layer_defined=.false.
    logical :: zone_by_template=.false.
    
    integer(i4), allocatable :: seg_node(:,:)
    integer(i4) :: nSeg        

    
    contains
    !----------------------------------------------------------------------
    subroutine list_file_elevation(fname,nprop,maxnnp)
        implicit none

        integer(i4) :: i
        integer(i4) :: maxnnp

        character*(*) fname
        character*11 file_type
        real(dp) :: nprop(maxnnp)
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
    subroutine ReadGridBuilderMesh(FNumMUT,GB_TRI_2D)
        implicit none
        type(mesh) GB_TRI_2D
        
        integer(i4) :: FNumMUT
        
        character(128) :: GBPathToFile

        integer(i4) :: i,j
        real(dp) :: x(3),y(3)
        real(dp) :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3)
        
        
        if(NeedGBName) then
            read(FNumMut,'(a80)') TmpSTR
            GB_TRI_2D%Name=TmpSTR
            call Msg('Name: '//trim(GB_TRI_2D%Name))
        else
            GB_TRI_2D%Name='TMPLT'
        end if
        
        read(FNumMut,'(a80)') GBPathToFile

        inquire(file=trim(GBPathToFile)//'.grd',exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('File not found: '//trim(GBPathToFile)//'.grd')
        end if
        
        call Msg(FileReadSTR//'GridBuilder file: '//trim(GBPathToFile)//'.grd')


        GB_TRI_2D%nNodesPerElement=3
        GB_TRI_2D%Element%Typ='triangle'
        GB_TRI_2D%TecplotTyp='fetriangle'
        
        !     NODE COORDINATES
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.xyc',form='unformatted')
        read(itmp) GB_TRI_2D%nNodes
    
        
        allocate(GB_TRI_2D%node(GB_TRI_2D%nNodes),stat=ialloc)
        call AllocChk(ialloc,'ReadGridBuilderMesh 2d node array')
        GB_TRI_2D%node%x = 0 ! automatic initialization
        GB_TRI_2D%node%y = 0 ! automatic initialization
        GB_TRI_2D%node%z = 0 ! automatic initialization
        read(itmp) (GB_TRI_2D%node(i)%x, GB_TRI_2D%node(i)%y, i=1,GB_TRI_2D%nNodes)
	    call freeunit(itmp)

        !     ELEMENT INCIDENCES
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.in3',form='unformatted')
        read(itmp) GB_TRI_2D%nElements

        allocate(GB_TRI_2D%Element(GB_TRI_2D%nElements), GB_TRI_2D%idNode(GB_TRI_2D%nNodesPerElement,GB_TRI_2D%nElements), stat=ialloc)
        call AllocChk(ialloc,'ReadGridBuilderMesh 2d element, idNode arrays')
        GB_TRI_2D%Element(:)%idZone = 0 ! automatic initialization
        GB_TRI_2D%idNode(:,:) = 0 ! automatic initialization
        read(itmp) (GB_TRI_2D%idNode(1,i),GB_TRI_2D%idNode(2,i),GB_TRI_2D%idNode(3,i), i=1,GB_TRI_2D%nElements)
	    call freeunit(itmp)

        !     Element zone numbers
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.ean',form='unformatted')  ! ean contains GB element area(aka zone) numbers 
        read(itmp) (GB_TRI_2D%Element(i)%idZone,i=1,GB_TRI_2D%nElements)
	    call freeunit(itmp)
        GB_TRI_2D%nZones=maxval(GB_TRI_2D%Element%idZone)
        allocate(GB_TRI_2D%Zone(GB_TRI_2D%nZones),stat=ialloc)
        call AllocChk(ialloc,'GB_TRI_2D%Zone array')
        
        do i=1,GB_TRI_2D%nElements
            ! xc and yc from circumcircles
            if(GB_TRI_2D%nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
            do j=1,GB_TRI_2D%nNodesPerElement
                x(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%x
                y(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%y
            end do
            call InnerCircle(x,y,GB_TRI_2D%Element(i)%xyArea,xc,yc,GB_TRI_2D%Element(i)%rCircle,lseg,aseg,dseg)
            
            GB_TRI_2D%Element(i)%SideLength(1)=lseg(1,2)
            GB_TRI_2D%Element(i)%SideLength(2)=lseg(2,3)
            GB_TRI_2D%Element(i)%SideLength(3)=lseg(3,1)
           
            GB_TRI_2D%Element(i)%xCircle=xc
            GB_TRI_2D%Element(i)%yCircle=yc
                
                
            ! zc from centroid of the idNode array coordinates
            zc=0.0
            do j=1,3
                zc=zc+GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%z
            end do
                
            GB_TRI_2D%Element(i)%x=xc
            GB_TRI_2D%Element(i)%y=yc
            GB_TRI_2D%Element(i)%z=zc/3
            GB_TRI_2D%Element(i)%zCircle=zc/3
        end do
        
        GB_TRI_2D%Element%is=0

        if(EnableTecplotOutput) then
            call MeshToTecplot(GB_TRI_2D)
        endif

        return
    end subroutine ReadGridBuilderMesh
    !-------------------------------------------------------------
    subroutine MESHToTecplot(LocalMesh)
        implicit none
        type(mesh) LocalMesh
        
        integer(i4) :: Fnum
        character(MAX_STR) :: FName
        integer(i4) :: i, j

        ! tecplot output file
        FName=trim(LocalMesh%name)//'.tecplot.dat'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(LocalMesh%name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","Zone","Element Area","Inner Circle Radius"'
        nVar=6

        !if(allocated(LocalMesh%rCircle)) then
        !    VarSTR=trim(VarSTR)//'"'//trim(LocalMesh%name)//'Inner circle radius",'
        !    nVar=nVar+1
        !end if
            
        write(FNum,'(a)') trim(VarSTR)


        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(LocalMesh%name)//'"  ,N=',LocalMesh%nNodes,', E=',LocalMesh%nElements,&
        ', datapacking=block, zonetype='//trim(LocalMesh%TecplotTyp)
        
        CellCenteredSTR=', VARLOCATION=([4,5'
        if(nVar.ge.6) then
            do j=6,nVar
                write(str2,'(i2)') j
                CellCenteredSTR=trim(CellCenteredSTR)//','//str2
            end do
        end if
        CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'
        write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)

        write(FNum,'(a)') '# x'
        write(FNum,'(5('//FMT_R8//'))') (LocalMesh%node(i)%x,i=1,LocalMesh%nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5('//FMT_R8//'))') (LocalMesh%node(i)%y,i=1,LocalMesh%nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5('//FMT_R8//'))') (LocalMesh%node(i)%z,i=1,LocalMesh%nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (LocalMesh%Element(i)%idZone,i=1,LocalMesh%nElements)
            
        write(FNum,'(a)') '# element area'
        write(FNum,'(5('//FMT_R8//'))') (LocalMesh%Element(i)%xyArea,i=1,LocalMesh%nElements)
                    
        write(FNum,'(a)') '# circle radius'
        write(FNum,'(5('//FMT_R8//'))') (LocalMesh%Element(i)%rCircle,i=1,LocalMesh%nElements)
            
        
        do i=1,LocalMesh%nElements
            if(LocalMesh%nNodesPerElement==2) then ! 2-node segment
                write(FNum,'(8i8)') (LocalMesh%idNode(j,i),j=1,2)
            else if(LocalMesh%nNodesPerElement==3) then ! 3-node triangle
                write(FNum,'(8i8)') (LocalMesh%idNode(j,i),j=1,3)
            else if(LocalMesh%nNodesPerElement==4) then ! 4-node quadrilateral
                if(LocalMesh%idNode(4,i) > 0) then
                    write(FNum,'(8i8)') (LocalMesh%idNode(j,i),j=1,4) 
                else
                    write(FNum,'(8i8)') (LocalMesh%idNode(j,i),j=1,3), LocalMesh%idNode(3,i) 
                end if
            else
                write(TmpSTR,'(i2)')LocalMesh%nNodesPerElement
                call ErrMsg(trim(LocalMesh%name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if

        end do
       
        call FreeUnit(FNum)
        
    end subroutine MeshToTecplot
    !----------------------------------------------------------------------
    subroutine GenerateSegmentsFromXYZEndpoints(FNumMut,SEG_3D)
        implicit none
        integer(i4) :: FNumMUT
        type(mesh) SEG_3D
        
        type(node), allocatable :: nodeTMP(:)

        integer(i4) :: i
        integer(i4) :: nElements
        integer(i4) :: nSizeInit, nNodesInit, nElementsInit
        !real(dp), allocatable :: xiTMP(:), yiTMP(:), ziTMP(:)  ! temporary xyz arrays
                
        real(dp) :: TotalLength
        real(dp) :: dx, dy, dz
        real(dp) :: cx, cy, cz

	    real(dp) :: xp(2)
	    real(dp) :: yp(2)
	    real(dp) :: zp(2)
	    xp(:) = 0
	    yp(:) = 0
	    zp(:) = 0
        
        ! generate segments from XYZ triples
        read(FNumMut,'(a80)') TmpSTR
        SEG_3D%Name=TmpSTR
        call Msg('Name: '//trim(SEG_3D%Name))

        SEG_3D%nNodesPerElement=2
        SEG_3D%Element%Typ='segment'
        SEG_3D%TecplotTyp='felineseg'

        
        
        nNodesInit=SEG_3D%nNodes
        nElementsInit=SEG_3D%nElements
        SEG_3D%nZones=SEG_3D%nZones+1

        nSizeInit=max(2,SEG_3D%nNodes)
	    allocate(nodeTMP(nSizeInit*2),stat=ialloc)
	    call AllocChk(ialloc,'nodeTMP arrays')
	    nodeTMP%x = -999.0d0
	    nodeTMP%y = -999.0d0
	    nodeTMP%z = -999.0d0
        
        if(.not. allocated(SEG_3D%node)) then  
            allocate(SEG_3D%node(nSizeInit),stat=ialloc)
	        call AllocChk(ialloc,'SEG_3D%node array')
	        SEG_3D%node%x = -999.0d0
	        SEG_3D%node%y = -999.0d0
	        SEG_3D%node%z = -999.0d0
        endif

        call Msg(TAB//'                X                Y                Z')

	    do i=1,2
			read(FNumMUT,*) xp(i),yp(i),zp(i)
            write(TmpSTR,'(i8,2x,5('//FMT_R8//'),a)') i,xp(i),yp(i),zp(i),'     '//TRIM(UnitsOfLength)
            call Msg(TAB//trim(TmpSTR))
        end do 
        
 		read(FNumMUT,*) nElements
        write(TmpSTR,'(a, i8)') 'Number of new segment elements: ',nElements 
        call Msg(TAB//trim(TmpSTR))
        
        
        TotalLength=sqrt((xp(1) - xp(2))**2 + (yp(1) - yp(2))**2 + (zp(1) - zp(2))**2)
        write(TmpSTR,'(a, '//FMT_R8//')') 'Total Length of new CLN: ',TotalLength 
        call Msg(TAB//trim(TmpSTR))
        if(TotalLength < 0.0001) then
            call Msg(TAB//'NOTE: Total Length of new CLN is less than 0.0001')
        else if(TotalLength < 1e-10) then   
            call ErrMsg(TAB//'Total Length of new CLN is essentially zero')
        endif
        
        dx=(xp(2) - xp(1))/nElements
        dy=(yp(2) - yp(1))/nElements
        dz=(zp(2) - zp(1))/nElements
        cx=xp(1)
        cy=yp(1)
        cz=zp(1)
        do i=1,nElements+1
            SEG_3D%nNodes=SEG_3D%nNodes+1
            if(SEG_3D%nNodes > nSizeInit) then
                nodeTMP(1:nSizeInit) = SEG_3D%node
                call move_alloc (nodeTMP, SEG_3D%node)
               
                nSizeInit=nSizeInit*2
	            allocate(nodeTMP(nSizeInit*2),stat=ialloc)
	            call AllocChk(ialloc,'nodeTMP arrays')
	            nodeTMP%x = -999.0d0
	            nodeTMP%y = -999.0d0
	            nodeTMP%z = -999.0d0

            endif
            SEG_3D%node(SEG_3D%nNodes)%x=cx
            SEG_3D%node(SEG_3D%nNodes)%y=cy
            SEG_3D%node(SEG_3D%nNodes)%z=cz
            cx=cx+dx
            cy=cy+dy
            cz=cz+dz
        end do
        
        ! Trim CLN xyz to final size
        nSizeInit=SEG_3D%nNodes
        deallocate(nodeTMP)
	    allocate(nodeTMP(nSizeInit*2),stat=ialloc)
	    call AllocChk(ialloc,'nodeTMP arrays')
	    nodeTMP%x = -999.0d0
	    nodeTMP%y = -999.0d0
	    nodeTMP%z = -999.0d0
        
        nodeTMP(1:nSizeInit) = SEG_3D%node
        call move_alloc (nodeTMP, SEG_3D%node)
        
        if(.not. allocated(SEG_3D%element)) then  
            SEG_3D%nElements=SEG_3D%nNodes-1
            allocate(SEG_3D%Element(SEG_3D%nElements), SEG_3D%idNode(SEG_3D%nNodesPerElement,SEG_3D%nElements), stat=ialloc)
            call AllocChk(ialloc,'Read_gbldr_slice 2d element arrays')
            SEG_3D%Element(:)%idZone = 0 ! automatic initialization
            SEG_3D%idNode(:,:) = 0 ! automatic initialization
            SEG_3D%element%iLayer = 0
            SEG_3D%element%x=0.0d0
            SEG_3D%element%y=0.0d0
            SEG_3D%element%z=0.0d0
            SEG_3D%element%Area=0.0d0
            SEG_3D%element%xyArea=0.0d0
            !SEG_3D%cell%Length=-999.0d0
            !SEG_3D%cell%LowestElevation=-999.0d0
            !SEG_3D%cell%SlopeAngle=-999.0d0
        else
            nSizeInit=SEG_3D%nElements
            SEG_3D%nElements=SEG_3D%nNodes-1
            call growInteger2dArray(SEG_3D%idNode,2,nSizeInit,SEG_3D%nElements)
            call growElementArray(SEG_3D%Element,nSizeInit,SEG_3D%nElements)
        end if

        ! generate line element incidences
        do i=nElementsInit+1,SEG_3D%nElements
            SEG_3D%element(i)%idZone=SEG_3D%nZones
            SEG_3D%idNode(1,i)=i
            SEG_3D%idNode(2,i)=i+1
            SEG_3D%element(i)%x=(SEG_3D%node(SEG_3D%idNode(2,i))%x + SEG_3D%node(SEG_3D%idNode(1,i))%x)/2.0d0
            SEG_3D%element(i)%y=(SEG_3D%node(SEG_3D%idNode(2,i))%y + SEG_3D%node(SEG_3D%idNode(1,i))%y)/2.0d0
            SEG_3D%element(i)%z=(SEG_3D%node(SEG_3D%idNode(2,i))%z + SEG_3D%node(SEG_3D%idNode(1,i))%z)/2.0d0
            !SEG_3D%cell(i)%Length=sqrt(       (SEG_3D%node(SEG_3D%idNode(2,i))%x -   SEG_3D%node(SEG_3D%idNode(1,i))%x)**2 + & 
            !                                        (SEG_3D%node(SEG_3D%idNode(2,i))%y -   SEG_3D%node(SEG_3D%idNode(1,i))%y)**2 + & 
            !                                        (SEG_3D%node(SEG_3D%idNode(2,i))%z -   SEG_3D%node(SEG_3D%idNode(1,i))%z)**2) 
            !SEG_3D%cell(i)%LowestElevation=min (SEG_3D%node(SEG_3D%idNode(2,i))%z,    SEG_3D%node(SEG_3D%idNode(1,i))%z)
            !SEG_3D%cell(i)%SlopeAngle=asin(abs (SEG_3D%node(SEG_3D%idNode(2,i))%z-    SEG_3D%node(SEG_3D%idNode(1,i))%z))* 180.0d0 * pi
        end do
                    
        !SEG_3D%IsDefined=.true.
    
        call Msg(' ')
        write(TmpSTR,'(a,i8)')    TAB//'Number of nodes         ',SEG_3D%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    TAB//'Number of elements      ',SEG_3D%nElements
        call Msg(TmpSTR)
        
        if(EnableTecplotOutput) then
            call MeshToTecplot(SEG_3D)
        endif




        continue 
    end subroutine GenerateSegmentsFromXYZEndpoints
    !----------------------------------------------------------------------
    subroutine GenerateUniformRectangles(FNumMut,U_RECT_2D)
        implicit none
        integer(i4) :: FNumMUT
        type(mesh) U_RECT_2D


        integer(i4) :: i, j, k
	    integer(i4) :: nbx, nby, nn2d, ne2d
        real(dp) :: xl, yl, delx, dely
        real(dp) :: xOffset, yOffset
        
        real(sp), allocatable :: xi(:)
        real(sp), allocatable :: yi(:)

        !     generate uniform rectangles
        read(FNumMut,'(a80)') TmpSTR
        U_RECT_2D%Name=TmpSTR
        call Msg('Name: '//trim(U_RECT_2D%Name))

        U_RECT_2D%nNodesPerElement=4
        U_RECT_2D%Element%Typ='rectangle'
        U_RECT_2D%TecplotTyp='fequadrilateral'


        !     xl, yl are grid lengths in x- and y-directions
        call Msg(FileReadSTR//'MUT file')
        read(FNumMUT,*) xl, nbx, xOffset
        write(TMPStr,'(a,'//FMT_R8//',a)') 'Mesh length in X        ',xl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')      'Number of elements in X ',nbx
        call Msg(TMPStr)
        write(TMPStr,'(a,'//FMT_R8//',a)') 'X Offset                ',xOffset,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)

        read(FNumMUT,*) yl, nby,yOffset
        write(TMPStr,'(a,'//FMT_R8//',a)') 'Mesh length in Y        ',yl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')      'Number of elements in Y ',nby
        call Msg(TMPStr)
        write(TMPStr,'(a,'//FMT_R8//',a)') 'Y Offset                ',yOffset,'     '//TRIM(UnitsOfLength)
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
            xi(i)=float(i-1)*delx+xOffset
        end do
        do i=1,ny
            yi(i)=float(i-1)*dely+yOffset
        end do

        !     generate 2D slice first
        U_RECT_2D%nNodes=nx*ny
        allocate(U_RECT_2D%node(U_RECT_2D%nNodes),stat=ialloc)
        call AllocChk(ialloc,'U_RECT_2D%node array')
        U_RECT_2D%node%x = 0 ! automatic initialization
        U_RECT_2D%node%y = 0 ! automatic initialization
        U_RECT_2D%node%z = 0 ! automatic initialization
        
        U_RECT_2D%nElements=(nx-1)*(ny-1)
        allocate(U_RECT_2D%Element(U_RECT_2D%nElements), U_RECT_2D%idNode(U_RECT_2D%nNodesPerElement,U_RECT_2D%nElements), stat=ialloc)
        call AllocChk(ialloc,'U_RECT_2D%Element array')
        U_RECT_2D%Element%idZone = 0 ! automatic initialization
        U_RECT_2D%idNode = 0 ! automatic initialization
        U_RECT_2D%element%iLayer = 0
        U_RECT_2D%element%x=0.0d0
        U_RECT_2D%element%y=0.0d0
        U_RECT_2D%element%z=0.0d0
        U_RECT_2D%element%Area=0.0d0
        U_RECT_2D%element%xyArea=0.0d0
        
        !     generate 2D node coordinates
        nn2d=0
        do i=1,ny
            do j=1,nx
                nn2d=nn2d+1
                U_RECT_2D%node(nn2d)%x=xi(j)
                U_RECT_2D%node(nn2d)%y=yi(i)
            end do
        end do

        !     generate 2D rectangular element incidences
        ne2d=0
        do i=1,ny-1
            k=1+(i-1)*nx
            do j=1,nx-1
                ne2d=ne2d+1
                U_RECT_2D%idNode(1,ne2d)=k
                U_RECT_2D%idNode(2,ne2d)=k+1
                U_RECT_2D%idNode(3,ne2d)=k+nx+1
                U_RECT_2D%idNode(4,ne2d)=k+nx
                U_RECT_2D%element(ne2d)%idZone = 1
                U_RECT_2D%element(ne2d)%iLayer = 1
                k=k+1
            end do
        end do

        do i=1,U_RECT_2D%nElements
            U_RECT_2D%element(i)%xyArea=  (U_RECT_2D%node(U_RECT_2D%idNode(2,i))%x - U_RECT_2D%node(U_RECT_2D%idNode(1,i))%x) * &
                                      (U_RECT_2D%node(U_RECT_2D%idNode(3,i))%y - U_RECT_2D%node(U_RECT_2D%idNode(1,i))%y)
            U_RECT_2D%element(i)%x=(U_RECT_2D%node(U_RECT_2D%idNode(2,i))%x + U_RECT_2D%node(U_RECT_2D%idNode(1,i))%x)/2.0d0
            U_RECT_2D%element(i)%y=(U_RECT_2D%node(U_RECT_2D%idNode(3,i))%y + U_RECT_2D%node(U_RECT_2D%idNode(1,i))%y)/2.0d0
            
            U_RECT_2D%element(i)%SideLength(1)=abs(U_RECT_2D%node(U_RECT_2D%idNode(2,i))%x - U_RECT_2D%node(U_RECT_2D%idNode(1,i))%x)
            U_RECT_2D%element(i)%SideLength(2)=abs(U_RECT_2D%node(U_RECT_2D%idNode(3,i))%y - U_RECT_2D%node(U_RECT_2D%idNode(2,i))%y)
            U_RECT_2D%element(i)%SideLength(3)=abs(U_RECT_2D%node(U_RECT_2D%idNode(4,i))%x - U_RECT_2D%node(U_RECT_2D%idNode(3,i))%x)
            U_RECT_2D%element(i)%SideLength(4)=abs(U_RECT_2D%node(U_RECT_2D%idNode(1,i))%y - U_RECT_2D%node(U_RECT_2D%idNode(4,i))%y)


        end do
                    
        U_RECT_2D%nZones=1
        U_RECT_2D%Element%idZone = 1 ! automatic initialization
        allocate(U_RECT_2D%Zone(U_RECT_2D%nZones),stat=ialloc)
        call AllocChk(ialloc,'U_RECT_2D%Zone array')

        U_RECT_2D%Element%is=0
        
 
        write(TmpSTR,'(a,i8)')    'Total number of nodes         ',U_RECT_2D%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    'Total number of elements      ',U_RECT_2D%nElements
        call Msg(TmpSTR)
        
        if(EnableTecplotOutput) then
            call MeshToTecplot(U_RECT_2D)
        endif

        return
    end subroutine GenerateUniformRectangles
    !----------------------------------------------------------------------
    subroutine GenerateVariableRectangles(FNum,TMPLT)
        implicit none
        integer(i4) :: FNum
        type(mesh) TMPLT


        integer(i4) :: i, j, k
	    integer(i4) :: nn2d, ne2d
        
        real(sp), allocatable :: xi(:)
        real(sp), allocatable :: yi(:)

        TMPLT%name='TMPLT'

        !     generate variable rectangles
        TMPLT%nNodesPerElement=4
        TMPLT%TecplotTyp='fequadrilateral'
        
        !     xl, yl are grid lengths in x- and y-directions
        read(FNum,*) nx
        write(TMPStr,'(a,i9)')      TAB//'Number of X-coordinates ',nx
        call Msg(TMPStr)
        allocate(xi(nx),stat=ialloc)
        call AllocChk(ialloc,'Gen_v_rects xi array')
        xi = 0.0d0 
        read(FNum,*) (xi(i),i=1,nx)

        read(FNum,*) ny
        write(TMPStr,'(a,i9)')      TAB//'Number of Y-coordinates ',ny
        call Msg(TMPStr)
        allocate(yi(ny),stat=ialloc)
        call AllocChk(ialloc,'Gen_v_rects yi array')
        yi = 0.0d0 
        read(FNum,*) (yi(i),i=1,ny)

        !     generate 2D slice first
        TMPLT%nNodes=nx*ny
        allocate(TMPLT%node(TMPLT%nNodes),stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d node arrays')
        TMPLT%node(:)%x = 0 ! automatic initialization
        TMPLT%node(:)%y = 0 ! automatic initialization
        TMPLT%node(:)%z = 0 ! automatic initialization
        
        TMPLT%nElements=(nx-1)*(ny-1)
        allocate(TMPLT%Element(TMPLT%nElements), TMPLT%idNode(TMPLT%nNodesPerElement,TMPLT%nElements), stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d element arrays')
        TMPLT%Element(:)%idZone = 0 ! automatic initialization
        TMPLT%idNode(:,:) = 0 ! automatic initialization
        TMPLT%element%iLayer = 0
        TMPLT%element%x=0.0d0
        TMPLT%element%y=0.0d0
        TMPLT%element%z=0.0d0
        TMPLT%element%Area=0.0d0
        TMPLT%element%xyArea=0.0d0

        !     generate 2D node coordinates
        nn2d=0
        do i=1,ny
            do j=1,nx
                nn2d=nn2d+1
                TMPLT%node(nn2d)%x=xi(j)
                TMPLT%node(nn2d)%y=yi(i)
            end do
        end do

        !     generate 2D rectangular element incidences
        ne2d=0
        do i=1,ny-1
            k=1+(i-1)*nx
            do j=1,nx-1
                ne2d=ne2d+1
                TMPLT%idNode(1,ne2d)=k
                TMPLT%idNode(2,ne2d)=k+1
                TMPLT%idNode(3,ne2d)=k+nx+1
                TMPLT%idNode(4,ne2d)=k+nx
                TMPLT%element(ne2d)%idZone = 1
                TMPLT%element(ne2d)%iLayer = 1
                k=k+1
            end do
        end do

        do i=1,TMPLT%nElements
            TMPLT%element(i)%xyArea=  (TMPLT%node(TMPLT%idNode(2,i))%x - TMPLT%node(TMPLT%idNode(1,i))%x) * &
                                      (TMPLT%node(TMPLT%idNode(3,i))%y - TMPLT%node(TMPLT%idNode(1,i))%y)
            TMPLT%element(i)%x=(TMPLT%node(TMPLT%idNode(2,i))%x + TMPLT%node(TMPLT%idNode(1,i))%x)/2.0d0
            TMPLT%element(i)%y=(TMPLT%node(TMPLT%idNode(3,i))%y + TMPLT%node(TMPLT%idNode(1,i))%y)/2.0d0
            
            TMPLT%element(i)%SideLength(1)=abs(TMPLT%node(TMPLT%idNode(2,i))%x - TMPLT%node(TMPLT%idNode(1,i))%x)
            TMPLT%element(i)%SideLength(2)=abs(TMPLT%node(TMPLT%idNode(3,i))%y - TMPLT%node(TMPLT%idNode(2,i))%y)
            TMPLT%element(i)%SideLength(3)=abs(TMPLT%node(TMPLT%idNode(4,i))%x - TMPLT%node(TMPLT%idNode(3,i))%x)
            TMPLT%element(i)%SideLength(4)=abs(TMPLT%node(TMPLT%idNode(1,i))%y - TMPLT%node(TMPLT%idNode(4,i))%y)


        end do
                    
        TMPLT%nZones=1
        TMPLT%Element%idZone = 1 ! automatic initialization
        allocate(TMPLT%Zone(TMPLT%nZones),stat=ialloc)
        call AllocChk(ialloc,'TMPLT%Zone array')

        !TMPLT%IsDefined=.true.
        
        !allocate(TMPLT%element%is(TMPLT%nElements),stat=ialloc)
        !call AllocChk(ialloc,'TMPLT element%is array')            
        TMPLT%Element%is=0

        call Msg(' ')
        write(TmpSTR,'(a,i8)')    TAB//'Number of nodes         ',TMPLT%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    TAB//'Number of elements      ',TMPLT%nElements
        call Msg(TmpSTR)


        return
    end subroutine GenerateVariableRectangles
    !!----------------------------------------------------------------------
    !subroutine GenerateRectanglesInteractive(FNum,TMPLT)
    !    implicit none
    !    integer(i4) :: FNum
    !    type(mesh) TMPLT
    !
    !    integer(i4) :: i, j, k
	   ! real(dp) :: xmin, xmax, x1, x2, dxstart, xfac, dxmax, xcur
	   ! real(dp) :: ymin, ymax, y1, y2, dystart, yfac, dymax, ycur
    !    logical :: reverse
    !
    !    real(sp), allocatable :: xi(:)
    !    real(sp), allocatable :: yi(:)
    !    TMPLT%name='TMPLT'
    !
    !    !     generate uniform rectangles
    !    TMPLT%nNodesPerElement=4
    !    TMPLT%TecplotTyp='fequadrilateral'
    !
    !
    !    allocate(xi(user_maxnx),yi(user_maxny),stat=ialloc)
    !    call check_alloc(ialloc,'gen_rect_i xi,yi arrays')
    !
	   ! xi(: ) =0.0d0
	   ! yi(: ) =0.0d0
    !
    !
    !    blockel=.true.
    !    kgrid=1
    !
    !    process_instructions: do
    !        read(igrok,'(a40)') instruction
    !        write(ieco,'(a)') ' '
    !        write(ieco,'(a,a)') '      GEN_RECTANGLES:    ',instruction
    !        write(*,'(a,a)')  '      GEN_RECTANGLES:    ',instruction
    !        call lcase(instruction)
    !
    !        ! Overall grid constraints
    !        if(instruction .eq. extents) then
    !            read(igrok,*) xmin,xmax
    !            call update_x(small,xmin)
    !            call update_x(small,xmax)
    !            read(igrok,*) ymin,ymax
    !            call update_y(small,ymin)
    !            call update_y(small,ymax)
    !            write(ieco,*) 'Overall grid extents'
    !            call rfm(ieco,'X',xmin,xmax)
    !            read(igrok,*) dxmax
    !            write(ieco,*) 'Maximum element size in X ',dxmax
    !            write(ieco,*)
    !
    !            ! generate graded x coordinates
    !        else if(instruction .eq. xgrade) then
    !            read(igrok,*) x1,x2,dxstart,xfac,dxmax
    !            write(ieco,*) 'Grade X coordinates from ',x1,' to ',x2
    !            write(ieco,*) 'Start element size ',dxstart
    !            write(ieco,*) 'Stretch factor ',xfac
    !            write(ieco,*) 'Max element size ',dxmax
    !            call update_x(small,x1)
    !            call update_x(small,x2)
    !            if(x1.gt.x2) then
    !                dxstart=-dxstart
    !                dxmax=-dxmax
    !                reverse=.true.
    !            else
    !                reverse=.false.
    !            end if
    !            xcur=x1
    !            91			xcur=xcur+dxstart
    !            if(.not.reverse .and. xcur.lt.x2) then
    !                if((xcur-x2).lt.abs(dxstart*1.5)) then
    !                    call update_x(small,xcur)
    !                    dxstart=min(dxstart*xfac,dxmax)
    !                    goto 91
    !                endif
    !            elseif(reverse .and. xcur.gt.x2) then
    !                if((x2-xcur).lt.abs(dxstart*1.5)) then
    !                    call update_x(small,xcur)
    !                    dxstart=max(dxstart*xfac,dxmax)
    !                    goto 91
    !                endif
    !            end if
    !
    !            !       generate y coordinates
    !        elseif(instruction .eq. ygrade) then
    !            read(igrok,*) y1,y2,dystart,yfac,dymax
    !            write(ieco,*) 'Grade Y coordinates from ',y1,' to ',y2
    !            write(ieco,*) 'Start element size ',dystart
    !            write(ieco,*) 'Stretch factor ',yfac
    !            write(ieco,*) 'Max element size ',dymax
    !            call update_y(small,y1)
    !            call update_y(small,y2)
    !            if(y1.gt.y2) then
    !                dystart=-dystart
    !                dymax=-dymax
    !                reverse=.true.
    !            else
    !                reverse=.false.
    !            end if
    !            ycur=y1
    !            92          ycur=ycur+dystart
    !            if(.not.reverse .and. ycur.lt.y2) then
    !                if((ycur-y2).lt.abs(dystart*1.5)) then
    !                    call update_y(small,ycur)
    !                    dystart=min(dystart*yfac,dymax)
    !                    goto 92
    !                endif
    !            elseif(reverse .and. ycur.gt.y2) then
    !                if((y2-ycur).lt.abs(dystart*1.5)) then
    !                    call update_y(small,ycur)
    !                    dystart=max(dystart*yfac,dymax)
    !                    goto 92
    !                endif
    !            end if
    !
    !
    !        elseif(instruction(1:3) .eq. end_cmd) then
			 !   call end_instruction('GENERATE RECTANGLES INTERACTIVE')
    !            exit process_instructions
    !        else
			 !   call input_error('GENERATE RECTANGLES INTERACTIVE: Unrecognized instruction')
    !        endif
    !    end do process_instructions
    !
    !
    !    call rqcksrt(nx,xi,user_maxnx)
    !    write(ieco,*) 'NX ',nx
    !    write(ieco,'(5g25.13)') (xi(i),i=1,nx)
    !
    !    call rqcksrt(ny,yi,user_maxny)
    !    write(ieco,*) 'NY ',ny
    !    write(ieco,'(5g25.13)') (yi(i),i=1,ny)
    !
    !
    !    ! generate 2D slice first
    !    nn2d=nx*ny
    !    ne2d=(nx-1)*(ny-1)
    !    allocate(x2d(nn2d),y2d(nn2d),el_area2d(ne2d),in2d(ne2d,4),stat=ialloc)
    !    call check_alloc(ialloc,'Gen_rectangles_interactive 2d slice arrays')
    !    x2d = 0 ! automatic initialization
    !    y2d = 0 ! automatic initialization
    !    el_area2d = 0 ! automatic initialization
    !    in2d = 0 ! automatic initialization
    !
    !
    !    !     generate 2D node coordinates
    !    nn2d=0
    !    do i=1,ny
    !        do j=1,nx
    !            nn2d=nn2d+1
    !            x2d(nn2d)=xi(j)
    !            y2d(nn2d)=yi(i)
    !        end do
    !    end do
    !
    !    !     generate 2D rectangular element incidences
    !    ne2d=0
    !    do i=1,ny-1
    !        k=1+(i-1)*nx
    !        do j=1,nx-1
    !            ne2d=ne2d+1
    !            in2d(ne2d,1)=k
    !            in2d(ne2d,2)=k+1
    !            in2d(ne2d,3)=k+nx+1
    !            in2d(ne2d,4)=k+nx
    !            !rt aug15.2001
    !            el_area2d(ne2d) = 1
    !            k=k+1
    !        end do
    !    end do
    !
    !
    !    return
    !end subroutine GenerateRectanglesInteractive

    !----------------------------------------------------------------------
    subroutine check_seg(i1,i2) 
	    implicit none
     
        integer(i4) :: j, k, i1, i2
	    logical :: seg
	 
        seg=.true. 
        do j=1,nseg 
		    if (i1.eq.seg_node(j,2) .and. i2.eq.seg_node(j,1) .or. i1.eq.seg_node(j,1) .and. i2.eq.seg_node(j,2)) then
			    seg=.false. 
			    do  k=j,nseg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
				    seg_node(k,1)=seg_node(k+1,1) 
				    seg_node(k,2)=seg_node(k+1,2) 
			    end do 
			    nseg=nseg-1 
			    exit
		    endif 
	    end do

        if (seg) then 
            call new_segment(i1,i2) 
        endif 

    end subroutine check_seg                                                            
    
    !----------------------------------------------------------------------
    subroutine new_segment(n1,n2)
	    implicit none

	    integer(i4) :: n1, n2

	    nseg=nseg+1

	    seg_node(nseg,1)=n1
	    seg_node(nseg,2)=n2

    end subroutine new_segment

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
        character(MAX_INST) :: bilinear_function_elevation_cmd=   'elevation from bilinear function in xy'
        !character(MAX_INST) :: sine_function_elevation_cmd	=   'elevation from sine function in xy'
        !character(MAX_INST) :: cosine_function_elevation_cmd	=   'elevation from cosine function in xy'
        
        integer(i4) :: FNumMUT
        type (mesh)  TMPLT
        

        integer(i4) :: j,k
        logical :: zone_by_template
        character(120) :: basefile

        real(dp), allocatable :: sub_thick(:)
        !real(dp) :: zelev
        real(dp) :: z_added
        !real(dp) :: zelev_proportional
        real(dp) :: sub_thick_frac
	    real(dp) :: tot_thick
        real(dp) :: base_offset

	    integer(i4) :: node_fixed
	    integer(i4) :: node3d
	    integer(i4) :: nel3d

        logical :: minimum_layer_thickness
        logical :: offset_base
	    logical :: proportional_sublayering

	    integer(i4) :: nz_temp=0	

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
			    write(TmpSTR,'('//FMT_R8//',a)') z_added,'     '//TRIM(UnitsOfLength)
			    call Msg(TAB//TAB//'Enforce minimum layer thickness of '//trim(TmpSTR))

            elseif(index(instruction,offset_base_cmd) /=0) then
			    offset_base = .true.
			    read(FNumMUT,*) base_offset
			    write(TmpSTR,'('//FMT_R8//',a)') base_offset,'     '//TRIM(UnitsOfLength)
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
			        write(TmpSTR,'(i4,2('//FMT_R8//'))') j,sub_thick(j)*tot_thick,sub_thick(j)
                    call Msg(TAB//TAB//trim(TmpSTR))
                end do

            elseif(index(instruction,constant_elevation_cmd) /=0) then
			    read(FNumMUT,*) base_elev(1)
			    write(TmpSTR,'('//FMT_R8//',a)') base_elev(1),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//TAB//'Layer base elevation '//TRIM(TmpSTR))
			    do j=2,TMPLT%nNodes
				    base_elev(j)=base_elev(1)
			    end do


            elseif(index(instruction,gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg(TAB//TAB//'Base elevation from '//trim(basefile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(basefile,base_elev,TMPLT%nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg(TAB//TAB//'Base elevation from '//trim(basefile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call list_file_elevation(basefile,base_elev,TMPLT%nNodes)
                
                

            elseif(index(instruction,xz_pairs_elevation_cmd) /=0) then
                call xz_pairs_elevation(FNumMUT,base_elev,TMPLT)
            
       !     elseif(index(instruction,raster_file_elevation_cmd) /=0) then
			    !read(FNumMUT,'(a)') topfile
			    !write(ieco,*) 'Layer top elevation from gb file  ',topfile
       !         call read_raster_to_mesh_elev(topfile,top_elev)
       !     
            elseif(index(instruction,bilinear_function_elevation_cmd) /=0) then
                call bilinear_function_in_xy(FNumMUT,base_elev,TMPLT)
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
            do j=1,TMPLT%nNodes
                base_elev(j)=base_elev(j)+base_offset
		    end do

	    end if

	    sub_thick_frac=0.0
        do k=1,nsublayer(nlayers)
            if(proportional_sublayering) sub_thick_frac=sub_thick_frac+sub_thick(k)

           !         new nodes
            node_fixed=0
            do j=1,TMPLT%nNodes
                if(base_elev(j) >= top_elev(j)) then
                    !rt-jun01
                    if(.not. minimum_layer_thickness) then
                        write(ieco,*) ' Error: Base of layer ',nlayers,' >= top'
                        write(ieco,*) ' At x: ',TMPLT%node(j)%x,' y: ',TMPLT%node(j)%y
                        write(ieco,*) ' Base elev= ', base_elev(j),' top elev= ',top_elev(j)

                        write(*,*) ' Error: Base of layer ',nlayers,' >= top'
                        write(*,*) ' At x: ',TMPLT%node(j)%x,' y: ',TMPLT%node(j)%y
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
                node3d=nsheet*TMPLT%nNodes+j
                x(node3d)=TMPLT%node(j)%x
                y(node3d)=TMPLT%node(j)%y
                if(proportional_sublayering) then
                    z(node3d)=zelev_proportional(top_elev(j),base_elev(j),sub_thick_frac,1.0d0)
                else
                    z(node3d)=zelev(top_elev(j),base_elev(j),k,nsublayer(nlayers))
                end if
            end do

            !         new elements
            do j=1,TMPLT%nElements
                nel3d=(nsheet-1)*TMPLT%nElements+j
                ilyr(nel3d)=nsheet
                if(nln==6) then ! prisms from triangles
                    in(1,nel3d)=TMPLT%idNode(1,j)+nsheet*TMPLT%nNodes
                    in(2,nel3d)=TMPLT%idNode(2,j)+nsheet*TMPLT%nNodes
                    in(3,nel3d)=TMPLT%idNode(3,j)+nsheet*TMPLT%nNodes
                    in(4,nel3d)=TMPLT%idNode(1,j)+(nsheet-1)*TMPLT%nNodes
                    in(5,nel3d)=TMPLT%idNode(2,j)+(nsheet-1)*TMPLT%nNodes
                    in(6,nel3d)=TMPLT%idNode(3,j)+(nsheet-1)*TMPLT%nNodes
                    if(zone_by_template) then
                        iprp(nel3d)=TMPLT%element(j)%idZone
                    else
                        iprp(nel3d)=nlayers
                    end if
                else ! blocks from rectangles, not currently supported
                    in(1,nel3d)=TMPLT%idNode(1,j)+nsheet*TMPLT%nNodes
                    in(2,nel3d)=TMPLT%idNode(2,j)+nsheet*TMPLT%nNodes
                    in(3,nel3d)=TMPLT%idNode(3,j)+nsheet*TMPLT%nNodes
                    in(4,nel3d)=TMPLT%idNode(4,j)+nsheet*TMPLT%nNodes
				    in(5,nel3d)=TMPLT%idNode(1,j)+(nsheet-1)*TMPLT%nNodes
                    in(6,nel3d)=TMPLT%idNode(2,j)+(nsheet-1)*TMPLT%nNodes
                    in(7,nel3d)=TMPLT%idNode(3,j)+(nsheet-1)*TMPLT%nNodes
                    in(8,nel3d)=TMPLT%idNode(4,j)+(nsheet-1)*TMPLT%nNodes
        
                    if(zone_by_template) then
                        iprp(nel3d)=TMPLT%element(j)%idZone
                    else
                        iprp(nel3d)=nlayers
                    end if
    
                end if
            end do


            zi(nsheet)=z((nsheet-1)*TMPLT%nNodes+1)
            nsheet=nsheet+1
            !call user_size_check(nsheet+1,user_nz,user_nz_str)
        end do

        if(minimum_layer_thickness) then
            write(ieco,*) ' Number of nodes for layer ',nlayers
            write(ieco,*) '  that have had their base elevation modified'
            write(ieco,*) ' by subtracting ',z_added,' is equal to',node_fixed
        end if

	    ! initialize base elevation for (potential) next layer
        do j=1,TMPLT%nNodes
            top_elev(j)=base_elev(j)
        end do

        if(allocated(sub_thick)) deallocate(sub_thick)
        

    end subroutine new_layer

    
    !----------------------------------------------------------------------
    subroutine read_gb_nprop(fname,nprop,maxnnp)
        implicit none

        integer(i4) :: i
        integer(i4) :: maxnnp

        character*(*) fname
        character*11 file_type
        real(dp) :: nprop(maxnnp)
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
        character(MAX_INST) :: bilinear_function_elevation_cmd=   'elevation from bilinear function in xy'
        !character(MAX_INST) :: sine_function_elevation_cmd	=   'elevation from sine function in xy'
        !character(MAX_INST) :: cosine_function_elevation_cmd	=   'elevation from cosine function in xy'
        
        integer(i4) :: FNumMUT
        type (mesh) TMPLT

        integer(i4) :: j
        character(120) :: topfile
        real(dp) :: top_offset
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
                write(TmpSTR,'(2('//FMT_R8//'),a)') top_elev(1),'     '//UnitsOfLength
                call Msg(TAB//trim(TmpSTR))
                do j=2,TMPLT%nNodes
	                top_elev(j)=top_elev(1)
                end do

            elseif(index(instruction, offset_top_cmd) /=0) then
                offset_top = .true.
                read(FNumMUT,*) top_offset
			    write(TmpSTR,'(2('//FMT_R8//'))') top_offset
                call Msg(TAB//trim(TmpSTR))

            elseif(index(instruction, gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg(TAB//TAB//'Top elevation from '//trim(topfile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(topfile,top_elev,TMPLT%nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg(TAB//TAB//'Top elevation from '//trim(topfile))
                call Msg(TAB//TAB//'Assumed units of length are '//TRIM(UnitsOfLength))
                call list_file_elevation(topfile,top_elev,TMPLT%nNodes)
                
            elseif(index(instruction, xz_pairs_elevation_cmd) /=0) then
                call xz_pairs_elevation(FNumMUT,top_elev,TMPLT)
                
       !     elseif(instruction .eq. raster_file_elevation_cmd) /=0) then
			    !read(FNumMUT,'(a)') topfile
			    !write(ieco,*) 'System top from file ',topfile
       !         call read_raster_to_mesh_elev(topfile,top_elev)

            elseif(index(instruction, bilinear_function_elevation_cmd) /=0) then
                call bilinear_function_in_xy(FNumMUT,top_elev,TMPLT)
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
            do j=1,TMPLT%nNodes
                top_elev(j)=top_elev(j)+top_offset
		    end do

	    end if
    end subroutine top_elevation
    
    !----------------------------------------------------------------------
    subroutine xyzFromListFile(FNum,xi,yi,zi,nPoints)
        implicit none
        integer(i4) :: FNum
        
        integer(i4) :: nSizeInit=2
        real(sp), allocatable :: xi(:), yi(:), zi(:)  ! xyz coordinate list defining CLN to be read
        real(sp), allocatable :: xiTMP(:), yiTMP(:), ziTMP(:)  ! temporary xyz arrays
        integer(i4) :: nPoints  ! number of points in list

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
                
                write(TmpSTR,'(i8,2x,3('//FMT_R8//'),a)') nPoints,xi(nPoints),yi(nPoints),zi(nPoints),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//trim(TmpSTR))

		    endif
        end do
        
       
        continue

    end subroutine xyzFromListFile

    !----------------------------------------------------------------------
    subroutine bilinear_function_in_xy(FNumMUT,nprop,TMPLT)
        implicit none
        
        integer(i4) :: FNumMUT
        type (mesh) TMPLT

        integer(i4) :: i
	    real(dp) :: xf2d, xt2d, yf2d, yt2d, a1, a2, a3, a4, a5

        real(dp) :: nprop(TMPLT%nNodes)

	    read(FNumMUT,*) xf2d, xt2d, yf2d, yt2d
	    call Msg(TAB//'Bilinear function for the following range:')
	    write(TMPSTR,'(a,'//FMT_R8//',a,'//FMT_R8//',a)') 'X from ',xf2d,' to ',xt2d,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a,'//FMT_R8//',a)') 'Y from ',yf2d,' to ',yt2d,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    call Msg(TAB//'z = z1 + slope_x*(x-xfrom) + curve_x*(x-xfrom)**2 + slope_y*(y-yfrom) + curve_y*(y-yfrom)**2 ')
	    read(FNumMUT,*) a1,a2,a3,a4,a5
	    write(TMPSTR,'(a,'//FMT_R8//',a)') 'Where:     z1 = ',a1,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      slope_x = ',a2,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      curve_x = ',a3,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      slope_y = ',a4,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      curve_y = ',a5,'     '//TRIM(UnitsOfLength)
        call Msg(TAB//trim(TmpSTR))
        do i=1,TMPLT%nNodes
		    if( TMPLT%node(i)%x.ge.xf2d .and. TMPLT%node(i)%x.le.xt2d .and. TMPLT%node(i)%y.ge.yf2d .and. TMPLT%node(i)%y.le.yt2d) then
			    nprop(i)=a1 + a2*(TMPLT%node(i)%x-xf2d) + a3*(TMPLT%node(i)%x-xf2d)**2 + a4*(TMPLT%node(i)%y-yf2d) + a5*(TMPLT%node(i)%y-yf2d)**2
		    end if
	    end do

    end subroutine bilinear_function_in_xy

    !----------------------------------------------------------------------
    subroutine xz_pairs_elevation(FNum,nprop,TMPLT)
        implicit none
        integer(i4) :: FNum
        type(mesh) TMPLT

        integer(i4) :: i, j
	    integer(i4) :: npairs
        real(dp) :: nprop(TMPLT%nNodes)
	    real(dp) :: t
                
        character(256) :: instruction

	    real(dp) :: xp(1000)
	    real(dp) :: zp(1000)
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
                
                write(TmpSTR,'(i8,2x,2('//FMT_R8//'),a)') npairs,xp(npairs),zp(npairs),'     '//TRIM(UnitsOfLength)
                call Msg(TAB//trim(TmpSTR))

		    endif
	    end do read_xz_pairs


        do i=1,TMPLT%nNodes
		    do j=1,npairs-1
			    if(TMPLT%node(i)%x >= xp(j) .and. TMPLT%node(i)%x <= xp(j+1)) then  ! interpolate
	                t=(TMPLT%node(i)%x-xp(j))/(xp(j+1)-xp(j))
				    nprop(i)=(1.0-t)*zp(j)+t*zp(j+1)
			    end if
		    end do
        end do



    end subroutine xz_pairs_elevation
    !----------------------------------------------------------------------
    function zelev(top,base,k,nk)
        implicit none

        real(dp) :: zelev
	    integer(i4) :: k, nk
	    real(dp) :: base, top

        zelev=top-(top-base)*k/(nk)

    end function zelev

    !----------------------------------------------------------------------
    function zelev_proportional(top,base,rsub,rsubtot)
        implicit none

	    real(dp) :: zelev_proportional
	
	    real(dp) :: base, top, rsub, rsubtot

        zelev_proportional=top-(top-base)*rsub/(rsubtot)

        return
    end function zelev_proportional

end Module MeshGen
    
module error_param
	integer :: ierr
end module error_param
 
module gb
	use GeneralRoutines
	use error_param

	implicit none
    
	logical		:: plan_view		! true for isotropic scaling in x and y

	integer(i4)		:: nn_in=1000			! number of nodes
	real(dp),allocatable	:: x_in(:)		! x coordinates
	real(dp),allocatable	:: y_in(:)		! y coordinates
	
	integer(i4) :: area_in						! number of areas
	integer(i4)	:: onbn_in						! number of outer boundry nodes  
	integer(i4)	:: nbn_in_max						! maximum number of outer boundry nodes  
	integer(i4),allocatable		:: obn_in(:)	! outer boundary node list
	integer(i4),allocatable		:: nbn_in(:)	! number of boundry nodes in area 
	integer(i4),allocatable		:: bn_in(:,:)   ! boundary node list
	real(dp),allocatable	:: elength_in(:)			! area element length x
	real(dp),allocatable	:: y_elength_in(:)			! area element length y
	real(dp),allocatable	:: stretch_factor_in(:)		! local element stretch factor
	integer(i4), allocatable	:: ndrop_rate_in(:)			! local node drop rate
	logical,allocatable		:: hole_in(:)	! true if area not to be filled with elements

	integer(i4)		:: nwells
	real(dp)	:: well_esize			! target elenemt size at well
	real(dp),allocatable	:: xw(:)		! x coordinates
	real(dp),allocatable	:: yw(:)		! y coordinates

	real(dp) :: segl_user

	integer(i4) :: ne_cur	! element

	!------------------------------------------------------------------------------------------
	integer(i4)		:: nn			! number of nodes
	real(dp),allocatable	:: x(:)		! x coordinates
	real(dp),allocatable	:: y(:)		! y coordinates
	
	integer(i4) :: area						! number of areas
	integer(i4)	:: onbn						! number of outer boundry nodes  
	integer(i4),allocatable		:: obn(:)	! outer boundary node list
	integer(i4),allocatable		:: nbn(:)	! number of boundry nodes in area 
	integer(i4),allocatable		:: bn(:,:)   ! boundary node list
	real(dp), allocatable	:: elength(:)
	real(dp), allocatable	:: y_elength(:)
	real(dp), allocatable	:: stretch_factor(:)
	integer(i4), allocatable	:: ndrop_rate(:) 
	logical,allocatable		:: hole(:)	! true if area not to be filled with elements



	integer(i4)		:: ne				! number of elements in mesh
	integer(i4),allocatable		:: in(:,:)		! element node lists
	integer(i4),allocatable		:: el_area(:)	! element zone (subarea) numbers

	integer(i4), allocatable :: node(:) ! node bit set array 
	integer(i4), allocatable :: elem(:) ! element bit set array 


	character*60 :: log_msg

	real(dp) :: xmin					! grid extents
	real(dp) :: xmax 
	real(dp) :: ymin 
	real(dp) :: ymax 

	!-----------------------------------------
	! current size of arrays
	integer(i4) :: nn_cur	! node

	!--------------------------------------------------------
	! area boundary arrays
	integer(i4) :: nb_cur
	integer(i4) :: na_cur

	integer(i4) :: no_cur

	integer(i4) :: nl_cur
	integer(i4), allocatable	:: lbn(:) ! local boundary node list for grid generation

	logical, allocatable :: plot_area(:)

	!--------------------------------------------------------
	! cut arrays 
	integer(i4) :: ncut
	integer(i4) :: n_int
	integer(i4) :: nc_cur
	integer(i4),allocatable :: nsg(:)
	integer(i4),allocatable :: nct(:)
	integer(i4),allocatable :: al(:)
	integer(i4),allocatable :: ae(:)
	integer(i4),allocatable :: usne(:)
	integer(i4),allocatable :: usnl(:)
	real*8,allocatable 	:: xcut(:)
	real*8,allocatable 	:: ycut(:)
	real*8,allocatable 	:: xi(:)
	real*8,allocatable 	:: yi(:)
	real,allocatable :: rc(:)
	real,allocatable :: rs(:)


	! common /segs/ 
	integer(i4) :: nseg
	integer(i4) :: ns_cur
	integer(i4), allocatable :: seg_node(:,:)
	integer(i4), allocatable :: seg_el(:,:)
	integer(i4), allocatable :: seg_area(:,:)
	integer(i4), allocatable :: seg_is(:)


	integer(i4), parameter :: gb_chosen		=  0
	integer(i4), parameter :: b_1st_type	=  1 
	integer(i4), parameter :: b_2nd_type	=  2
	integer(i4), parameter :: b_3rd_type	=  3
	integer(i4), parameter :: b_td_type		=  4
	integer(i4), parameter :: b_output		=  5
	integer(i4), parameter :: out_bndy		= 10
	integer(i4), parameter :: any_bndy		= 11
	integer(i4), parameter :: gb_well			= 12
	integer(i4), parameter :: vec_plotted	= 13

	integer(i4), parameter		:: maxwelln=1000
	!integer(i4), allocatable	:: well_nde(:)


	! local grid gen parameters
	real :: eleng, y_eleng,stretch
	integer(i4) :: ndrop
	integer(i4) :: ncount_grade

	! kriging
	logical :: do_drift
	logical :: do_orig
	integer(i4),parameter :: maxnsvbin=100
	integer(i4) :: npmin
	integer(i4) :: npmax
	integer(i4) :: kdrift
	real(dp) :: xsd
	real(dp) :: ysd
	integer(i4),parameter :: maxmat=500        ! maximum size for krig array


	! working variables and arrays for grid generation
	integer(i4) :: da1,da2,db1,db2,sn,usn,usn2,dsn,dsn2
	integer(i4) :: iad(-2:2)
	logical :: stuck
	integer(i4) :: lnbn
	integer(i4) :: nd

	logical :: do_grade=.false.  ! hardwire for now


	integer(i4), parameter                     :: maxnicon=40 ! node interconnections
	integer(i4), allocatable :: nicon(:)       ! # of connections
	integer(i4), allocatable :: icon(:,:)        ! connection list
	integer(i4), allocatable :: eicon(:,:,:)       ! element connection list
	logical recalc_nicon
    
    contains
    

	!----------------------------------------------------------------------
	subroutine GridBuilder(ierror)
		implicit none

		integer(i4) :: ierror
		integer(i4) :: i, k, nes

		ierr=0

		! call Msg('Revision 15: Fast removal of short segments')
	
		!open(99,file='test.txt',status='unknown')

		! initial arrays for nodes..
		nn_cur=1
		call initialize_node_arrays(nn_in)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		! initial arrays for elements... 
		ne_cur=1
		call initialize_element_arrays(1000)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		! initial arrays for outer boundary nodes... 
		no_cur=1
		call initialize_outer_boundary_array(onbn_in)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		! initial arrays for segments... 
		ns_cur=1
		call initialize_segment_arrays(1000)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		! initial arrays for boundary nodes... 
		nb_cur=1
		na_cur=1
		call initialize_boundary_arrays(area_in,nbn_in_max)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		! initial arrays for cuts... 
		nc_cur=1
		call initialize_cut_arrays(1000)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif


		! use caller boundary data to form dll outer boundary data (import_ob_batch)
		call process_gen_file
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		call fix_short_segs_batch
		if(ierr /= 0) then
			ierror=ierr
			return
		endif
	
		call gen_uni_bnodes     ! generate uniformly spaced boundary nodes
		if(ierr /= 0) then
			ierror=ierr
			return
		endif


		call Msg('Generating mesh...')
		ne=0
		do k=1,area
			if (.not. hole(k)) then
				nes=ne+1
				call gen_convex(k) ! fill an area with elements
				if(ierr /= 0) then
					ierror=ierr
					return
				endif
				do i=nes,ne
					el_area(i)=k
				end do
			endif
		end do

		call grid_limits

		call set_ob_ab

		call Msg('Relaxing grid...')

		recalc_nicon=.true.
		call relax_grid



		if(ierr /= 0) then
			ierror=ierr
			return
		endif


		if(nwells > 0) then
			call Msg('Adding wells...')
			call make_wells
			if(ierr /= 0) then
				ierror=ierr
				return
			endif

			call refine_wells
			if(ierr /= 0) then
				ierror=ierr
				return
			endif
		endif


		if(allocated(nicon)) deallocate(nicon,icon,eicon)


	end subroutine GridBuilder


	!----------------------------------------------------------------------
	subroutine find_node(x1,y1,p_node)
		implicit none

		integer(i4) :: i

		integer(i4) p_node
		real(dp) :: x1, y1, dist_min, f1

		i=1
		dist_min=1.0e20
		97    continue
			!if(plan_view)then
				f1=sqrt((x1-x(i))**2+((y1-y(i)))**2)
			!else
			!	f1=sqrt((x1-x(i))**2+((y1-y(i))*xy_ratio)**2)
			!endif
			if(f1.lt.dist_min) then
				p_node=i
				dist_min=f1
			endif
			i=i+1
		if (i.LE.nn) goto 97
	end subroutine find_node
	!-----------------------------------------------------------------------
	subroutine grid_limits
		implicit none

		integer(i4) :: i

		xmin=1.e20
		xmax=-1.e20
		ymin=1.e20
		ymax=-1.e20
		do  i=1,nn
			if(x(i).lt.xmin) xmin=x(i)
			if(x(i).gt.xmax) xmax=x(i)
			if(y(i).lt.ymin) ymin=y(i)
			if(y(i).gt.ymax) ymax=y(i)
		end do
	end subroutine grid_limits
	!----------------------------------------------------------------------
	 subroutine set_ob_ab
		implicit none

		integer(i4) :: i, j

		do  j=1,onbn
			call set(node(obn(j)),out_bndy)
			call set(node(obn(j)),b_2nd_type)
		end do

		do i=1,area
			do  j=1,nbn(i)
				call set(node(bn(i,j)),any_bndy)
			end do
		end do

	end subroutine set_ob_ab

	!*** Grid relaxation routines ***
	!----------------------------------------------------------------------
	subroutine relax_grid
		implicit none 
    
		integer(i4) :: i, j
		real(dp) :: maxdiff, tol, xm, ym, diff

		call node_connections
		if(ierr /= 0) return

		tol=(xmax-xmin)/1.e-5
		10    continue
			maxdiff=0.0
			do i=1,nn
				if (.not. bcheck(node(i),any_bndy) .and. .not. bcheck(node(i),well)) then
					xm=0.0d0
					ym=0.0d0
					do j=1,nicon(i)
						xm=xm+x(icon(i,j))
						ym=ym+y(icon(i,j))
					end do
					diff=sqrt((x(i)-xm/nicon(i))**2 + (y(i)-ym/nicon(i))**2)
					maxdiff=max(diff,maxdiff)
					x(i)=xm/nicon(i)
					y(i)=ym/nicon(i)
				endif
			end do
		if (maxdiff.gt.tol) goto 10

	end subroutine relax_grid
	!----------------------------------------------------------------------
	subroutine node_connections

		integer(i4) :: i, j, k, l, node1, node2, i1, i2, ic, n1, n2

		integer(i4), allocatable :: new_order(:),icon_w(:),eicon_w(:,:)
		real(dp), allocatable :: angle(:)

		if(.not. recalc_nicon) return

		if(allocated(nicon)) deallocate(nicon,icon,eicon)
		allocate(nicon(nn),icon(nn,maxnicon),eicon(nn,maxnicon,2),stat=ialloc)
		call check_alloc(ialloc,'Node connection arrays')
		if(ierr /= 0) return


		allocate(new_order(maxnicon),icon_w(maxnicon),eicon_w(maxnicon,2),angle(maxnicon), stat=ialloc)
		call check_alloc(ialloc,'Node connection working arrays')
		if(ierr /= 0) return

		!     Clear connection data
		nicon(:)=0
		icon(:,:)=0
		eicon(:,:,:)=0

		do l=1,ne
			node1=in(l,1)
			node2=in(l,2)
			call make_list(l,node1,node2)
			node2=in(l,3)
			call make_list(l,node1,node2)
			node1=in(l,2)
			node2=in(l,1)
			call make_list(l,node1,node2)
			node2=in(l,3)
			call make_list(l,node1,node2)
			node1=in(l,3)
			node2=in(l,1)
			call make_list(l,node1,node2)
			node2=in(l,2)
			call make_list(l,node1,node2)
		end do

		! Compute angles and sort
		do i=1,nn
			if(nicon(i).gt.2) then
				i1=icon(i,1)
				!         Change i1 to downstream boundary node if in list
				do j=1,nicon(i)
					ic=icon(i,j)
					if(bcheck(node(ic),out_bndy)) then
						k=1
						30 continue  ! loop over outer boundary
							n1=obn(k)
							n2=obn(k+1)
							if(n1.eq.i .and. n2.eq.ic) then
								i1=ic
								goto 110
							endif
							k=k+1
						if(k.lt.onbn) goto 30
					endif
				end do
				110       continue

				do j=1,nicon(i)
					i2=icon(i,j)
					if(i2.eq.i1) then
						angle(j)=0.0
					else
						call int_angle(x(i2),y(i2),x(i),y(i),x(i1),y(i1),angle(j))
					endif
				end do
				call indexx(nicon(i),angle,new_order,maxnicon)

				! Copy orig sorted to work array
				do j=1,nicon(i)
					icon_w(j)=icon(i,new_order(j))
					eicon_w(j,1)=eicon(i,new_order(j),1)
					eicon_w(j,2)=eicon(i,new_order(j),2)
				end do

				! Copy work to orig
				do j=1,nicon(i)
					icon(i,j)=icon_w(j)
					eicon(i,j,1)=eicon_w(j,1)
					eicon(i,j,2)=eicon_w(j,2)
				end do
			endif

		end do

		recalc_nicon=.false.

		deallocate(new_order,icon_w,eicon_w,angle)

	end subroutine node_connections
	!----------------------------------------------------------------------
	subroutine make_list(nel,node1,node2)
		implicit none

		integer(i4) :: j, node1, node2, nel 

		logical lsted

		lsted=.false.
		if (nicon(node1).gt.0) then
			do j=1,nicon(node1)
				if (node2.eq.icon(node1,j)) then
					lsted=.true.
					if(eicon(node1,j,1) /= nel) then
						eicon(node1,j,2)=nel
					endif
				endif
			end do
		endif
		if (.not. lsted) then
			nicon(node1)=nicon(node1)+1
			icon(node1,nicon(node1))=node2
			eicon(node1,nicon(node1),1)=nel
		endif

	end subroutine make_list
	!----------------------------------------------------------------------
	subroutine indexx(n,arrin,indx,np)
		implicit none

		integer(i4) :: i, j, l, n, np, ir, indxt
		real(dp) :: arrin(np), q
		integer(i4) indx(np)

		do j=1,n
			indx(j)=j
		end do

		l=n/2+1
		ir=n
		10    continue
			if(l.gt.1)then
				l=l-1
				indxt=indx(l)
				q=arrin(indxt)
			else
				indxt=indx(ir)
				q=arrin(indxt)
				indx(ir)=indx(1)
				ir=ir-1
				if(ir.eq.1)then
					indx(1)=indxt
					goto 30
				endif
			endif
			i=l
			j=l+l
			20 if(j.le.ir)then
				if(j.lt.ir)then
					if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
				endif
				if(q.lt.arrin(indx(j)))then
					indx(i)=indx(j)
					i=j
					j=j+j
				else
					j=ir+1
				endif
				go to 20
			endif
			indx(i)=indxt
		go to 10
		30    return
	end subroutine indexx

	!*** Well positioning and refinement routines ***
	!----------------------------------------------------------------------
	subroutine make_wells
		implicit none

		integer(i4) :: i, nde


		call node_connections
		if(ierr /= 0) return

		do i=1,nwells      ! move and fix well nodes
			call find_node(xw(i),yw(i),nde)
			if(.not. bcheck(node(nde),well)) then
				x(nde)=xw(i)
				y(nde)=yw(i)
				call set(node(nde),well)
			endif
		end do

		call relax_grid
		if(ierr /= 0) return

	end subroutine make_wells
	!----------------------------------------------------------------------
	subroutine refine_wells
		implicit none

		integer(i4) :: i, j, l, nde, nn_old

		logical :: stop_refining
		real(dp) :: spacing

		do i=1,nwells  ! fix well node neighbours
			call find_node(xw(i),yw(i),nde)
			do j=1,nicon(nde)
				call set(node(icon(nde,j)),well)
			end do
		end do

		nn_old=nn

		refine_loop: do

			stop_refining=.true.

			do l=1,ne   ! no elements chosen
				call clear(elem(l),chosen)
			end do


			do i=1,nwells
				call find_node(xw(i),yw(i),nde)
				call node_spacing(nde,spacing)
				if(ierr /= 0) return
				if(spacing > well_esize) then
					call choose_e_connected(nde)
					stop_refining=.false.
				endif
			end do

			if(stop_refining) exit refine_loop

			call refine_chosen
			if(ierr /= 0) return

		end do refine_loop

		do i=nn_old+1,nn  ! fix new nodes too
			call set(node(i),well)
		end do

	end subroutine refine_wells
	!----------------------------------------------------------------------
	subroutine choose_e_connected(nde)
		implicit none

		integer(i4) :: l, nde


		do l=1,ne
			if(in(l,1) == nde .or. in(l,2) == nde .or. in(l,3) == nde)      then
				call set(elem(l),chosen)
			endif
		end do

	end subroutine choose_e_connected
	!----------------------------------------------------------------------
	subroutine node_spacing(i,spacing)
		implicit none

		integer(i4) :: i, j

		real(dp) :: spacing, dist

		call node_connections
		if(ierr /= 0) return

		spacing=0.0
		do j=1,nicon(i)
			dist=sqrt((x(i)-x(icon(i,j)))**2 + (y(i)-y(icon(i,j)))**2)
			spacing=spacing+dist
		end do

		spacing=spacing/nicon(i)

	end subroutine node_spacing


	!----------------------------------------------------------------------
	subroutine check_alloc(ialloc,string)
		use error_param
		implicit none
    
		integer(i4) :: ialloc
		character*(*) string

		if(ialloc.ne.0) then
			ierr=1
		endif

	end subroutine check_alloc


	!-----------------------------------------------------------------------
	!*** Mesh refinement routines ***
	!-----------------------------------------------------------------------
	subroutine refine_chosen
		implicit none

		integer(i4) :: l, nil, nnn, nne

		integer(i4),allocatable :: nnl(:)
	
		allocate(nnl(4*nn), stat=ialloc)
		call check_alloc(ialloc,'refine_chosen work array')
		if(ierr /= 0 ) return

		nnl(:)=0

		nil=0
		nnn=nn
		nne=ne
		do l=1,ne
			if (bcheck(elem(l),chosen)) then
				call refine_element(l,.true.,nil,nnn,nne,nnl)
				if(ierr /= 0) goto 1000
			endif
		end do
		if (nnn.gt.nn) then
			15      sn=nnn
				do  l=1,ne
					if (.not. bcheck(elem(l),chosen)) then
						call check_two(l,nil,nnn,nne,nnl)
						if(ierr /= 0) goto 1000
					endif
				end do
			if(nnn.gt.sn) goto 15
			do  l=1,ne
				if (.not. bcheck(elem(l),chosen)) then
					call check_one(l,nil,nne,nnl)
					if(ierr /= 0) goto 1000
				endif
			end do
		endif
		nn=nnn
		ne=nne
		!call calc_bandwidth
		!recalc_segs=.true.
		recalc_nicon=.true.

		1000 deallocate(nnl)

	end subroutine refine_chosen
	!----------------------------------------------------------------------
	subroutine refine_element(l,set_chosen,nil,nnn,nne,nnl)
		! check each side to see if node already exists
		implicit none

		integer(i4) :: l, i1, i2, i3, n1, n2, n3, nnn, nil, nne 

		integer(i4) :: nnl(4*nn)
		integer(i4) :: n_temp, e_temp
		logical set_chosen

		i1=in(l,1)
		i2=in(l,2)
		i3=in(l,3)
		call check_exist(i1,i2,n1,nnn,nil,nnl)
		if(ierr /= 0) return
		call check_exist(i2,i3,n2,nnn,nil,nnl)
		if(ierr /= 0) return
		call check_exist(i3,i1,n3,nnn,nil,nnl)
		if(ierr /= 0) return
		n_temp=in(l,2)
		e_temp=el_area(l)
		call add_element(nne,n_temp,n2,n1,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(elem(nne),chosen)
		endif
		n_temp=in(l,3)
		call add_element(nne,n_temp,n3,n2,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(elem(nne),chosen)
		endif
		call add_element(nne,n1,n2,n3,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(elem(nne),chosen)
		endif
		in(l,2)=n1
		in(l,3)=n3

	end subroutine refine_element
	!----------------------------------------------------------------------
	subroutine check_exist(i1,i2,ninc,nnn,nil,nnl)
		implicit none

		integer(i4) :: i, i1, i2, nnn, nil, ninc 

		logical node_exists
		integer(i4) :: nnl(4*nn)
		real*8 :: xm
		real*8 :: ym

		xm=0.5*x(i1)+0.5*x(i2)
		ym=0.5*y(i1)+0.5*y(i2)
		node_exists=.false.
		if (nil.gt.0)then
			i=1
			10      continue
				if (abs(xm-x(nnl(i))).lt.1.e-5) then
					if(abs(ym-y(nnl(i))).lt.1.e-5) then
						node_exists=.true.
						ninc=nnl(i)
						call delete_nnl(nnl,i,nil)
						nil=nil-1
						i=i-1
					endif
				endif
				i=i+1
			if (i.LE.nil .aND. .not. node_exists) goto 10
		endif
		if (.not. node_exists) then
			call new_node(nnn,xm,ym)
			if(ierr /= 0) return
			call update_bnodes(i1,i2,nnn)
			if(ierr /= 0) return
			nil=nil+1
			nnl(nil)=nnn
			ninc=nnn
		endif

	end subroutine check_exist
	!----------------------------------------------------------------------
	subroutine delete_nnl(nnl,i,nil)
		implicit none

		integer(i4) :: i,j, nil

		integer(i4) :: nnl(4*nn)
	
		do j=i,nil-1
			nnl(j)=nnl(j+1)
		end do      

	end subroutine delete_nnl
	!----------------------------------------------------------------------
	subroutine check_two(l,nil,nnn,nne,nnl)
		! check each side to see if node already exists
		implicit none

		integer(i4) :: i, l, nil, nnn, nne, new_node

		integer(i4) :: nnl(4*nn)
		real*8 :: x1,y1,x2,y2,x3,y3

		new_node=0
		x1=0.5*x(in(l,1))+0.5*x(in(l,2))
		y1=0.5*y(in(l,1))+0.5*y(in(l,2))
		x2=0.5*x(in(l,2))+0.5*x(in(l,3))
		y2=0.5*y(in(l,2))+0.5*y(in(l,3))
		x3=0.5*x(in(l,3))+0.5*x(in(l,1))
		y3=0.5*y(in(l,3))+0.5*y(in(l,1))
		i=1
		10    continue
			if (abs(x1-x(nnl(i))).lt.1.e-5) then
				if(abs(y1-y(nnl(i))).lt.1.e-5) then
					new_node=new_node+1
				endif
			endif
			if (abs(x2-x(nnl(i))).lt.1.e-5) then
				if(abs(y2-y(nnl(i))).lt.1.e-5) then
					new_node=new_node+1
				endif
			endif
			if (abs(x3-x(nnl(i))).lt.1.e-5) then
				if(abs(y3-y(nnl(i))).lt.1.e-5) then
					new_node=new_node+1
				endif
			endif
			i=i+1
		if(i.le.nil .and. new_node.le.1) goto 10
		if (new_node.gt.1) then
			call refine_element(l,.false.,nil,nnn,nne,nnl)
			if(ierr /= 0) return
		endif

	end subroutine check_two
	!----------------------------------------------------------------------
	subroutine check_one(l,nil,nne,nnl)
		! check each side to see if node already exists
		implicit none

		integer(i4) :: i, l, nil, nne, n1

		integer(i4) :: nnl(4*nn)
		logical done
		real*8 :: x1,y1,x2,y2,x3,y3

		done=.false.
		x1=0.5*x(in(l,1))+0.5*x(in(l,2))
		y1=0.5*y(in(l,1))+0.5*y(in(l,2))
		x2=0.5*x(in(l,2))+0.5*x(in(l,3))
		y2=0.5*y(in(l,2))+0.5*y(in(l,3))
		x3=0.5*x(in(l,3))+0.5*x(in(l,1))
		y3=0.5*y(in(l,3))+0.5*y(in(l,1))
		i=1
		10    continue
			if (abs(x1-x(nnl(i))).lt.1.e-5) then
				if(abs(y1-y(nnl(i))).lt.1.e-5) then
					n1=nnl(i)
					call add_element(nne,n1,in(l,2),in(l,3),el_area(l))
					if(ierr /= 0) return
					in(l,2)=n1
					done=.true.
				endif
			endif
			if (abs(x2-x(nnl(i))).lt.1.e-5) then
				if(abs(y2-y(nnl(i))).lt.1.e-5) then
					n1=nnl(i)
					call add_element(nne,n1,in(l,3),in(l,1),el_area(l))
					if(ierr /= 0) return
					in(l,3)=n1
					done=.true.
				endif
			endif
			if (abs(x3-x(nnl(i))).lt.1.e-5) then
				if(abs(y3-y(nnl(i))).lt.1.e-5) then
					n1=nnl(i)
					call add_element(nne,n1,in(l,2),in(l,3),el_area(l))
					if(ierr /= 0) return
					in(l,3)=n1
					done=.true.
				endif
			endif
			i=i+1
		if(i.le.nil .and. .not. done) goto 10

	end subroutine check_one

	!----------------------------------------------------------------------
	subroutine initialize_segment_arrays(proposed)
		implicit none

		integer(i4) :: proposed

		if(proposed > ns_cur) ns_cur=proposed

		if(allocated(seg_node)) then
			deallocate(seg_node, seg_el, seg_area, seg_is)
		endif

		allocate(seg_node(ns_cur,2),seg_el(ns_cur,2), seg_area(ns_cur,2), seg_is(ns_cur),stat=ialloc)
		call check_alloc(ialloc,'initial segment arrays')
		if(ierr /= 0) return

		seg_node(:,:)=0
		seg_area(:,:)=0
		seg_el(:,:)=0
		seg_is(:)=0

	end subroutine initialize_segment_arrays
	!----------------------------------------------------------------------
	subroutine reallocate_segment_arrays(ns_rqst)
		implicit none

		real, parameter :: ns_mult=1.5
		integer(i4) :: ns_new

		integer(i4) :: ns_rqst, i

		integer(i4), allocatable :: seg_node_tmp(:,:) 
		integer(i4), allocatable :: seg_el_tmp(:,:) 
		integer(i4), allocatable :: seg_area_tmp(:,:) 
		integer(i4), allocatable :: seg_is_tmp(:) 

		if(ns_rqst >= ns_cur) then  ! reallocate seg_node etc

			ns_new=nint(ns_rqst*ns_mult)

			allocate(seg_node_tmp(ns_new,2),seg_el_tmp(ns_new,2), seg_area_tmp(ns_new,2), seg_is_tmp(ns_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp segment arrays')
			if(ierr /= 0) return
	
			seg_node_tmp(:,:)=0
			seg_el_tmp(:,:)=0
			seg_area_tmp(:,:)=0
			seg_is_tmp(:)=0

			! copy current data
			do i=1,ns_cur
				seg_node_tmp(i,:)	=	seg_node(i,:)
				seg_el_tmp(i,:)		=	seg_el(i,:)
				seg_area_tmp(i,:)	=	seg_area(i,:)
				seg_is_tmp(i)		=	seg_is(i)
			end do

			! destroy arrays
			deallocate(seg_node, seg_area, seg_el, seg_is)
			! reallocate
			allocate(seg_node(ns_new,2),seg_el(ns_new,2), seg_area(ns_new,2), seg_is(ns_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate segment arrays')
			if(ierr /= 0) return

			! copy current data
			do i=1,ns_cur
				seg_node(i,:)	=	seg_node_tmp(i,:)	
				seg_el(i,:)		=	seg_el_tmp(i,:)		
				seg_area(i,:)	=	seg_area_tmp(i,:)	
				seg_is(i)		=	seg_is_tmp(i)	
			end do

			ns_cur=ns_new

		end if

	end subroutine reallocate_segment_arrays
	!----------------------------------------------------------------------
	subroutine new_segment(nnseg,n1,n2,a1,a2,l1,l2)
		implicit none

		integer(i4) :: nnseg, n1, n2, a1, a2, l1, l2

		nnseg=nnseg+1

		call reallocate_segment_arrays(nnseg)
		if(ierr /= 0) return

		seg_node(nnseg,1)=n1
		seg_node(nnseg,2)=n2
		seg_area(nnseg,1)=a1
		seg_area(nnseg,2)=a2
		seg_el(nnseg,1)=l1
		seg_el(nnseg,2)=l2

	end subroutine new_segment
	!----------------------------------------------------------------------
	subroutine initialize_cut_arrays(proposed)
		implicit none
		integer(i4) :: proposed

		if(proposed > nc_cur) nc_cur=proposed

		if(allocated(nsg)) then
			deallocate(nsg,nct,al,ae,usne,usnl,xcut,ycut,xi,yi,rc,rs)
		endif

		allocate(nsg(nc_cur),nct(nc_cur),al(nc_cur),ae(nc_cur),usne(nc_cur), &
			usnl(nc_cur),xcut(nc_cur),ycut(nc_cur),xi(nc_cur),yi(nc_cur),rc(nc_cur), &
			rs(nc_cur), stat=ialloc)
		call check_alloc(ialloc,'initial cut arrays')
		if(ierr /= 0) return

		nsg(:)=0
		nct(:)=0
		al(:)=0
		ae(:)=0
		usne(:)=0
		usnl(:)=0
		xcut(:)=0
		ycut(:)=0
		xi(:)=0.0d0
		yi(:)=0.0d0
		rc(:)=0.0d0
		rs(:)=0.0d0

	end subroutine initialize_cut_arrays
	!----------------------------------------------------------------------
	subroutine reallocate_cut_arrays(nc_rqst)
		implicit none

		real, parameter :: nc_mult=1.5

		integer(i4) :: i
		integer(i4) :: nc_rqst, nc_new

		integer(i4), allocatable :: nsg_tmp(:)
		integer(i4), allocatable :: nct_tmp(:)
		integer(i4), allocatable :: al_tmp(:)
		integer(i4), allocatable ::	ae_tmp(:)
		integer(i4), allocatable ::	usne_tmp(:)
		integer(i4), allocatable ::	usnl_tmp(:)
		integer(i4), allocatable ::	xcut_tmp(:)
		integer(i4), allocatable ::	ycut_tmp(:)
		integer(i4), allocatable ::	xi_tmp(:)
		integer(i4), allocatable ::	yi_tmp(:)
		integer(i4), allocatable ::	rc_tmp(:)
		integer(i4), allocatable ::	rs_tmp(:)

		if(nc_rqst >= nc_cur) then  ! reallocate nsg etc

			nc_new=nint(nc_rqst*nc_mult)

			allocate(nsg_tmp(nc_cur), &
				nct_tmp(nc_cur), &
				al_tmp(nc_cur), &
				ae_tmp(nc_cur), &
				usne_tmp(nc_cur), &
				usnl_tmp(nc_cur), &
				xcut_tmp(nc_cur), &
				ycut_tmp(nc_cur), &
				xi_tmp(nc_cur), &
				yi_tmp(nc_cur), &
				rc_tmp(nc_cur), &
				rs_tmp(nc_cur), stat=ialloc)
			call check_alloc(ialloc,'allocate temp cut boundary arrays')
			if(ierr /= 0) return
	
			nsg_tmp(:)=0
			nct_tmp(:)=0
			al_tmp(:)=0
			ae_tmp(:)=0
			usne_tmp(:)=0
			usnl_tmp(:)=0
			xcut_tmp(:)=0
			ycut_tmp(:)=0
			xi_tmp(:)=0.0d0
			yi_tmp(:)=0.0d0
			rc_tmp(:)=0.0d0
			rs_tmp(:)=0.0d0

			! copy current data
			do i=1,nc_cur
				nsg_tmp(i)	=nsg(i)
				nct_tmp(i)	=nct(i)
				al_tmp(i)	=al(i)
				ae_tmp(i)	=ae(i)
				usne_tmp(i)	=usne(i)
				usnl_tmp(i)	=usnl(i)
				xcut_tmp(i)	=xcut(i)
				ycut_tmp(i)	=ycut(i)
				xi_tmp(i)	=xi(i)
				yi_tmp(i)	=yi(i)
				rc_tmp(i)	=rc(i)
				rs_tmp(i)	=rs(i)
			end do

			! destroy arrays
			deallocate(nsg,nct,al,ae,usne,usnl,xcut,ycut,xi,yi,rc,rs)
			! reallocate
			allocate(nsg(nc_new), &
				nct(nc_new), &
				al(nc_new), &
				ae(nc_new), &
				usne(nc_new), &
				usnl(nc_new), &
				xcut(nc_new), &
				ycut(nc_new), &
				xi(nc_new), &
				yi(nc_new), &
				rc(nc_new), &
				rs(nc_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate cut arrays')
			if(ierr /= 0) return
	
			nsg(:)=0
			nct(:)=0
			al(:)=0
			ae(:)=0
			usne(:)=0
			usnl(:)=0
			xcut(:)=0
			ycut(:)=0
			xi(:)=0.0d0
			yi(:)=0.0d0
			rc(:)=0.0d0
			rs(:)=0.0d0

			! copy current data
			do i=1,nc_cur
				nsg(i)	=	nsg_tmp(i)	
				nct(i)	=	nct_tmp(i)	
				al(i)	=	al_tmp(i)	
				ae(i)	=	ae_tmp(i)	
				usne(i)	=	usne_tmp(i)	
				usnl(i)	=	usnl_tmp(i)	
				xcut(i)	=	xcut_tmp(i)	
				ycut(i)	=	ycut_tmp(i)	
				xi(i)	=	xi_tmp(i)	
				yi(i)	=	yi_tmp(i)	
				rc(i)	=	rc_tmp(i)	
				rs(i)	=	rs_tmp(i)	
			end do

			nc_cur=nc_new

		end if
	end subroutine reallocate_cut_arrays
	!----------------------------------------------------------------------
	subroutine reallocate_lbn(nl_rqst)
		implicit none

		real, parameter :: nl_mult=1.5

		integer(i4) :: i
		integer(i4) :: nl_rqst, nl_new

		integer(i4), allocatable :: lbn_tmp(:)	! boundary node list


		if(nl_rqst >= nl_cur) then  ! reallocate in etc

			nl_new=nint(nl_rqst*nl_mult)

			allocate(lbn_tmp(0:nl_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp lbn array')
			if(ierr /= 0) return
	
			lbn_tmp(:)=0

			! copy current data
			do i=0,nl_cur
				lbn_tmp(i)	=	lbn(i)
			end do

			! destroy arrays
			deallocate(lbn)
			! reallocate
			allocate(lbn(0:nl_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate lbn array')
			if(ierr /= 0) return

			! copy current data
			do i=0,nl_cur
				lbn(i)		=	lbn_tmp(i)
			end do

			nl_cur=nl_new

		end if
	end subroutine reallocate_lbn
	!----------------------------------------------------------------------
	subroutine reallocate_outer_boundary_arrays(nb_rqst)
		implicit none

		real, parameter :: nb_mult=1.5

		integer(i4) :: i
		integer(i4) :: nb_rqst, nb_new

		integer(i4), allocatable :: obn_tmp(:)	! outer boundary node list


		if(nb_rqst >= no_cur) then  ! reallocate in etc

			nb_new=nint(nb_rqst*nb_mult)

			allocate(obn_tmp(0:nb_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp outer boundary arrays')
			if(ierr /= 0) return

			obn_tmp(:)=0

			! copy current data
			do i=0,no_cur
				obn_tmp(i)	=	obn(i)
			end do

			! destroy arrays
			deallocate(obn)
			! reallocate
			allocate(obn(0:nb_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate outer boundary arrays')
			if(ierr /= 0) return

			! copy current data
			do i=0,no_cur
				obn(i)		=	obn_tmp(i)
			end do

			no_cur=nb_new

		end if
	end subroutine reallocate_outer_boundary_arrays
	!----------------------------------------------------------------------
	subroutine reallocate_boundary_arrays(na_rqst, nb_rqst)
		implicit none

		real, parameter :: nb_mult=1.5
		real, parameter :: na_mult=1.5

		integer(i4) :: i, j
		integer(i4) :: na_rqst, na_new
		integer(i4) :: nb_rqst, nb_new

		integer(i4), allocatable :: nbn_tmp(:)		! # of nodes in list
		integer(i4), allocatable :: bn_tmp(:,:)		! area node list
		real, allocatable :: elength_tmp(:)
		real, allocatable :: y_elength_tmp(:)
		real, allocatable :: stretch_factor_tmp(:)
		integer(i4), allocatable :: ndrop_rate_tmp(:)		! area node list
		logical, allocatable :: hole_tmp(:)
		logical, allocatable :: plot_area_tmp(:)

		if(nb_rqst >= nb_cur .or. na_rqst >= na_cur) then  ! reallocate in etc

			if(nb_rqst >= nb_cur) then
				nb_new=nint(nb_rqst*nb_mult)
			else
				nb_new=nb_cur
			endif
			if(na_rqst >= na_cur) then
				na_new=nint(na_rqst*na_mult)
			else
				na_new=na_cur
			endif

			allocate(nbn_tmp(na_new),bn_tmp(na_new,0:nb_new), &
				elength_tmp(na_new),y_elength_tmp(na_new), &
				stretch_factor_tmp(na_new),ndrop_rate_tmp(na_new), &
				hole_tmp(na_new),plot_area_tmp(na_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp boundary arrays')
			if(ierr /= 0) return

			nbn_tmp(:)=0
			bn_tmp(:,:)=0
			elength_tmp(:)=0.0
			y_elength_tmp(:)=0.0
			stretch_factor_tmp(:)=0.0
			ndrop_rate_tmp(:)=0.0
			hole_tmp(:)=.false.
			plot_area_tmp(:)=.false.


			! copy current data
			do i=1,na_cur
				nbn_tmp(i)	=	nbn(i)
				elength_tmp(i)=elength(i)
				y_elength_tmp(i)=y_elength(i)
				stretch_factor_tmp(i)=stretch_factor(i)
				ndrop_rate_tmp(i)=ndrop_rate(i)
				hole_tmp(i)	=	hole(i)
				plot_area_tmp(i)	=	plot_area(i)
				do j=0,nb_cur
					bn_tmp(i,j)		=	bn(i,j)
				end do
			end do

			! destroy arrays
			deallocate(nbn, bn, elength, y_elength,  stretch_factor, ndrop_rate, hole, plot_area)
			! reallocate
			allocate(nbn(na_new),bn(na_new,0:nb_new), &
				elength(na_new),y_elength(na_new), &
				stretch_factor(na_new),ndrop_rate(na_new), &
				hole(na_new),plot_area(na_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate boundary arrays')
			if(ierr /= 0) return

			nbn(:)=0
			bn(:,:)=0
			elength(:)=0.0
			y_elength(:)=0.0
			stretch_factor(:)=2.0
			ndrop_rate(:)=1
			hole(:)=.false.
			plot_area(:)=.false.

			! copy current data
			do i=1,na_cur
				nbn(i)		=	nbn_tmp(i)
				hole(i)		=	hole_tmp(i)
				elength(i)		=	elength_tmp(i)
				y_elength(i)		=	y_elength_tmp(i)
				stretch_factor(i)	=	stretch_factor_tmp(i)
				ndrop_rate(i)		=	ndrop_rate_tmp(i)
				plot_area(i)		=	plot_area_tmp(i)
				do j=0,nb_cur
					bn(i,j)		=	bn_tmp(i,j)
				end do
			end do

			deallocate(nbn_tmp,bn_tmp, elength_tmp, y_elength_tmp,  stretch_factor_tmp, ndrop_rate_tmp, hole_tmp, plot_area_tmp)

			nb_cur=nb_new
			na_cur=na_new

		end if
	end subroutine reallocate_boundary_arrays
	!----------------------------------------------------------------------
	subroutine new_obnd_node(nnd,i1)
		implicit none

		integer(i4) :: nnd, i1

		nnd=nnd+1

		call reallocate_outer_boundary_arrays(nnd)
		if(ierr /= 0) return

		obn(nnd)=i1

	end subroutine new_obnd_node
	!----------------------------------------------------------------------
	subroutine new_bnd_node(iarea,nnd,i1)
		implicit none

		integer(i4) :: nnd, i1, iarea

		nnd=nnd+1

		call reallocate_boundary_arrays(iarea,nnd)
		if(ierr /= 0) return

		bn(iarea,nnd)=i1
		nbn(iarea)=nnd

	end subroutine new_bnd_node
	!----------------------------------------------------------------------
	subroutine initialize_boundary_arrays(a_proposed,b_proposed)
		implicit none
		integer(i4) :: b_proposed
		integer(i4) :: a_proposed

		if(b_proposed > nb_cur) nb_cur=b_proposed
		if(a_proposed > na_cur) na_cur=a_proposed

		allocate(nbn(na_cur),bn(na_cur,0:nb_cur), &
			elength(na_cur), y_elength(na_cur), &
			stretch_factor(na_cur), ndrop_rate(na_cur), &
			hole(na_cur), plot_area(na_cur), stat=ialloc)
		call check_alloc(ialloc,'initial boundary arrays')
		if(ierr /= 0) return

		nbn(:)=0
		bn(:,:)=0
		elength(:)=0.0
		y_elength(:)=0.0
		stretch_factor(:)=2.0
		ndrop_rate(:)=1
		hole(:)=.false.
		plot_area(:)=.false. 

	end subroutine initialize_boundary_arrays
	!----------------------------------------------------------------------
	subroutine initialize_outer_boundary_array(b_proposed)
		implicit none
		integer(i4) :: b_proposed

		if(b_proposed > no_cur) no_cur=b_proposed

		allocate(obn(0:no_cur), stat=ialloc)
		call check_alloc(ialloc,'initial outer boundary arrays')
		if(ierr /= 0) return

		obn(:)=0

	end subroutine initialize_outer_boundary_array


	!----------------------------------------------------------------------
	subroutine reallocate_element_arrays(ne_rqst)
		implicit none

		real, parameter :: ne_mult=1.5

		integer(i4) :: ne_rqst, i, ne_new

		integer(i4), allocatable :: in_tmp(:,:)		! element node lists
		integer(i4), allocatable :: elem_tmp(:)		! element bit setting
		integer(i4), allocatable :: el_area_tmp(:) ! element area number 


		if(ne_rqst >= ne_cur) then  ! reallocate in etc

			ne_new=nint(ne_rqst*ne_mult)

			allocate(in_tmp(ne_new,3),elem_tmp(ne_new), el_area_tmp(ne_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp element arrays')
			if(ierr /= 0) return

			in_tmp(:,:)=0.0d0
			elem_tmp(:)=0.0d0
			el_area_tmp(:)=0

			! copy current data
			do i=1,ne_cur
				in_tmp(i,:)		=	in(i,:)
				elem_tmp(i)			=	elem(i)
				el_area_tmp(i)	=	el_area(i)
			end do

			! destroy arrays
			deallocate(in, elem, el_area)
			! reallocate
			allocate(in(ne_new,3),elem(ne_new),el_area(ne_new),  stat=ialloc)
			call check_alloc(ialloc,'reallocate element arrays')
			if(ierr /= 0) return

			! copy current data
			do i=1,ne_cur
				in(i,:)		=	in_tmp(i,:)
				elem(i)		=	elem_tmp(i)
				el_area(i)	=	el_area_tmp(i)
			end do

			ne_cur=ne_new

		end if
	end subroutine reallocate_element_arrays
	!----------------------------------------------------------------------
	subroutine add_element(nel,i1,i2,i3,e_area)
		implicit none

		integer(i4) :: i1, i2, i3, nel, e_area

		nel=nel+1  ! we are going to make a new element

		call reallocate_element_arrays(nel)
		if(ierr /= 0) return

		in(nel,1)=i1
		in(nel,2)=i2
		in(nel,3)=i3
		elem(nel)=0	
		el_area(nel)=e_area		

	end subroutine add_element
	!----------------------------------------------------------------------
	subroutine initialize_element_arrays(e_proposed)
		implicit none
		integer(i4) :: e_proposed

		if(e_proposed > ne_cur) ne_cur=e_proposed

		if(allocated(in)) then
			deallocate(in, elem, el_area)
		endif

		allocate(in(ne_cur,3),elem(ne_cur),el_area(ne_cur), stat=ialloc)
		call check_alloc(ialloc,'initial element arrays')
		if(ierr /= 0) return

		in(:,:)=0
		elem(:)=0
		el_area(:)=0 


	end subroutine initialize_element_arrays
	!----------------------------------------------------------------------
	subroutine initialize_node_arrays(n_proposed)
		implicit none

		integer(i4) :: n_proposed

		if(n_proposed > nn_cur) nn_cur=n_proposed

		if(allocated(x)) then
			deallocate(x,y,node)
		endif

		allocate(x(nn_cur),y(nn_cur),node(nn_cur), stat=ialloc)
		call check_alloc(ialloc,'initial node arrays')
		if(ierr /= 0) return

		x(:)=0
		y(:)=0
		node(:)=0 

	end subroutine initialize_node_arrays
	!----------------------------------------------------------------------
	subroutine reallocate_node_arrays(nn_rqst)
		implicit none

		real, parameter :: nn_mult=1.5
		integer(i4) :: nn_new

		integer(i4) :: nn_rqst, i

		real(dp),allocatable :: x_tmp(:)		! x coordinates
		real(dp),allocatable :: y_tmp(:)		! y coordinates
		integer(i4), allocatable :: node_tmp(:) ! node bit set array 

		if(nn_rqst >= nn_cur) then  ! reallocate x, y, node

			nn_new=nint(nn_rqst*nn_mult)

			allocate(x_tmp(nn_new),y_tmp(nn_new), node_tmp(nn_new), stat=ialloc)
			call check_alloc(ialloc,'allocate temp node arrays')
			if(ierr /= 0) return

			x_tmp(:)=0.0d0
			y_tmp(:)=0.0d0
			node_tmp(:)=0

			! copy current data
			do i=1,nn_cur
				x_tmp(i)	=	x(i)
				y_tmp(i)	=	y(i)
				node_tmp(i)	=	node(i)
			end do

			! destroy arrays
			deallocate(x,y,node)
			! reallocate
			allocate(x(nn_new),y(nn_new), node(nn_new), stat=ialloc)
			call check_alloc(ialloc,'reallocate node arrays')
			if(ierr /= 0) return

			! copy current data
			do i=1,nn_cur
				x(i)		=	x_tmp(i)
				y(i)		=	y_tmp(i)
				node(i)		=	node_tmp(i)
			end do



			nn_cur=nn_new

		end if

	end subroutine reallocate_node_arrays
	!----------------------------------------------------------------------
	subroutine new_node(nnd,xnew,ynew)
		implicit none

		integer(i4) :: nnd
		real(dp) :: xnew, ynew

		nnd=nnd+1

		call reallocate_node_arrays(nnd)
		if(ierr /= 0) return

		x(nnd)=xnew
		y(nnd)=ynew
		node(nnd)=0	

	end subroutine new_node
	!----------------------------------------------------------------------
	subroutine gen_uni_bnodes
		implicit none

		integer(i4) :: i, n1, n2, i1
		real(dp) :: x1, y1, x2, y2, d, rmu, rmu1, rmu2, del_rmu, rmu_el


		integer(i4) :: nseg_old
		real(dp), allocatable :: size_nde(:)  ! smallest element size (seg length or elength) at a node 

		allocate(size_nde(nn), stat=ialloc)
		call check_alloc(ialloc,'gen_uni_bnodes work arrays')

		size_nde(:)=1.e20  ! array assignment

		call calc_segments

		! set array size_nde
		do i=1,nseg
			n1=seg_node(i,1)
			n2=seg_node(i,2)
			d=sqrt((x(n2)-x(n1))**2+(y(n2)-y(n1))**2)

			size_nde(n1)=min(size_nde(n1),d)   					! segment length
			size_nde(n1)=min(size_nde(n1),elength(seg_area(i,1)))	! target length	
			if(seg_area(i,2) /= 0) size_nde(n1)=min(size_nde(n1),elength(seg_area(i,2)))	! target length area 2	

			size_nde(n2)=min(size_nde(n2),d)   					! segment length
			size_nde(n2)=min(size_nde(n2),elength(seg_area(i,1)))	! target length	
			if(seg_area(i,2) /= 0) size_nde(n2)=min(size_nde(n2),elength(seg_area(i,2)))	! target length area 2	
		end do

		call Msg('Generating boundary nodes...')

		! loop over segments and satisfy elength, inserting nodes as necessary 
		nseg_old=nseg
		seg_old_loop: do i=1,nseg_old
			n1=seg_node(i,1)
			n2=seg_node(i,2)
			x1=x(n1)
			y1=y(n1)
			x2=x(n2)
			y2=y(n2)
			d=sqrt((x2-x1)**2+(y2-y1)**2)
			rmu1=size_nde(n1)/d	  ! length factor at n1
			rmu2=size_nde(n2)/d	  ! length factor at n2
			if(rmu1>.6 .and. rmu2>.6) cycle

			! enforce elength as maximum size
			rmu_el=elength(seg_area(i,1))/d
			stretch=stretch_factor(seg_area(i,1))
			if(seg_area(i,2) /= 0) then
				rmu_el=min(rmu_el,elength(seg_area(i,2))/d)	! rmu_el max for elength
				stretch=min(stretch,stretch_factor(seg_area(i,2)))
			end if
		
			i1=nn+1
			if(rmu1 <= rmu2) then  ! node 1 hase smaller length factor
				rmu=0.0
				del_rmu=min(rmu_el,rmu1*stretch*.707)
				do
					rmu=rmu+del_rmu
					if(1.0-rmu < del_rmu*0.75) exit

					! add node
					call new_node(nn,x1*(1.0-rmu)+x2*rmu,y1*(1.0-rmu)+y2*rmu)
					call update_bnodes(n1,n2,nn)
					n1=nn
					del_rmu=min(rmu_el,del_rmu*stretch*.707)
				end do
			else				  ! node 2 hase smaller length factor
				rmu=0.0
				del_rmu=min(rmu_el,rmu2*stretch*.707)
				do
					rmu=rmu+del_rmu
					if(1.0-rmu < del_rmu*0.75) exit

					! add node
					call new_node(nn,x2*(1.0-rmu)+x1*rmu,y2*(1.0-rmu)+y1*rmu)
					call update_bnodes(n1,n2,nn)
					n2=nn
					del_rmu=min(rmu_el,del_rmu*stretch*.707)
				end do
			endif
		
		end do seg_old_loop

		deallocate(size_nde)

	end subroutine gen_uni_bnodes
	!----------------------------------------------------------------------
	subroutine update_area_boundary(narea,un,new)
		implicit none 

		integer(i4) :: i, k, narea, un, new

		i=1
		10    continue
			if(bn(narea,i).eq.new) then
				return
			endif
			if(bn(narea,i).eq.un) then
				k=i
			endif
			i=i+1
		if(i.le.nbn(narea)) goto 10

		call reallocate_boundary_arrays(narea,nbn(narea)+1)
		if(ierr /= 0) return

		if(k.eq.nbn(narea)) then
			bn(narea,0)=new
		endif
		do  i=nbn(narea),k+1,-1
			bn(narea,i+1)=bn(narea,i)
		end do
		bn(narea,k+1)=new
		nbn(narea)=nbn(narea)+1

	end subroutine update_area_boundary
	!----------------------------------------------------------------------
	subroutine update_outer_boundary(un,new)
		implicit none 

		integer(i4) :: i, k, un, new

		i=1
		10    continue
			if(obn(i).eq.new) then
				return
			endif
			if(obn(i).eq.un) then
				k=i
			endif
			i=i+1
		if(i.le.onbn) goto 10

		call reallocate_outer_boundary_arrays(onbn+1)
		if(ierr /= 0) return

		if(k.eq.onbn) then
			obn(0)=new
		endif
		do i=onbn,k+1,-1
			obn(i+1)=obn(i)
		end do
		obn(k+1)=new
		onbn=onbn+1

	end subroutine update_outer_boundary
	!----------------------------------------------------------------------
	subroutine gen_convex(reg)
		implicit none

		integer(i4) :: i, lev, nng
		real(dp) :: crit_angle, des_angle, phi, fac, aa1, ab1, aa2, ab2, aspect
		integer(i4) :: reg
		logical :: node_inside

		nl_cur=nb_cur

		allocate(lbn(0:nl_cur),stat=ialloc)
		call check_alloc(ialloc,'gen_convex lbn')
		if(ierr /= 0) return


		stuck=.false.

		ncount_grade=0

		do i=0,nbn(reg)
			lbn(i)=bn(reg,i)
		end do      
	
		eleng=elength(reg)
		stretch=stretch_factor(reg)
		ndrop=ndrop_rate(reg)

		lnbn=nbn(reg)
		lev=1
		crit_angle=150.0
		des_angle=110.
		aspect=.4
		i=1
		sn=lbn(0)
		30    continue


			! compute the incidences of the nodes +/- 2 from nd
			iad(0)=i	
			iad(-1)=i-1
			if(i.eq.1) then
				iad(-2)=lnbn-1
			else
				iad(-2)=i-2
			endif
			if(i.eq.lnbn-1) then
				iad(1)=lnbn
				iad(2)=1
			else if(i.eq.lnbn) then
				iad(1)=1
				iad(2)=2
			else
				iad(1)=i+1
				iad(2)=i+2
			endif
			usn2=lbn(iad(-2))
			usn=lbn(iad(-1))
			nd=lbn(i)
			dsn=lbn(iad(1))
			dsn2=lbn(iad(2))
			call int_angle_nng(usn,nd,dsn,crit_angle,des_angle,phi,nng)
			if (phi.lt.185.0)then
				select case (nng)
				case (0)
					call check_node_in_el(node_inside)	
					if(.not. node_inside) then
						call nng_0(reg,i,des_angle,aspect)
						if(ierr /= 0) goto 1000
					endif

				case (1)
					!if(do_grade) then
					!	call krig_point(x(nd),y(nd),fac,d1,d2,d3)
					!else
						fac=1.
					!endif
					call nng_1(reg,fac,i)
					if(ierr /= 0) goto 1000
				case (2)
					!if(do_grade) then
					!	call krig_point(x(nd),y(nd),fac,d1,d2,d3)
					!else
						fac=1.
					!endif
					call nng_2(reg,fac,i)
					if(ierr /= 0) goto 1000
				end  select
			else
				if(stuck) then
					call fix_stuck(reg,fac,i)
					if(ierr /= 0) goto 1000
				endif
			endif

			i=i+1
			if(i.gt.lnbn) i=1
			if(nd.eq.sn) then
				if(.not. stuck) then
					stuck=.true.
				else
					if(des_angle > 180.) then
						write(log_msg,'(a,i10)') 'Generator got stuck in area ',reg
						call Msg(log_msg)
						ierr=2
						goto 1000
					endif
					des_angle=min(180.,des_angle+5.)
					aspect=max(.01,aspect-.01)
				endif
			endif

			! call key_pause
			! call draw_node(nd,red,sn_sz)

		if(lnbn.gt.4) goto 30  ! loop around current area boundary

		if(lnbn.eq.3) then
			i=2
			nd=lbn(i)
			dsn=lbn(i+1)
			usn=lbn(i-1)
			call add_element(ne,nd,dsn,usn,reg)
			if(ierr /= 0) goto 1000
		else
			da1=lbn(1)
			db1=lbn(2)
			da2=lbn(3)
			db2=lbn(4)
			call int_angle(x(db2),y(db2),x(da1),y(da1),x(db1),y(db1),aa1)
			call int_angle(x(da1),y(da1),x(db1),y(db1),x(da2),y(da2),ab1)
			call int_angle(x(db1),y(db1),x(da2),y(da2),x(db2),y(db2),aa2)
			call int_angle(x(da2),y(da2),x(db2),y(db2),x(da1),y(da1),ab2)
			if(aa1+aa2.gt.ab1+ab2) then
				call add_element(ne,da1,db1,da2,reg)
				if(ierr /= 0) goto 1000
				call add_element(ne,da1,da2,db2,reg)
				if(ierr /= 0) goto 1000
			else
				call add_element(ne,da1,db1,db2,reg)
				if(ierr /= 0) goto 1000
				call add_element(ne,db1,da2,db2,reg)
				if(ierr /= 0) goto 1000
			endif
		endif

		1000 deallocate(lbn)

	end subroutine gen_convex
	!----------------------------------------------------------------------
	subroutine check_node_in_el(node_inside)	
		implicit none

		integer(i4) :: j
		real(dp) :: f1, f2, f3
		logical :: node_inside

		node_inside=.false.

		! loop over boundary nodes between dsn and usn
		j=iad(2)
		10    continue
			f1=(y(lbn(j))-y(nd))*(x(dsn)-x(nd))-(x(lbn(j))-x(nd))*(y(dsn)-y(nd))
			if (f1.gt.1e-10) then
				f2=(y(lbn(j))-y(dsn))*(x(usn)-x(dsn))-(x(lbn(j))-x(dsn))*(y(usn)-y(dsn))
				if (f2.gt.1.e-10) then
					f3=(y(lbn(j))-y(usn))*(x(nd)-x(usn))-(x(lbn(j))-x(usn))*(y(nd)-y(usn))
					if (f3.gt.1.e-10) then
						node_inside=.true.
					endif
				endif
			endif
			j=j+1
			if(j.ge.lnbn) j=0
		if (lbn(j).ne.usn .and. .not. node_inside) goto 10

	end subroutine check_node_in_el
	!----------------------------------------------------------------------
	subroutine nng_0(reg,i,des_angle,aspect)
		implicit none

		integer(i4) :: i, j
		real(dp) :: des_angle, aspect,olap, d, du, dd
		logical :: overlap
		integer(i4) ::reg

		overlap=.false.
		olap=0.
		j=iad(2)
		if(j.ge.lnbn) j=0
		40    continue
			call check_olap(usn,dsn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if(j.ne.iad(-2) .and. .not. overlap) goto 40

		if (.not. overlap) then
			! skip if a neighbouring segment length is much smaller than  proposed one
			d=sqrt((x(usn)-x(dsn))**2 + (y(usn)-y(dsn))**2)  
			du=sqrt((x(usn)-x(usn2))**2 + (y(usn)-y(usn2))**2)/d  
			dd=sqrt((x(dsn)-x(dsn2))**2 + (y(dsn)-y(dsn2))**2)/d  

			if(lnbn > 5 .and. (du < aspect .or. dd < aspect)) then
				return
			endif

			if(i.eq.lnbn) then
				lnbn=lnbn-1
				lbn(0)=lbn(lnbn)
			else
				lnbn=lnbn-1
				do j=i,lnbn
					lbn(j)=lbn(j+1)
				end do
			endif
			call add_element(ne,nd,dsn,usn,reg)
			sn=usn
			des_angle=110.0
			aspect=.4
			stuck=.false.
			i=i-1
		endif

	end subroutine nng_0
	!----------------------------------------------------------------------
	subroutine nng_1(reg,fac,i)
		implicit none

		integer(i4) :: i, j
		logical :: overlap
		real(dp) :: olap
		real(dp) :: fac

		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) reg

		olap=0.5
		call gen_node(x(usn),y(usn),x(nd),y(nd),a1x,a1y,fac)
		call gen_node(x(nd),y(nd),x(dsn),y(dsn),a2x,a2y,fac)
		call new_node(nn,(a1x+a2x)/2.0,(a1y+a2y)/2.0)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		50    continue
			call check_olap(dsn,nn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 50

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			60      continue
				call check_olap(usn,nn,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-2) .and. .not. overlap) goto 60
		endif

		if(.not.overlap) then
		j=iad(1)
		if(j.ge.lnbn) j=0
		70      continue
			call check_olap(nd,nn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 70
		endif

		if(.not. overlap) then
			if(.not. in_ob(x(nn),y(nn))) then
				ierr=5
				write(log_msg,'(a,i10)') 'Node outside outer boundary, area ',reg
				call Msg(log_msg)
				return
			endif
			sn=usn
			lbn(i)=nn
			if(i.eq.lnbn) lbn(0)=nn
			call add_element(ne,nd,nn,usn,reg)
			if(ierr /= 0) return
			call add_element(ne,nd,dsn,nn,reg)
			if(ierr /= 0) return
			i=i+1
			!if(i.gt.lnbn) i=i-1
		else
			!     call draw_node(nd,blue,sn_sz)
			!call key_pause
			nn=nn-1
			!call nng_0
		endif

	end subroutine nng_1
	!----------------------------------------------------------------------
	subroutine gen_node(x1,y1,x2,y2,xval,yval,fac)
		implicit none

		real(dp) :: x1,y1,x2,y2,xval,yval,xp,yp,dpt
		real(dp) :: side, e_fac, fac
		!
		side=sqrt((x1-x2)**2+(y1-y2)**2)
		xp=0.5*(x1+x2)
		yp=0.5*(y1+y2)
		e_fac=eleng*fac/side
		if(e_fac > 1.0) then  ! grade larger
			e_fac=min(e_fac,stretch)
		else                  ! grade smaller
			e_fac=max(e_fac,1.0/stretch)
		endif
		dpt=sqrt(side*side-side*side/4.0)*.707
		xval=xp-dpt*e_fac/(side/2.0)*(yp-y1)
		yval=yp+dpt*e_fac/(side/2.0)*(xp-x1)

	end subroutine gen_node
	!----------------------------------------------------------------------
	subroutine nng_2(reg,fac,i)
		implicit none

		integer(i4) :: i,j
		real(dp) :: olap
		real(dp) :: fac
		logical :: overlap !, in_ob
		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) :: reg

		olap=0.5
		call gen_node(x(usn),y(usn),x(nd),y(nd),a1x,a1y,fac)
		call gen_node(x(nd),y(nd),x(dsn),y(dsn),a2x,a2y,fac)
		call new_node(nn,a1x,a1y)
		if(ierr /= 0) return
		call new_node(nn,a2x,a2y)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		olap1: do
			call check_olap(dsn,nn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
			if (j.eq.iad(-1) .or. overlap) exit olap1
		end do olap1

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap2: do
				call check_olap(usn,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-2) .or. overlap) exit olap2
			end do olap2
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap3: do
				call check_olap(nd,nn,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap3
			end do olap3
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap4: do
				call check_olap(nd,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap4
			end do olap4
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap5: do
				call check_olap(nn,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap5
			end do olap5
		endif

		if(.not. overlap) then
			if(.not. in_ob(x(nn),y(nn))) then
				ierr=5
				write(log_msg,'(a,i10)') 'Node outside outer boundary, area ',reg
				call Msg(log_msg)
				return
			endif
			call reallocate_lbn(lnbn+1)
			if(ierr /= 0) return
			sn=usn
			if(i.eq.lnbn) then
				lbn(0)=nn
			else
				do j=lnbn,i+1,-1
					lbn(j+1)=lbn(j)
				end do
			endif
			lnbn=lnbn+1
			if(lnbn > 2*nbn(reg)) then
				ierr=3
				write(log_msg,'(a,i10)') 'GridBuilder failed in area ',reg 
				call Msg(log_msg)
				call write_area_boundary_as_dig(reg) 
				return
			endif	 
			lbn(i)=nn-1
			lbn(i+1)=nn
			call add_element(ne,nd,nn-1,usn,reg)
			if(ierr /= 0) return
			call add_element(ne,nd,nn,nn-1,reg)
			if(ierr /= 0) return
			call add_element(ne,nd,dsn,nn,reg)
			if(ierr /= 0) return
		else
			nn=nn-2
		endif

	end subroutine nng_2
	!----------------------------------------------------------------------
	subroutine write_area_boundary_as_dig(i)
		implicit none

		integer(i4) :: i,j

		open(888,file='area.dig',status='unknown')
		write(888,*) 'Area ',i
		write(888,*) .true.
		write(888,*) '-999. -999.'
		do j=1,nbn(i)
		write(888,*) x(bn(i,j)),y(bn(i,j))
		end do
		write(888,*) '-999. -999.'
		close(888)

	end subroutine write_area_boundary_as_dig
	!----------------------------------------------------------------------
	subroutine fix_stuck(reg,fac,i)
		implicit none

		integer(i4) :: i,j
		real(dp) :: olap
		real(dp) :: fac
		logical :: overlap
		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) :: reg

		olap=0.8
		call gen_node(x(usn),y(usn),x(nd),y(nd),a1x,a1y,fac)
		call gen_node(x(nd),y(nd),x(dsn),y(dsn),a2x,a2y,fac)
		call new_node(nn,a1x,a1y)
		if(ierr /= 0) return
		call new_node(nn,a2x,a2y)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		10    continue
			call check_olap(dsn,nn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 10

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			20      continue
				call check_olap(usn,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-2) .and. .not. overlap) goto 20
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			30      continue
				call check_olap(nn,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 30
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			40      continue
				call check_olap(nd,nn-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 40
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			50      continue
				call check_olap(nd,nn,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 50
		endif

		if(.not. overlap) then
			sn=dsn
			lbn(i)=nn-1
			lnbn=lnbn+1
			if(lnbn > 2*nbn(reg)) then
				ierr=3
				write(log_msg,'(a,i10)') 'GridBuilder failed in area ',reg 
				call Msg(log_msg)
				call write_area_boundary_as_dig(reg) 
				return
			endif	 
			do j=lnbn,i+2,-1
				lbn(j)=lbn(j-1)
			end do
			lbn(i+1)=nn
			if(i.eq.lnbn) lbn(0)=nn
			call add_element(ne,nd,nn-1,usn,reg)
			if(ierr /= 0) return
			call add_element(ne,nd,nn,nn-1,reg)
			if(ierr /= 0) return
			call add_element(ne,nd,dsn,nn,reg)
			if(ierr /= 0) return
			stuck=.false.
		else
			nn=nn-2
		endif

	end subroutine fix_stuck
	!----------------------------------------------------------------------
	subroutine check_olap(i1,i2,i3,i_4,olap,overlap)
		implicit none

		integer(i4) :: i1, i2, i3, i_4
		logical :: overlap
		real(dp) :: del,rmu,rmu2,xin,yin, olap
		!
		del=(x(i2)-x(i1))*(y(i3)-y(i_4))-(y(i2)-y(i1))*(x(i3)-x(i_4))
		if (abs(del).gt.0.0) then
			rmu=((y(i3)-y(i_4))*(x(i3)-x(i1))-(x(i3)-x(i_4))*(y(i3)-y(i1)))/del
			if (rmu.ge.0.0 .and. rmu.le.1.0+olap) then
				xin=rmu*x(i2)+(1.0-rmu)*x(i1)
				yin=rmu*y(i2)+(1.0-rmu)*y(i1)
				if (abs(x(i3)-x(i_4)).gt.0.0) then
					rmu2=(xin-x(i_4))/(x(i3)-x(i_4))
				else
					rmu2=(yin-y(i_4))/(y(i3)-y(i_4))
				endif
				if (rmu2.ge.0.0 .and. rmu2.le.1.0) then
					overlap=.true.
				endif
			endif
		endif
	end subroutine check_olap
	!----------------------------------------------------------------------
	subroutine int_angle(x1,y1,x0,y0,x2,y2,phi)
		implicit none

		real(dp) :: x1,y1,x0,y0,x2,y2,xr,yr,phi
		real(dp) :: theta1, theta2, fxy

		xr=x1-x0
		yr=y1-y0
		if (xr.lt.0.0) then
			if(abs(yr).gt.0.0) then
				theta1=180.0+atan(yr/xr)*180.0/PI
			else
				theta1=180.0
			endif
		else if (xr.gt.0.0) then
			if(yr.gt.0.0) then
				theta1=atan(yr/xr)*180.0/PI
			else if (yr.lt.0.0) then
				theta1=360.0+atan(yr/xr)*180.0/PI
			else
				theta1=0.0
			endif
		else
			if(yr.gt.0.0) then
				theta1=90.0
			else
				theta1=270.0
			endif
		endif

		xr=x2-x0
		yr=y2-y0
		if (xr.lt.0.0) then
			if(abs(yr).gt.0.0) then
				theta2=180.0+atan(yr/xr)*180.0/PI
			else
				theta2=180.0
			endif
		else if (xr.gt.0.0) then
			if(yr.gt.0.0) then
				theta2=atan(yr/xr)*180.0/PI
			else if (yr.lt.0.0) then
				theta2=360.0+atan(yr/xr)*180.0/PI
			else
			theta2=0.0
			endif
		else
			if(yr.gt.0.0) then
				theta2=90.0
			else
				theta2=270.0
			endif
		endif

		fxy=(x2-x1)*(y0-y1)-(y2-y1)*(x0-x1)
		phi=abs(theta1-theta2)
		if (fxy.lt.0.0) then
			if(phi.gt.180.0) then
				phi=360.0-phi
			endif
		else
			if(phi.lt.180.0) then
				phi=360.0-phi
			endif
		endif

	end subroutine int_angle
	!----------------------------------------------------------------------
	subroutine int_angle_nng(n1,n0,n2,crit_angle,des_angle,phi,nng)
		implicit none
		integer(i4) :: n1, n0, n2, nng
		real(dp) :: x1,y1,x2,y2,x0,y0,xr,yr
		real(dp) :: theta1, theta2, fxy, phi, des_angle, crit_angle, s1, s2, savg

		x1=x(n1)
		y1=y(n1)
		x0=x(n0)
		y0=y(n0)
		x2=x(n2)
		y2=y(n2)

		xr=x1-x0
		yr=y1-y0
		if (xr.lt.0.0) then
			if(abs(yr).gt.0.0) then
				theta1=180.0+atan(yr/xr)*180.0/PI
			else
				theta1=180.0
			endif
		else if (xr.gt.0.0) then
			if(yr.gt.0.0) then
				theta1=atan(yr/xr)*180.0/PI
			else if (yr.lt.0.0) then
				theta1=360.0+atan(yr/xr)*180.0/PI
			else
				theta1=0.0
			endif
		else
			if(yr.gt.0.0) then
				theta1=90.0
			else
				theta1=270.0
			endif
		endif

		xr=x2-x0
		yr=y2-y0
		if (xr.lt.0.0) then
			if(abs(yr).gt.0.0) then
				theta2=180.0+atan(yr/xr)*180.0/PI
			else
				theta2=180.0
			endif
		else if (xr.gt.0.0) then
			if(yr.gt.0.0) then
				theta2=atan(yr/xr)*180.0/PI
			else if (yr.lt.0.0) then
				theta2=360.0+atan(yr/xr)*180.0/PI
			else
				theta2=0.0
			endif
		else
			if(yr.gt.0.0) then
				theta2=90.0
			else
				theta2=270.0
			endif
		endif

		fxy=(x2-x1)*(y0-y1)-(y2-y1)*(x0-x1)
		phi=abs(theta1-theta2)
		if (fxy.lt.0.0) then
			if(phi.gt.180.0) then
				phi=360.0-phi
			endif
		else
			if(phi.lt.180.0) then
				phi=360.0-phi
			endif
		endif

		if(phi <= des_angle) then
			nng=0
		else if(phi <= crit_angle) then
			nng=1
		else if(phi <= 185.) then
			nng=2
		endif

		if(nng == 2) then
			s1=sqrt((x1-x0)**2+(y1-y0)**2)
			s2=sqrt((x2-x0)**2+(y2-y0)**2)
			savg=(s1+s2)/2.0
			if(savg < eleng*.75) then
				ncount_grade=ncount_grade+1
				if(ncount_grade >= ndrop) then
					nng=1
					ncount_grade=0
				endif
			endif
		endif

	end subroutine int_angle_nng
	!----------------------------------------------------------------------
	subroutine update_bnodes(i1,i2,new)
		implicit none

		integer(i4) :: i,j,k, i1, i2, new

		logical found
		!
		!     this routine loops over the boundary nodes of each area and if it
		!     finds i1 and i2 then it inserts node new in the area boundary list and
		!     sets the node new any_bndy flag. if area 0 or hole then sets the node
		!     new out_bndy flag.
		!
		call clear(node(new),any_bndy)
		call clear(node(new),out_bndy)
		call set(node(new),b_2nd_type)

		k=0
		found=.false.
		20      continue  ! loop over outer boundary
			if(i1.eq.obn(k) .and. i2.eq.obn(k+1) .or. i2.eq.obn(k) .and. i1.eq.obn(k+1)) then

				call reallocate_outer_boundary_arrays(onbn+1)
				if(ierr /= 0) return

				found=.true.
				! update outer boundary flag and boundary condition if necessary
				if(j.eq.0) then
					call set(node(new),out_bndy)
				elseif(hole(j)) then
					call set(node(new),out_bndy)
				endif
				call set(node(new),any_bndy)
				! insert it in the list
				do i=onbn,k+1,-1
					obn(i+1)=obn(i)
				end do
				onbn=onbn+1
				obn(k+1)=new
			endif
			k=k+1
		if(k.lt.onbn .and. .not. found) goto 20

		do j=1,area
			k=0
			found=.false.
			30      continue  ! loop over area j boundary
				if(i1.eq.bn(j,k) .and. i2.eq.bn(j,k+1) .or. i2.eq.bn(j,k) .and. i1.eq.bn(j,k+1)) then

					call reallocate_boundary_arrays(j,nbn(j)+1)
					if(ierr /= 0) return

					found=.true.
					! update outer boundary flag and boundary condition if necessary
					if(hole(j)) then
						call set(node(new),out_bndy)
					endif
					call set(node(new),any_bndy)
					! insert it in the list
					do i=nbn(j),k+1,-1
						bn(j,i+1)=bn(j,i)
					end do
					nbn(j)=nbn(j)+1
					bn(j,k+1)=new
				endif
				k=k+1
			if(k.lt.nbn(j) .and. .not. found) goto 30
		end do

	end subroutine update_bnodes
	!----------------------------------------------------------------------
	  subroutine propose_drop_node(nd1,nd2, dropped)
		implicit none

		integer(i4) :: i, j, k, nd1, nd2, pos_1, pos_2

		logical has_1, has_2
		logical :: dropped

		dropped=.false.

		! first check outer boundary with 3 boundary nodes for the case where both nodes are present
		if(onbn == 3) then
			has_1=.false.
			has_2=.false.
			do i=1,onbn
				if (obn(i).eq.nd1) then
					has_1=.true.
				endif
				if (obn(i).eq.nd2) then
					has_2=.true.
				endif
			end do

			if(has_1 .and. has_2) then
				return
			endif
		endif


		! first check all areas with 3 boundary nodes for the case where both nodes are present
		do k=1,area
			if(nbn(k) == 3) then
				has_1=.false.
				has_2=.false.
				do i=1,nbn(k)
					if (bn(k,i).eq.nd1) then
						has_1=.true.
					endif
					if (bn(k,i).eq.nd2) then
						has_2=.true.
					endif
				end do

				if(has_1 .and. has_2) then
					return
				endif
			endif
		end do

		do k=1,area
			has_1=.false.
			has_2=.false.
			do i=1,nbn(k)
				if (bn(k,i).eq.nd1) then
					has_1=.true.
					pos_1=i
				endif
				if (bn(k,i).eq.nd2) then
					has_2=.true.
					pos_2=i
				endif
			end do


			if(has_1 .and. has_2) then   ! Has both so delete nd2 from list and reduce nbn(k) by 1
				if(pos_2 < nbn(k)) then
					do j=pos_2,nbn(k)-1
						bn(k,j)=bn(k,j+1)
					end do
				endif
				bn(k,nbn(k))=0  ! zero old entry
				nbn(k)=nbn(k)-1
				bn(k,0)=bn(k,nbn(k))
			else if(has_1 .and. .not. has_2) then
				! keeping nd1 so do nothing
			else if(has_2 .and. .not. has_1) then
				bn(k,pos_2)=nd1  ! replace nd2 with nd1 in boundary list
			endif
               
		end do

		! node nd1 is moved to the midpoint of nd1--nd2
		x(nd1)=(x(nd1)+x(nd2))/2.
		y(nd1)=(y(nd1)+y(nd2))/2.

	
		! node nd2 is now moved to nn 
		x(nd2)=x(nn)
		y(nd2)=y(nn)
	
		! if an area boundary list contained node nn, it is now renumbered to nd2
		do k=1,area
			do i=0,nbn(k)
				if (bn(k,i).eq.nn) then 
					bn(k,i)=nd2         ! update boundary node list
				endif
			end do
		end do      


		has_1=.false.
		has_2=.false.
		do i=1,onbn
			if (obn(i).eq.nd1) then
				has_1=.true.
				pos_1=i
			endif
			if (obn(i).eq.nd2) then
				has_2=.true.
				pos_2=i
			endif
		end do


		if(has_1 .and. has_2) then   ! Has both so delete nd2 from list and reduce nbn(k) by 1
			if(pos_2 < onbn) then
				do j=pos_2,onbn-1
					obn(j)=obn(j+1)
				end do
			endif
			obn(onbn)=0  ! zero old entry
			onbn=onbn-1
			obn(0)=obn(onbn)
		else if(has_1 .and. .not. has_2) then
			! keeping nd1 so do nothing
		else if(has_2 .and. .not. has_1) then
			obn(pos_2)=nd1  ! replace nd2 with nd1 in boundary list
		endif


		! if outer boundary list contained node nn, it is now renumbered to nd2
		do i=0,onbn
			if (obn(i).eq.nn) then 
				obn(i)=nd2         ! update boundary node list
			endif
		end do

		dropped=.true.
		nn=nn-1

	end subroutine propose_drop_node
	!----------------------------------------------------------------------
	subroutine shuffle_arrays(jnt)
		implicit none

		integer(i4) :: i,j
		real, allocatable :: rdum(:)
		integer(i4), allocatable :: ldum(:),jnt(:),ndummy(:,:), bdummy(:,:)

		allocate(rdum(nn),ldum(nn),jnt(0:nn),ndummy(ne,3),bdummy(0:area,0:nb_cur), stat=ialloc)
		call check_alloc(ialloc,'shuffle_arrays work arrays')


		jnt(0)=0
		do i=1,nn
			rdum(jnt(i))=x(i)
		end do

		do i=1,nn
			x(i)=rdum(i)
		end do

		do i=1,nn
			rdum(jnt(i))=y(i)
		end do

		do i=1,nn
			y(i)=rdum(i)
		end do

		do i=1,nn
			ldum(jnt(i))=node(i)
		end do

		do i=1,nn
			node(i)=ldum(i)
		end do

		do i=1,ne
			ndummy(i,1)=jnt(in(i,1))
			ndummy(i,2)=jnt(in(i,2))
			ndummy(i,3)=jnt(in(i,3))
		end do

		do i=1,ne
			in(i,1)=ndummy(i,1)
			in(i,2)=ndummy(i,2)
			in(i,3)=ndummy(i,3)
		end do

		! outer boundary
		j=1
		do i=0,onbn
			bdummy(j,i)=jnt(obn(i))
		end do
		do i=0,onbn
			obn(i)=bdummy(j,i)
		end do

		do j=1,area
			do i=0,nbn(j)
				bdummy(j,i)=jnt(bn(j,i))
			end do
			do i=0,nbn(j)
				bn(j,i)=bdummy(j,i)
			end do
		end do

		deallocate(rdum,ldum,jnt,ndummy,bdummy)

	end subroutine shuffle_arrays
	!----------------------------------------------------------------------
	subroutine fix_short_segs_batch
		implicit none		

		integer(i4) :: i, nd1, nd2, pass, nseg_old
		real(dp) :: seg_l
		logical :: dropped
		character*60 :: pass_msg

		logical,allocatable :: n_dropped(:)   ! node has been dropped

		allocate(n_dropped(nn),stat=ialloc)
		call check_alloc(ialloc,'fix_short_segs_batch work array')  

		pass=0
		nseg_old=0
		outer_loop: do 
			n_dropped(:)=.false.
			call calc_segments
			if(nseg==nseg_old) exit outer_loop

			nseg_old=nseg
			pass=pass+1
			write(pass_msg,'(a,i4,a)') 'Remove short segments, pass ',pass,'...' 
			call Msg(pass_msg)
			do i=1,nseg
				nd1=seg_node(i,1)
				nd2=seg_node(i,2)
				if(.not. n_dropped(nd1) .and. .not. n_dropped(nd2)) then  ! only do anything if neither node changed
					seg_l=sqrt((x(nd1)-x(nd2))**2+(y(nd1)-y(nd2))**2)
					if(seg_l.lt.segl_user) then
						call propose_drop_node(nd1,nd2,dropped)
						if(dropped) then
							n_dropped(nd2)=.true.
							n_dropped(nn+1)=.true.
						endif
					endif
				endif
			end do
		end do outer_loop

		deallocate(n_dropped)

	end subroutine fix_short_segs_batch
	!----------------------------------------------------------------------
	subroutine direct(xm,ym,x1,y1,x2,y2,in)
		implicit none

		logical :: in
		real(dp) :: xm,ym,x1,y1,x2,y2
		real(dp) :: f1

		in=.false.
		f1=(ym-y1)*(x2-x1)-(xm-x1)*(y2-y1)
		if(f1.gt.0.0) then
			in=.true.
		endif
	end subroutine direct
	!----------------------------------------------------------------------
	subroutine process_gen_file
		implicit none

		integer(i4) :: i, j, n_area
		integer(i4), allocatable :: jnt(:)   ! new node numbers i.e. jnt(10) = new node number for old node 10

		allocate(jnt(nn_in),stat=ialloc) 
		call check_alloc(ialloc,'process_gen_file work array')
		if(ierr /= 0) return

		jnt(:)=0  ! if not assigned yet

		area=0
		nn=0

		! Copy to dll outer boundary node list
		onbn=onbn_in
		call reallocate_outer_boundary_arrays(onbn)
		if(ierr /= 0) goto 1000
		do i=1,onbn_in
			call new_node(nn,x_in(obn_in(i)),y_in(obn_in(i)))  ! all outer boundary nodes are kept
			if(ierr /= 0) goto 1000
			call set(node(nn),out_bndy)
			call set(node(nn),any_bndy)
			jnt(obn_in(i))=nn 
			obn(i)=nn
		end do
		obn(0)=jnt(obn_in(0))


		! Form area boundaries from bn_in arrays
		call Msg('Processing polygons...')
		do n_area=1,area_in
			if(bn_in(n_area,0) /= 0) then   ! treat as a polygon
				area=area+1
				plot_area(area)=.true.
				nbn(area)=nbn_in(n_area)
				call reallocate_boundary_arrays(area,nbn(area))
				if(ierr /= 0) goto 1000
				do j=0,nbn_in(n_area)
				
					if(jnt(bn_in(area,j)) == 0) then   ! this node hasn't been assigned yet
						call new_node(nn,x_in(bn_in(area,j)),y_in(bn_in(area,j)))  ! keep this boundary node
						if(ierr /= 0) goto 1000
						call set(node(nn),out_bndy)
						call set(node(nn),any_bndy)
						jnt(bn_in(area,j))=nn 
						bn(area,j)=nn
					else
						bn(area,j)=jnt(bn_in(area,j))   ! re-assign new node number 
					endif
				end do
			endif
			elength(area)=elength_in(area)
			if(.not. plan_view) then
				y_elength(area)=y_elength_in(area)
			end if
			stretch_factor(area)=stretch_factor_in(area)
			ndrop_rate(area)=ndrop_rate_in(area)
		end do

		! treat the rest as cuts
		call msg('Processing addlines...')
		do n_area=1,area_in
			if(bn_in(n_area,0) == 0) then   ! treat as a cut (i.e. addline in feflow)
				call reallocate_cut_arrays(nbn_in(n_area))
				if(ierr /= 0) goto 1000
				do j=1,nbn_in(n_area)
					xcut(j)=x_in(bn_in(n_area,j))
					ycut(j)=y_in(bn_in(n_area,j))
				end do
				ncut=nbn_in(n_area)
				call add_cut
				if(ierr /= 0) goto 1000
			endif
		end do	
	
		1000 deallocate(jnt) 

	end subroutine process_gen_file
	!----------------------------------------------------------------------
	subroutine set_ob_dir
		implicit none

		integer(i4) :: i
		real(dp), allocatable :: dum_x(:),dum_y(:)
		real(dp) :: ang, tot_ang1, tot_ang2
	
		allocate(dum_x(nn), dum_y(nn), stat=ialloc)
		call check_alloc(ialloc,'set_ob_dir work arrays')
		if(ierr /= 0) return
	
		call int_angle(x(nn),y(nn),x(1),y(1),x(2),y(2),ang)
		tot_ang1=ang
		do i=2,nn-1
			call int_angle(x(i-1),y(i-1),x(i),y(i),x(i+1),y(i+1),ang)
			tot_ang1=tot_ang1+ang
		end do
		call int_angle(x(nn-1),y(nn-1),x(nn),y(nn),x(1),y(1),ang)
	
		call int_angle(x(1),y(1),x(nn),y(nn),x(nn-1),y(nn-1),ang)
		tot_ang2=ang
		do i=nn-1,2,-1
			call int_angle(x(i+1),y(i+1),x(i),y(i),x(i-1),y(i-1),ang)
			tot_ang2=tot_ang2+ang
		end do
		call int_angle(x(2),y(2),x(1),y(1),x(nn),y(nn),ang)
	
		if(tot_ang1.lt.tot_ang2) goto 1000
	
		do i=1,nn
			dum_x(i)=x(i)
			dum_y(i)=y(i)
		end do
	
		do i=1,nn
			x(i)=dum_x(nn-i+1)
			y(i)=dum_y(nn-i+1)
		end do
	
		1000 deallocate(dum_x, dum_y)

	end subroutine set_ob_dir
	!----------------------------------------------------------------------
	subroutine add_cut
		implicit none

		integer(i4) :: jj,ncut_new
		logical :: split

		10    call calc_segments
			if(ierr /= 0) return
			call analyze_cut(split)
			if(ierr /= 0) return
			if(n_int.gt.0) then
			call tie_in_cut
			if(ierr /= 0) return
			endif
			if(split) then
				xcut(1)=xi(n_int)
				ycut(1)=yi(n_int)
				ncut_new=1
				do jj=nct(n_int)+1,ncut
					ncut_new=ncut_new+1
					call reallocate_cut_arrays(ncut_new)
					if(ierr /= 0) return
					xcut(ncut_new)=xcut(jj)
					ycut(ncut_new)=ycut(jj)
				end do
				ncut=ncut_new
				goto 10
			endif

	end subroutine add_cut
	!----------------------------------------------------------------------
	subroutine update_area(i_int,tie_in_start,nds,tie_in_end,nde)
		implicit none

		integer(i4) :: i, k, i_int, nds, nde, nc1, nc2, nbn_old, ndum, old_nn, nn_s, nn_e, inc

		real(dp) :: rs1, rs2

		logical :: tie_in_start,tie_in_end

		integer(i4),allocatable :: ldum(:)
		allocate(ldum(0:nb_cur),stat=ialloc)
		call check_alloc(ialloc,'update_area work array')
		if(ierr /= 0) return

		!     This version only updates the nodes which make up the segments which
		!     are cut and adds the new segments formed by the cut.  The area
		!     boundary lists are formed once later.
		!
		k=ae(i_int-1)
		!
		!     If the intercept points are tied in to existing nodes, the variables
		!     nds and nde will contain the appropriate node numbers, and nothing
		!     needs to be done here.  If new nodes are being generated at each
		!     intercept, compute the new node number, assign the node coordinates
		!     and update the area boundary list.  The new numbers will be stored in
		!     nds and nde.
		if(.not. tie_in_start) then
			call update_area_boundary(k,usne(i_int-1),nds)
			if(ierr /= 0) goto 1000
		endif
		!
		if(.not. tie_in_end) then
			!     If the cut leaves and enters the boundary in the same segment then
			!     the usn dsn relationship of the second new node will be altered by the
			!     insertion of the first.  This is corrected here.
			if(nsg(i_int-1).eq.nsg(i_int)) then   ! same segment
				!       If first intercept is leaving an area with a
				!       lower number than it is entering, then the rs was calculated for
				!       the area leaving.  It should be corrected to apply to the area
				!       entering so that it is inserted properly in update area.
				if(al(i_int-1).gt.0 .and. al(i_int-1).lt.ae(i_int-1)) then
					rs1=1.-rs(i_int-1)
					rs2=1.-rs(i_int)
				else
					rs1=rs(i_int-1)
					rs2=rs(i_int)
				endif
				if(rs1.lt.rs2) then   ! nds upstream of nde
					call update_area_boundary(k,nds,nde)
					if(ierr /= 0) goto 1000
				else                        ! nde upstream of nds
					call update_area_boundary(k,usnl(i_int),nde)
					if(ierr /= 0) goto 1000
					usne(i_int)=nds
				endif
			else
				call update_area_boundary(k,usnl(i_int),nde)
				if(ierr /= 0) goto 1000
			endif
		endif
		!
		!     Make a copy of area k boundary list and save the position of the
		!     start (nc1) and end (nc2) nodes in the list.
		area=area+1
		call reallocate_boundary_arrays(area,1)
		if(ierr /= 0) goto 1000
		if(allocated(elength_in)) then
			if(area > area_in) then
				elength(area)=elength_in(1)
			else
				elength(area)=elength_in(area)
			endif
		endif
	!	elength(area)=elength_in(area)
		hole(area)=.false.
		plot_area(area)=.true.
		do i=0,nbn(k)
			ldum(i)=bn(k,i)
			if(bn(k,i).eq.nds) nc1=i
			if(bn(k,i).eq.nde) nc2=i
		end do
		nbn_old=nbn(k)
		!
		!     If the cut number nct is the same for the start and end intercepts then
		!     there are no intervening cut nodes and we simply split the old boundary
		!     list in two around the cut without generating any new nodes.
		!     Also, if the cut number of the start is 1 less than the cut number of
		!     the end and the rcut of the start is 1.0 (then they are essentially in
		!     the same cut) and we don't have to insert nodes.
		!
		if(nct(i_int-1).eq.nct(i_int) .or. (nct(i_int-1).eq.nct(i_int)-1 .and. abs(rc(i_int-1)-1.0).lt.1.e-37) ) then  ! no new nodes needed
			!       If nc1 is greater than nc2 then the cut enters the area downstream
			!       of the point of exit.  In this case swap the start and end node
			!       numbers.
			if(nc1.gt.nc2) then
				ndum=nc2
				nc2=nc1
				nc1=ndum
			endif
			!
			!       Copy the boundary portion from 1 to nds
			nbn(k)=0
			do i=1,nc1
				call reallocate_boundary_arrays(area,nbn(k))
				if(ierr /= 0) goto 1000
				nbn(k)=nbn(k)+1
				bn(k,nbn(k))=ldum(i)
			end do
			!       Copy the boundary portion from nde to the end
			do i=nc2,nbn_old
				call reallocate_boundary_arrays(area,nbn(k))
				if(ierr /= 0) goto 1000
				nbn(k)=nbn(k)+1
				bn(k,nbn(k))=ldum(i)
			end do
			bn(k,0)=ldum(nbn_old)
			!
			!       Create the new area by copying the nodes from nds to nde
			nbn(area)=0
			do i=nc1,nc2
				nbn(area)=nbn(area)+1
				bn(area,nbn(area))=ldum(i)
				!
				!     If the final intercept n_int is entering the original area k, we may
				!     have to modify update the information if it now enters the new area
				!     instead.
				if(ae(n_int).eq.k) then
					if(usnl(n_int).eq.bn(area,nbn(area))) ae(n_int)=area
				endif
			end do
			bn(area,0)=ldum(nc2)
			!
		else  ! new nodes needed
			old_nn=nn
			!       Define the coordinates of the new nodes
			do i=nct(i_int-1)+1,nct(i_int)
				call new_node(nn,xcut(i),ycut(i))
				if(ierr /= 0) goto 1000
			end do
			!       Store the start and end numbers of the new nodes
			nn_s=old_nn+1
			nn_e=nn
			!
			!       If nc1 is greater than nc2 then the cut enters the area downstream
			!       of the point of exit.  In this case swap the start and end node
			!       numbers and the counter direction inc.
			inc=1
			if(nc1.gt.nc2) then
				ndum=nc2
				nc2=nc1
				nc1=ndum
				nn_s=nn
				nn_e=old_nn+1
				inc=-1
			endif
			!
			!       Copy the boundary portion from 1 to nc1
			nbn(k)=0
			do i=1,nc1
				nbn(k)=nbn(k)+1
				bn(k,nbn(k))=ldum(i)
			end do
			!       Insert the new nodes
			do i=nn_s,nn_e,inc
				call reallocate_boundary_arrays(area,nbn(k))
				if(ierr /= 0) goto 1000
				nbn(k)=nbn(k)+1
				bn(k,nbn(k))=i
			end do
		
			!       Copy the boundary portion from nde to the end
			do  i=nc2,nbn_old
				call reallocate_boundary_arrays(area,nbn(k))
				if(ierr /= 0) goto 1000
				nbn(k)=nbn(k)+1
				bn(k,nbn(k))=ldum(i)
			end do
			bn(k,0)=ldum(nbn_old)
		
			! Copy the part of the new are from nc1 to nc2
			nbn(area)=0
			do i=nc1,nc2
				nbn(area)=nbn(area)+1
				bn(area,nbn(area))=ldum(i)
			
				! If the final intercept n_int is entering the original area k, we may
				! have to modify update the information if it now enters the new area
				! instead.
				if(ae(n_int).eq.k) then
					if(usnl(n_int).eq.bn(area,nbn(area))) ae(n_int)=area
				endif
			
			end do
			! Insert the new nodes
			do i=nn_e,nn_s,-inc
				nbn(area)=nbn(area)+1
				bn(area,nbn(area))=i
			end do
			bn(area,0)=nn_s
		endif

		1000 deallocate(ldum)
	
	end subroutine update_area
	!----------------------------------------------------------------------
	subroutine update_outer_area(i_int,tie_in_start,nds,tie_in_end,nde)
		implicit none

		integer(i4) :: k, i_int, nds, nde

		real(dp) :: rs1, rs2

		logical :: tie_in_start,tie_in_end

		integer(i4),allocatable :: ldum(:)
		allocate(ldum(0:nb_cur),stat=ialloc)
		call check_alloc(ialloc,'update_area work array')
		if(ierr /= 0) return

		!     This version only updates the nodes which make up the segments which
		!     are cut and adds the new segments formed by the cut.  The area
		!     boundary lists are formed once later.
		!
		k=ae(i_int-1)
		!
		!     If the intercept points are tied in to existing nodes, the variables
		!     nds and nde will contain the appropriate node numbers, and nothing
		!     needs to be done here.  If new nodes are being generated at each
		!     intercept, compute the new node number, assign the node coordinates
		!     and update the area boundary list.  The new numbers will be stored in
		!     nds and nde.
		if(.not. tie_in_start) then
			call update_outer_boundary(usne(i_int-1),nds)
			if(ierr /= 0) goto 1000
		endif
		!
		if(.not. tie_in_end) then
			!     If the cut leaves and enters the boundary in the same segment then
			!     the usn dsn relationship of the second new node will be altered by the
			!     insertion of the first.  This is corrected here.
			if(nsg(i_int-1).eq.nsg(i_int)) then   ! same segment
				!       If first intercept is leaving an area with a
				!       lower number than it is entering, then the rs was calculated for
				!       the area leaving.  It should be corrected to apply to the area
				!       entering so that it is inserted properly in update area.
				if(al(i_int-1).gt.0 .and. al(i_int-1).lt.ae(i_int-1)) then
					rs1=1.-rs(i_int-1)
					rs2=1.-rs(i_int)
				else
					rs1=rs(i_int-1)
					rs2=rs(i_int)
				endif
				if(rs1.lt.rs2) then   ! nds upstream of nde
					call update_outer_boundary(nds,nde)
					if(ierr /= 0) goto 1000
				else                        ! nde upstream of nds
					call update_outer_boundary(usnl(i_int),nde)
					if(ierr /= 0) goto 1000
					usne(i_int)=nds
				endif
			else
				call update_outer_boundary(usnl(i_int),nde)
				if(ierr /= 0) goto 1000
			endif
		endif

		1000 deallocate(ldum)
	
	end subroutine update_outer_area
	!----------------------------------------------------------------------
	subroutine calc_al_ae(i_int,skip)
		implicit none

		integer(i4) :: i, i_int, n0, n1, n2, n3, nc_ds
		real(dp) :: phi, phi2  
		logical :: entering,skip
		real(dp) :: xds,yds
		integer(i4) :: a1,a2
		!
		skip=.false.
		if(i_int.gt.1) then
			i=1
			10      continue
				if(abs(xi(i)-xi(i_int)).lt.1.e-37 .and. abs(yi(i)-yi(i_int)).lt.1.e-37) then
					skip=.true.
				endif
				i=i+1
			if(i.lt.i_int .and. .not. skip) goto 10
			if(skip) then
				return
			endif
		endif
		!
		!     Given the intercept number and info, this routine determines which
		!     area the segment containing the intercept is entering (ae) and which
		!     it is leaving (al)
		!
		!     These are the area and node numbers of the boundary segment we are
		!     testing
		a1=seg_area(nsg(i_int),1)
		a2=seg_area(nsg(i_int),2)
		n1=seg_node(nsg(i_int),1)
		n2=seg_node(nsg(i_int),2)
		!
		!     This is the cut node number downstream of the intercept (inside area)
		nc_ds=nct(i_int)+1
		!
		!     Simplest case is where  0.0 < rs and rs < 1.0
		if(rs(i_int).gt.0.0 .and. rs(i_int).lt.1.0) then
			call direct(xcut(nc_ds),ycut(nc_ds), x(n1),y(n1),x(n2),y(n2),entering)
			if(entering) then
				ae(i_int)=a1
				usne(i_int)=n1
				al(i_int)=a2
				usnl(i_int)=n2
			else
				ae(i_int)=a2
				usne(i_int)=n2
				al(i_int)=a1
				usnl(i_int)=n1
			endif
	
		else
			!
			!       Special case where the intercept hits an existing boundary node.
			!       Depending on where the cut hits the segment, we will get either the
			!       downstream node number n3 or the upstream node number n0.  We will
			!       then check the angles to see which area is being entered.  Have to
			!       watch out for a node which lies on 3 or more segments and be sure to
			!       get the proper node n0 or n3.
			!
			!       We also have to check if the cut segment r is 1.0.  In this case
			!       nc_ds and either n1 or n2 are the same.  We should calculate an x
			!       and y coordinate xds,yds of a point at an r=1.1 or so, which will be
			!       downstream on the cut.  Use this point for calcs.
			if(abs(rc(i_int)-1.0).lt.1.e-37) then
				xds=xcut(nct(i_int))*(-0.1)+xcut(nc_ds)*1.1
				yds=ycut(nct(i_int))*(-0.1)+ycut(nc_ds)*1.1
			else
				xds=xcut(nc_ds)
				yds=ycut(nc_ds)
			endif
			!
			if(abs(rs(i_int)-1.0).lt.1.e-37) then  ! get downstream node number n3
				call find_dsn(a1,n1,n2,n3)
				call int_angle(x(n1),y(n1),x(n2),y(n2),x(n3),y(n3),phi)
				call int_angle(x(n1),y(n1),x(n2),y(n2),xds,yds,phi2)
				if(phi2.lt.phi) then
					entering=.true.
				else
					entering=.false.
				endif
				if(entering) then
					ae(i_int)=a1
					usne(i_int)=n1
					al(i_int)=a2
					usnl(i_int)=n3
				else
					call find_dsn(a2,n1,n2,n3)
					call int_angle(x(n3),y(n3),x(n2),y(n2),x(n1),y(n1),phi)
					call int_angle(x(n3),y(n3),x(n2),y(n2),xds,yds,phi2)
					if(phi2.lt.phi) then
						entering=.true.
					else
						entering=.false.
					endif
					if(entering) then
						ae(i_int)=a2
						usne(i_int)=n3
						al(i_int)=a1
						usnl(i_int)=n2
					else
						skip=.true.
						return
					endif
				endif
			else                  ! get upstream node number n0
				call find_dsn(a1,n2,n1,n0)
				call int_angle(x(n0),y(n0),x(n1),y(n1),x(n2),y(n2),phi)
				call int_angle(x(n0),y(n0),x(n1),y(n1),xds,yds,phi2)
				if(phi2.lt.phi) then
					entering=.true.
				else
					entering=.false.
				endif
				if(entering) then
					ae(i_int)=a1
					usne(i_int)=n0
					al(i_int)=a2
					usnl(i_int)=n2
				else
					call find_dsn(a2,n2,n1,n0)
					call int_angle(x(n2),y(n2),x(n1),y(n1),x(n0),y(n0),phi)
					call int_angle(x(n2),y(n2),x(n1),y(n1),xds,yds,phi2)
					if(phi2.lt.phi) then
						entering=.true.
					else
						entering=.false.
					endif
					if(entering) then
						ae(i_int)=a2
						usne(i_int)=n2
						al(i_int)=a1
						usnl(i_int)=n0
					else
						skip=.true.
						return
					endif
				endif
			endif
		endif
		!
	end subroutine calc_al_ae
	!----------------------------------------------------------------------
	subroutine analyze_cut(split)
		implicit none

		integer(i4) :: i, j, k, ia, id, ig, ih, ii, ij
		real(dp) :: xc1, yc1, xc2, yc2
		real(dp) :: xs1, ys1, xs2, ys2
		real(dp) :: x_tmp, y_tmp
		real(dp) :: del, del2
		real(dp) :: rseg, rcut
		real(dp) :: b,c, e, f 

		logical :: extended,split,skip,a_repeat

		extended=.false.
		split=.false.
		! This subroutine computes all potential intercepts between the cut and the
		! area boundaries and puts them in order from start to end of cut
		! Restart here if cut must be extended
		11    continue
		!
		! Loop over the cut segment by segment
		n_int=0
		xi(:)=0.0
		yi(:)=0.0
		do i=1,ncut-1
			xc1=xcut(i)
			yc1=ycut(i)
			xc2=xcut(i+1)
			yc2=ycut(i+1)
			! Loop over the existing segments
			do j=1,nseg
				xs1=x(seg_node(j,1))
				ys1=y(seg_node(j,1))
				xs2=x(seg_node(j,2))
				ys2=y(seg_node(j,2))
				!
				!  The following 6 lines determine if the two segments intersect
				del=(xs1-xs2)*(yc2-yc1)-(ys1-ys2)*(xc2-xc1)
				del2=(xc1-xc2)*(ys2-ys1)-(yc1-yc2)*(xs2-xs1)
				if (abs(del).gt.0.0 .and. abs(del2).gt.0.0) then
					rseg=1.-((yc2-yc1)*(xc2-xs2)-(xc2-xc1)*(yc2-ys2))/del
					rcut=1.-((ys2-ys1)*(xs2-xc2)-(xs2-xs1)*(ys2-yc2))/del2
					if (rseg.ge.0.0  .AND. rseg.le.1.0 .AND. rcut.ge.0.0 .AND. rcut.le.1.0)    then
					
						! Skip this intercept if rcut=0.0 and it's not the first cut
						! segment.
						if(rcut.lt.1.e-10 .and. i.gt.1) exit
						!
						! They intersect, update the information for intercept n_int
						! where:
						!   xi  = x coordinate
						!   yi  = y coordinate
						!   nsg = boundary segment number
						!   rs  = fraction of boundary segment from start to intercept
						!   nct = cutting segment number
						!   rc  = fraction of cut segment from start to intercept
						!   al  = area # segment is leaving (calculated in calc_al_ae)
						!   ae  = area # segment is entering             "
						a_repeat=.false.            
						if(n_int > 0) then ! check for duplicate intercept 
							x_tmp=xs1*(1.0-rseg)+xs2*rseg
							y_tmp=ys1*(1.0-rseg)+ys2*rseg
							do k=1,n_int
								if(abs(x_tmp-xi(k)) < 1e-5 .and. abs(y_tmp-yi(k)< 1.e-5)) then
									a_repeat=.true.
								endif
							end do
						endif
						if(.not. a_repeat) then
							n_int=n_int+1
							xi(n_int)=xs1*(1.0-rseg)+xs2*rseg
							yi(n_int)=ys1*(1.0-rseg)+ys2*rseg
							nsg(n_int)=j
							rs(n_int)=rseg
							nct(n_int)=i
							rc(n_int)=rcut
							call calc_al_ae(n_int,skip)
							if(skip) n_int=n_int-1   ! abandon intercept
						endif
					endif
				endif
			end do
		end do
		!
		if(n_int.eq.0) then
			if(.not. extended) then
				call extend_cut_upstream
				if(ierr /= 0) return
				call extend_cut_downstream
				if(ierr /= 0) return
				extended=.true.
				goto 11
			else
				return
			endif
		else
			if(nct(1).ne.1 .and. al(1).ne.0) then
				call extend_cut_upstream
				if(ierr /= 0) return
				goto 11
			endif
			if(nct(n_int).ne.ncut-1 .and. ae(n_int).ne.0) then
				call extend_cut_downstream
				if(ierr /= 0) return
				goto 11
			endif
		endif
		!
		! sort the intercepts from start of cut to end of cut
		do j=2,n_int
			ia=nct(j)
			b=xi(j)
			c=yi(j)
			id=nsg(j)
			e=rc(j)
			f=rs(j)
			ig=ae(j)
			ih=usne(j)
			ii=al(j)
			ij=usnl(j)
			do i=j-1,1,-1
				if(nct(i).lt.ia .or. nct(i).eq.ia .and. rc(i).lt.e) goto 32
				nct(i+1)=nct(i)
				rs(i+1)=rs(i)
				xi(i+1)=xi(i)
				yi(i+1)=yi(i)
				nsg(i+1)=nsg(i)
				rc(i+1)=rc(i)
				ae(i+1)=ae(i)
				usne(i+1)=usne(i)
				al(i+1)=al(i)
				usnl(i+1)=usnl(i)
			end do
			i=0
			32      nct(i+1)=ia
			xi(i+1)=b
			yi(i+1)=c
			nsg(i+1)=id
			rc(i+1)=e
			rs(i+1)=f
			ae(i+1)=ig
			usne(i+1)=ih
			al(i+1)=ii
			usnl(i+1)=ij
		end do
	
		do i=1,n_int
			if(i.gt.1 .and. i.lt.n_int) then
				do j=1,i-1
					if(ae(i).eq.ae(j)) then
						!              call range_error('An area may only be crossed once',
						!     &         'with a single cut. ','Truncating invalid portion.')
						n_int=i
						split=.true.
						goto 90
					endif
				end do
			endif
		end do
	
		90    continue
	
	end subroutine analyze_cut
	!----------------------------------------------------------------------
	subroutine extend_cut_downstream
		implicit none

		integer(i4) :: j
		real(dp) :: rext
		real(dp) :: xc1, yc1, xc2, yc2
		real(dp) :: xs1, ys1, xs2, ys2
		real(dp) :: del, del2
		real(dp) :: rseg, rtmp

		!       There are nodes downstream of the last intercept which are inside
		!       the outer boundary but which will be lost unless the cut is extended
		!       Make room for a new cut node
		! Loop over the existing segments to find nearest intercept to 2nd cut node
		rext=1.e20
		xc1=xcut(ncut-1)
		yc1=ycut(ncut-1)
		xc2=xcut(ncut)
		yc2=ycut(ncut)
		do j=1,nseg
			xs1=x(seg_node(j,1))
			ys1=y(seg_node(j,1))
			xs2=x(seg_node(j,2))
			ys2=y(seg_node(j,2))
			!  The following 6 lines determine if the two segments intersect
			del=(xs1-xs2)*(yc2-yc1)-(ys1-ys2)*(xc2-xc1)
			del2=(xc1-xc2)*(ys2-ys1)-(yc1-yc2)*(xs2-xs1)
			if (abs(del).gt.0.0 .and. abs(del2).gt.0.0) then
				rseg=1.-((yc2-yc1)*(xc2-xs2)-(xc2-xc1)*(yc2-ys2))/del
				rtmp=1.-((ys2-ys1)*(xs2-xc2)-(xs2-xs1)*(ys2-yc2))/del2
				if (rseg.ge.0.0  .AND. rseg.le.1.0 .AND. rtmp.lt.rext .AND. rtmp.gt.0.0)    then
					rext=rtmp+.01
				endif
			endif
		end do
		!       Establish the new node way out there
		ncut=ncut+1
		call reallocate_cut_arrays(ncut)
		if(ierr /= 0) return
		xcut(ncut)=xc1*(1.0-rext)+xc2*rext
		ycut(ncut)=yc1*(1.0-rext)+yc2*rext

	end subroutine extend_cut_downstream
	!----------------------------------------------------------------------
	subroutine extend_cut_upstream
		implicit none

		integer(i4) :: j
		real(dp) :: rext
		real(dp) :: xc1, yc1, xc2, yc2
		real(dp) :: xs1, ys1, xs2, ys2
		real(dp) :: del, del2
		real(dp) :: rseg, rtmp
	
		!       There are nodes upstream of the first intercept which are inside
		!       the outer boundary but which will be lost unless the cut is extended
		!       Make room for a new cut node
		! Loop over the existing segments to find nearest intercept to 2nd cut node
		rext=-1.e20
		xc1=xcut(1)
		yc1=ycut(1)
		xc2=xcut(2)
		yc2=ycut(2)
		do j=1,nseg
			xs1=x(seg_node(j,1))
			ys1=y(seg_node(j,1))
			xs2=x(seg_node(j,2))
			ys2=y(seg_node(j,2))
			!  The following 6 lines determine if the two segments intersect
			del=(xs1-xs2)*(yc2-yc1)-(ys1-ys2)*(xc2-xc1)
			del2=(xc1-xc2)*(ys2-ys1)-(yc1-yc2)*(xs2-xs1)
			if (abs(del).gt.0.0 .and. abs(del2).gt.0.0) then
				rseg=1.-((yc2-yc1)*(xc2-xs2)-(xc2-xc1)*(yc2-ys2))/del
				rtmp=1.-((ys2-ys1)*(xs2-xc2)-(xs2-xs1)*(ys2-yc2))/del2
				if (rseg.ge.0.0  .AND. rseg.le.1.0 .AND. rtmp.gt.rext .AND. rtmp.lt.1.0)    then
					rext=rtmp-.01
				endif
			endif
		end do

		do j=ncut,1,-1
			xcut(j+1)=xcut(j)
			ycut(j+1)=ycut(j)
		end do

		xcut(1)=xc1*(1.0-rext)+xc2*rext
		ycut(1)=yc1*(1.0-rext)+yc2*rext
		ncut=ncut+1
		call reallocate_cut_arrays(ncut)
		if(ierr /= 0) return

	end subroutine extend_cut_upstream
	!----------------------------------------------------------------------
	subroutine tie_in_cut
		implicit none

		integer(i4) :: i, nds, nde

		logical :: tie_in_start,tie_in_end
	
		!  This routine determines whether or not to create a new node at the
		!  intercept or tie in to an existing node.
		!
		call check_tie_in(xi(1),yi(1),tie_in_start,nds)
		if(ierr /= 0) return
		if(.not. tie_in_start) then
			if(al(1).eq.0) then
				call update_outer_boundary(usne(1),nds)
				if(ierr /= 0) return
			else
				call update_area_boundary(al(1),usnl(1),nds)
				if(ierr /= 0) return
			endif
		endif

		if(n_int.ge.2) then
			do i=2,n_int
				call check_tie_in(xi(i),yi(i),tie_in_end,nde)
				if(ierr /= 0) return
				if(ae(i-1).eq.0) then   ! outer boundary
					call update_outer_area(i,tie_in_start,nds,tie_in_end,nde)
					if(ierr /= 0) return
				else
					call update_area(i,tie_in_start,nds,tie_in_end,nde)
					if(ierr /= 0) return
				endif
				tie_in_start=tie_in_end
				nds=nde
			end do
		else
			nde=nds
		endif

		if(.not. tie_in_end) then
			if(ae(n_int).eq.0) then
				call update_outer_boundary(usnl(n_int),nde)
				if(ierr /= 0) return
			else
				call update_area_boundary(ae(n_int),usne(n_int),nde)
				if(ierr /= 0) return
			endif
		endif

	end subroutine tie_in_cut
	!----------------------------------------------------------------------
	subroutine check_tie_in(xtie,ytie,tie_in,p_node)
		implicit none

		logical tie_in
		integer(i4) p_node
		real*8 xtie,ytie,difx,dify
	
		! Find closest node and calculate distances
		call gen_find_node(xtie,ytie,p_node)
		difx=abs(xtie-x(p_node))
		dify=abs(ytie-y(p_node))
	
		if (difx.lt.1.e-5 .and. dify.lt.1.e-5) then ! node exists
			tie_in=.true.
		else
			tie_in=.false.
			call new_node(nn,xtie,ytie)
			if(ierr /= 0) return
			p_node=nn
		endif
	
	end subroutine check_tie_in
	!----------------------------------------------------------------------
	subroutine find_dsn(karea,nopp,nhere,nfind)
		implicit none

		integer(i4) :: j, n1, n2, nhere, nopp, karea, nfind

		!     This subroutine finds the node number nfind of the segment which is
		!     connected to the segment defined by nodes nopp and nhere, nopp being
		!     the node on the opposite end.  We have to make sure one of the area
		!     numbers of the found segment matches area to rule out segments that
		!     join this node but do not bound the area of interest, which could
		!     happen if three or more segments meet at a node.
		logical :: found
	
		found=.false.
		j=1
		10    continue
			n1=seg_node(j,1)
			n2=seg_node(j,2)
			if (n1.eq.nhere) then
				if(n2.ne.nopp) then
					if(seg_area(j,1).eq.karea .or. seg_area(j,2).eq.karea) then
						nfind=n2
						goto 20
					endif
				endif
			endif
			if (n2.eq.nhere) then
				if(n1.ne.nopp) then
					if(seg_area(j,1).eq.karea .or. seg_area(j,2).eq.karea) then
						nfind=n1
						goto 20
					endif
				endif
			endif
			j=j+1
		if(j.le.nseg) goto 10
	
		20  continue  
	
	end subroutine find_dsn
	!----------------------------------------------------------------------
	subroutine calc_segments
		implicit none

		integer(i4) :: i, j, k, n1, n2 

		logical prev_def

		!     Form a list of unique boundary segment pairs.  These pairs have the
		!     following properties:
		!        (1) seg_node(i,1) is upstream node for seg_area(i,1)
		!            seg_node(i,2) is upstream node for seg_area(i,2)
		!        (2) if seg_area(i,2) is 0 then this is an outer boundary segment

		nseg=0
		seg_node(:,:)=0
		seg_area(:,:)=0

		call Msg('Calculating segment information...')
		do k=1,area
			i=0
			30      continue  ! loop over area segments
				n1=bn(k,i)
				n2=bn(k,i+1)
				if(n1==0 .or. n2==0) then
					ierr=6
					write(log_msg,'(a,i10)') 'A node with incidence zero arose'
					call Msg(log_msg)
					return
				endif
				prev_def=.false.
				j=1
				40        continue
					if (n1.eq.seg_node(j,2) .and. n2.eq.seg_node(j,1)) then
						prev_def=.true.
						seg_area(j,2)=k
					endif
					j=j+1
				if(.not. prev_def .and. j.le.nseg) goto 40
				if (.not. prev_def) then
					call new_segment(nseg,n1,n2,k,0,0,0)
					if(ierr /= 0) return
				endif
				i=i+1
			if(i.lt.nbn(k)) goto 30
		end do

	end subroutine calc_segments
	!----------------------------------------------------------------------
	subroutine gen_find_node(x1,y1,p_node)
		implicit none

		integer(i4) :: i, p_node
		real(dp) :: x1,y1, dist_min, f1

		i=1
		dist_min=1.0e20
		97    continue
			f1=sqrt((x1-x(i))**2+((y1-y(i)))**2)
			!f1=sqrt((x1-x(i))**2+((y1-y(i))*scr_ratio)**2)
			if(f1.lt.dist_min) then
				p_node=i
				dist_min=f1
			endif
			i=i+1
		if (i.LE.nn) goto 97

	end subroutine gen_find_node

    !------------------------------------------------------------------------------------------------------
    function in_ob(px,py)
        implicit none

        LOGICAL :: in_ob

        integer :: i, cn, n
        real(dp) :: px,py, vt

        cn = 0  ! the crossing number counter

        do i=0,onbn-1
           if (((y(obn(i)) <= py) .and. (y(obn(i+1)) > py))   & ! an upward crossing
            .or.  ((y(obn(i)) > py) .and. (y(obn(i+1)) <= py)))  then ! a downward crossing
                ! compute the actual edge-ray intersect x-coordinate
                vt = (py - y(obn(i))) / (y(obn(i+1)) - y(obn(i)))
                if (px < x(obn(i)) + vt * (x(obn(i+1)) - x(obn(i)))) then ! px < intersect
                    cn=cn+1   ! a valid crossing of y=P.y right of P.x
                endif
            end if
        end do

        n=mod(cn,2)
        if(n==1) then
            in_ob=.true.
        else
            in_ob=.false.
        endif

    end function in_ob

   


end module gb
