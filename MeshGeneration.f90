Module MeshGen
    use GeneralRoutines
    use NumericalMesh
    use Tecplot
    implicit none
        
    logical :: NeedMeshName=.false. ! flag to indicate if GridBuilder name is needed


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
        
        
        if(NeedMeshName) then
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
        GB_TRI_2D%Element(:)%Typ='triangle'
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
    subroutine TriangularElementProperties(GB_TRI_2D)
        implicit none
        type(mesh) GB_TRI_2D
        
        integer(i4) :: i,j
        real(dp) :: x(3),y(3)
        real(dp) :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3), xyTotalArea
        
        xyTotalArea=0.0
            
        do i=1,GB_TRI_2D%nElements
            ! xc and yc from circumcircles
            if(GB_TRI_2D%nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
            do j=1,GB_TRI_2D%nNodesPerElement
                x(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%x
                y(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%y
            end do
            call InnerCircle(x,y,GB_TRI_2D%Element(i)%xyArea,xc,yc,GB_TRI_2D%Element(i)%rCircle,lseg,aseg,dseg)
            xyTotalArea=xyTotalArea+GB_TRI_2D%Element(i)%xyArea
            
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
        
        write(TMPStr,'(a,'//FMT_R4//')') 'Triangular mesh area:',xyTotalArea
        call Msg(TMPStr)

    end subroutine TriangularElementProperties

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
        call Msg(FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(LocalMesh%name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","Zone","xyArea","rCircle"'
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
    !-------------------------------------------------------------
    subroutine MeshToQGIS(LocalMesh)  ! write csv file with xyz coordinates for QGIS
        implicit none
        type(mesh) LocalMesh
        
        integer(i4) :: Fnum
        character(MAX_STR) :: FName
        integer(i4) :: i

        ! csv output file for nodes
        FName=trim(LocalMesh%name)//'_nodes.csv'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(FileCreateSTR//'QGIS csv node file: '//trim(FName))

        write(FNum,'(a)') 'x,y,z'
        do i=1,LocalMesh%nNodes
            write(FNum,'('//FMT_R8//',a,'//FMT_R8//',a,'//FMT_R8//')')LocalMesh%node(i)%x,', ',LocalMesh%node(i)%y,', ',LocalMesh%node(i)%z
        end do
       
        call FreeUnit(FNum)
        
        ! csv output file for elements
        FName=trim(LocalMesh%name)//'_elements.csv'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(FileCreateSTR//'QGIS csv cell file: '//trim(FName))

        write(FNum,'(a)') 'x,y,z'
        do i=1,LocalMesh%nElements
            write(FNum,'('//FMT_R8//',a,'//FMT_R8//',a,'//FMT_R8//')')LocalMesh%element(i)%x,', ',LocalMesh%element(i)%y,', ',LocalMesh%element(i)%z
        end do
       
        call FreeUnit(FNum)
        
    end subroutine MeshToQGIS
    
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
        if(NeedMeshName) then
            read(FNumMut,'(a80)') TmpSTR
            SEG_3D%Name=TmpSTR
            call Msg('Name: '//trim(SEG_3D%Name))
        else
            SEG_3D%Name='TMPLT'
        end if

        SEG_3D%nNodesPerElement=2
        SEG_3D%Element(:)%Typ='segment'
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

        call Msg('                X                Y                Z')

	    do i=1,2
			read(FNumMUT,*) xp(i),yp(i),zp(i)
            write(TmpSTR,'(i8,2x,5('//FMT_R8//'),a)') i,xp(i),yp(i),zp(i),'     '//TRIM(UnitsOfLength)
            call Msg(trim(TmpSTR))
        end do 
        
 		read(FNumMUT,*) nElements
        write(TmpSTR,'(a, i8)') 'Number of new segment elements: ',nElements 
        call Msg(trim(TmpSTR))
        
        
        TotalLength=sqrt((xp(1) - xp(2))**2 + (yp(1) - yp(2))**2 + (zp(1) - zp(2))**2)
        write(TmpSTR,'(a, '//FMT_R8//')') 'Total Length of new CLN: ',TotalLength 
        call Msg(trim(TmpSTR))
        if(TotalLength < 0.0001) then
            call Msg('NOTE: Total Length of new CLN is less than 0.0001')
        else if(TotalLength < 1e-10) then   
            call ErrMsg('Total Length of new CLN is essentially zero')
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
        write(TmpSTR,'(a,i8)')    'Number of nodes         ',SEG_3D%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    'Number of elements      ',SEG_3D%nElements
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
        if(NeedMeshName) then
            read(FNumMut,'(a80)') TmpSTR
            U_RECT_2D%Name=TmpSTR
            call Msg('Name: '//trim(U_RECT_2D%Name))
        else
            U_RECT_2D%Name='TMPLT'
        end if

        U_RECT_2D%nNodesPerElement=4
        U_RECT_2D%Element(:)%Typ='rectangle'
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

        read(FNumMUT,*) yl, nby, yOffset
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
        write(TMPStr,'(a,i9)')      'Number of X-coordinates ',nx
        call Msg(TMPStr)
        allocate(xi(nx),stat=ialloc)
        call AllocChk(ialloc,'Gen_v_rects xi array')
        xi = 0.0d0 
        read(FNum,*) (xi(i),i=1,nx)

        read(FNum,*) ny
        write(TMPStr,'(a,i9)')      'Number of Y-coordinates ',ny
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
        write(TmpSTR,'(a,i8)')    'Number of nodes         ',TMPLT%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    'Number of elements      ',TMPLT%nElements
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
                call Msg('end new layer')
                exit read_layer_instructions
            else
                call Msg(instruction)
            end if

            if(index(instruction,layer_name_cmd) /=0) then
                read(FNumMUT,'(a)') layer_name(nlayers)
                call Msg(layer_name(nlayers))

            elseif(index(instruction,minimum_layer_thickness_cmd) /=0) then
			    minimum_layer_thickness = .true.
			    read(FNumMUT,*) z_added
			    write(TmpSTR,'('//FMT_R8//',a)') z_added,'     '//TRIM(UnitsOfLength)
			    call Msg('Enforce minimum layer thickness of '//trim(TmpSTR))

            elseif(index(instruction,offset_base_cmd) /=0) then
			    offset_base = .true.
			    read(FNumMUT,*) base_offset
			    write(TmpSTR,'('//FMT_R8//',a)') base_offset,'     '//TRIM(UnitsOfLength)
			    call Msg('Offset layer base by '//trim(TmpSTR))

            elseif(index(instruction,uniform_sublayers_cmd) /=0) then
			    read(FNumMUT,*) nsublayer(nlayers)
			    nz_temp = nz_temp + nsublayer(nlayers) 		
			    !call user_size_check(nz_temp,user_nz,user_nz_str)
			    write(TmpSTR,'(i4)') nsublayer(nlayers)
			    call Msg('Number of uniform sublayers '//trim(TmpSTR))

            elseif(index(instruction,proportional_sublayers_cmd) /=0) then
			    proportional_sublayering=.true.
			    read(FNumMUT,*) nsublayer(nlayers)
			    nz_temp = nz_temp + nsublayer(nlayers) 		
			    !call user_size_check(nz_temp,user_nz,user_nz_str)
			    write(TmpSTR,'(i4)') nsublayer(nlayers)
			    call Msg('Number of proportional sublayers '//trim(TmpSTR))

                allocate(sub_thick(nsublayer(nlayers)),stat=ialloc)
                call AllocChk(ialloc,'new_layer proportional sublayering array')
                sub_thick(: )= 0.0d0
                tot_thick=0.0
                do j=1,nsublayer(nlayers)
                    read(FNumMUT,*) sub_thick(j)
                    tot_thick=tot_thick+sub_thick(j)
                end do
                call Msg(' Sub#   Thickness       Fraction')

                do j=1,nsublayer(nlayers)
                    sub_thick(j)=sub_thick(j)/tot_thick
			        write(TmpSTR,'(i4,2('//FMT_R8//'))') j,sub_thick(j)*tot_thick,sub_thick(j)
                    call Msg(trim(TmpSTR))
                end do

            elseif(index(instruction,constant_elevation_cmd) /=0) then
			    read(FNumMUT,*) base_elev(1)
			    write(TmpSTR,'('//FMT_R8//',a)') base_elev(1),'     '//TRIM(UnitsOfLength)
                call Msg('Layer base elevation '//TRIM(TmpSTR))
			    do j=2,TMPLT%nNodes
				    base_elev(j)=base_elev(1)
			    end do


            elseif(index(instruction,gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg('Base elevation from '//trim(basefile))
                call Msg('Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(basefile,base_elev,TMPLT%nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') basefile
			    call Msg('Base elevation from '//trim(basefile))
                call Msg('Assumed units of length are '//TRIM(UnitsOfLength))
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
			    call ErrMsg('Unrecognized instruction: new layer')
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
        character(MAX_INST) :: ElevationFromQgisCSVFile_cmd		=   'elevation from qgis csv file'
        !character(MAX_INST) :: gms_file_elevation_cmd		=   'elevation from gms file'
        !character(MAX_INST) :: raster_file_elevation_cmd		=   'elevation from raster file'
        character(MAX_INST) :: bilinear_function_elevation_cmd=   'elevation from bilinear function in xy'
        !character(MAX_INST) :: sine_function_elevation_cmd	=   'elevation from sine function in xy'
        !character(MAX_INST) :: cosine_function_elevation_cmd	=   'elevation from cosine function in xy'
        
        integer(i4) :: FNumMUT
        type (mesh) TMPLT
        
        integer(i4) :: FnumTop
		character(MAX_STR) :: FNameTop


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
                call Msg('end top elevation')
                exit read_top
            else
                call LwrCse(instruction)
                call Msg(instruction)
            end if    

            if(index(instruction,constant_elevation_cmd) /=0) then
                read(FNumMUT,*) top_elev(1)
                write(TmpSTR,'(2('//FMT_R8//'),a)') top_elev(1),'     '//UnitsOfLength
                call Msg(trim(TmpSTR))
                do j=2,TMPLT%nNodes
	                top_elev(j)=top_elev(1)
                end do

            elseif(index(instruction, offset_top_cmd) /=0) then
                offset_top = .true.
                read(FNumMUT,*) top_offset
			    write(TmpSTR,'(2('//FMT_R8//'))') top_offset
                call Msg(trim(TmpSTR))

            elseif(index(instruction, gb_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg('Top elevation from '//trim(topfile))
                call Msg('Assumed units of length are '//TRIM(UnitsOfLength))
                call read_gb_nprop(topfile,top_elev,TMPLT%nNodes)

            elseif(index(instruction,list_file_elevation_cmd) /=0) then
			    read(FNumMUT,'(a)') topfile
			    call Msg('Top elevation from '//trim(topfile))
                call Msg('Assumed units of length are '//TRIM(UnitsOfLength))
                call list_file_elevation(topfile,top_elev,TMPLT%nNodes)
                
            elseif(index(instruction, xz_pairs_elevation_cmd) /=0) then
                call xz_pairs_elevation(FNumMUT,top_elev,TMPLT)
            
            elseif(index(instruction, ElevationFromQgisCSVFile_cmd) /=0) then
                read(FNumMUT,'(a)') FNameTop 
                inquire(file=FNameTop,exist=FileExists)
                if(.not. FileExists) then
			        call ErrMsg('File not found: '//trim(FNameTop))
                endif
                
                call Msg(FileReadSTR//'Top_elevation: QGIS CSV file: '//trim(FNameTop))

		        call OpenAscii(FnumTop,FNameTop)

                call ReadDRealArray(FnumTop,top_elev)
                
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
			    call ErrMsg('Unrecognized instruction: top elevation')
            end if

        end do read_top
    
	    if(offset_top) then
            do j=1,TMPLT%nNodes
                top_elev(j)=top_elev(j)+top_offset
		    end do

	    end if
    end subroutine top_elevation
    
    !----------------------------------------------------------------------
    subroutine xyzFromList(FNum,xi,yi,zi,nPoints)
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


        call Msg('                X         Y        Z')

	    nPoints=0
	    do
		    read(FNum,'(a)',iostat=status) instruction
		    if(status /= 0) exit

		    call LwrCse(instruction)

		    if(index(instruction,'end') > 0) then
                call Msg('end xyz list of points')
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
                call Msg(trim(TmpSTR))

		    endif
        end do
        
       
        continue

    end subroutine xyzFromList

    !----------------------------------------------------------------------
    subroutine bilinear_function_in_xy(FNumMUT,nprop,TMPLT)
        implicit none
        
        integer(i4) :: FNumMUT
        type (mesh) TMPLT

        integer(i4) :: i
	    real(dp) :: xf2d, xt2d, yf2d, yt2d, a1, a2, a3, a4, a5

        real(dp) :: nprop(TMPLT%nNodes)

	    read(FNumMUT,*) xf2d, xt2d, yf2d, yt2d
	    call Msg('Bilinear function for the following range:')
	    write(TMPSTR,'(a,'//FMT_R8//',a,'//FMT_R8//',a)') 'X from ',xf2d,' to ',xt2d,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a,'//FMT_R8//',a)') 'Y from ',yf2d,' to ',yt2d,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    call Msg('z = z1 + slope_x*(x-xfrom) + curve_x*(x-xfrom)**2 + slope_y*(y-yfrom) + curve_y*(y-yfrom)**2 ')
	    read(FNumMUT,*) a1,a2,a3,a4,a5
	    write(TMPSTR,'(a,'//FMT_R8//',a)') 'Where:     z1 = ',a1,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      slope_x = ',a2,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      curve_x = ',a3,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      slope_y = ',a4,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
	    write(TMPSTR,'(a,'//FMT_R8//',a)') '      curve_y = ',a5,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
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


        call Msg('                X                Z')

	    npairs=0
	    read_xz_pairs:do
		    read(FNum,'(a)',iostat=status) instruction
		    if(status /= 0) exit

		    len=len_trim(instruction)
            call LwrCse(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg('end elevation from xz pairs')
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
                call Msg(trim(TmpSTR))

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
    
