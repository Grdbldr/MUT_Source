Module MeshGeneration
    use GeneralRoutines
    use NumericalMesh
    use Tecplot
    implicit none

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
    subroutine GenerateUniformRectangles(FNum,U_RECT_2D)
        implicit none
        integer(i4) :: FNum
        type(mesh) U_RECT_2D


        integer(i4) :: i, j, k
	    integer(i4) :: nbx, nby, nn2d, ne2d
        real(dp) :: xl, yl, delx, dely
        real(dp) :: xOffset, yOffset
        
        real(sp), allocatable :: xi(:)
        real(sp), allocatable :: yi(:)

        U_RECT_2D%name='U_RECT_2D'

        !     generate uniform rectangles
        U_RECT_2D%nNodesPerElement=4
        U_RECT_2D%Element%Typ='rectangle'
        U_RECT_2D%TecplotTyp='fequadrilateral'


        !     xl, yl are grid lengths in x- and y-directions
        read(FNum,*) xl, nbx, xOffset
        write(TMPStr,'(a,'//FMT_R8//',a)') TAB//'Mesh length in X        ',xl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')      TAB//'Number of elements in X ',nbx
        call Msg(TMPStr)
        write(TMPStr,'(a,'//FMT_R8//',a)') TAB//'X Offset                ',xOffset,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)

        read(FNum,*) yl, nby,yOffset
        write(TMPStr,'(a,'//FMT_R8//',a)') TAB//'Mesh length in Y        ',yl,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')      TAB//'Number of elements in Y ',nby
        call Msg(TMPStr)
        write(TMPStr,'(a,'//FMT_R8//',a)') TAB//'Y Offset                ',yOffset,'     '//TRIM(UnitsOfLength)
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
        
        !if(EnableTecplotOutput) then
        !    call GBToTecplot(U_RECT_2D)
        !endif

    
        call Msg(' ')
        write(TmpSTR,'(a,i8)')    TAB//'Number of nodes         ',U_RECT_2D%nNodes
        call Msg(TmpSTR)
        write(TmpSTR,'(a,i8)')    TAB//'Number of elements      ',U_RECT_2D%nElements
        call Msg(TmpSTR)
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

end Module MeshGeneration
