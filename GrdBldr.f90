module error_param
	integer :: ierr
end module error_param
 
module gb
	use GeneralRoutines
    use NumericalMesh
	use error_param
    use BasicTypes
    use MeshGen

	implicit none
    
    ! The derived type t_Areas has nAreasCutLines t_line components:
    type :: t_Area
        type (t_line) :: CutBoundary
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: nAreas 
    end type t_Area

    character(50) :: title
	real :: scr_ratio
    
   
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
	real(sp),allocatable	:: elength_in(:)			! area element length x
	real(sp),allocatable	:: y_elength_in(:)			! area element length y
	real(sp),allocatable	:: stretch_factor_in(:)		! local element stretch factor
	integer(i4), allocatable	:: ndrop_rate_in(:)			! local node drop rate
	logical,allocatable		:: hole_in(:)	! true if area not to be filled with elements

	integer(i4)		:: nwells
	real(dp)	:: well_esize			! target element size at well
	real(dp),allocatable	:: xw(:)		! xell x coordinates
	real(dp),allocatable	:: yw(:)		! well y coordinates

	real(dp) :: segl_user

	integer(i4) :: ne_cur	! element

	integer(i4) :: area						! number of areas
	integer(i4)	:: onbn						! number of outer boundry nodes  
	integer(i4),allocatable		:: obn(:)	! outer boundary node list
	integer(i4),allocatable		:: nbn(:)	! number of boundry nodes in area 
	integer(i4),allocatable		:: bn(:,:)   ! boundary node list
	real(sp), allocatable	:: elength(:)
	real(sp), allocatable	:: y_elength(:)
	real(sp), allocatable	:: stretch_factor(:)
	integer(i4), allocatable	:: ndrop_rate(:) 
	logical,allocatable		:: hole(:)	! true if area not to be filled with elements
    

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
	integer(i4) :: gbNseg
	integer(i4) :: ns_cur
    
    
	integer(i4), allocatable :: gbSeg_node(:,:)
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
	subroutine GridBuilder(FNumMUT,GB_GEN,ierror)
		implicit none
		type(mesh) GB_GEN
        
        integer(i4) :: FNumMUT

        character(MAX_INST) :: instruction
        character(MAX_INST) :: WellsFrom_ID_X_Y_File_CMD='wells from id_x_y file'
        character(MAX_INST) :: RefineInsidePolygon_CMD='refine inside polygon'


		integer(i4) :: ierror
		integer(i4) :: i, k, nes
        
        real(dp) :: RealInput

		ierr=0

        
        GB_GEN%TecplotTyp='fetriangle'
        
        GB_GEN%nNodes=0
        allocate(GB_GEN%node(100),stat=ialloc)
        call AllocChk(ialloc,'GridBuilder: GB_GEN%node array')
        GB_GEN%node%x = 0.0d0 ! automatic initialization
        GB_GEN%node%y = 0.0d0 ! automatic initialization
        GB_GEN%node%z = 0.0d0 ! automatic initialization

        GB_GEN%nNodesPerElement=3
        GB_GEN%nElements=0
        allocate(GB_GEN%Element(100), &
            GB_GEN%idNode(GB_GEN%nNodesPerElement,100), stat=ialloc)
        call AllocChk(ialloc,'GridBuilder: GB_GEN%Element, GB_GEN%idNode arrays')
        GB_GEN%element%x = 0.0d0 ! automatic initialization
        GB_GEN%element%y = 0.0d0 ! automatic initialization
        GB_GEN%element%z = 0.0d0 ! automatic initialization
        GB_GEN%Element(:)%idZone = 0 ! automatic initialization
        GB_GEN%idNode(:,:) = 0 ! automatic initialization

        call DefineOuterBoundary(FNumMUT)
        
        ! this will be modified if we implement add_cuts
        GB_GEN%nZones=1
        allocate(GB_GEN%Zone(GB_GEN%nZones), stat=ialloc)
        call AllocChk(ialloc,'GridBuilder: GB_GEN%Zone array')
        GB_GEN%Zone(1)%id=1
        GB_GEN%Zone(1)%name='Zone 1'
        GB_GEN%Zone(1)%is=0

                 
        call set_ob_dir
       
        ! Read a target element length
        read(FNumMUT,*) RealInput
        if(allocated(elength_in)) then
            elength_in(:)=RealInput
            write(TmpSTR,'(a, '//FMT_R8//')') 'Target Element Length: ',RealInput 
			call Msg(trim(TmpSTR))
			call Msg('Assumed units of length are '//TRIM(UnitsOfLength))

        else
            call ErrMsg('elength_in array not allocated before TargetElementLength command')
        endif

        

        
  !
		!! initial arrays for segments... 
		!ns_cur=1
		!call initialize_segment_arrays(1000)
		!if(ierr /= 0) then
		!	ierror=ierr
		!	return
		!endif
  !
		!! initial arrays for boundary nodes... 
		!nb_cur=1
		!na_cur=1
		!call initialize_boundary_arrays(area_in,nbn_in_max)
		!if(ierr /= 0) then
		!	ierror=ierr
		!	return
		!endif
  !
		! initial arrays for cuts... 
		nc_cur=1
		call initialize_cut_arrays(1000)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif


		! use caller boundary data to form dll outer boundary data (import_ob_batch)
		call process_gen_file(GB_GEN)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif

		call fix_short_segs_batch(GB_GEN)
		if(ierr /= 0) then
			ierror=ierr
			return
		endif
	
		call gen_uni_bnodes(GB_GEN)     ! generate uniformly spaced boundary nodes
		if(ierr /= 0) then
			ierror=ierr
			return
		endif


		call Msg('Generating mesh...')
		GB_GEN%nElements=0
		do k=1,area
			if (.not. hole(k)) then
				nes=GB_GEN%nElements+1
				call gen_convex(GB_GEN,k) ! fill an area with elements
				if(ierr /= 0) then
					ierror=ierr
					return
				endif
				do i=nes,GB_GEN%nElements
					GB_GEN%element(i)%idZone=k
				end do
			endif
		end do

		call MeshExtents(GB_GEN)

		call set_ob_ab(GB_GEN)

		call Msg('Relaxing grid...')

		recalc_nicon=.true.
		call relax_grid(GB_GEN)



		if(ierr /= 0) then
			ierror=ierr
			return
		endif

        GridBuilderMods_InstructionLoop:do
            
            read(FNumMUT,'(a)',iostat=status) instruction
            call LwrCse(instruction)
            if(index(instruction, 'end') /=0) then
                call Msg('end GridBuilderMods_InstructionLoop')
                exit GridBuilderMods_InstructionLoop
            else
                call LwrCse(instruction)
                call Msg(instruction)
            end if    
          
            if(index(instruction, WellsFrom_ID_X_Y_File_CMD)  /= 0) then
                call DefineWells(FNumMUT)
                ! Read a target element length
				read(FNumMUT,*) well_esize			! target element size at well
				write(TmpSTR,'(a, '//FMT_R8//')') 'Target Element size at well: ',well_esize 
				call Msg(trim(TmpSTR))
				call Msg('Assumed units of length are '//TRIM(UnitsOfLength))

            else if(index(instruction, RefineInsidePolygon_CMD)  /= 0) then
                call RefineInsidePolygon(GB_GEN,FNumMUT)
            else
                call ErrMsg('MUSG?:'//instruction)
            end if
        end do GridBuilderMods_InstructionLoop

		if(nwells > 0) then
			call Msg('Adding wells...')
			call make_wells(GB_GEN)
			if(ierr /= 0) then
				ierror=ierr
				return
			endif

			call refine_wells(GB_GEN)
			if(ierr /= 0) then
				ierror=ierr
				return
			endif
        endif

        recalc_nicon=.true.
		call relax_grid(GB_GEN)

        GB_GEN%Element%Typ='triangle'


		if(allocated(nicon)) deallocate(nicon,icon,eicon)
        
        continue


    end subroutine GridBuilder

    !----------------------------------------------------------------------
    subroutine DefineOuterBoundary(FNumMUT)
		implicit none
        type(t_line) :: OuterBoundary
        integer(i4) :: i 
        integer(i4) :: FNumMUT
        integer(i4) :: FnumLine
		character(MAX_STR) :: FNameLine
        
        
   
		read(FNumMUT,'(a)') FNameLine 
        inquire(file=FNameLine,exist=FileExists)
        if(.not. FileExists) then
			call ErrMsg('File not found: '//trim(FNameLine))
        endif
                
        call Msg(FileReadSTR//'OuterBoundaryFrom_ID_X_Y file: '//trim(FNameLine))

		call OpenAscii(FnumLine,FNameLine)
        call LineFrom_ID_X_Y_File(FnumLine,OuterBoundary)
                
        OuterBoundary%Name=Trim(FNameline)
                
        ! default setting for 1 area
        nn_in=OuterBoundary%nPoints
        area_in=1
        plan_view=.true.
        scr_ratio=1.607142
		! xmin,xmax,ymin,ymax calculated later when grid generated
        ! scr_ratio not used 
        segl_user=OuterBoundary%nPoints-2   ! number of segments in outer boundary
        
		allocate(x_in(nn_in), &
				y_in(nn_in), &
				stat=ialloc)
		call AllocChk(ialloc,'OuterBoundaryFrom_ID_X_Y_File_CMD: x_in, y_in')

		x_in(:)=OuterBoundary%Point(:)%x
        y_in(:)=OuterBoundary%Point(:)%y
                
        onbn_in=OuterBoundary%nPoints
        allocate(obn_in(0:OuterBoundary%nPoints),stat=ialloc)
		call AllocChk(ialloc,'OuterBoundaryFrom_ID_X_Y_File_CMD: obn_in')
		obn_in(0:)=OuterBoundary%point(0:)%id
                

		allocate(elength_in(Area_in), &
				y_elength_in(Area_in), &
				stretch_factor_in(Area_in), &
				ndrop_rate_in(Area_in), &
				hole_in(Area_in), &
				nbn_in(Area_in), &
				bn_in(Area_in,0:onbn_in), &
				stat=ialloc)
		call AllocChk(ialloc,'OuterBoundaryFrom_ID_X_Y_File_CMD: x_in, y_in')

		elength_in(:)=(MAXval(OuterBoundary%Point(:)%x) - MINval(OuterBoundary%Point(:)%x))/10.
        y_elength_in(:)=(MAXval(OuterBoundary%Point(:)%y) - MINval(OuterBoundary%Point(:)%y))/10.
        stretch_factor_in(:)=2.0
		ndrop_rate_in(:)=1
		hole_in(:)=.false.
        do i=1,area_in
			nbn_in(i)=OuterBoundary%nPoints
            bn_in(i,0:OuterBoundary%nPoints)=OuterBoundary%Point(0:)%id
        end do

        nbn_in_max=0
		do i=1,area_in
			if(nbn_in(i) > nbn_in_max) nbn_in_max=nbn_in(i)
        end do
                
        xmin=minval(x_in)
        xmax=maxval(x_in)
        ymin=minval(y_in)
        ymax=maxval(y_in)
        
        ! initial arrays for outer boundary nodes... 
		call initialize_outer_boundary_array(OuterBoundary%nPoints)
		call initialize_segment_arrays(1000)
		call initialize_boundary_arrays(area_in,nbn_in_max)
        
    end subroutine DefineOuterBoundary
    !----------------------------------------------------------------------
    subroutine RefineInsidePolygon(GB_GEN,FNumMUT)
		implicit none
        type(mesh) :: GB_GEN
        type(t_line) :: Polygon
        integer(i4) :: i 
        integer(i4) :: FNumMUT
        integer(i4) :: FnumLine
		character(MAX_STR) :: FNameLine
        
        LOGICAL :: IsInPoly
        
        
   
		read(FNumMUT,'(a)') FNameLine 
        inquire(file=FNameLine,exist=FileExists)
        if(.not. FileExists) then
			call ErrMsg('File not found: '//trim(FNameLine))
        endif
                
        call Msg(FileReadSTR//'RefineInsidePolygon file: '//trim(FNameLine))

		call OpenAscii(FnumLine,FNameLine)
        call LineFrom_ID_X_Y_File(FnumLine,Polygon)
        
        if(Polygon%LineTyp /= 'Polygon') then
            write(TmpSTR,'(a)') 'RefineInsidePolygon file '//trim(FNameLine)//' must be of type polygon'
            call ErrMsg(TmpSTR)
        endif
                
        Polygon%Name=Trim(FNameline)
                
        ! Clear chosen elements
		do i=1,GB_GEN%nElements
			call clear(GB_GEN%element(i)%is,chosen)
        end do
        ! Triangle properties
        call TriangularElementProperties(GB_GEN)

        ! Find elements within region boundary
		do i=1,GB_GEN%nElements
            IsInPoly=.false.
            IsInPoly=in_poly(Polygon%NPoints,Polygon%Point%x,Polygon%Point%y,GB_GEN%Element(i)%xCircle,GB_GEN%Element(i)%yCircle)
			if(IsInPoly) then
				call set(GB_GEN%element(i)%is,chosen)
                !write(TMPStr,'(a,i8,a)') ' Element ',i,' marked for refinement'
                !write(*,*) trim(TMPStr)
			endif
        end do
        
        call refine_chosen(GB_GEN)
        	
    end subroutine RefineInsidePolygon
    !----------------------------------------------------------------------
    subroutine DefineWells(FNumMUT)
		implicit none
        type(t_pointset) :: Wells
        integer(i4) :: FNumMUT
        integer(i4) :: FnumLine
		character(MAX_STR) :: FNameLine
        
        
   
		read(FNumMUT,'(a)') FNameLine 
        inquire(file=FNameLine,exist=FileExists)
        if(.not. FileExists) then
			call ErrMsg('File not found: '//trim(FNameLine))
        endif
                
        call Msg(FileReadSTR//'WellsFrom_ID_X_Y file: '//trim(FNameLine))

		call OpenAscii(FnumLine,FNameLine)
        call WellsFrom_ID_X_Y_File(FnumLine,Wells)
        
        Wells%Name=Trim(FNameline)
                
        ! default setting for 1 area
        nwells=Wells%nPoints
        
        allocate(xw(nwells), &
				 yw(nwells), &
				stat=ialloc)
		call AllocChk(ialloc,'WellsFrom_ID_X_Y_File_CMD: x_in, y_in')
        
        xw(:)=Wells%Point(:)%x
        yw(:)=Wells%Point(:)%y


        
    end subroutine DefineWells
	!----------------------------------------------------------------------
	subroutine read_gendat(FNumMUT)
		implicit none
        integer(i4) :: FNumMUT

		integer :: i,j

		read(FNumMut,'(a80)') TmpSTR
        call Msg('Read GB Gen file: '//trim(TmpSTR))

	    call getunit(itmp)
		open(itmp,file=trim(TmpSTR),status='old',form='formatted')
		read(itmp,'(a)') title
		read(itmp,*) nn_in,area_in
		read(itmp,*) plan_view
		read(itmp,*) xmin,xmax,ymin,ymax
		read(itmp,*) scr_ratio
		read(itmp,*) segl_user
        
        allocate(x_in(nn_in), &
				y_in(nn_in), &
				stat=ialloc)
        call AllocChk(ialloc,'read_gendat: x_in, y_in')

		do i=1,nn_in
			read(itmp,*) x_in(i),y_in(i)
		end do

		read(itmp,*) onbn_in
        allocate(obn_in(0:onbn_in),stat=ialloc)
        call AllocChk(ialloc,'read_gendat: obn_in')
		read(itmp,*) (obn_in(i),i=0,onbn_in)


		read(itmp,*) nbn_in_max

        allocate(elength_in(nn_in), &
				y_elength_in(nn_in), &
				stretch_factor_in(nn_in), &
				ndrop_rate_in(nn_in), &
				hole_in(nn_in), &
				nbn_in(nn_in), &
				bn_in(nn_in,0:nbn_in_max), &
				stat=ialloc)
        call AllocChk(ialloc,'read_gendat: x_in, y_in')
		do i=1,area_in
			if (plan_view) then
				read(itmp,*) elength_in(i)
			else
				read(itmp,*) elength_in(i), y_elength_in(i)
			endif
			read(itmp,*) stretch_factor_in(i)
			read(itmp,*) ndrop_rate_in(i)
			read(itmp,*) hole_in(i)
			read(itmp,*) nbn_in(i)
			read(itmp,*) (bn_in(i,j),j=0,nbn_in(i))
		end do

		close(itmp)
		!xmin_in = x_in(1)
		!ymin_in = y_in(1)
		!xmax_in = xmin_in
		!ymax_in = ymin_in
		!do i=2,nn_in
		!	xmin_in=min(xmin_in,x_in(i))
		!	ymin_in=min(ymin_in,y_in(i))
		!	xmax_in=max(xmin_in,x_in(i))
		!	ymax_in=max(ymin_in,y_in(i))
		!end do

    	

    end subroutine read_gendat
	!----------------------------------------------------------------------
	subroutine save_gendat(FNumMUT)
		implicit none
        integer(i4) :: FNumMUT

		integer :: i,j

		read(FNumMut,'(a80)') TmpSTR
        call Msg(FileCreateSTR//'Grid generation file: '//trim(TmpSTR))

	    call getunit(itmp)
		open(itmp,file=trim(TmpSTR),status='unknown',form='formatted')
		write(itmp,'(a)') ' dummy grid title'
		write(itmp,*) nn_in,area_in
		write(itmp,*) plan_view
		write(itmp,*) xmin,xmax,ymin,ymax
		write(itmp,*) scr_ratio
		write(itmp,*) segl_user

		do  i=1,nn_in
			write(itmp,*) x_in(i),y_in(i)
		end do

		write(itmp,*) onbn_in
		write(itmp,*) (obn_in(i),i=0,onbn_in)

		write(itmp,*) nbn_in_max

		do i=1,area_in
			if (plan_view) then
				write(itmp,*) elength_in(i)
			else
				write(itmp,*) elength_in(i), y_elength_in(i)
			endif
			write(itmp,*) stretch_factor_in(i)
			write(itmp,*) ndrop_rate_in(i)
			write(itmp,*) hole_in(i)
			write(itmp,*) nbn_in(i)
			write(itmp,'(8i10)') (bn_in(i,j),j=0,nbn_in(i))
		end do


		close(itmp)
	end subroutine save_gendat
    
	!!----------------------------------------------------------------------
	!subroutine find_node_old(x1,y1,p_node)
	!	implicit none
 !
	!	integer :: i
 !
	!	integer p_node
	!	real(dp) :: x1, y1, dist_min, f1
 !
	!	i=1
	!	dist_min=1.0e20
	!	97    continue
	!		!if(plan_view)then
	!			f1=sqrt((x1-x_in(i))**2+((y1-y_in(i)))**2)
	!		!else
	!		!	f1=sqrt((x1-x(i))**2+((y1-y(i))*xy_ratio)**2)
	!		!endif
	!		if(f1.lt.dist_min) then
	!			p_node=i
	!			dist_min=f1
	!		endif
	!		i=i+1
	!	if (i.LE.nn_in) goto 97
	!end subroutine find_node
	!----------------------------------------------------------------------
	subroutine find_node(GB_GEN,x1,y1,p_node)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i

		integer(i4) p_node
		real(dp) :: x1, y1, dist_min, f1

		i=1
		dist_min=1.0e20
		97    continue
			!if(plan_view)then
				f1=sqrt((x1-GB_GEN%node(i)%x)**2+((y1-GB_GEN%node(i)%y))**2)
			!else
			!	f1=sqrt((x1-GB_GEN%node(i)%x)**2+((y1-GB_GEN%node(i)%y)*xy_ratio)**2)
			!endif
			if(f1.lt.dist_min) then
				p_node=i
				dist_min=f1
			endif
			i=i+1
		if (i.LE.GB_GEN%nNodes) goto 97
	end subroutine find_node
	!----------------------------------------------------------------------
	 subroutine set_ob_ab(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j

		do  j=1,onbn_in
			call set(GB_GEN%node(obn_in(j))%is,out_bndy)
			call set(GB_GEN%node(obn_in(j))%is,b_2nd_type)
		end do

		do i=1,area_in
			do  j=1,nbn_in(i)
				call set(GB_GEN%node(bn_in(i,j))%is,any_bndy)
			end do
		end do

	end subroutine set_ob_ab

	!*** Grid relaxation routines ***
	!----------------------------------------------------------------------
	subroutine relax_grid(GB_GEN)
		implicit none 
 		type(mesh) GB_GEN
   
		integer(i4) :: i, j
		real(dp) :: maxdiff, tol, xm, ym, diff

		call node_connections(GB_GEN)
		if(ierr /= 0) return

		tol=(GB_GEN%xmax-GB_GEN%xmin)/1.e-5
		10    continue
			maxdiff=0.0
			do i=1,GB_GEN%nNodes
				if (.not. bcheck(GB_GEN%node(i)%is,any_bndy) .and. .not. bcheck(GB_GEN%node(i)%is,well)) then
					xm=0.0d0
					ym=0.0d0
					do j=1,nicon(i)
						xm=xm+GB_GEN%node(icon(i,j))%x
						ym=ym+GB_GEN%node(icon(i,j))%y
					end do
					diff=sqrt((GB_GEN%node(i)%x-xm/nicon(i))**2 + (GB_GEN%node(i)%y-ym/nicon(i))**2)
					maxdiff=max(diff,maxdiff)
					GB_GEN%node(i)%x=xm/nicon(i)
					GB_GEN%node(i)%y=ym/nicon(i)
				endif
			end do
		if (maxdiff.gt.tol) goto 10

	end subroutine relax_grid
	!----------------------------------------------------------------------
	subroutine node_connections(GB_GEN)
		implicit none 
        type(mesh) GB_GEN

		integer(i4) :: i, j, k, l, node1, node2, i1, i2, ic, n1, n2

		integer(i4), allocatable :: new_order(:),icon_w(:),eicon_w(:,:)
		real(dp), allocatable :: angle(:)

		if(.not. recalc_nicon) return

		if(allocated(nicon)) deallocate(nicon,icon,eicon)
		allocate(nicon(GB_GEN%nNodes),icon(GB_GEN%nNodes,maxnicon),eicon(GB_GEN%nNodes,maxnicon,2),stat=ialloc)
		call AllocChk(ialloc,'Node connection arrays')
		if(ierr /= 0) return


		allocate(new_order(maxnicon),icon_w(maxnicon),eicon_w(maxnicon,2),angle(maxnicon), stat=ialloc)
		call AllocChk(ialloc,'Node connection working arrays')
		if(ierr /= 0) return

		!     Clear connection data
		nicon(:)=0
		icon(:,:)=0
		eicon(:,:,:)=0

		do l=1,GB_GEN%nElements
			node1=GB_GEN%idNode(1,l)
			node2=GB_GEN%idNode(2,l)
			call make_list(l,node1,node2)
			node2=GB_GEN%idNode(3,l)
			call make_list(l,node1,node2)
			node1=GB_GEN%idNode(2,l)
			node2=GB_GEN%idNode(1,l)
			call make_list(l,node1,node2)
			node2=GB_GEN%idNode(3,l)
			call make_list(l,node1,node2)
			node1=GB_GEN%idNode(3,l)
			node2=GB_GEN%idNode(1,l)
			call make_list(l,node1,node2)
			node2=GB_GEN%idNode(2,l)
			call make_list(l,node1,node2)
		end do

		! Compute angles and sort
		do i=1,GB_GEN%nNodes
			if(nicon(i).gt.2) then
				i1=icon(i,1)
				!         Change i1 to downstream boundary node if in list
				do j=1,nicon(i)
					ic=icon(i,j)
					if(bcheck(GB_GEN%node(ic)%is,out_bndy)) then
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
						call int_angle(GB_GEN%node(i2)%x,GB_GEN%node(i2)%y,GB_GEN%node(i)%x,GB_GEN%node(i)%y,GB_GEN%node(i1)%x,GB_GEN%node(i1)%y,angle(j))
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
	subroutine make_wells(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, nde


		call node_connections(GB_GEN)
		if(ierr /= 0) return

		do i=1,nwells      ! move and fix well nodes
			call find_node(GB_GEN,xw(i),yw(i),nde)
			if(.not. bcheck(GB_GEN%node(nde)%is,well)) then
				GB_GEN%node(nde)%x=xw(i)
				GB_GEN%node(nde)%y=yw(i)
				call set(GB_GEN%node(nde)%is,well)
			endif
		end do

		call relax_grid(GB_GEN)
		if(ierr /= 0) return

	end subroutine make_wells
!	!----------------------------------------------------------------------
!	subroutine refine_wells_old
!		implicit none
!
!		integer :: i, j, l, nde, nn_old
!
!		logical :: stop_refining
!		real(dp) :: spacing
!
!		do i=1,nwells  ! fix well node neighbours
!			call find_node(xw(i),yw(i),nde)
!			do j=1,nicon(nde)
!				call set(node(icon(nde,j)),well)
!			end do
!		end do
!
!		nn_old=nn
!
!		refine_loop: do
!
!			stop_refining=.true.
!
!			do l=1,ne   ! no elements chosen
!				call clear(elem(l),chosen)
!			end do
!
!
!			do i=1,nwells
!				call find_node(xw(i),yw(i),nde)
!				call node_spacing(nde,spacing)
!				if(ierr /= 0) return
!				if(spacing > well_esize) then
!					call choose_e_connected(nde)
!					stop_refining=.false.
!				endif
!			end do
!
!			if(stop_refining) exit refine_loop
!
!			call refine_chosen
!			if(ierr /= 0) return
!
!		end do refine_loop
!
!		do i=nn_old+1,nn  ! fix new nodes too
!			call set(node(i),well)
!		end do
!
!	end subroutine refine_wells_old
!!----------------------------------------------------------------------
	subroutine refine_wells(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j, l, nde, nn_old

		logical :: stop_refining
		real(dp) :: spacing

		do i=1,nwells  ! fix well node neighbours
			call find_node(GB_GEN,xw(i),yw(i),nde)
			do j=1,nicon(nde)
				call set(GB_GEN%node(icon(nde,j))%is,well)
			end do
		end do

		nn_old=GB_GEN%nNodes

		refine_loop: do

			stop_refining=.true.

			do l=1,GB_GEN%nElements   ! no elements chosen
				call clear(GB_GEN%element(l)%is,chosen)
			end do


			do i=1,nwells
				call find_node(GB_GEN,xw(i),yw(i),nde)
				call node_spacing(GB_GEN,nde,spacing)
				if(ierr /= 0) return
				if(spacing > well_esize) then
                    !write(*,*) 'well ',i,' spacing ',spacing

					call choose_e_connected(GB_GEN,nde)
					stop_refining=.false.
				endif
			end do

			if(stop_refining) exit refine_loop

			call refine_chosen(GB_GEN)
			if(ierr /= 0) return

		end do refine_loop

		do i=nn_old+1,GB_GEN%nNodes  ! fix new nodes too
			call set(GB_GEN%node(i)%is,well)
		end do

	end subroutine refine_wells
	!----------------------------------------------------------------------
	subroutine choose_e_connected(GB_GEN,nde)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: l, nde


		do l=1,GB_GEN%nElements
			if(GB_GEN%idNode(1,l) == nde .or. GB_GEN%idNode(2,l) == nde .or. GB_GEN%idNode(3,l) == nde)      then
				call set(GB_GEN%element(l)%is,chosen)
			endif
		end do

	end subroutine choose_e_connected
	!----------------------------------------------------------------------
	subroutine node_spacing(GB_GEN,i,spacing)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j

		real(dp) :: spacing, dist

		call node_connections(GB_GEN)
		if(ierr /= 0) return

		spacing=0.0
		do j=1,nicon(i)
			dist=sqrt((GB_GEN%node(i)%x-GB_GEN%node(icon(i,j))%x)**2 + (GB_GEN%node(i)%y-GB_GEN%node(icon(i,j))%y)**2)
			spacing=spacing+dist
		end do

		spacing=spacing/nicon(i)

	end subroutine node_spacing

	!-----------------------------------------------------------------------
	!*** Mesh refinement routines ***
	!-----------------------------------------------------------------------
	subroutine refine_chosen(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: l, nil, nnn, nne, gbNodesIn

		integer(i4),allocatable :: nnl(:)
	
		allocate(nnl(4*GB_GEN%nNodes), stat=ialloc)
		call AllocChk(ialloc,'refine_chosen work array')
		if(ierr /= 0 ) return

		nnl(:)=0

		nil=0
		nnn=GB_GEN%nNodes
        gbNodesIn=GB_GEN%nNodes
		nne=GB_GEN%nElements
		do l=1,GB_GEN%nElements
			if (bcheck(GB_GEN%element(l)%is,chosen)) then
				call refine_element(GB_GEN,l,.true.,nil,nnn,nne,nnl)
				if(ierr /= 0) goto 1000
			endif
        end do
		
        if (nnn.gt.gbNodesIn) then
			15      sn=nnn
				do  l=1,GB_GEN%nElements
					if (.not. bcheck(GB_GEN%element(l)%is,chosen)) then
						call check_two(GB_GEN,l,nil,nnn,nne,nnl)
						if(ierr /= 0) goto 1000
					endif
				end do
			if(nnn.gt.sn) goto 15
			do  l=1,GB_GEN%nElements
				if (.not. bcheck(GB_GEN%element(l)%is,chosen)) then
					call check_one(GB_GEN,l,nil,nnl)
					if(ierr /= 0) goto 1000
				endif
			end do
		endif
		!GB_GEN%nNodes=nnn
		!GB_GEN%nElements=nne
		!call calc_bandwidth
		!recalc_segs=.true.
		recalc_nicon=.true.

		1000 deallocate(nnl)

	end subroutine refine_chosen
	!----------------------------------------------------------------------
	subroutine refine_element(GB_GEN,l,set_chosen,nil,nnn,nne,nnl)
		! check each side to see if node already exists
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: l, i1, i2, i3, n1, n2, n3, nnn, nil, nne 

		integer(i4) :: nnl(4*GB_GEN%nNodes)
		integer(i4) :: n_temp, e_temp
		logical set_chosen

		i1=GB_GEN%idNode(1,l)
		i2=GB_GEN%idNode(2,l)
		i3=GB_GEN%idNode(3,l)
		call check_exist(GB_GEN,i1,i2,n1,nnn,nil,nnl)
		if(ierr /= 0) return
		call check_exist(GB_GEN,i2,i3,n2,nnn,nil,nnl)
		if(ierr /= 0) return
		call check_exist(GB_GEN,i3,i1,n3,nnn,nil,nnl)
		if(ierr /= 0) return
		n_temp=GB_GEN%idNode(2,l)
		e_temp=GB_GEN%element(l)%idZone
		call add_element(GB_GEN,n_temp,n2,n1,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(GB_GEN%element(nne)%is,chosen)
		endif
		n_temp=GB_GEN%idNode(3,l)
		call add_element(GB_GEN,n_temp,n3,n2,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(GB_GEN%element(nne)%is,chosen)
		endif
		call add_element(GB_GEN,n1,n2,n3,e_temp)
		if(ierr /= 0) return
		if (set_chosen) then
			call set(GB_GEN%element(nne)%is,chosen)
		endif
		GB_GEN%idNode(2,l)=n1
		GB_GEN%idNode(3,l)=n3

	end subroutine refine_element
	!----------------------------------------------------------------------
	subroutine check_exist(GB_GEN,i1,i2,ninc,nnn,nil,nnl)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, i1, i2, nnn, nil, ninc, gbnNodesBefore 

		logical node_exists
		integer(i4) :: nnl(4*GB_GEN%nNodes)
		real*8 :: xm
		real*8 :: ym

		xm=0.5*GB_GEN%node(i1)%x+0.5*GB_GEN%node(i2)%x
		ym=0.5*GB_GEN%node(i1)%y+0.5*GB_GEN%node(i2)%y
		node_exists=.false.
		if (nil.gt.0)then
			i=1
			10      continue
				if (abs(xm-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
					if(abs(ym-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
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
            ! write(*,*) GB_GEN%nNodes,' check_exist'
            gbnNodesBefore=GB_GEN%nNodes
			call new_node(GB_GEN,xm,ym)
            nnn=nnn+GB_GEN%nNodes - gbnNodesBefore
			if(ierr /= 0) return
			call update_bnodes(GB_GEN,i1,i2,nnn)
			if(ierr /= 0) return
			nil=nil+1
			nnl(nil)=GB_GEN%nNodes
			ninc=GB_GEN%nNodes
		endif

	end subroutine check_exist
	!----------------------------------------------------------------------
	subroutine delete_nnl(nnl,i,nil)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j, nil

		integer(i4) :: nnl(4*GB_GEN%nNodes)
	
		do j=i,nil-1
			nnl(j)=nnl(j+1)
		end do      

	end subroutine delete_nnl
	!----------------------------------------------------------------------
	subroutine check_two(GB_GEN,l,nil,nnn,nne,nnl)
		! check each side to see if node already exists
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, l, nil, nnn, nne, nSplitSides

		integer(i4) :: nnl(4*GB_GEN%nNodes)
		real*8 :: x1,y1,x2,y2,x3,y3

		nSplitSides=0
		x1=0.5*GB_GEN%node(GB_GEN%idNode(1,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(2,l))%x
		y1=0.5*GB_GEN%node(GB_GEN%idNode(1,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(2,l))%y
		x2=0.5*GB_GEN%node(GB_GEN%idNode(2,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(3,l))%x
		y2=0.5*GB_GEN%node(GB_GEN%idNode(2,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(3,l))%y
		x3=0.5*GB_GEN%node(GB_GEN%idNode(3,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(1,l))%x
		y3=0.5*GB_GEN%node(GB_GEN%idNode(3,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(1,l))%y
		i=1
		10    continue
			if (abs(x1-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y1-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					nSplitSides=nSplitSides+1
				endif
			endif
			if (abs(x2-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y2-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					nSplitSides=nSplitSides+1
				endif
			endif
			if (abs(x3-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y3-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					nSplitSides=nSplitSides+1
				endif
			endif
			i=i+1
		if(i.le.nil .and. nSplitSides.le.1) goto 10
		if (nSplitSides.gt.1) then
			call refine_element(GB_GEN,l,.true.,nil,nnn,nne,nnl)  ! fred was false
			if(ierr /= 0) return
		endif

	end subroutine check_two
	!----------------------------------------------------------------------
	subroutine check_one(GB_GEN,l,nil,nnl)
		! check each side to see if node already exists
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, l, nil, n1, n2, n3, iz

		integer(i4) :: nnl(4*GB_GEN%nNodes)
		logical done
		real*8 :: x1,y1,x2,y2,x3,y3

		done=.false.
		x1=0.5*GB_GEN%node(GB_GEN%idNode(1,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(2,l))%x
		y1=0.5*GB_GEN%node(GB_GEN%idNode(1,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(2,l))%y
		x2=0.5*GB_GEN%node(GB_GEN%idNode(2,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(3,l))%x
		y2=0.5*GB_GEN%node(GB_GEN%idNode(2,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(3,l))%y
		x3=0.5*GB_GEN%node(GB_GEN%idNode(3,l))%x+0.5*GB_GEN%node(GB_GEN%idNode(1,l))%x
		y3=0.5*GB_GEN%node(GB_GEN%idNode(3,l))%y+0.5*GB_GEN%node(GB_GEN%idNode(1,l))%y
		i=1
		10    continue
			if (abs(x1-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y1-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					n1=nnl(i)
                    n2=GB_GEN%idNode(2,l)
                    n3=GB_GEN%idNode(3,l)
                    iz=GB_GEN%element(l)%idZone
					call add_element(GB_GEN,n1,n2,n3,iz)
					if(ierr /= 0) return
					GB_GEN%idNode(2,l)=n1
					done=.true.
				endif
			endif
			if (abs(x2-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y2-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					n1=nnl(i)
                    n2=GB_GEN%idNode(3,l)
                    n3=GB_GEN%idNode(1,l)
                    iz=GB_GEN%element(l)%idZone
					call add_element(GB_GEN,n1,n2,n3,iz)
					if(ierr /= 0) return
					GB_GEN%idNode(3,l)=n1
					done=.true.
				endif
			endif
			if (abs(x3-GB_GEN%node(nnl(i))%x).lt.1.e-5) then
				if(abs(y3-GB_GEN%node(nnl(i))%y).lt.1.e-5) then
					n1=nnl(i)
                    n2=GB_GEN%idNode(2,l)
                    n3=GB_GEN%idNode(3,l)
                    iz=GB_GEN%element(l)%idZone
					call add_element(GB_GEN,n1,n2,n3,iz)
					if(ierr /= 0) return
					GB_GEN%idNode(3,l)=n1
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

		allocate(gbSeg_node(ns_cur,2),seg_el(ns_cur,2), seg_area(ns_cur,2), seg_is(ns_cur),stat=ialloc)
		call AllocChk(ialloc,'initial segment arrays')
		if(ierr /= 0) return

		gbSeg_node(:,:)=0
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

		integer(i4), allocatable :: gbSeg_node_tmp(:,:) 
		integer(i4), allocatable :: seg_el_tmp(:,:) 
		integer(i4), allocatable :: seg_area_tmp(:,:) 
		integer(i4), allocatable :: seg_is_tmp(:) 

		if(ns_rqst >= ns_cur) then  ! reallocate gbSeg_node etc

			ns_new=nint(ns_rqst*ns_mult)

			allocate(gbSeg_node_tmp(ns_new,2),seg_el_tmp(ns_new,2), seg_area_tmp(ns_new,2), seg_is_tmp(ns_new), stat=ialloc)
			call AllocChk(ialloc,'allocate temp segment arrays')
			if(ierr /= 0) return
	
			gbSeg_node_tmp(:,:)=0
			seg_el_tmp(:,:)=0
			seg_area_tmp(:,:)=0
			seg_is_tmp(:)=0

			! copy current data
			do i=1,ns_cur
				gbSeg_node_tmp(i,:)	=	gbSeg_node(i,:)
				seg_el_tmp(i,:)		=	seg_el(i,:)
				seg_area_tmp(i,:)	=	seg_area(i,:)
				seg_is_tmp(i)		=	seg_is(i)
			end do

			! destroy arrays
			deallocate(gbSeg_node, seg_area, seg_el, seg_is)
			! reallocate
			allocate(gbSeg_node(ns_new,2),seg_el(ns_new,2), seg_area(ns_new,2), seg_is(ns_new), stat=ialloc)
			call AllocChk(ialloc,'reallocate segment arrays')
			if(ierr /= 0) return

			! copy current data
			do i=1,ns_cur
				gbSeg_node(i,:)	=	gbSeg_node_tmp(i,:)	
				seg_el(i,:)		=	seg_el_tmp(i,:)		
				seg_area(i,:)	=	seg_area_tmp(i,:)	
				seg_is(i)		=	seg_is_tmp(i)	
			end do

			ns_cur=ns_new

		end if

	end subroutine reallocate_segment_arrays
	!----------------------------------------------------------------------
	subroutine gbNewSegment(nnseg,n1,n2,a1,a2,l1,l2)
		implicit none

		integer(i4) :: nnseg, n1, n2, a1, a2, l1, l2

		nnseg=nnseg+1

		call reallocate_segment_arrays(nnseg)
		if(ierr /= 0) return

		gbSeg_node(nnseg,1)=n1
		gbSeg_node(nnseg,2)=n2
		seg_area(nnseg,1)=a1
		seg_area(nnseg,2)=a2
		seg_el(nnseg,1)=l1
		seg_el(nnseg,2)=l2

	end subroutine gbNewSegment
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
		call AllocChk(ialloc,'initial cut arrays')
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
			call AllocChk(ialloc,'allocate temp cut boundary arrays')
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
			call AllocChk(ialloc,'reallocate cut arrays')
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
			call AllocChk(ialloc,'allocate temp lbn array')
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
			call AllocChk(ialloc,'reallocate lbn array')
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
			call AllocChk(ialloc,'allocate temp outer boundary arrays')
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
			call AllocChk(ialloc,'reallocate outer boundary arrays')
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
			call AllocChk(ialloc,'allocate temp boundary arrays')
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
			call AllocChk(ialloc,'reallocate boundary arrays')
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
		call AllocChk(ialloc,'initial boundary arrays')
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
		call AllocChk(ialloc,'initial outer boundary arrays')
		if(ierr /= 0) return

		obn(:)=0

	end subroutine initialize_outer_boundary_array


	!----------------------------------------------------------------------
	subroutine add_element(GB_GEN,i1,i2,i3,e_area)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i1, i2, i3, e_area,idum=3


        call GrowElementArray(GB_GEN%element,GB_GEN%nElements,GB_GEN%nElements+1)	
        call GrowInteger2dArray(GB_GEN%idNode,idum,GB_GEN%nElements,GB_GEN%nElements+1)	
		!call reallocate_element_arrays(nel)
		if(ierr /= 0) return

        GB_GEN%nElements=GB_GEN%nElements+1  ! we are going to make a new element

		GB_GEN%idNode(1,GB_GEN%nElements)=i1
		GB_GEN%idNode(2,GB_GEN%nElements)=i2
		GB_GEN%idNode(3,GB_GEN%nElements)=i3
		GB_GEN%element(GB_GEN%nElements)%is=0	
		GB_GEN%element(GB_GEN%nElements)%id=GB_GEN%nElements	
 		GB_GEN%element(GB_GEN%nElements)%idZone=e_area		

        ! write(*,*) GB_GEN%nElements,' element ',i1,i2,i3

	end subroutine add_element
	!----------------------------------------------------------------------
	subroutine new_node(GB_Gen,xnew,ynew)
		implicit none
		type(mesh) GB_GEN

		real(dp) :: xnew, ynew


        call GrowNodeArray(GB_GEN%Node,GB_GEN%nNodes,GB_GEN%nNodes+1)	
		!call reallocate_node_arrays(nnd)
		if(ierr /= 0) return

        GB_GEN%nNodes=GB_GEN%nNodes+1

        GB_GEN%node(GB_GEN%nNodes)%id=GB_GEN%nNodes
		GB_GEN%node(GB_GEN%nNodes)%x=xnew
		GB_GEN%node(GB_GEN%nNodes)%y=ynew
		GB_GEN%node(GB_GEN%nNodes)%z=0.0d0
		GB_GEN%node(GB_GEN%nNodes)%is=0	
        
         ! write(*,*) GB_GEN%nNodes,' node ',GB_GEN%nNodes, xnew, ynew

	end subroutine new_node
	!----------------------------------------------------------------------
	subroutine gen_uni_bnodes(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, n1, n2, i1
		real(dp) :: x1, y1, x2, y2, d, rmu, rmu1, rmu2, del_rmu, rmu_el


		integer(i4) :: nseg_old
		real(dp), allocatable :: size_nde(:)  ! smallest element size (seg length or elength) at a node 

		allocate(size_nde(GB_GEN%nNodes), stat=ialloc)
		call AllocChk(ialloc,'gen_uni_bnodes work arrays')

		size_nde(:)=1.e20  ! array assignment

		call calc_segments

		! set array size_nde
		do i=1,gbNseg
			n1=gbSeg_node(i,1)
			n2=gbSeg_node(i,2)
			d=sqrt((GB_GEN%node(n2)%x-GB_GEN%node(n1)%x)**2+(GB_GEN%node(n2)%y-GB_GEN%node(n1)%y)**2)

			size_nde(n1)=min(size_nde(n1),d)   					! segment length
			size_nde(n1)=min(size_nde(n1),elength(seg_area(i,1)))	! target length	
			if(seg_area(i,2) /= 0) size_nde(n1)=min(size_nde(n1),elength(seg_area(i,2)))	! target length area 2	

			size_nde(n2)=min(size_nde(n2),d)   					! segment length
			size_nde(n2)=min(size_nde(n2),elength(seg_area(i,1)))	! target length	
			if(seg_area(i,2) /= 0) size_nde(n2)=min(size_nde(n2),elength(seg_area(i,2)))	! target length area 2	
		end do

		call Msg('Generating boundary nodes...')

		! loop over segments and satisfy elength, inserting nodes as necessary 
		nseg_old=gbNseg
		seg_old_loop: do i=1,nseg_old
			n1=gbSeg_node(i,1)
			n2=gbSeg_node(i,2)
			x1=GB_GEN%node(n1)%x
			y1=GB_GEN%node(n1)%y
			x2=GB_GEN%node(n2)%x
			y2=GB_GEN%node(n2)%y
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
		
			i1=GB_GEN%nNodes+1
			if(rmu1 <= rmu2) then  ! node 1 hase smaller length factor
				rmu=0.0
				del_rmu=min(rmu_el,rmu1*stretch*.707)
				do
					rmu=rmu+del_rmu
					if(1.0-rmu < del_rmu*0.75) exit

					! add node
                     ! write(*,*) GB_GEN%nNodes,' gen_uni_bnodes a'

					call new_node(GB_GEN,x1*(1.0-rmu)+x2*rmu,y1*(1.0-rmu)+y2*rmu)
					call update_bnodes(GB_GEN,n1,n2,GB_GEN%nNodes)
					n1=GB_GEN%nNodes
					del_rmu=min(rmu_el,del_rmu*stretch*.707)
				end do
			else				  ! node 2 hase smaller length factor
				rmu=0.0
				del_rmu=min(rmu_el,rmu2*stretch*.707)
				do
					rmu=rmu+del_rmu
					if(1.0-rmu < del_rmu*0.75) exit

					! add node
                     ! write(*,*) GB_GEN%nNodes,' gen_uni_bnodes b'
					call new_node(GB_GEN,x2*(1.0-rmu)+x1*rmu,y2*(1.0-rmu)+y1*rmu)
					call update_bnodes(GB_GEN,n1,n2,GB_GEN%nNodes)
					n2=GB_GEN%nNodes
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
	subroutine gen_convex(GB_GEN,reg)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, lev, nng
		real(dp) :: crit_angle, des_angle, phi, fac, aa1, ab1, aa2, ab2, aspect
		integer(i4) :: reg
		logical :: node_inside

		nl_cur=nb_cur

		allocate(lbn(0:nl_cur),stat=ialloc)
		call AllocChk(ialloc,'gen_convex lbn')
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
			call int_angle_nng(GB_GEN,usn,nd,dsn,crit_angle,des_angle,phi,nng)
			if (phi.lt.185.0)then
				select case (nng)
				case (0)
					call check_node_in_el(GB_GEN,node_inside)	
					if(.not. node_inside) then
						call nng_0(GB_GEN,reg,i,des_angle,aspect)
						if(ierr /= 0) goto 1000
					endif

				case (1)
					!if(do_grade) then
					!	call krig_point(GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,fac,d1,d2,d3)
					!else
						fac=1.
					!endif
					call nng_1(GB_GEN,reg,fac,i)
					if(ierr /= 0) goto 1000
				case (2)
					!if(do_grade) then
					!	call krig_point(GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,fac,d1,d2,d3)
					!else
						fac=1.
					!endif
					call nng_2(GB_GEN,reg,fac,i)
					if(ierr /= 0) goto 1000
				end  select
			else
				if(stuck) then
					call fix_stuck(GB_GEN,reg,fac,i)
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
			call add_element(GB_GEN,nd,dsn,usn,reg)
			if(ierr /= 0) goto 1000
		else
			da1=lbn(1)
			db1=lbn(2)
			da2=lbn(3)
			db2=lbn(4)
			call int_angle(GB_GEN%node(db2)%x,GB_GEN%node(db2)%y,GB_GEN%node(da1)%x,GB_GEN%node(da1)%y,GB_GEN%node(db1)%x,GB_GEN%node(db1)%y,aa1)
			call int_angle(GB_GEN%node(da1)%x,GB_GEN%node(da1)%y,GB_GEN%node(db1)%x,GB_GEN%node(db1)%y,GB_GEN%node(da2)%x,GB_GEN%node(da2)%y,ab1)
			call int_angle(GB_GEN%node(db1)%x,GB_GEN%node(db1)%y,GB_GEN%node(da2)%x,GB_GEN%node(da2)%y,GB_GEN%node(db2)%x,GB_GEN%node(db2)%y,aa2)
			call int_angle(GB_GEN%node(da2)%x,GB_GEN%node(da2)%y,GB_GEN%node(db2)%x,GB_GEN%node(db2)%y,GB_GEN%node(da1)%x,GB_GEN%node(da1)%y,ab2)
			if(aa1+aa2.gt.ab1+ab2) then
				call add_element(GB_GEN,da1,db1,da2,reg)
				if(ierr /= 0) goto 1000
				call add_element(GB_GEN,da1,da2,db2,reg)
				if(ierr /= 0) goto 1000
			else
				call add_element(GB_GEN,da1,db1,db2,reg)
				if(ierr /= 0) goto 1000
				call add_element(GB_GEN,db1,da2,db2,reg)
				if(ierr /= 0) goto 1000
			endif
		endif

		1000 deallocate(lbn)

	end subroutine gen_convex
	!----------------------------------------------------------------------
	subroutine check_node_in_el(GB_GEN,node_inside)	
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: j
		real(dp) :: f1, f2, f3
		logical :: node_inside

		node_inside=.false.

		! loop over boundary nodes between dsn and usn
		j=iad(2)
		10    continue
			f1=(GB_GEN%node(lbn(j))%y-GB_GEN%node(nd)%y)*(GB_GEN%node(dsn)%x-GB_GEN%node(nd)%x)-(GB_GEN%node(lbn(j))%x-GB_GEN%node(nd)%x)*(GB_GEN%node(dsn)%y-GB_GEN%node(nd)%y)
			if (f1.gt.1e-10) then
				f2=(GB_GEN%node(lbn(j))%y-GB_GEN%node(dsn)%y)*(GB_GEN%node(usn)%x-GB_GEN%node(dsn)%x)-(GB_GEN%node(lbn(j))%x-GB_GEN%node(dsn)%x)*(GB_GEN%node(usn)%y-GB_GEN%node(dsn)%y)
				if (f2.gt.1.e-10) then
					f3=(GB_GEN%node(lbn(j))%y-GB_GEN%node(usn)%y)*(GB_GEN%node(nd)%x-GB_GEN%node(usn)%x)-(GB_GEN%node(lbn(j))%x-GB_GEN%node(usn)%x)*(GB_GEN%node(nd)%y-GB_GEN%node(usn)%y)
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
	subroutine nng_0(GB_GEN,reg,i,des_angle,aspect)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j
		real(dp) :: des_angle, aspect,olap, d, du, dd
		logical :: overlap
		integer(i4) ::reg

		overlap=.false.
		olap=0.
		j=iad(2)
		if(j.ge.lnbn) j=0
		40    continue
			call check_olap(GB_GEN,usn,dsn,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if(j.ne.iad(-2) .and. .not. overlap) goto 40

		if (.not. overlap) then
			! skip if a neighbouring segment length is much smaller than  proposed one
			d=sqrt((GB_GEN%node(usn)%x-GB_GEN%node(dsn)%x)**2 + (GB_GEN%node(usn)%y-GB_GEN%node(dsn)%y)**2)  
			du=sqrt((GB_GEN%node(usn)%x-GB_GEN%node(usn2)%x)**2 + (GB_GEN%node(usn)%y-GB_GEN%node(usn2)%y)**2)/d  
			dd=sqrt((GB_GEN%node(dsn)%x-GB_GEN%node(dsn2)%x)**2 + (GB_GEN%node(dsn)%y-GB_GEN%node(dsn2)%y)**2)/d  

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
			call add_element(GB_GEN,nd,dsn,usn,reg)
			sn=usn
			des_angle=110.0
			aspect=.4
			stuck=.false.
			i=i-1
		endif

	end subroutine nng_0
	!----------------------------------------------------------------------
	subroutine nng_1(GB_GEN,reg,fac,i)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j
		logical :: overlap
		real(dp) :: olap
		real(dp) :: fac

		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) reg

		olap=0.5
		call gen_node(GB_GEN%node(usn)%x,GB_GEN%node(usn)%y,GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,a1x,a1y,fac)
		call gen_node(GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,GB_GEN%node(dsn)%x,GB_GEN%node(dsn)%y,a2x,a2y,fac)
         ! write(*,*) GB_GEN%nNodes,' nng_1'
		call new_node(GB_GEN,(a1x+a2x)/2.0,(a1y+a2y)/2.0)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		50    continue
			call check_olap(GB_GEN,dsn,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 50

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			60      continue
				call check_olap(GB_GEN,usn,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-2) .and. .not. overlap) goto 60
		endif

		if(.not.overlap) then
		j=iad(1)
		if(j.ge.lnbn) j=0
		70      continue
			call check_olap(GB_GEN,nd,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 70
		endif

		if(.not. overlap) then
			if(.not. in_ob(GB_GEN,GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y)) then
				ierr=5
				write(log_msg,'(a,i10)') 'Node outside outer boundary, area ',reg
				call Msg(log_msg)
				return
			endif
			sn=usn
			lbn(i)=GB_GEN%nNodes
			if(i.eq.lnbn) lbn(0)=GB_GEN%nNodes
			call add_element(GB_GEN,nd,GB_GEN%nNodes,usn,reg)
			if(ierr /= 0) return
			call add_element(GB_GEN,nd,dsn,GB_GEN%nNodes,reg)
			if(ierr /= 0) return
			i=i+1
			!if(i.gt.lnbn) i=i-1
		else
			!     call draw_node(nd,blue,sn_sz)
			!call key_pause
			GB_GEN%nNodes=GB_GEN%nNodes-1
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
	subroutine nng_2(GB_GEN,reg,fac,i)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j
		real(dp) :: olap
		real(dp) :: fac
		logical :: overlap !, in_ob
		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) :: reg

		olap=0.5
		call gen_node(GB_GEN%node(usn)%x,GB_GEN%node(usn)%y,GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,a1x,a1y,fac)
		call gen_node(GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,GB_GEN%node(dsn)%x,GB_GEN%node(dsn)%y,a2x,a2y,fac)
         ! write(*,*) GB_GEN%nNodes,' nng_2 a'
		call new_node(GB_GEN,a1x,a1y)
		if(ierr /= 0) return
         ! write(*,*) GB_GEN%nNodes,' nng_2 b'
		call new_node(GB_GEN,a2x,a2y)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		olap1: do
			call check_olap(GB_GEN,dsn,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
			if (j.eq.iad(-1) .or. overlap) exit olap1
		end do olap1

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap2: do
				call check_olap(GB_GEN,usn,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-2) .or. overlap) exit olap2
			end do olap2
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap3: do
				call check_olap(GB_GEN,nd,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap3
			end do olap3
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap4: do
				call check_olap(GB_GEN,nd,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap4
			end do olap4
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			olap5: do
				call check_olap(GB_GEN,GB_GEN%nNodes,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
				if (j.eq.iad(-1) .or. overlap) exit olap5
			end do olap5
		endif

		if(.not. overlap) then
			if(.not. in_ob(GB_GEN,GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y)) then
				ierr=5
				write(log_msg,'(a,i10)') 'Node outside outer boundary, area ',reg
				call Msg(log_msg)
				return
			endif
			call reallocate_lbn(lnbn+1)
			if(ierr /= 0) return
			sn=usn
			if(i.eq.lnbn) then
				lbn(0)=GB_GEN%nNodes
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
			lbn(i)=GB_GEN%nNodes-1
			lbn(i+1)=GB_GEN%nNodes
			call add_element(GB_GEN,nd,GB_GEN%nNodes-1,usn,reg)
			if(ierr /= 0) return
			call add_element(GB_GEN,nd,GB_GEN%nNodes,GB_GEN%nNodes-1,reg)
			if(ierr /= 0) return
			call add_element(GB_GEN,nd,dsn,GB_GEN%nNodes,reg)
			if(ierr /= 0) return
		else
			GB_GEN%nNodes=GB_GEN%nNodes-2
         ! write(*,*) GB_GEN%nNodes,' nng_2 overlap'
		endif

	end subroutine nng_2
	!----------------------------------------------------------------------
	subroutine write_area_boundary_as_dig(i)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j

		open(888,file='area.dig',status='unknown')
		write(888,*) 'Area ',i
		write(888,*) .true.
		write(888,*) '-999. -999.'
		do j=1,nbn(i)
		write(888,*) GB_GEN%node(bn(i,j))%x,GB_GEN%node(bn(i,j))%y
		end do
		write(888,*) '-999. -999.'
		close(888)

	end subroutine write_area_boundary_as_dig
	!----------------------------------------------------------------------
	subroutine fix_stuck(GB_GEN,reg,fac,i)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j
		real(dp) :: olap
		real(dp) :: fac
		logical :: overlap
		real(dp) :: a1x,a1y,a2x,a2y
		integer(i4) :: reg

		olap=0.8
		call gen_node(GB_GEN%node(usn)%x,GB_GEN%node(usn)%y,GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,a1x,a1y,fac)
		call gen_node(GB_GEN%node(nd)%x,GB_GEN%node(nd)%y,GB_GEN%node(dsn)%x,GB_GEN%node(dsn)%y,a2x,a2y,fac)
         ! write(*,*) GB_GEN%nNodes,' fix_stuck a'
		call new_node(GB_GEN,a1x,a1y)
		if(ierr /= 0) return
         ! write(*,*) GB_GEN%nNodes,' fix_stuck b'
		call new_node(GB_GEN,a2x,a2y)
		if(ierr /= 0) return

		! check for grid overlap
		overlap=.false.
		j=iad(2)
		if(j.ge.lnbn) j=0
		10    continue
			call check_olap(GB_GEN,dsn,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
			j=j+1
			if(j.ge.lnbn) j=0
		if (j.ne.iad(-1) .and. .not. overlap) goto 10

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			20      continue
				call check_olap(GB_GEN,usn,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-2) .and. .not. overlap) goto 20
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			30      continue
				call check_olap(GB_GEN,GB_GEN%nNodes,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 30
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			40      continue
				call check_olap(GB_GEN,nd,GB_GEN%nNodes-1,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 40
		endif

		if(.not.overlap) then
			j=iad(1)
			if(j.ge.lnbn) j=0
			50      continue
				call check_olap(GB_GEN,nd,GB_GEN%nNodes,lbn(j),lbn(j+1),olap,overlap)
				j=j+1
				if(j.ge.lnbn) j=0
			if (j.ne.iad(-1) .and. .not. overlap) goto 50
		endif

		if(.not. overlap) then
			sn=dsn
			lbn(i)=GB_GEN%nNodes-1
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
			lbn(i+1)=GB_GEN%nNodes
			if(i.eq.lnbn) lbn(0)=GB_GEN%nNodes
			call add_element(GB_GEN,nd,GB_GEN%nNodes-1,usn,reg)
			if(ierr /= 0) return
			call add_element(GB_GEN,nd,GB_GEN%nNodes,GB_GEN%nNodes-1,reg)
			if(ierr /= 0) return
			call add_element(GB_GEN,nd,dsn,GB_GEN%nNodes,reg)
			if(ierr /= 0) return
			stuck=.false.
		else
			GB_GEN%nNodes=GB_GEN%nNodes-2
		endif

	end subroutine fix_stuck
	!----------------------------------------------------------------------
	subroutine check_olap(GB_GEN,i1,i2,i3,i_4,olap,overlap)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i1, i2, i3, i_4
		logical :: overlap
		real(dp) :: del,rmu,rmu2,xin,yin, olap
		!
		del=(GB_GEN%node(i2)%x-GB_GEN%node(i1)%x)*(GB_GEN%node(i3)%y-GB_GEN%node(i_4)%y)-(GB_GEN%node(i2)%y-GB_GEN%node(i1)%y)*(GB_GEN%node(i3)%x-GB_GEN%node(i_4)%x)
		if (abs(del).gt.0.0) then
			rmu=((GB_GEN%node(i3)%y-GB_GEN%node(i_4)%y)*(GB_GEN%node(i3)%x-GB_GEN%node(i1)%x)-(GB_GEN%node(i3)%x-GB_GEN%node(i_4)%x)*(GB_GEN%node(i3)%y-GB_GEN%node(i1)%y))/del
			if (rmu.ge.0.0 .and. rmu.le.1.0+olap) then
				xin=rmu*GB_GEN%node(i2)%x+(1.0-rmu)*GB_GEN%node(i1)%x
				yin=rmu*GB_GEN%node(i2)%y+(1.0-rmu)*GB_GEN%node(i1)%y
				if (abs(GB_GEN%node(i3)%x-GB_GEN%node(i_4)%x).gt.0.0) then
					rmu2=(xin-GB_GEN%node(i_4)%x)/(GB_GEN%node(i3)%x-GB_GEN%node(i_4)%x)
				else
					rmu2=(yin-GB_GEN%node(i_4)%y)/(GB_GEN%node(i3)%y-GB_GEN%node(i_4)%y)
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
	subroutine int_angle_nng(GB_GEN,n1,n0,n2,crit_angle,des_angle,phi,nng)
		implicit none
		type(mesh) GB_GEN
		
        integer(i4) :: n1, n0, n2, nng
		real(dp) :: x1,y1,x2,y2,x0,y0,xr,yr
		real(dp) :: theta1, theta2, fxy, phi, des_angle, crit_angle, s1, s2, savg

		x1=GB_GEN%node(n1)%x
		y1=GB_GEN%node(n1)%y
		x0=GB_GEN%node(n0)%x
		y0=GB_GEN%node(n0)%y
		x2=GB_GEN%node(n2)%x
		y2=GB_GEN%node(n2)%y

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
	subroutine update_bnodes(GB_GEN,i1,i2,new)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j,k, i1, i2, new

		logical found
		!
		!     this routine loops over the boundary nodes of each area and if it
		!     finds i1 and i2 then it inserts node new in the area boundary list and
		!     sets the node new any_bndy flag. if area 0 or hole then sets the node
		!     new out_bndy flag.
		!
		call clear(GB_GEN%node(new)%is,any_bndy)
		call clear(GB_GEN%node(new)%is,out_bndy)
		call set(GB_GEN%node(new)%is,b_2nd_type)

		k=0
		found=.false.
		20      continue  ! loop over outer boundary
			if(i1.eq.obn(k) .and. i2.eq.obn(k+1) .or. i2.eq.obn(k) .and. i1.eq.obn(k+1)) then

				call reallocate_outer_boundary_arrays(onbn+1)
				if(ierr /= 0) return

				found=.true.
				! update outer boundary flag and boundary condition if necessary
				if(j.eq.0) then
					call set(GB_GEN%node(new)%is,out_bndy)
				elseif(hole(j)) then
					call set(GB_GEN%node(new)%is,out_bndy)
				endif
				call set(GB_GEN%node(new)%is,any_bndy)
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
						call set(GB_GEN%node(new)%is,out_bndy)
					endif
					call set(GB_GEN%node(new)%is,any_bndy)
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
	  subroutine propose_drop_node(GB_GEN,nd1,nd2, dropped)
		implicit none
		type(mesh) GB_GEN

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
		GB_GEN%node(nd1)%x=(GB_GEN%node(nd1)%x+GB_GEN%node(nd2)%x)/2.
		GB_GEN%node(nd1)%y=(GB_GEN%node(nd1)%y+GB_GEN%node(nd2)%y)/2.

	
		! node nd2 is now moved to nn 
		GB_GEN%node(nd2)%x=GB_GEN%node((GB_GEN%nNodes))%x
		GB_GEN%node(nd2)%y=GB_GEN%node(GB_GEN%nNodes)%y
	
		! if an area boundary list contained node nn, it is now renumbered to nd2
		do k=1,area
			do i=0,nbn(k)
				if (bn(k,i).eq.GB_GEN%nNodes) then 
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
			if (obn(i).eq.GB_GEN%nNodes) then 
				obn(i)=nd2         ! update boundary node list
			endif
		end do

		dropped=.true.
		GB_GEN%nNodes=GB_GEN%nNodes-1

	end subroutine propose_drop_node
	!----------------------------------------------------------------------
	subroutine shuffle_arrays(jnt)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i,j
		real, allocatable :: rdum(:)
		integer(i4), allocatable :: ldum(:),jnt(:),ndummy(:,:), bdummy(:,:)

		allocate(rdum(GB_GEN%nNodes),ldum(GB_GEN%nNodes),jnt(0:GB_GEN%nNodes),ndummy(GB_GEN%nElements,3),bdummy(0:area,0:nb_cur), stat=ialloc)
		call AllocChk(ialloc,'shuffle_arrays work arrays')


		jnt(0)=0
		do i=1,GB_GEN%nNodes
			rdum(jnt(i))=GB_GEN%node(i)%x
		end do

		do i=1,GB_GEN%nNodes
			GB_GEN%node(i)%x=rdum(i)
		end do

		do i=1,GB_GEN%nNodes
			rdum(jnt(i))=GB_GEN%node(i)%y
		end do

		do i=1,GB_GEN%nNodes
			GB_GEN%node(i)%y=rdum(i)
		end do

		do i=1,GB_GEN%nNodes
			ldum(jnt(i))=GB_GEN%node(i)%is
		end do

		do i=1,GB_GEN%nNodes
			GB_GEN%node(i)%is=ldum(i)
		end do

		do i=1,GB_GEN%nElements
			ndummy(i,1)=jnt(GB_GEN%idNode(1,i))
			ndummy(i,2)=jnt(GB_GEN%idNode(2,i))
			ndummy(i,3)=jnt(GB_GEN%idNode(3,i))
		end do

		do i=1,GB_GEN%nElements
			GB_GEN%idNode(1,i)=ndummy(i,1)
			GB_GEN%idNode(2,i)=ndummy(i,2)
			GB_GEN%idNode(3,i)=ndummy(i,3)
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
	subroutine fix_short_segs_batch(GB_GEN)
		implicit none		
		type(mesh) GB_GEN

		integer(i4) :: i, nd1, nd2, pass, nseg_old
		real(dp) :: seg_l
		logical :: dropped
		character*60 :: pass_msg

		logical,allocatable :: n_dropped(:)   ! node has been dropped

		allocate(n_dropped(GB_GEN%nNodes),stat=ialloc)
		call AllocChk(ialloc,'fix_short_segs_batch work array')  

		pass=0
		nseg_old=0
		outer_loop: do 
			n_dropped(:)=.false.
			call calc_segments
			if(gbNseg==nseg_old) exit outer_loop

			nseg_old=gbNseg
			pass=pass+1
			write(pass_msg,'(a,i4,a)') 'Remove short segments, pass ',pass,'...' 
			call Msg(pass_msg)
			do i=1,gbNseg
				nd1=gbSeg_node(i,1)
				nd2=gbSeg_node(i,2)
				if(.not. n_dropped(nd1) .and. .not. n_dropped(nd2)) then  ! only do anything if neither node changed
					seg_l=sqrt((GB_GEN%node(nd1)%x-GB_GEN%node(nd2)%x)**2+(GB_GEN%node(nd1)%y-GB_GEN%node(nd2)%y)**2)
					if(seg_l.lt.segl_user) then
						call propose_drop_node(GB_GEN,nd1,nd2,dropped)
						if(dropped) then
							n_dropped(nd2)=.true.
							n_dropped(GB_GEN%nNodes+1)=.true.
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
	subroutine process_gen_file(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, j, n_area
		integer(i4), allocatable :: jnt(:)   ! new node numbers i.e. jnt(10) = new node number for old node 10

		allocate(jnt(0:nn_in),stat=ialloc) 
		call AllocChk(ialloc,'process_gen_file work array')
		if(ierr /= 0) return

		jnt(:)=0  ! if not assigned yet

		area=0

		! Add OuterBoundary to GB_GEN
		onbn=onbn_in
		call reallocate_outer_boundary_arrays(onbn)
		if(ierr /= 0) goto 1000
		do i=1,onbn_in
			 ! write(*,*) GB_GEN%nNodes,' process_gen_file a'
			call new_node(GB_GEN,x_in(obn_in(i)),y_in(obn_in(i)))  ! all outer boundary nodes are kept
			if(ierr /= 0) goto 1000
			call set(GB_GEN%node(GB_GEN%nNodes)%is,out_bndy)
			call set(GB_GEN%node(GB_GEN%nNodes)%is,any_bndy)
			jnt(obn_in(i))=GB_GEN%nNodes 
			obn(i)=GB_GEN%nNodes
		end do
		obn(0)=jnt(obn_in(0))


		! Form first area from outer boundary
		call Msg('Processing polygons...')
		!do n_area=1,area_in
        n_area=1
			!if(OuterBoundary%Linetyp=='Polygon') then   
				area=area+1
				plot_area(area)=.true.
				nbn(area)=nbn_in(n_area)
				call reallocate_boundary_arrays(area,nbn(area))
				if(ierr /= 0) goto 1000
				do j=0,nbn_in(n_area)
				
					if(jnt(bn_in(area,j)) == 0) then   ! this node hasn't been assigned yet
						 ! write(*,*) GB_GEN%nNodes,' process_gen_file a'
						call new_node(GB_GEN,x_in(bn_in(area,j)),y_in(bn_in(area,j)))  ! keep this boundary node
						if(ierr /= 0) goto 1000
						call set(GB_GEN%node(GB_GEN%nNodes)%is,out_bndy)
						call set(GB_GEN%node(GB_GEN%nNodes)%is,any_bndy)
						jnt(bn_in(area,j))=GB_GEN%nNodes 
						bn(area,j)=GB_GEN%nNodes
					else
						bn(area,j)=jnt(bn_in(area,j))   ! re-assign new node number 
					endif
				end do
			!endif
			elength(area)=elength_in(area)
			if(.not. plan_view) then
				y_elength(area)=y_elength_in(area)
			end if
			stretch_factor(area)=stretch_factor_in(area)
			ndrop_rate(area)=ndrop_rate_in(area)
		!end do

		! treat the rest as cuts
		call msg('Processing addlines...')
		do n_area=2,area_in
            elength(area)=elength_in(area)
			if(.not. plan_view) then
				y_elength(area)=y_elength_in(area)
			end if
			stretch_factor(area)=stretch_factor_in(area)
			ndrop_rate(area)=ndrop_rate_in(area)

			!if(OuterBoundary%Linetyp=='Polyline') then   ! treat as a cut 
				call reallocate_cut_arrays(nbn_in(n_area))
				if(ierr /= 0) goto 1000
				do j=1,nbn_in(n_area)
					xcut(j)=x_in(bn_in(n_area,j))
					ycut(j)=y_in(bn_in(n_area,j))
				end do
				ncut=nbn_in(n_area)
				call add_cut(GB_GEN)
				if(ierr /= 0) goto 1000
			!endif
		end do	
	
		1000 deallocate(jnt) 

	end subroutine process_gen_file
	!----------------------------------------------------------------------
	subroutine set_ob_dir
		implicit none

		integer :: i
		real(dp), allocatable :: dum_x(:),dum_y(:)
		real(dp) :: ang, tot_ang1, tot_ang2
	
		allocate(dum_x(nn_in), dum_y(nn_in), stat=ialloc)
		call AllocChk(ialloc,'set_ob_dir work arrays')
		if(ierr /= 0) return
	
		call int_angle(x_in(nn_in),y_in(nn_in),x_in(1),y_in(1),x_in(2),y_in(2),ang)
		tot_ang1=ang
		do i=2,nn_in-1
			call int_angle(x_in(i-1),y_in(i-1),x_in(i),y_in(i),x_in(i+1),y_in(i+1),ang)
			tot_ang1=tot_ang1+ang
		end do
		call int_angle(x_in(nn_in-1),y_in(nn_in-1),x_in(nn_in),y_in(nn_in),x_in(1),y_in(1),ang)
	
		call int_angle(x_in(1),y_in(1),x_in(nn_in),y_in(nn_in),x_in(nn_in-1),y_in(nn_in-1),ang)
		tot_ang2=ang
		do i=nn_in-1,2,-1
			call int_angle(x_in(i+1),y_in(i+1),x_in(i),y_in(i),x_in(i-1),y_in(i-1),ang)
			tot_ang2=tot_ang2+ang
		end do
		call int_angle(x_in(2),y_in(2),x_in(1),y_in(1),x_in(nn_in),y_in(nn_in),ang)
	
		if(tot_ang1.lt.tot_ang2) goto 1000
	
		do i=1,nn_in
			dum_x(i)=x_in(i)
			dum_y(i)=y_in(i)
		end do
	
		do i=1,nn_in
			x_in(i)=dum_x(nn_in-i+1)
			y_in(i)=dum_y(nn_in-i+1)
		end do
	
		1000 deallocate(dum_x, dum_y)

	end subroutine set_ob_dir
	!----------------------------------------------------------------------
    subroutine set_ob_dir_new(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i
		real(dp), allocatable :: dum_x(:),dum_y(:)
		real(dp) :: ang, tot_ang1, tot_ang2
	
		allocate(dum_x(GB_GEN%nNodes), dum_y(GB_GEN%nNodes), stat=ialloc)
		call AllocChk(ialloc,'set_ob_dir work arrays')
		if(ierr /= 0) return
	
		call int_angle(GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y,GB_GEN%node(1)%x,GB_GEN%node(1)%y,GB_GEN%node(2)%x,GB_GEN%node(2)%y,ang)
		tot_ang1=ang
		do i=2,GB_GEN%nNodes-1
			call int_angle(GB_GEN%node(i-1)%x,GB_GEN%node(i-1)%y,GB_GEN%node(i)%x,GB_GEN%node(i)%y,GB_GEN%node(i+1)%x,GB_GEN%node(i+1)%y,ang)
			tot_ang1=tot_ang1+ang
		end do
		call int_angle(GB_GEN%node(GB_GEN%nNodes-1)%x,GB_GEN%node(GB_GEN%nNodes-1)%y,GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y,GB_GEN%node(1)%x,GB_GEN%node(1)%y,ang)
	
		call int_angle(GB_GEN%node(1)%x,GB_GEN%node(1)%y,GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y,GB_GEN%node(GB_GEN%nNodes-1)%x,GB_GEN%node(GB_GEN%nNodes-1)%y,ang)
		tot_ang2=ang
		do i=GB_GEN%nNodes-1,2,-1
			call int_angle(GB_GEN%node(i+1)%x,GB_GEN%node(i+1)%y,GB_GEN%node(i)%x,GB_GEN%node(i)%y,GB_GEN%node(i-1)%x,GB_GEN%node(i-1)%y,ang)
			tot_ang2=tot_ang2+ang
		end do
		call int_angle(GB_GEN%node(2)%x,GB_GEN%node(2)%y,GB_GEN%node(1)%x,GB_GEN%node(1)%y,GB_GEN%node((GB_GEN%nNodes))%x,GB_GEN%node(GB_GEN%nNodes)%y,ang)
	
		if(tot_ang1.lt.tot_ang2) goto 1000
	
		do i=1,GB_GEN%nNodes
			dum_x(i)=GB_GEN%node(i)%x
			dum_y(i)=GB_GEN%node(i)%y
		end do
	
		do i=1,GB_GEN%nNodes
			GB_GEN%node(i)%x=dum_x(GB_GEN%nNodes-i+1)
			GB_GEN%node(i)%y=dum_y(GB_GEN%nNodes-i+1)
		end do
	
		1000 deallocate(dum_x, dum_y)

	end subroutine set_ob_dir_new
	!----------------------------------------------------------------------
	subroutine add_cut(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: jj,ncut_new
		logical :: split

		10    call calc_segments
			if(ierr /= 0) return
			call analyze_cut(GB_GEN,split)
			if(ierr /= 0) return
			if(n_int.gt.0) then
			call tie_in_cut(GB_GEN)
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
		type(mesh) GB_GEN

		integer(i4) :: i, k, i_int, nds, nde, nc1, nc2, nbn_old, ndum, old_nn, nn_s, nn_e, inc

		real(dp) :: rs1, rs2

		logical :: tie_in_start,tie_in_end

		integer(i4),allocatable :: ldum(:)
		allocate(ldum(0:nb_cur),stat=ialloc)
		call AllocChk(ialloc,'update_area work array')
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
			old_nn=GB_GEN%nNodes
			!       Define the coordinates of the new nodes
			do i=nct(i_int-1)+1,nct(i_int)
			 ! write(*,*) GB_GEN%nNodes,' update_area'
				call new_node(GB_GEN,xcut(i),ycut(i))
				if(ierr /= 0) goto 1000
			end do
			!       Store the start and end numbers of the new nodes
			nn_s=old_nn+1
			nn_e=GB_GEN%nNodes
			!
			!       If nc1 is greater than nc2 then the cut enters the area downstream
			!       of the point of exit.  In this case swap the start and end node
			!       numbers and the counter direction inc.
			inc=1
			if(nc1.gt.nc2) then
				ndum=nc2
				nc2=nc1
				nc1=ndum
				nn_s=GB_GEN%nNodes
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
		call AllocChk(ialloc,'update_area work array')
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
	subroutine calc_al_ae(GB_GEN,i_int,skip)
		implicit none
		type(mesh) GB_GEN

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
		n1=gbSeg_node(nsg(i_int),1)
		n2=gbSeg_node(nsg(i_int),2)
		!
		!     This is the cut node number downstream of the intercept (inside area)
		nc_ds=nct(i_int)+1
		!
		!     Simplest case is where  0.0 < rs and rs < 1.0
		if(rs(i_int).gt.0.0 .and. rs(i_int).lt.1.0) then
			call direct(xcut(nc_ds),ycut(nc_ds), GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,entering)
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
				call int_angle(GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,GB_GEN%node(n3)%x,GB_GEN%node(n3)%y,phi)
				call int_angle(GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,xds,yds,phi2)
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
					call int_angle(GB_GEN%node(n3)%x,GB_GEN%node(n3)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,phi)
					call int_angle(GB_GEN%node(n3)%x,GB_GEN%node(n3)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,xds,yds,phi2)
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
				call int_angle(GB_GEN%node(n0)%x,GB_GEN%node(n0)%y,GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,phi)
				call int_angle(GB_GEN%node(n0)%x,GB_GEN%node(n0)%y,GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,xds,yds,phi2)
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
					call int_angle(GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,GB_GEN%node(n0)%x,GB_GEN%node(n0)%y,phi)
					call int_angle(GB_GEN%node(n2)%x,GB_GEN%node(n2)%y,GB_GEN%node(n1)%x,GB_GEN%node(n1)%y,xds,yds,phi2)
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
	subroutine analyze_cut(GB_GEN,split)
		implicit none
		type(mesh) GB_GEN

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
			do j=1,gbNseg
				xs1=GB_GEN%node(gbSeg_node(j,1))%x
				ys1=GB_GEN%node(gbSeg_node(j,1))%y
				xs2=GB_GEN%node(gbSeg_node(j,2))%x
				ys2=GB_GEN%node(gbSeg_node(j,2))%y
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
							call calc_al_ae(GB_GEN,n_int,skip)
							if(skip) n_int=n_int-1   ! abandon intercept
						endif
					endif
				endif
			end do
		end do
		!
		if(n_int.eq.0) then
			if(.not. extended) then
				call extend_cut_upstream(GB_GEN)
				if(ierr /= 0) return
				call extend_cut_downstream(GB_GEN)
				if(ierr /= 0) return
				extended=.true.
				goto 11
			else
				return
			endif
		else
			if(nct(1).ne.1 .and. al(1).ne.0) then
				call extend_cut_upstream(GB_GEN)
				if(ierr /= 0) return
				goto 11
			endif
			if(nct(n_int).ne.ncut-1 .and. ae(n_int).ne.0) then
				call extend_cut_downstream(GB_GEN)
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
	subroutine extend_cut_downstream(GB_GEN)
		implicit none
		type(mesh) GB_GEN

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
		do j=1,gbNseg
			xs1=GB_GEN%node(gbSeg_node(j,1))%x
			ys1=GB_GEN%node(gbSeg_node(j,1))%y
			xs2=GB_GEN%node(gbSeg_node(j,2))%x
			ys2=GB_GEN%node(gbSeg_node(j,2))%y
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
	subroutine extend_cut_upstream(GB_GEN)
		implicit none
		type(mesh) GB_GEN

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
		do j=1,gbNseg
			xs1=GB_GEN%node(gbSeg_node(j,1))%x
			ys1=GB_GEN%node(gbSeg_node(j,1))%y
			xs2=GB_GEN%node(gbSeg_node(j,2))%x
			ys2=GB_GEN%node(gbSeg_node(j,2))%y
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
    subroutine tie_in_cut(GB_GEN)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, nds, nde

		logical :: tie_in_start,tie_in_end
	
		!  This routine determines whether or not to create a new node at the
		!  intercept or tie in to an existing node.
		!
		call check_tie_in(GB_GEN,xi(1),yi(1),tie_in_start,nds)
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
				call check_tie_in(GB_GEN,xi(i),yi(i),tie_in_end,nde)
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
	subroutine check_tie_in(GB_GEN,xtie,ytie,tie_in,p_node)
		implicit none
		type(mesh) GB_GEN

		logical tie_in
		integer(i4) p_node
		real*8 xtie,ytie,difx,dify
	
		! Find closest node and calculate distances
		call gen_find_node(GB_GEN,xtie,ytie,p_node)
		difx=abs(xtie-GB_GEN%node(p_node)%x)
		dify=abs(ytie-GB_GEN%node(p_node)%y)
	
		if (difx.lt.1.e-5 .and. dify.lt.1.e-5) then ! node exists
			tie_in=.true.
		else
			tie_in=.false.
			 ! write(*,*) GB_GEN%nNodes,' check_tie_in'
			call new_node(GB_GEN,xtie,ytie)
			if(ierr /= 0) return
			p_node=GB_GEN%nNodes
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
			n1=gbSeg_node(j,1)
			n2=gbSeg_node(j,2)
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
		if(j.le.gbNseg) goto 10
	
		20  continue  
	
	end subroutine find_dsn
	!----------------------------------------------------------------------
	subroutine calc_segments
		implicit none

		integer(i4) :: i, j, k, n1, n2 

		logical prev_def

		!     Form a list of unique boundary segment pairs.  These pairs have the
		!     following properties:
		!        (1) gbSeg_node(i,1) is upstream node for seg_area(i,1)
		!            gbSeg_node(i,2) is upstream node for seg_area(i,2)
		!        (2) if seg_area(i,2) is 0 then this is an outer boundary segment

		gbNseg=0
		gbSeg_node(:,:)=0
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
					if (n1.eq.gbSeg_node(j,2) .and. n2.eq.gbSeg_node(j,1)) then
						prev_def=.true.
						seg_area(j,2)=k
					endif
					j=j+1
				if(.not. prev_def .and. j.le.gbNseg) goto 40
				if (.not. prev_def) then
					call gbNewSegment(gbNseg,n1,n2,k,0,0,0)
					if(ierr /= 0) return
				endif
				i=i+1
			if(i.lt.nbn(k)) goto 30
		end do

	end subroutine calc_segments
	!----------------------------------------------------------------------
	subroutine gen_find_node(GB_GEN,x1,y1,p_node)
		implicit none
		type(mesh) GB_GEN

		integer(i4) :: i, p_node
		real(dp) :: x1,y1, dist_min, f1

		i=1
		dist_min=1.0e20
		97    continue
			f1=sqrt((x1-GB_GEN%node(i)%x)**2+((y1-GB_GEN%node(i)%y))**2)
			if(f1.lt.dist_min) then
				p_node=i
				dist_min=f1
			endif
			i=i+1
		if (i.LE.GB_GEN%nNodes) goto 97

	end subroutine gen_find_node

    !------------------------------------------------------------------------------------------------------
    function in_ob(GB_GEN,px,py)
        implicit none
		type(mesh) GB_GEN

        LOGICAL :: in_ob

        integer :: i, cn, n
        real(dp) :: px,py, vt

        cn = 0  ! the crossing number counter

        do i=0,onbn-1
           if (((GB_GEN%node(obn(i))%y <= py) .and. (GB_GEN%node(obn(i+1))%y > py))   & ! an upward crossing
            .or.  ((GB_GEN%node(obn(i))%y > py) .and. (GB_GEN%node(obn(i+1))%y <= py)))  then ! a downward crossing
                ! compute the actual edge-ray intersect x-coordinate
                vt = (py - GB_GEN%node(obn(i))%y) / (GB_GEN%node(obn(i+1))%y - GB_GEN%node(obn(i))%y)
                if (px < GB_GEN%node(obn(i))%x + vt * (GB_GEN%node(obn(i+1))%x - GB_GEN%node(obn(i))%x)) then ! px < intersect
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
