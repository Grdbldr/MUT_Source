Module MeshGeneration
    use GeneralRoutines
    use Tecplot
    implicit none
    
    contains

    !----------------------------------------------------------------------
    subroutine MUSG_GenerateCLNDomain(FNum,TECPLOT_CLN)
        implicit none
        integer :: FNum
        type(TecplotDomain) TECPLOT_CLN
        
        character(256) :: Instruction
        character(60) :: xyzListOfPoints_cmd			=   'xyz list of points'
        character(60) :: CLNnCells_cmd			=   'cln number of cells'
        
        real, allocatable :: xi(:), yi(:), zi(:)  ! xyz coordinate list defining CLN to be read
        integer :: nPoints  ! number of points in list
        real :: CLNnCells ! Desired number of cells along CLN
        
	    ! Process CLN grid generation instructions

        read_Instructions: do
            read(FNum,'(a60)',iostat=status) Instruction
            if(status /= 0) exit

		    call lcase(instruction)

            if(index(Instruction,'end') /= 0) then
                call Msg(TAB//'end generate cln domain instructions')
                exit read_Instructions
            else
                call Msg('')
                call Msg(TAB//Instruction)
            end if
                

            if(index(Instruction, xyzListOfpoints_cmd)  /= 0) then
                call GetXYZ(FNum,xi,yi,zi,nPoints)

            else if(index(Instruction, CLNnCells_cmd)  /= 0) then
                read(FNum,*) CLNnCells
                
            else
			    call ErrMsg(TAB//'Unrecognized instruction: generate cln domain')
            end if

        end do read_Instructions
        
        
        continue
        
    end subroutine MUSG_GenerateCLNDomain
    !----------------------------------------------------------------------
    subroutine GetXYZ(FNum,xi,yi,zi,nPoints)
        implicit none
        integer :: FNum
        
        integer :: nSizeInit=2
        real, allocatable :: xi(:), yi(:), zi(:)  ! xyz coordinate list defining CLN to be read
        real, allocatable :: xiTMP(:), yiTMP(:), ziTMP(:)  ! temporary xyz arrays
        integer :: nPoints  ! number of points in list

        integer :: i, j
	    real(dr) :: t
                
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

		    call lcase(instruction)

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
				    call ErrMsg('Bad xz pair')
                endif
                
                write(TmpSTR,'(i8,2x,3g15.5)') nPoints,xi(nPoints),yi(nPoints),zi(nPoints)
                call Msg(TAB//trim(TmpSTR))

		    endif
        end do
        
       
        continue

    end subroutine GetXYZ

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
        write(TMPStr,'(a,g15.3)') TAB//'Mesh length in X        ',xl
        call Msg(TMPStr)
        write(TMPStr,'(a,i9)')    TAB//'Number of elements in X ',nbx
        call Msg(TMPStr)

        read(FNum,*) yl, nby
        write(TMPStr,'(a,g15.3)') TAB//'Mesh length in Y        ',yl
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
            TMPLT.SideLength(2,i)=abs(TMPLT.x(TMPLT.iNode(3,i)) - TMPLT.x(TMPLT.iNode(2,i)))
            TMPLT.SideLength(3,i)=abs(TMPLT.x(TMPLT.iNode(4,i)) - TMPLT.x(TMPLT.iNode(3,i)))
            TMPLT.SideLength(4,i)=abs(TMPLT.x(TMPLT.iNode(1,i)) - TMPLT.x(TMPLT.iNode(4,i)))


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
            call lcase(instruction)

            if(index(instruction,'end') /= 0) then
                call Msg(TAB//'end xz pairs instructions')
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
                
                write(TmpSTR,'(i8,2x,2g15.5)') npairs,xp(npairs),zp(npairs)
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

end Module MeshGeneration
