module BasicTypes
    use KindParameters, only: dp, i4
    use GeneralRoutines
    implicit none(type, external)

    !! Everything in the module is private,
    !private
    !
    !! excePoint these derived types:
    !public :: t_point, t_segment, t_triangle

    ! The derived type t_point has x, y, z data components:
    type :: t_point
        real(dp) :: x
        real(dp) :: y
        real(dp) :: z
        character(len=:), allocatable :: name
        integer(i4) :: id
    end type t_point 

    ! The derived type t_segment has 2 t_point components:
    type :: t_segment
        type (t_point) :: Point(2)
        character(len=:), allocatable :: name
        integer(i4) :: id
    end type t_segment 
  
    ! The derived type t_triangle has 3 t_point components:
    type :: t_triangle
        type (t_point) :: Point(3)
        character(len=:), allocatable :: name
        integer(i4) :: id
    end type t_triangle 
  
    ! The derived type t_triangle has 3 t_point components:

    ! The derived type t_shape has nPoints t_point components:
    type :: t_line
        type (t_point), allocatable :: Point(:)
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: nPoints  
        character(40) :: LineTyp ! line type eg polyline or polygon
        real(dp) :: Length ! line length
        real(dp) :: Area ! line area if polygon
    end type t_line
  
    type :: t_pointset
        type (t_point), allocatable :: Point(:)
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: nPoints  
        character(40) :: PointTyp ! line type eg polyline or polygon
        real(dp) :: Length ! line length
        real(dp) :: Area ! line area if polygon
    end type t_pointset

    contains
    !----------------------------------------------------------------------
    subroutine GrowLineArray(iArray,nSizeIn,nSizeout)
        implicit none
        type(t_line), allocatable :: iArray(:)
        type(t_line), allocatable :: iTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        integer(i4) :: iSize
        
        if(.not. allocated(iArray)) then
            allocate(iArray(nSizeout),stat=ialloc)
            return
        endif
        
        iSize=size(iArray)
        if(nSizeout < iSize) then
            return
        endif
        
        if(nSizeIn < nSizeout) then
            allocate(iTMP(nSizeout),stat=ialloc)
	        call AllocChk(ialloc,'GrowLineArray iTMP array')
            iTMP(1:nSizeIn) = iArray
            call move_alloc (iTMP, iArray)
        else
            return
        endif
        
    end subroutine GrowLineArray
    !----------------------------------------------------------------------
    subroutine LineFrom_ID_X_Y_File(fnum,Line)
        implicit none
        type(t_line), intent(out) :: Line

        integer(i4) :: i, Fnum
	    character(MAX_LBL) :: VarSTR

        line%nPoints=0
	    read(FNum,'(a)') VarSTR
        CountLoop:do
     	    read(FNum,*,iostat=status) line%nPoints
            if(status/=0) then
                exit CountLoop
            end if
        end do CountLoop
        
        allocate(Line%Point(0:line%nPoints),stat=ialloc)
	    call AllocChk(ialloc,'LineFrom_ID_X_Y_File Line%Point array')

        rewind(FNum)
	    read(FNum,'(a)') VarSTR
        ReadLoop:do i=0,line%nPoints
     	    read(FNum,*,iostat=status) line%point(i)%id,line%point(i)%x,line%point(i)%y
        end do ReadLoop
        
        
        if(distance2Points(Line%Point(0),Line%Point(line%nPoints))<1.0e-6_dp) then
            Line%LineTyp='Polygon'
            line%point(0)%id=line%point(line%nPoints)%id
        else
            Line%LineTyp='Polyline'
        end if
        
        Line%Length=LineLength(Line)
        
	    call freeunit(FNum)

    end subroutine LineFrom_ID_X_Y_File

    function LineLength(Line) result(Length)
        implicit none
        type(t_line), intent(in) :: Line
        real(dp) :: Length
        integer(i4) :: i
        Length=0.0_dp
        do i=1,Line%nPoints
            Length=Length+Distance2Points(Line%Point(i-1),Line%Point(i))
        end do
    end function LineLength
    
    function distance2Points(Point1,Point2) result(Dist)
        implicit none
        type(t_point), intent(in) :: Point1, Point2
        real(dp) :: Dist
        Dist=sqrt( (Point2%x-Point1%x)**2 + (Point2%y-Point1%y)**2 + (Point2%z-Point1%z)**2 )
    end function Distance2Points
    
    !----------------------------------------------------------------------
    subroutine WellsFrom_ID_X_Y_File(fnum,wells)
        implicit none
        type(t_pointset), intent(out) :: wells

        integer(i4) :: i, Fnum
	    character(MAX_LBL) :: VarSTR

        wells%nPoints=0
	    read(FNum,'(a)') VarSTR
        CountLoop:do
     	    read(FNum,*,iostat=status) wells%nPoints
            if(status/=0) then
                exit CountLoop
            end if
        end do CountLoop
        
        allocate(Wells%Point(wells%nPoints),stat=ialloc)
	    call AllocChk(ialloc,'WellsFrom_ID_X_Y_File wells%Point array')

        rewind(FNum)
	    read(FNum,'(a)') VarSTR
        ReadLoop:do i=1,wells%nPoints
     	    read(FNum,*,iostat=status) wells%point(i)%id,wells%point(i)%x,wells%point(i)%y
        end do ReadLoop
        
	    call freeunit(FNum)

    end subroutine WellsFrom_ID_X_Y_File


end module