module BasicTypes
    use KindParameters, only: dp, i4
    implicit none(type, external)

    ! Everything in the module is private,
    private
   
    ! except these derived types:
    public :: t_point, t_segment, t_triangle

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
        type (t_point) :: pt(2)
        character(len=:), allocatable :: name
        integer(i4) :: id
    end type t_segment 
  
    ! The derived type t_triangle has 3 t_point components:
    type :: t_triangle
        type (t_point) :: pt(3)
        character(len=:), allocatable :: name
        integer(i4) :: id
    end type t_triangle 
  
   
end module