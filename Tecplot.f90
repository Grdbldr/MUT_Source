module Tecplot !
    use iso_c_binding
    use GeneralRoutines
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
        !character(128) :: ElementType      ! for tecplot, febrick (GWF), fequadrilateral(SWF), felineseg(CLN)

        character(11) :: Name='none'
            
    end type TecplotDomain


    ! General parameters 
	integer(i4) :: ieco = 0
	integer(i4) :: nln
	integer(i4) :: len
	integer(i4) :: nfile
    real(dp)	:: xc, yc, zc
    integer(i4) :: nx, ny, nz
    
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
    


end module Tecplot !
