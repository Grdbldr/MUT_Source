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
        integer(i4) :: nElements                ! number of Elements in the mesh
        integer(i4) :: nLayers                 ! number of layers in the mesh 
        integer(i4) :: nNold               ! number of nodes in the mesh  

        integer(i4) :: nNodesPerElement        ! number of nodes/Element  
        integer(i4), allocatable :: iNode(:,:)  ! node list for Element (nElements, nNodesPerElement)
           
        !! arrays of size nElements
        real(dp), allocatable :: xElement(:)      ! Element x coordinate
        real(dp), allocatable :: yElement(:)      ! Element y coordinate
        real(dp), allocatable :: zElement(:)      ! Element z coordinate
        integer(i4), allocatable :: iLayer(:)      ! Element layer number
        integer(i4), allocatable :: iZone(:)       ! Element zone number
        
        ! Element properties for TMPLT, 2D in XY
        real(dp), allocatable :: ElementArea(:)   ! element area
        real(dp), allocatable :: SideLength(:,:) ! length of each element side (nNodesPerElement,nElements)
        real(dp), allocatable :: xSide(:,:)      ! x coordinate of either: inner circle radius tangent to side (triangles) or, midpoint(rectangles) (nNodesPerElement,nElements)
        real(dp), allocatable :: ySide(:,:)      ! y coordinate of either: inner circle radius tangent to side (triangles) or, midpoint(rectangles) (nNodesPerElement,nElements)
        ! Triangles
        real(dp), allocatable :: rCircle(:)      ! inner circle radius 
        real(dp), allocatable :: xCircle(:)      ! x coordinate of inner circle centre
        real(dp), allocatable :: yCircle(:)      ! y coordinate of inner circle centre
        real(dp), allocatable :: zCircle(:)      ! z coordinate of inner circle centre, 0 by default for TMPLT
        
        real(dp), allocatable :: Length(:) ! length of CLN cell
        real(dp), allocatable :: LowestElevation(:) ! lowest point of CLN cell
        real(dp), allocatable :: SlopeAngle(:) ! angel of CLN cell with horizontal
        
        ! Element connections
        integer(i4), allocatable :: njag      ! total number of connections for mesh
        integer(i4), allocatable :: ia(:)      ! size nElements, number of connections/Element
        integer(i4), allocatable :: ConnectionList(:,:)    ! connected to cell list (MAX_CNCTS,nCells)
        integer(i4), allocatable :: ThroughFace(:,:)  ! connected through face  (MAX_CNCTS,nCells)
        real(dp), allocatable :: ConnectionLength(:,:)    ! variable CLN in modflow, not to be confused with CLN (Connected Linear Network)
        real(dp), allocatable :: PerpendicularArea(:,:)   ! FAHL in modflow

        ! of size nNold
        real(dp), allocatable :: x(:) 
        real(dp), allocatable :: y(:)
        real(dp), allocatable :: z(:)
        
        integer(i4) :: nZones                  ! number of zones in domain
        integer(i4),allocatable	:: Element_is(:)  ! size nElements,  bit setting e.g. chosen/not chosen
        integer(i4),allocatable	:: Node_is(:)  ! size nNold,  bit setting e.g. chosen/not chosen
        integer(i4),allocatable	:: Zone_is(:)  ! size nZones,  bit setting e.g. chosen/not chosen
        
        ! Faces
        logical :: FacesCalculated = .false.
        integer(i4) :: nFaces  = 0
        integer(i4) :: nNodesPerFace
        integer(i4) :: nFacesPerElement
        integer(i4), allocatable :: LocalFaceNodes(:,:) ! nNodesPerFace, nFacesPerElement
        integer(i4), allocatable :: FaceHost(:,:) ! (nFacesPerElement,nElements)
        integer(i4), allocatable :: FaceNeighbour(:,:) ! (nFacesPerElement,nElements)
        real(dp), allocatable :: FaceCentroidX(:,:) ! (nFacesPerElement,nElements)
        real(dp), allocatable :: FaceCentroidY(:,:) ! (nFacesPerElement,nElements)
        real(dp), allocatable :: FaceCentroidZ(:,:) ! (nFacesPerElement,nElements)

            
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
