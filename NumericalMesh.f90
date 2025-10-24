module NumericalMesh
    use BasicTypes
    use GeneralRoutines
    implicit none
    
    real(sp) :: MinSeparationDistance=0.0001

    
    type node 
        real(dp) :: x   ! coordinates
        real(dp) :: y
        real(dp) :: z
        
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
    end type node 
    
    type element
        character(40) :: Typ ! element typ eg triangle, quadrilateral, prism, block etc
        !character(len=:), allocatable :: Typ ! number of nodes in element
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
        integer(i4) :: idZone
        integer(i4) :: iLayer ! layer number for extruded mesh
        
        real(dp) :: x    ! coordinates of element centroid
        real(dp) :: y
        real(dp) :: z

        real(dp) :: area  ! area of the element if 2D
        real(dp) :: xyArea  ! area of the 2D element in XY 
        
        real(dp) :: xCircle  ! triangle inner circle
        real(dp) :: yCircle
        real(dp) :: zCircle
        real(dp) :: rCircle     ! triangle inner circle radius
        !real(dp) :: xTangent(3) ! triangle inner circle radius tangent to side
        !real(dp) :: yTangent(3)         
        
        real(dp) :: xSide(4)      ! x coordinate of either: inner circle radius tangent to side (triangles) or, midpoint(rectangles) 
        real(dp) :: ySide(4)      ! y coordinate of either: inner circle radius tangent to side (triangles) or, midpoint(rectangles) 
        real(dp) :: SideLength(4) ! length of sides if triangle or rectangle
        
       
    end type element    
    
    
    type zone 
        character(len=:), allocatable :: name
        integer(i4) :: id
        integer(i4) :: is
    end type zone 

    type mesh
        character(len=:), allocatable :: name
        integer(i4) :: id
        !character(128) :: ElementType      ! eg triangle, quadrilateral, prism, block etc
        character(MAX_LBL) :: STR_LengthUnit
  
        character(40) :: TecplotTyp ! element typ eg fetriangle, fequadrilateral, feblock etc

        integer(i4) :: nNodes              ! number of nodes in the mesh
        type(node), allocatable :: node(:) ! array of nodes
        real(dp) :: xMin, xMax, yMin, yMax, zMin, zMax ! bounding box of the mesh
       
        integer(i4) :: nElements              ! number of elements in the mesh
        type(element), allocatable :: element(:) ! array of elements

        integer(i4) :: nNodesPerElement ! number of nodes in element
        integer(i4), allocatable :: idNode(:,:) ! array of local node ids for elements
        
        ! Element or cell connection 
        integer(i4) :: njag      ! total number of connections for mesh
        integer(i4), allocatable :: ia(:)      ! number of connectionsper element or cell
        integer(i4), allocatable :: ConnectionList(:,:)    ! connected to cell list (MAX_CNCTS,nCells)
        integer(i4), allocatable :: ThroughFace(:,:)  ! connected through face  (MAX_CNCTS,nCells)

        
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
        
        integer(i4) :: nZones
        type(zone), allocatable :: zone(:) ! array of zones
        
        integer(i4) :: nLayers                 ! number of layers in the mesh 
        



    end type mesh
   
    type MeshGroup
        integer (i4) :: nMesh=0
        type(mesh), allocatable :: mesh(:) ! array of meshes
    end type MeshGroup



    contains

	!-----------------------------------------------------------------------
	subroutine mesh_limits(D)
		implicit none
		type(mesh) D

		D%xmin=MINVAL(D%node%x)
		D%xmax=MAXVAL(D%node%x)
		D%ymin=MINVAL(D%node%y)
		D%ymax=MAXVAL(D%node%y)
		D%zmin=MINVAL(D%node%z)
		D%zmax=MAXVAL(D%node%z)

    end subroutine mesh_limits

    !----------------------------------------------------------------------
    subroutine BuildFaceTopologyFrommesh(D)
        implicit none

        type (mesh)  D

        integer(i4) :: i, j, k, l
        
        call StopWatch(1,'BuildFaceTopologyFrommesh')
        call Msg('Building face topology from model domain%..') 

        
        ! Local node numbers for 2D and 3D element faces 
        select case (D%TecplotTyp)
        case ('felineseg')
                D.nNodesPerFace=1
                D.nFacesPerElement=2
                allocate(D.LocalFaceNodes(D.nNodesPerFace, D.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'D.LocalFaceNodes  fetriangle')
		                                 ! end1   end2   
	            !data D.LocalFaceNodes/     1,      2   /
	            D.LocalFaceNodes(1,1)=1    ! end1
                D.LocalFaceNodes(1,2)=2    ! end2   
        
        case ('fetriangle')
                D.nNodesPerFace=2
                D.nFacesPerElement=3
                allocate(D.LocalFaceNodes(D.nNodesPerFace, D.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'D.LocalFaceNodes  fetriangle')
		                                 ! side1   side2   side3 
	            !data D.LocalFaceNodes/    1,2,      2,3,    3,1   /
	            D.LocalFaceNodes(1,1)=1; D.LocalFaceNodes(2,1)=2    ! side1
                D.LocalFaceNodes(1,2)=2; D.LocalFaceNodes(2,2)=3    ! side2   
                D.LocalFaceNodes(1,3)=3; D.LocalFaceNodes(2,3)=1    ! side3 
            
        case ('fequadrilateral')
                D.nNodesPerFace=2
                D.nFacesPerElement=4
                allocate(D.LocalFaceNodes(D.nNodesPerFace, D.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'D.LocalFaceNodes  fequadrilateral')
		                                 ! side1     side2   side3   side4 
	            !data D.LocalFaceNodes/    1,2,      2,3,    3,4,    4,1   /
	            D.LocalFaceNodes(1,1)=1; D.LocalFaceNodes(2,1)=2    ! side1
                D.LocalFaceNodes(1,2)=2; D.LocalFaceNodes(2,2)=3    ! side2   
                D.LocalFaceNodes(1,3)=3; D.LocalFaceNodes(2,3)=4    ! side3 
                D.LocalFaceNodes(1,4)=4; D.LocalFaceNodes(2,4)=1    ! side3 
                
        case ('feprism')
            D.nNodesPerFace=4
            D.nFacesPerElement=5
            allocate(D.LocalFaceNodes(D.nNodesPerFace, D.nFacesPerElement),stat=ialloc)
            call AllocChk(ialloc,'D.LocalFaceNodes  feprism')
                                        ! bottom      top         side1       side2       side3 
	        !data D.LocalFaceNodes/    1,2,3,0,    4,5,6,0,    1,2,5,4,    1,3,6,4,    2,3,6,5   /
	        D.LocalFaceNodes(1,1)=1; D.LocalFaceNodes(2,1)=2; D.LocalFaceNodes(3,1)=3; D.LocalFaceNodes(4,1)=0    ! bottom
	        D.LocalFaceNodes(1,2)=4; D.LocalFaceNodes(2,2)=5; D.LocalFaceNodes(3,2)=6; D.LocalFaceNodes(4,2)=0    ! top
	        D.LocalFaceNodes(1,3)=1; D.LocalFaceNodes(2,3)=2; D.LocalFaceNodes(3,3)=5; D.LocalFaceNodes(4,3)=4    ! side1
	        D.LocalFaceNodes(1,4)=1; D.LocalFaceNodes(2,4)=3; D.LocalFaceNodes(3,4)=6; D.LocalFaceNodes(4,4)=4    ! side2
	        D.LocalFaceNodes(1,5)=2; D.LocalFaceNodes(2,5)=3; D.LocalFaceNodes(3,5)=6; D.LocalFaceNodes(4,5)=5    ! side3

        case ('febrick')
            D.nNodesPerFace=4
            D.nFacesPerElement=6
            allocate(D.LocalFaceNodes(D.nNodesPerFace, D.nFacesPerElement),stat=ialloc)
            call AllocChk(ialloc,'D.LocalFaceNodes  febrick')
                                        ! bottom      top         front       back        left        right 
	        !data D.LocalFaceNodes/    1,2,3,4,    5,6,7,8,    1,2,6,5,    4,3,7,8,    1,5,8,4,    2,6,7,3   /
	        D.LocalFaceNodes(1,1)=1; D.LocalFaceNodes(2,1)=2; D.LocalFaceNodes(3,1)=3; D.LocalFaceNodes(4,1)=4    ! bottom
	        D.LocalFaceNodes(1,2)=5; D.LocalFaceNodes(2,2)=6; D.LocalFaceNodes(3,2)=7; D.LocalFaceNodes(4,2)=8    ! top
	        D.LocalFaceNodes(1,3)=1; D.LocalFaceNodes(2,3)=2; D.LocalFaceNodes(3,3)=6; D.LocalFaceNodes(4,3)=5    ! front
	        D.LocalFaceNodes(1,4)=4; D.LocalFaceNodes(2,4)=3; D.LocalFaceNodes(3,4)=7; D.LocalFaceNodes(4,4)=8    ! back
	        D.LocalFaceNodes(1,5)=1; D.LocalFaceNodes(2,5)=5; D.LocalFaceNodes(3,5)=8; D.LocalFaceNodes(4,5)=4    ! left
	        D.LocalFaceNodes(1,6)=2; D.LocalFaceNodes(2,6)=6; D.LocalFaceNodes(3,6)=7; D.LocalFaceNodes(4,6)=3    ! right
            
        case default
            call ErrMsg('Tecplot Element Type '//trim(D%TecplotTyp)//' not supported')
        end select  
  
        ! *** ASSUMPTION: If two face centroids are coincident, then the faces are shared by neighbouring elements
        allocate(D.FaceCentroidX(D.nFacesPerElement,D.nElements), &
                 D.FaceCentroidY(D.nFacesPerElement,D.nElements), &
                 D.FaceCentroidZ(D.nFacesPerElement,D.nElements),stat=ialloc)
        call AllocChk(ialloc,'Face Centroid arrays')
        
        allocate(D.FaceHost(D.nFacesPerElement,D.nElements), &
                 D.FaceNeighbour(D.nFacesPerElement,D.nElements),stat=ialloc)
        call AllocChk(ialloc,'Face host/neighbour arrays')
        D.FaceHost(:,:)=0
               
        D.nFaces=0
        do i=1,D.nElements
            do j=1,D.nFacesPerElement
                D.FaceHost(j,i)=i
                D.nFaces=D.nFaces+1
                D.FaceCentroidX(j,i)=0.0d0
                D.FaceCentroidY(j,i)=0.0d0
                D.FaceCentroidZ(j,i)=0.0d0
                do k=1,D.nNodesPerFace
                    D.FaceCentroidX(j,i)=D.FaceCentroidX(j,i)+D%node(D.idNode(D.LocalFaceNodes(k,j),i))%x
                    D.FaceCentroidY(j,i)=D.FaceCentroidY(j,i)+D%node(D.idNode(D.LocalFaceNodes(k,j),i))%y
                    D.FaceCentroidZ(j,i)=D.FaceCentroidZ(j,i)+D%node(D.idNode(D.LocalFaceNodes(k,j),i))%z
                end do
                D.FaceCentroidX(j,i)=D.FaceCentroidX(j,i)/D.nNodesPerFace
                D.FaceCentroidY(j,i)=D.FaceCentroidY(j,i)/D.nNodesPerFace
                D.FaceCentroidZ(j,i)=D.FaceCentroidZ(j,i)/D.nNodesPerFace
            end do
        end do
        
        D.FaceNeighbour(:,:)=0
        do i=1,D.nElements
            do j=1,D.nFacesPerElement
                SearchLoop:do k=i+1,D.nElements
                    do l=1,D.nFacesPerElement
                        if(abs(D.FaceCentroidX(j,i)-D.FaceCentroidX(l,k)) < MinSeparationDistance .AND. &
                           abs(D.FaceCentroidY(j,i)-D.FaceCentroidY(l,k)) < MinSeparationDistance .AND. &
                           abs(D.FaceCentroidZ(j,i)-D.FaceCentroidZ(l,k)) < MinSeparationDistance) then ! shared face
                            D.FaceNeighbour(j,i)=k    
                            D.FaceNeighbour(l,k)=i 
                            exit SearchLoop
                        endif
                    end do
                end do SearchLoop
            end do
        end do
        
        call ElapsedTime(1)
        
        continue
                
     
    end subroutine BuildFaceTopologyFrommesh

    !----------------------------------------------------------------------
    subroutine GrowMeshArray(iArray,nSizeIn,nSizeout)
        implicit none
        type(mesh), allocatable :: iArray(:)
        type(mesh), allocatable :: iTMP(:)
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
	        call AllocChk(ialloc,'GrowMeshArray iTMP array')
            iTMP(1:nSizeIn) = iArray
            call move_alloc (iTMP, iArray)
        else
            return
        endif
    end subroutine GrowMeshArray
    !----------------------------------------------------------------------
    subroutine GrowElementArray(iArray,nSizeIn,nSizeout)
        implicit none
        type(element), allocatable :: iArray(:)
        type(element), allocatable :: iTMP(:)
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
	        call AllocChk(ialloc,'GrowElementArray iTMP array')
            iTMP(1:nSizeIn) = iArray
            call move_alloc (iTMP, iArray)
        else
            return
        endif

    end subroutine GrowElementArray
    !----------------------------------------------------------------------
    subroutine GrowNodeArray(iArray,nSizeIn,nSizeout)
        implicit none
        type(node), allocatable :: iArray(:)
        type(node), allocatable :: iTMP(:)
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
	        call AllocChk(ialloc,'GrowNodeArray iTMP array')
            iTMP(1:nSizeIn) = iArray
            call move_alloc (iTMP, iArray)
        else
            return
        endif
        
    end subroutine GrowNodeArray
    !----------------------------------------------------------------------
    subroutine GrowZoneArray(iArray,nSizeIn,nSizeout)
        implicit none
        type(zone), allocatable :: iArray(:)
        type(zone), allocatable :: iTMP(:)
        integer(i4) :: nSizeIn, nSizeOut
        
        allocate(iTMP(nSizeout),stat=ialloc)
	    call AllocChk(ialloc,'GrowZoneArray iTMP array')
        iTMP(1:nSizeIn) = iArray
        call move_alloc (iTMP, iArray)
        
    end subroutine GrowZoneArray
    !----------------------------------------------------------------------
    subroutine InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
        implicit none

        real(dp), intent(in) :: x(3),y(3)
        real(dp), intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)

        integer(i4) :: i,j
        integer(i4) :: npt=3
        real(dp) :: s,x1,x2,y1,y2
        real(dp) :: small = 1.0d-10
        real(dp) :: PI

        PI = 4.0d0*atan(1.0)

        x1=x(2)-x(1); y1=y(2)-y(1)
        x2=x(3)-x(1); y2=y(3)-y(1)
        area=0.5d0*dabs(x1*y2-x2*y1)

        lseg=0.0d0
        aseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                lseg(i,j)=dsqrt((x(i)-x(j))**2.0d0+(y(i)-y(j))**2.0d0)
                lseg(j,i)=lseg(i,j)
                if(dabs(x(i)-x(j))<small) then
                    aseg(i,j)=0.5d0*PI
                    aseg(j,i)=0.5d0*PI
                elseif(x(i)>x(j)) then
                    aseg(i,j)=atan((y(i)-y(j))/(x(i)-x(j)))
                    aseg(j,i)=aseg(i,j)
                else
                    aseg(i,j)=atan((y(j)-y(i))/(x(j)-x(i)))
                    aseg(j,i)=aseg(i,j)
                end if
            end do
        end do

        s=0.5d0*(lseg(1,2)+lseg(2,3)+lseg(3,1))
        xc=0.5d0*(lseg(1,2)*x(3)+lseg(2,3)*x(1)+lseg(3,1)*x(2))/s
        yc=0.5d0*(lseg(1,2)*y(3)+lseg(2,3)*y(1)+lseg(3,1)*y(2))/s
        radius=dsqrt((s-lseg(1,2))*(s-lseg(2,3))*(s-lseg(3,1))/s)

        dseg(:,:)=radius
        dseg(1,1)=0.0d0; dseg(2,2)=0.0d0; dseg(3,3)=0.0d0

        return
    end subroutine InnerCircle

    !----------------------------------------------------------------------
    subroutine OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
        implicit none

        real(dp), intent(in) :: x(3),y(3)
        real(dp), intent(out) :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)
        logical, intent(out) :: bad_triangle

        integer(i4) :: i,j,k
        integer(i4) :: npt=3
        real(dp) :: x1,x2,y1,y2,a11,a12,a21,a22,r1,r2,det,xmid,ymid,side(3)
        real(dp) :: small = 1.0d-10
        real(dp) :: PI

        PI = 4.0d0*atan(1.0)

        x1=x(2)-x(1); y1=y(2)-y(1)
        x2=x(3)-x(1); y2=y(3)-y(1)
        area=0.5d0*dabs(x1*y2-x2*y1)

        lseg=0.0d0
        aseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                lseg(i,j)=dsqrt((x(i)-x(j))**2.0d0+(y(i)-y(j))**2.0d0)
                lseg(j,i)=lseg(i,j)
                if(dabs(x(i)-x(j))<small) then
                    aseg(i,j)=0.5d0*PI
                    aseg(j,i)=0.5d0*PI
                elseif(x(i)>x(j)) then
                    aseg(i,j)=atan((y(i)-y(j))/(x(i)-x(j)))
                    aseg(j,i)=aseg(i,j)
                else
                    aseg(i,j)=atan((y(j)-y(i))/(x(j)-x(i)))
                    aseg(j,i)=aseg(i,j)
                end if
            end do
        end do

        side(1)=lseg(2,3); side(2)=lseg(3,1); side(3)=lseg(1,2)
        if(side(1)>=max(side(2),side(3))) then
            k=1
        elseif(side(2)>=side(3)) then
            k=2
        else
            k=3
        end if
        i=k+1; if(i>3) i=i-3
        j=k+2; if(j>3) j=j-3
        bad_triangle=.false.
        if(side(k)**2.0d0>1.0001d0*(side(i)**2.0d0+side(j)**2.0d0)) bad_triangle=.true.

        a11=2.0d0*(x(1)-x(2)); a12=2.0d0*(y(1)-y(2)); a21=2.0d0*(x(1)-x(3)); a22=2.0d0*(y(1)-y(3))
        r1=x(1)*x(1)-x(2)*x(2)+y(1)*y(1)-y(2)*y(2); r2=x(1)*x(1)-x(3)*x(3)+y(1)*y(1)-y(3)*y(3)
        det=a11*a22-a12*a21
        if(dabs(det)<small) then
            stop 'bad triangle'
        end if
        xc=1.0d0/det*(a22*r1-a12*r2)
        yc=1.0d0/det*(-a21*r1+a11*r2)

        radius=dsqrt((xc-x(1))**2.0d0+(yc-y(1))**2.0d0)
        radius=dsqrt((xc-x(2))**2.0d0+(yc-y(2))**2.0d0)
        radius=dsqrt((xc-x(3))**2.0d0+(yc-y(3))**2.0d0)

        dseg=0.0d0
        do i=1,npt
            do j=i+1,npt
                xmid=0.5d0*(x(i)+x(j)); ymid=0.5d0*(y(i)+y(j))
                dseg(i,j)=dsqrt((xc-xmid)**2.0d0+(yc-ymid)**2.0d0)
                dseg(j,i)=dseg(i,j)
            end do
        end do

        return
    end subroutine OuterCircle


end module NumericalMesh 
