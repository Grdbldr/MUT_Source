module NumericalMesh
    use BasicTypes
    use GeneralRoutines
    use ErrorHandling, only: ERR_INVALID_INPUT, HandleError
    implicit none
    
    real(sp) :: MinSeparationDistance=0.0001

    
    type node 
        real(dp) :: x   ! coordinates
        real(dp) :: y
        real(dp) :: z
        
        integer(i4) :: id
        integer(i4) :: is
    end type node 
    
    type element
        character(40) :: Typ ! element typ eg triangle, quadrilateral, prism, block etc
        !character(len=:), allocatable :: Typ ! number of nodes in element
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

    integer(i4), allocatable :: seg_node(:,:)
    integer(i4) :: nSeg        


    contains

    subroutine SaveMeshBIN(M)
        implicit none
        type(mesh) M
        integer :: i, j
        
        character(128) :: FName
        integer(i4) :: FNum
        ! save mesh to binary file
        FName=trim(M%name)//'.MeshBIN'
        call OpenBinary(FNum,FName)
        call Msg('  ')
        call Msg(FileCreateSTR//'Binary mesh file: '//trim(FName))
        
        write(FNum) M%name
        
        write(FNum) M%nNodes
        do i=1,M%nNodes
            write(FNum) M%node(i)%id,M%node(i)%x,M%node(i)%y,M%node(i)%z,M%node(i)%is
        end do
        write(FNum) M%xMin, M%xMax, M%yMin, M%yMax, M%zMin, M%zMax
        ! write(*,*)'extents ', M%xMin, M%xMax, M%yMin, M%yMax, M%zMin, M%zMax
        
        write(FNum) M%nElements
        write(FNum) M%nNodesPerElement
        write(FNum) M%TecplotTyp
        write(FNum) M%idNode
        
        do i=1,M%nElements
            write(FNum) M%element(i)%id,M%element(i)%typ,M%element(i)%idZone,M%element(i)%iLayer
            write(FNum) M%element(i)%x,M%element(i)%y,M%element(i)%z,M%element(i)%is
            write(FNum) M%element(i)%area,M%element(i)%xyArea
            write(FNum) M%element(i)%xCircle,M%element(i)%yCircle,M%element(i)%zCircle,M%element(i)%rCircle
            do j=1,M%nNodesPerElement
                write(FNum) M%element(i)%xSide,M%element(i)%ySide,M%element(i)%SideLength
            end do
        end do
        
        write(FNum) M%nZones
        do i=1,M%nZones
                write(FNum) M%zone(i)%name
                ! write(*,'(a)')'written zone name '// trim(M%zone(i)%name)
                write(FNum) M%zone(i)%id,M%element(i)%is
        end do
 
        write(FNum) M%njag 
        write(FNum) M%ia(:)      
        do i=1,M%nElements
            write(FNum) M%ConnectionList(:,i)  
        end do
        do i=1,M%nElements
            write(FNum) (M%ThroughFace(j,i),j=1,MAX_CNCTS)  
        end do

        write(FNum) M%nFaces
        write(FNum) M%nNodesPerFace
        write(FNum) M%nFacesPerElement
        write(FNum) ((M%LocalFaceNodes(i,j),i=1,M%nNodesPerFace),   j=1,M%nFacesPerElement) 
        do j=1,M%nElements
            write(FNum) (M%FaceHost(i,j),      i=1,M%nFacesPerElement)
            write(FNum) (M%FaceNeighbour(i,j), i=1,M%nFacesPerElement)
            write(FNum) (M%FaceCentroidX(i,j), i=1,M%nFacesPerElement)
            write(FNum) (M%FaceCentroidY(i,j), i=1,M%nFacesPerElement)
            write(FNum) (M%FaceCentroidZ(i,j), i=1,M%nFacesPerElement)
        end do
        
        write(FNum) M%nLayers                 ! number of layers in the mesh 


        call FreeUnit(FNum)
    end subroutine SaveMeshBIN

    subroutine ReadMeshBIN(M)
        implicit none
        type(mesh) M
        integer :: i, j
        
        character(128) :: FName
        integer(i4) :: FNum
        ! save mesh to binary file
        FName=trim(M%name)//'.MeshBIN'
        call OpenBinary(FNum,FName)
        call Msg('  ')
        call Msg(FileCreateSTR//'Binary mesh file: '//trim(FName))
        
        read(FNum) M%name
        ! write(*,'(a)')'mesh name '// trim(M%name)
        
        read(FNum) M%nNodes
        allocate(M%Node(M%nNodes), stat=ialloc)
        call AllocChk(ialloc,'ReadMeshBIN: M%Node arrays')
        do i=1,M%nNodes
            read(FNum) M%node(i)%id,M%node(i)%x,M%node(i)%y,M%node(i)%z,M%node(i)%is
        end do
        read(FNum) M%xMin, M%xMax, M%yMin, M%yMax, M%zMin, M%zMax
        
        read(FNum) M%nElements
        read(FNum) M%nNodesPerElement
        read(FNum) M%TecplotTyp
        allocate(M%Element(M%nElements), &
            M%idNode(M%nNodesPerElement,M%nElements), stat=ialloc)
        call AllocChk(ialloc,'ReadMeshBIN: M%Element, M%idNode arrays')
        read(FNum) M%idNode
        do i=1,M%nElements
            read(FNum) M%element(i)%id,M%element(i)%typ,M%element(i)%idZone,M%element(i)%iLayer
            read(FNum) M%element(i)%x,M%element(i)%y,M%element(i)%z,M%element(i)%is
            read(FNum) M%element(i)%area,M%element(i)%xyArea
            read(FNum) M%element(i)%xCircle,M%element(i)%yCircle,M%element(i)%zCircle,M%element(i)%rCircle
            do j=1,M%nNodesPerElement
                read(FNum) M%element(i)%xSide,M%element(i)%ySide,M%element(i)%SideLength
            end do
        end do
 
        read(FNum) M%nZones
        ! write(*,*) 'nzones ',M%nZones
        allocate(M%Zone(M%nZones),stat=ialloc)
        call AllocChk(ialloc,'ReadMeshBIN: M%Zone array')
        do i=1,M%nZones
            read(FNum) M%zone(i)%name
            ! write(*,'(a)')'zone name '// trim(M%zone(i)%name)
            read(FNum) M%zone(i)%id,M%element(i)%is
        end do
        
        read(FNum) M%njag
        
        allocate(M%ia(M%nElements), &
            M%ConnectionList(MAX_CNCTS,M%nElements), &
            M%ThroughFace(MAX_CNCTS,M%nElements), &
            stat=ialloc)
        call AllocChk(ialloc,'ReadMeshBIN: M%ia, M%ConnectionList, M%ThroughFace arrays')
        read(FNum) M%ia(:)      
        do i=1,M%nElements
            read(FNum) M%ConnectionList(:,i)  
        end do
        do i=1,M%nElements
            read(FNum) M%ThroughFace(:,i)
        end do
        
        read(FNum) M%nFaces
        read(FNum) M%nNodesPerFace
        read(FNum) M%nFacesPerElement
        
        allocate(M%LocalFaceNodes(M%nNodesPerFace, M%nFacesPerElement),stat=ialloc)
        call AllocChk(ialloc,'ReadMeshBIN: M%LocalFaceNodes array')
        read(FNum) ((M%LocalFaceNodes(i,j),i=1,M%nNodesPerFace),   j=1,M%nFacesPerElement) 
        
        allocate(M%FaceCentroidX(M%nFacesPerElement,M%nElements), &
            M%FaceCentroidY(M%nFacesPerElement,M%nElements), &
            M%FaceCentroidZ(M%nFacesPerElement,M%nElements),stat=ialloc)
        call AllocChk(ialloc,'Face Centroid arrays')
        
        allocate(M%FaceHost(M%nFacesPerElement,M%nElements), &
                 M%FaceNeighbour(M%nFacesPerElement,M%nElements),stat=ialloc)
        call AllocChk(ialloc,'Face host/neighbour arrays')

        
        do j=1,M%nElements
            read(FNum) (M%FaceHost(i,j),      i=1,M%nFacesPerElement)
            read(FNum) (M%FaceNeighbour(i,j), i=1,M%nFacesPerElement)
            read(FNum) (M%FaceCentroidX(i,j), i=1,M%nFacesPerElement)
            read(FNum) (M%FaceCentroidY(i,j), i=1,M%nFacesPerElement)
            read(FNum) (M%FaceCentroidZ(i,j), i=1,M%nFacesPerElement)
        end do
        
        read(FNum) M%nLayers                 ! number of layers in the mesh 


        call FreeUnit(FNum)
       
        write(TMPStr,'(a,i8)') 'Number of nodes:', M%nNodes
        call Msg(TMPStr)
        write(TMPStr,'(a,i8)') 'Number of elements:', M%nElements
        call Msg(TMPStr)
        write(TMPStr,'(a,a)') 'Element type:', M%TecplotTyp
        call Msg(TMPStr)
        !write(TMPStr,'(a,3f10)') 'First node coordinates:', M%node(1)%x, M%node(1)%y, M%node(1)%z
        !call Msg(TMPStr)
        write(TMPStr,'(a,a)') 'First element type:', M%element(1)%typ
        call Msg(TMPStr)
        
    end subroutine ReadMeshBIN

    !subroutine SaveMeshTin(M)
    !    implicit none
    !    type(mesh) M
    !    
    !    integer :: i
    !    
    !    character(128) :: FName
    !    integer(i4) :: FNum
    !    ! save mesh to TIN file
    !    FName=trim(M%name)//'.tin'
    !    call OpenAscii(FNum,FName)
    !    call Msg('  ')
    !    call Msg(FileCreateSTR//'Ascii mesh TIN file: '//trim(FName))
    !    write(FNum,'(a)') 'TIN                /* File type identifier */'
    !    write(FNum,'(a)') 'BEGT               /* Beginning of TIN group */'
    !    write(FNum,'(a)') 'TNAM '//trim(M%name)//'          /* Name of TIN */'
    !    !write(FNum,'(a, 3i5, a)') 'TCOL ',(M%id),'            /* TIN material id */
    !    write(FNum,'(a, i8)') 'VERT ',M%nNodes,'            /* Beg. of vertices */'
    !    do i=1,M%nNodes
    !        write(FNum,'(3('//FMT_R8//'),i8)') M%node(i)%x,M%node(i)%y,M%node(i)%z,0
    !    end do
    !    write(FNum,'(a, i8)') 'TRI ',M%nElements,'            /* Beg. of triangles */'
    !    do i=1,M%nElements
    !        write(FNum,'(3i8)') M%idNode(1,i),M%idNode(2,i),M%idNode(3,i)
    !    end do
    !    write(FNum,'(a)') 'ENDT               /* End of TIN group */'
    
    !end subroutine SaveMeshTin

	!-----------------------------------------------------------------------
	subroutine MeshExtents(M)
		implicit none
		type(mesh) M

		M%xmin=MINVAL(M%node%x)
		M%xmax=MAXVAL(M%node%x)
		M%ymin=MINVAL(M%node%y)
		M%ymax=MAXVAL(M%node%y)
		M%zmin=MINVAL(M%node%z)
		M%zmax=MAXVAL(M%node%z)

    end subroutine MeshExtents

    !----------------------------------------------------------------------
    subroutine BuildFaceTopologyFrommesh(M)
        implicit none

        type (mesh)  M

        integer(i4) :: i, j, k, l
        
        if(ALLOCATED(M%FaceHost)) return ! already built
        
        call StopWatch(1,'BuildFaceTopologyFrommesh')
        call Msg('Building face topology from model domain...') 

        
        ! Local node numbers for 2D and 3D element faces 
        select case (M%TecplotTyp)
        case ('felineseg')
                M.nNodesPerFace=1
                M.nFacesPerElement=2
                allocate(M.LocalFaceNodes(M.nNodesPerFace, M.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'M.LocalFaceNodes  fetriangle')
		                                 ! end1   end2   
	            !data M.LocalFaceNodes/     1,      2   /
	            M.LocalFaceNodes(1,1)=1    ! end1
                M.LocalFaceNodes(1,2)=2    ! end2   
        
        case ('fetriangle')
                M.nNodesPerFace=2
                M.nFacesPerElement=3
                allocate(M.LocalFaceNodes(M.nNodesPerFace, M.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'M.LocalFaceNodes  fetriangle')
		                                 ! side1   side2   side3 
	            !data M.LocalFaceNodes/    1,2,      2,3,    3,1   /
	            M.LocalFaceNodes(1,1)=1; M.LocalFaceNodes(2,1)=2    ! side1
                M.LocalFaceNodes(1,2)=2; M.LocalFaceNodes(2,2)=3    ! side2   
                M.LocalFaceNodes(1,3)=3; M.LocalFaceNodes(2,3)=1    ! side3 
            
        case ('fequadrilateral')
                M.nNodesPerFace=2
                M.nFacesPerElement=4
                allocate(M.LocalFaceNodes(M.nNodesPerFace, M.nFacesPerElement),stat=ialloc)
                call AllocChk(ialloc,'M.LocalFaceNodes  fequadrilateral')
		                                 ! side1     side2   side3   side4 
	            !data M.LocalFaceNodes/    1,2,      2,3,    3,4,    4,1   /
	            M.LocalFaceNodes(1,1)=1; M.LocalFaceNodes(2,1)=2    ! side1
                M.LocalFaceNodes(1,2)=2; M.LocalFaceNodes(2,2)=3    ! side2   
                M.LocalFaceNodes(1,3)=3; M.LocalFaceNodes(2,3)=4    ! side3 
                M.LocalFaceNodes(1,4)=4; M.LocalFaceNodes(2,4)=1    ! side3 
                
        case ('feprism')
            M.nNodesPerFace=4
            M.nFacesPerElement=5
            allocate(M.LocalFaceNodes(M.nNodesPerFace, M.nFacesPerElement),stat=ialloc)
            call AllocChk(ialloc,'M.LocalFaceNodes  feprism')
                                        ! bottom      top         side1       side2       side3 
	        !data M.LocalFaceNodes/    1,2,3,0,    4,5,6,0,    1,2,5,4,    1,3,6,4,    2,3,6,5   /
	        M.LocalFaceNodes(1,1)=1; M.LocalFaceNodes(2,1)=2; M.LocalFaceNodes(3,1)=3; M.LocalFaceNodes(4,1)=0    ! bottom
	        M.LocalFaceNodes(1,2)=4; M.LocalFaceNodes(2,2)=5; M.LocalFaceNodes(3,2)=6; M.LocalFaceNodes(4,2)=0    ! top
	        M.LocalFaceNodes(1,3)=1; M.LocalFaceNodes(2,3)=2; M.LocalFaceNodes(3,3)=5; M.LocalFaceNodes(4,3)=4    ! side1
	        M.LocalFaceNodes(1,4)=1; M.LocalFaceNodes(2,4)=3; M.LocalFaceNodes(3,4)=6; M.LocalFaceNodes(4,4)=4    ! side2
	        M.LocalFaceNodes(1,5)=2; M.LocalFaceNodes(2,5)=3; M.LocalFaceNodes(3,5)=6; M.LocalFaceNodes(4,5)=5    ! side3

        case ('febrick')
            M.nNodesPerFace=4
            M.nFacesPerElement=6
            allocate(M.LocalFaceNodes(M.nNodesPerFace, M.nFacesPerElement),stat=ialloc)
            call AllocChk(ialloc,'M.LocalFaceNodes  febrick')
                                        ! bottom      top         front       back        left        right 
	        !data M.LocalFaceNodes/    1,2,3,4,    5,6,7,8,    1,2,6,5,    4,3,7,8,    1,5,8,4,    2,6,7,3   /
	        M.LocalFaceNodes(1,1)=1; M.LocalFaceNodes(2,1)=2; M.LocalFaceNodes(3,1)=3; M.LocalFaceNodes(4,1)=4    ! bottom
	        M.LocalFaceNodes(1,2)=5; M.LocalFaceNodes(2,2)=6; M.LocalFaceNodes(3,2)=7; M.LocalFaceNodes(4,2)=8    ! top
	        M.LocalFaceNodes(1,3)=1; M.LocalFaceNodes(2,3)=2; M.LocalFaceNodes(3,3)=6; M.LocalFaceNodes(4,3)=5    ! front
	        M.LocalFaceNodes(1,4)=4; M.LocalFaceNodes(2,4)=3; M.LocalFaceNodes(3,4)=7; M.LocalFaceNodes(4,4)=8    ! back
	        M.LocalFaceNodes(1,5)=1; M.LocalFaceNodes(2,5)=5; M.LocalFaceNodes(3,5)=8; M.LocalFaceNodes(4,5)=4    ! left
	        M.LocalFaceNodes(1,6)=2; M.LocalFaceNodes(2,6)=6; M.LocalFaceNodes(3,6)=7; M.LocalFaceNodes(4,6)=3    ! right
            
        case default
            call HandleError(ERR_INVALID_INPUT, 'Tecplot Element Type '//trim(M%TecplotTyp)//' not supported', 'set_local_face_nodes')
        end select  
  
        ! *** ASSUMPTION: If two face centroids are coincident, then the faces are shared by neighbouring elements
        allocate(M.FaceCentroidX(M.nFacesPerElement,M.nElements), &
                 M.FaceCentroidY(M.nFacesPerElement,M.nElements), &
                 M.FaceCentroidZ(M.nFacesPerElement,M.nElements),stat=ialloc)
        call AllocChk(ialloc,'Face Centroid arrays')
        
        allocate(M.FaceHost(M.nFacesPerElement,M.nElements), &
                 M.FaceNeighbour(M.nFacesPerElement,M.nElements),stat=ialloc)
        call AllocChk(ialloc,'Face host/neighbour arrays')
        M.FaceHost(:,:)=0
               
        M.nFaces=0
        do i=1,M.nElements
            do j=1,M.nFacesPerElement
                M.FaceHost(j,i)=i
                M.nFaces=M.nFaces+1
                M.FaceCentroidX(j,i)=0.0d0
                M.FaceCentroidY(j,i)=0.0d0
                M.FaceCentroidZ(j,i)=0.0d0
                do k=1,M.nNodesPerFace
                    M.FaceCentroidX(j,i)=M.FaceCentroidX(j,i)+M%node(M.idNode(M.LocalFaceNodes(k,j),i))%x
                    M.FaceCentroidY(j,i)=M.FaceCentroidY(j,i)+M%node(M.idNode(M.LocalFaceNodes(k,j),i))%y
                    M.FaceCentroidZ(j,i)=M.FaceCentroidZ(j,i)+M%node(M.idNode(M.LocalFaceNodes(k,j),i))%z
                end do
                M.FaceCentroidX(j,i)=M.FaceCentroidX(j,i)/M.nNodesPerFace
                M.FaceCentroidY(j,i)=M.FaceCentroidY(j,i)/M.nNodesPerFace
                M.FaceCentroidZ(j,i)=M.FaceCentroidZ(j,i)/M.nNodesPerFace
            end do
        end do
        
        M.FaceNeighbour(:,:)=0
        do i=1,M.nElements
            do j=1,M.nFacesPerElement
                SearchLoop:do k=i+1,M.nElements
                    do l=1,M.nFacesPerElement
                        if(abs(M.FaceCentroidX(j,i)-M.FaceCentroidX(l,k)) < MinSeparationDistance .AND. &
                           abs(M.FaceCentroidY(j,i)-M.FaceCentroidY(l,k)) < MinSeparationDistance .AND. &
                           abs(M.FaceCentroidZ(j,i)-M.FaceCentroidZ(l,k)) < MinSeparationDistance) then ! shared face
                            M.FaceNeighbour(j,i)=k    
                            M.FaceNeighbour(l,k)=i 
                            exit SearchLoop
                        endif
                    end do
                end do SearchLoop
            end do
        end do
        
        call ElapsedTime(1)
        
        continue
                
     
    end subroutine BuildFaceTopologyFrommesh
    !-------------------------------------------------------------
    subroutine BuildMeshCentredIaJa(M)
        implicit none
        type(mesh) M
        
        integer(i4) :: i, j, k, l
        integer(i4) :: iEl, jEl
        integer(i4) :: iNd, jNd
        integer(i4) :: j1, j2
        
        real(dp) :: SeparationDistance
        
        real(dp) :: xMidpoint(M%nNodesPerElement,M%nElements)
        real(dp) :: yMidpoint(M%nNodesPerElement,M%nElements)
        real(dp) :: zMidpoint(M%nNodesPerElement,M%nElements)

        if(ALLOCATED(M%ia)) return ! already built

        call StopWatch(1,'BuildMeshCentredIaJa')
        call Msg(' ')
        call Msg('  Generating cell connection arrays for domain '//trim(M%name)//'...')

        
        allocate(M%ia(M%nElements), &
                 M%ConnectionList(MAX_CNCTS,M%nElements), &
                 M%ThroughFace(MAX_CNCTS,M%nElements), &
                 stat=ialloc)
        call AllocChk(ialloc,trim(M%name)//' connection arrays')

        M%ia=0
        M%ConnectionList=0
        M%ThroughFace=0
        
        do i=1,M%nElements   ! First element connection is to itself
            M%ia(i)=1
            M%ConnectionList(M%ia(i),i)=-i     ! Negative entry shows start of element i list
        end do


        ! Element side midpoints
        do i=1,M%nElements
            do j=1,M%nNodesPerElement
                j1=M%idNode(j,i)
                if(j < M%nNodesPerElement) then
                    j2=M%idNode(j+1,i)
                else
                    j2=M%idNode(1,i)
                end if

                xMidpoint(j,i)=(M%node(j1)%x+M%node(j2)%x)/2.0d0
                yMidpoint(j,i)=(M%node(j1)%y+M%node(j2)%y)/2.0d0
                zMidpoint(j,i)=(M%node(j1)%z+M%node(j2)%z)/2.0d0

            end do
        end do


        ! Brute force search for neighbours using side midpoints
        do iEl=1,M%nElements ! Loop over elements
            do k=1,M%nNodesPerElement ! Loop over nodes in element
                iNd=M%idNode(k,iEl)

                do jEl=iEl+1,M%nElements ! Loop over rest of elements
                    do l=1,M%nNodesPerElement ! Loop over nodes in next element
                        jNd=M%idNode(l,jEl)
                        SeparationDistance=sqrt((xMidpoint(k,iEl) - xMidpoint(l,jEl))**2 + &
                            (yMidpoint(k,iEl) - yMidpoint(l,jEl))**2 + &
                            (zMidpoint(k,iEl) - zMidpoint(l,jEl))**2 )
                        if(SeparationDistance < MinSeparationDistance) then
                            ! iEl is neighbour of jEl
                            M%ia(iEl)=M%ia(iEl)+1
                            M%ConnectionList(M%ia(iEl),iEl)=jEl
                            M%ThroughFace(M%ia(iEl),iEl)=k
                            
                            ! jEl is neighbour of iEl
                            M%ia(jEl)=M%ia(jEl)+1
                            M%ConnectionList(M%ia(jEl),jEl)=iEl
                            M%ThroughFace(M%ia(jEl),jEl)=l
                        endif
                    end do
                end do
            end do
        end do
        
        M%njag=0
        do i=1,M%nElements
            M%njag=M%njag + M%ia(i)
        end do

        
        call ElapsedTime(1)

        return

    end subroutine BuildMeshCentredIaJa
    !----------------------------------------------------------------------
    subroutine FlagOuterBoundaryNodes(M)
        implicit none

        type (mesh)  M

        integer(i4) :: i, in1, in2, in3, in4 
        
        allocate(seg_node(M%nElements*4,2))
        
        !     construct the array of boundary segment nodes
        call Msg('Find outer boundary segments...') 
        if(M%nNodesPerElement==3) then
            seg_node(1,1)=M%idNode(1,1) 
            seg_node(1,2)=M%idNode(2,1) 
            seg_node(2,1)=M%idNode(2,1) 
            seg_node(2,2)=M%idNode(3,1) 
            seg_node(3,1)=M%idNode(3,1) 
            seg_node(3,2)=M%idNode(1,1) 
            nseg=3 

            do  i=2,M%nElements
                in1=M%idNode(1,i) 
                in2=M%idNode(2,i) 
                in3=M%idNode(3,i) 
                call check_seg(in1,in2) 
                call check_seg(in2,in3) 
                call check_seg(in3,in1) 
            end do 
        else if(M%nNodesPerElement==4) then
            seg_node(1,1)=M%idNode(1,1) 
            seg_node(1,2)=M%idNode(2,1) 
            seg_node(2,1)=M%idNode(2,1) 
            seg_node(2,2)=M%idNode(3,1) 
            seg_node(3,1)=M%idNode(3,1) 
            seg_node(3,2)=M%idNode(4,1) 
            seg_node(4,1)=M%idNode(4,1) 
            seg_node(4,2)=M%idNode(1,1) 
            nseg=4 

            do  i=2,M%nElements
                in1=M%idNode(1,i) 
                in2=M%idNode(2,i) 
                in3=M%idNode(3,i) 
                in4=M%idNode(4,i) 
                call check_seg(in1,in2) 
                call check_seg(in2,in3) 
                call check_seg(in3,in4) 
                call check_seg(in4,in1) 
            end do 
        end if

     
        do  i=1,nseg                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
            call set(M%node(seg_node(i,1))%is,BoundaryNode) 
            call set(M%node(seg_node(i,2))%is,BoundaryNode) 
        end do 
        
        continue
     
    end subroutine FlagOuterBoundaryNodes
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
