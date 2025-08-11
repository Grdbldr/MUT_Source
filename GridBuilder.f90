module GB
    use GeneralRoutines
    use fem
    implicit none

    type gbmesh
        character(len=:), allocatable :: name
        integer(i4) :: id
        character(128) :: ElementType      ! eg triangle, quadrilateral, prism, block etc
        character(MAX_LBL) :: STR_LengthUnit

        integer(i4) :: nNodesNew               ! number of nodes in the mesh

    end type gbmesh

    contains
    !----------------------------------------------------------------------
    subroutine ReadGridBuilderMesh(FNumMUT)
        use NumericalMesh
        implicit none
        
        integer(i4) :: FNumMUT
        
        character(128) :: GBPrefix

        integer(i4) :: i,j
        real(dp) :: x(3),y(3)
        real(dp) :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3)
        
        type(mesh) :: GBMesh
        GBMesh%Name="Grid Builder Mesh"
        
        !rgm oct-95  added this so only grid builder prefix needed
        !     prefix of grid files
        read(FNumMut,'(a80)') GBPrefix

        inquire(file=trim(GBprefix)//'.grd',exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('File not found: '//trim(GBprefix)//'.grd')
        end if

        GBMesh%nNodesPerElement=3
        GBMesh%Element%Typ='fetriangle'
        
        !     NODE COORDINATES
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.xyc',form='unformatted')
        read(itmp) GBMesh%nNodes
    
        
        allocate(GBMesh%node(GBMesh%nNodes),stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d node arrays')
        GBMesh%node(:)%x = 0 ! automatic initialization
        GBMesh%node(:)%y = 0 ! automatic initialization
        GBMesh%node(:)%z = 0 ! automatic initialization
        read(itmp) (GBMesh%node(i)%x, GBMesh%node(i)%y, i=1,GBMesh%nNodes)
	    call freeunit(itmp)

        !     ELEMENT INCIDENCES
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.in3',form='unformatted')
        read(itmp) GBMesh%nElements

        allocate(GBMesh%Element(GBMesh%nElements), GBMesh%idNode(GBMesh%nNodesPerElement,GBMesh%nElements), stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d element arrays')
        GBMesh%Element(:)%idZone = 0 ! automatic initialization
        GBMesh%idNode(:,:) = 0 ! automatic initialization
        read(itmp) (GBMesh%idNode(1,i),GBMesh%idNode(2,i),GBMesh%idNode(3,i), i=1,GBMesh%nElements)
	    call freeunit(itmp)

        !     Element area numbers
	    call getunit(itmp)
        open(itmp,file=trim(GBprefix)//'.ean',form='unformatted')
        read(itmp) (GBMesh%Element(i)%idZone,i=1,GBMesh%nElements)
	    call freeunit(itmp)
        GBMesh%Element%nZones=maxval(GBMesh%Element%idZone)
        
        !allocate(TMPLT.ElementArea(TMPLT.nElements),TMPLT.rCircle(TMPLT.nElements),TMPLT.xCircle(TMPLT.nElements),&
        !    TMPLT.yCircle(TMPLT.nElements),TMPLT.zCircle(TMPLT.nElements),TMPLT.xElement(TMPLT.nElements), TMPLT.yElement(TMPLT.nElements),&
        !    TMPLT.zElement(TMPLT.nElements),stat=ialloc)
        !call AllocChk(ialloc,'GB Inner circle arrays')
        !
        !allocate(TMPLT.SideLength(TMPLT.nNodesPerElement,TMPLT.nElements),stat=ialloc)
        !call AllocChk(ialloc,'GB SideLlength array')
        !
        !do i=1,TMPLT.nElements
        !    ! xc and yc from circumcircles
        !    if(TMPLT.nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
        !    do j=1,TMPLT.nNodesPerElement
        !        x(j)=TMPLT.x(TMPLT.iNode(j,i))
        !        y(j)=TMPLT.y(TMPLT.iNode(j,i))
        !    end do
        !    call InnerCircle(x,y,TMPLT.ElementArea(i),xc,yc,TMPLT.rCircle(i),lseg,aseg,dseg)
        !    
        !    TMPLT.SideLength(1,i)=lseg(1,2)
        !    TMPLT.SideLength(2,i)=lseg(2,3)
        !    TMPLT.SideLength(3,i)=lseg(3,1)
        !   
        !    TMPLT.xCircle(i)=xc
        !    TMPLT.yCircle(i)=yc
        !        
        !        
        !    ! zc from centroid of the iNode array coordinates
        !    zc=0.0
        !    do j=1,3
        !        zc=zc+TMPLT.z(TMPLT.iNode(j,i))
        !    end do
        !        
        !    TMPLT.xElement(i)=xc
        !    TMPLT.yElement(i)=yc
        !    TMPLT.zElement(i)=zc/3
        !    TMPLT.zCircle(i)=zc/3
        !   
        !end do
        !            
        !
        !TMPLT.IsDefined=.true.
        !allocate(TMPLT.Element_Is(TMPLT.nElements),stat=ialloc)
        !call AllocChk(ialloc,'TMPLT Element_Is array')            
        !TMPLT.Element_Is(:)=0
        !
        !write(TmpSTR,'(a,i8)') TAB//'Number of nodes:       ',TMPLT.nNold
        !call Msg(TmpSTR)
        !write(TmpSTR,'(a,i8)') TAB//'Number of elements:    ',TMPLT.nElements
        !call Msg(TmpSTR)
        !
        !TMPLT.STR_LengthUnit=UnitsOfLength
        !write(TmpSTR,'(a)') TAB//'Assumed length Units:  '//trim(UnitsOfLength)
        !call Msg(TmpSTR)

        return
    end subroutine ReadGridBuilderMesh

    
    end module GB
