    character(MAX_INST) :: HGSToModflowStructure_CMD='hgs to modflow structure'

            else if(index(instruction, HGSToModflowStructure_CMD)  /= 0) then
                ! Add components to Modflow data structure from an existing HGS model
                call HGSToModflowStructure(FnumMUT,Modflow,TMPLT_GWF,TMPLT_SWF)
                call IaJa_MeshCentredFromTecplot(TMPLT_GWF)

                JustBuilt=.true.


    !-------------------------------------------------------------
    subroutine HGSToModflowStructure(FNumMUT, Modflow, TMPLT_GWF, TMPLT_SWF)
        implicit none


        integer :: FNumMUT
        type (HGSProject) hgs
        type (ModflowProject) Modflow
        type (TecplotDomain) TMPLT_GWF
        type (TecplotDomain) TMPLT_SWF

        ! read prefix for HGS project
        read(FNumMUT,'(a)') hgs.Prefix
		call LwrCse(hgs.Prefix)
        call Msg('HGS model prefix: '//hgs.Prefix)


        call HGS_GetMeshComponents(FNumMUT, hgs)

        ! Flip HGS from bottom-up to top-down node and element numbering
        call FlipHGSNumsBottomToTop(hgs)
        call HGS_ToTecplot(FNumMUT, hgs)

        ! Porous media aka GWF data
        if(hgs.mesh.nn == 0) then  ! something is wrong, no pm domain present
            call ErrMsg('*** HGS number of nodes is zero, no PM domain present')
        end if

        select case (hgs.mesh.nln)
        case (4)
            call ErrMsg('HGS 4-node tetrahedral elements not supported')
        case (6)
            call Msg('*** This HGS mesh has 6-node prism elements')
            TMPLT_GWF.ElementType='febrick'
        case (8)
            call Msg('*** This HGS mesh has 8-node brick elements')
            TMPLT_GWF.ElementType='febrick'
        case default
            write(TMPStr,'(i2)') hgs.mesh.nln
            call ErrMsg(trim(TmpSTR)//'-node HGS GWF elements not supported')
        end select

        ! Overland flow aka SWF data
        if(hgs.mesh.nolfe>0) then  ! olf domain present
            !call AddSWFFiles(FNumMUT, Modflow)
            select case (hgs.mesh.olfnln)
            case (4)
                if(hgs.mesh.inolf(4,1)==0) then
                    call Msg('*** This HGS mesh has 3-node triangular elements (stored as quads with 4th entry as zero')
                    TMPLT_SWF.ElementType='fequadrilateral'
                else
                    call Msg('*** This HGS mesh has 4-node quadrilateral elements stored as quads with 4th entry as zero')
                    TMPLT_SWF.ElementType='fequadrilateral'
                end if
            case default
                write(TMPStr,'(i2)') hgs.mesh.olfnln
                call ErrMsg(trim(TmpSTR)//'-node HGS OLF elements not supported')
            end select

            if(NodalControlVolume) then
                call ErrMsg('call HgsOlfToNodeCenteredModflow(hgs,modflow)')
            else ! MeshCentered
                call ErrMsg('!call HgsOlfToMeshCenteredModflow(hgs,TMPLT_SWF)')
            end if

            call DisplayDomainAttributes(TMPLT_SWF)
            !call FindNeighbours(Modflow,modflow.SWF)
            call SWFToTecplot(Modflow,TMPLT_SWF)
            call IaJa_MeshCentredFromTecplot(TMPLT_SWF)
            !call WriteSWFFiles(Modflow)
        end if


        if(NodalControlVolume) then
            call ErrMsg('!call HgsPmToNodeCenteredModflow(hgs,modflow)')
        else ! MeshCentered
            call ErrMsg('!call HgsPmToMeshCenteredModflow(hgs,TMPLT_GWF)')
        end if

        call DisplayDomainAttributes(TMPLT_GWF)

        call GWFToTecplot(Modflow,TMPLT_GWF)
        call IaJa_MeshCentredFromTecplot(TMPLT_GWF)
        !call WriteGWFFiles(Modflow)

        ! Fracture element data
        if(hgs.mesh.nef>0) then  ! fracture domain present
            call Msg('*** HGS fracture domain present but not supported')
        end if

    end subroutine HGSToModflowStructure

    !-------------------------------------------------------------

  !  !-------------------------------------------------------------
  !  subroutine HgsPmToMeshCenteredModflow(Hgs,TMPLT_GWF)
  !      implicit none
  !      type (HgsProject) Hgs
  !      type (TecplotDomain) TMPLT_GWF
  !      integer :: i, j
  !
  !
  !      TMPLT_GWF.meshtype='UNSTRUCTURED'
  !
  !      if(.not. allocated(TMPLT_GWF.x)) then
  !          ! These HGS node coordinates are used in the tecplot output file
  !          TMPLT_GWF.nNodes=hgs.mesh.nn
  !          allocate(TMPLT_GWF.x(TMPLT_GWF.nNodes),TMPLT_GWF.y(TMPLT_GWF.nNodes),TMPLT_GWF.z(TMPLT_GWF.nNodes), stat=ialloc)
  !          call AllocChk(ialloc,'GWF node coordinate arrays')
  !          TMPLT_GWF.x(:)= hgs.mesh.x(:)
  !          TMPLT_GWF.y(:)= hgs.mesh.y(:)
  !          TMPLT_GWF.z(:)= hgs.mesh.z(:)
  !      end if
  !
  !
  !      TMPLT_GWF.nLayers=hgs.nsheet-1
  !      !TMPLT_GWF.iz=0
  !      !TMPLT_GWF.nodelay=hgs.ne2d   ! number of modflow cells per layer
  !
  !
  !      TMPLT_GWF.nNodesPerElement=hgs.mesh.nln
  !      TMPLT_GWF.nElements=hgs.mesh.ne
  !
  !      ! Just define these for now
  !      !TMPLT_GWF.ic=0
  !
  !      !
  !      !allocate(TMPLT_GWF.xCell(TMPLT_GWF.nCells),TMPLT_GWF.yCell(TMPLT_GWF.nCells),TMPLT_GWF.zCell(TMPLT_GWF.nCells),TMPLT_GWF.iLayer(TMPLT_GWF.nCells),stat=ialloc)
  !      !call AllocChk(ialloc,'GWF cell coordinate and layer arrays')
  !
  !      !allocate(hgs.mesh.in(hgs.mesh.nln,hgs.mesh.ne),stat=ialloc)
  !      allocate(TMPLT_GWF.iNode(TMPLT_GWF.nNodesPerElement,TMPLT_GWF.nElements),stat=ialloc)
  !      call AllocChk(ialloc,'GWF iNode arrays')
  !      TMPLT_GWF.iNode(:,:) = hgs.mesh.in(:,:)
  !
  !      !! Layer has quasi-3D confining bed below if non-zero
  !      !allocate(TMPLT_GWF.Laybcd(TMPLT_GWF.nLayers),stat=ialloc)
  !      !call AllocChk(ialloc,'GWF.Laybcd array')
  !      !TMPLT_GWF.Laybcd(:)=0
  !
  !
  !      !!read(itmp) ((hgs.mesh.in(j,i),j=1,hgs.mesh.nln),i=1,hgs.mesh.ne)
  !      !read(itmp) ((TMPLT_GWF.iNode(j,i),j=1,TMPLT_GWF.nNodesPerElement),i=1,TMPLT_GWF.nCells)
  !
  !      !if(modflow.InnerCircles) then
  !      !    ! Use the SWF cel xy coords for the GWF cell but z as the centroid of the iNode array z coordinates
  !      !    do i=1,TMPLT_GWF.nCells
  !      !        zc=0.0
  !      !        do j=1,TMPLT_GWF.nNodesPerElement
  !      !            zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
  !      !        end do
  !      !        TMPLT_GWF.xCell(i)=Modflow.SWF.xCell(myMOD(i,Modflow.SWF.nCells))
  !      !        TMPLT_GWF.yCell(i)=Modflow.SWF.yCell(myMOD(i,Modflow.SWF.nCells))
  !      !        TMPLT_GWF.zCell(i)=zc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.iLayer(i)=1
  !      !    end do
  !      !
  !      !else
  !      !    ! Generate the modflow cell coordinates as the centroid of the iNode array coordinates
  !      !    do i=1,TMPLT_GWF.nCells
  !      !        xc=0.0
  !      !        yc=0.0
  !      !        zc=0.0
  !      !        do j=1,TMPLT_GWF.nNodesPerElement
  !      !            xc=xc+TMPLT_GWF.x(TMPLT_GWF.iNode(j,i))
  !      !            yc=yc+TMPLT_GWF.y(TMPLT_GWF.iNode(j,i))
  !      !            zc=zc+TMPLT_GWF.z(TMPLT_GWF.iNode(j,i))
  !      !        end do
  !      !        TMPLT_GWF.xCell(i)=xc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.yCell(i)=yc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.zCell(i)=zc/TMPLT_GWF.nNodesPerElement
  !      !        TMPLT_GWF.iLayer(i)=1
  !      !    end do
  !      !end if
  !
  !      allocate(TMPLT_GWF.iZone(TMPLT_GWF.nElements),stat=ialloc)
  !      call AllocChk(ialloc,'Read 3d Element property array')
  !      TMPLT_GWF.iZone = 0 ! automatic initialization
  !      TMPLT_GWF.iZone(:)=hgs.iprop(:)
	 !
  !      call freeunit(itmp)
  !  end subroutine HgsPmToMeshCenteredModflow
  !
  !  !-------------------------------------------------------------
  !  subroutine HgsOlfToMeshCenteredModflow(hgs,TMPLT_SWF)
  !      implicit none
  !      type (HgsProject) Hgs
  !      type (TecplotDomain) TMPLT_SWF
  !
  !      integer :: i,j,i1
  !      real*8 :: x(3),y(3)
  !      real*8 :: area,xc,yc,radius,lseg(3,3),aseg(3,3),dseg(3,3)
  !
  !      integer, allocatable :: TMPArray(:,:)
  !
  !     TMPLT_SWF.meshtype='UNSTRUCTURED'
  !
  !      TMPLT_SWF.nNodes=hgs.nnolf
  !
  !      allocate(TMPLT_SWF.x(TMPLT_SWF.nNodes),TMPLT_SWF.y(TMPLT_SWF.nNodes),TMPLT_SWF.z(TMPLT_SWF.nNodes), stat=ialloc)
  !      call AllocChk(ialloc,'SWF node coordinate arrays')
  !      TMPLT_SWF.x = 0 ! automatic initialization
  !      TMPLT_SWF.y = 0 ! automatic initialization
  !      TMPLT_SWF.z = 0 ! automatic initialization
  !
  !
  !      allocate(TMPLT_SWF.gwf_nn_from_swf_nn(TMPLT_SWF.nNodes),&
  !               TMPLT_SWF.swf_nn_from_gwf_nn(TMPLT_GWF.nNodes), stat=ialloc)
  !      call AllocChk(ialloc,'SWF link_pm arrays')
  !      TMPLT_SWF.gwf_nn_from_swf_nn(:) = hgs.link_Olf2pm(:)
  !      TMPLT_SWF.swf_nn_from_gwf_nn(:) = hgs.link_pm2Olf(:)
  !
  !      if(.not. allocated(Modflow.GWF.x)) then
  !          ! These HGS node coordinates are used in the tecplot output file
  !          Modflow.GWF.nNodes=hgs.mesh.nn
  !          allocate(Modflow.GWF.x(Modflow.GWF.nNodes),Modflow.GWF.y(Modflow.GWF.nNodes),Modflow.GWF.z(Modflow.GWF.nNodes), stat=ialloc)
  !          call AllocChk(ialloc,'GWF node coordinate arrays')
  !          Modflow.GWF.x(:)= hgs.mesh.x(:)
  !          Modflow.GWF.y(:)= hgs.mesh.y(:)
  !          Modflow.GWF.z(:)= hgs.mesh.z(:)
  !      end if
  !
  !      do i=1,TMPLT_SWF.nNodes
  !          TMPLT_SWF.x(i)=Modflow.GWF.x(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !          TMPLT_SWF.y(i)=Modflow.GWF.y(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !          TMPLT_SWF.z(i)=Modflow.GWF.z(TMPLT_SWF.gwf_nn_from_swf_nn(i))
  !      end do
  !
  !
  !      TMPLT_SWF.nNodesPerElement = hgs.mesh.OlfNLN
  !
  !      TMPLT_SWF.nElements = hgs.mesh.nolfe
  !
  !      allocate(TMPLT_SWF.xCell(TMPLT_SWF.nCells),TMPLT_SWF.yCell(TMPLT_SWF.nCells),TMPLT_SWF.zCell(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF cell coordinate arrays')
  !      allocate(TMPLT_SWF.iLayer(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF layer array')
  !
		!!allocate(hgs.mesh.inolf(hgs.mesh.OlfNLN,hgs.mesh.nolfe), &
		!!	hgs.iolf_id_elem(hgs.mesh.nolfe), &
		!!	hgs.iolf_3d_elem_map(hgs.mesh.nolfe), &
		!!	stat=ialloc)
		!!call AllocChk(ialloc,'Overland element arrays')
  !      allocate(TMPLT_SWF.iNode(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF iNode arrays')
  !      TMPLT_SWF.iNode(:,:) = hgs.mesh.inolf(:,:)
  !
  !      ! These numbers represent the pm node number that is coincicent to the olf node
  !      allocate(TMPArray(TMPLT_SWF.nNodesPerElement,TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'SWF TMPArray')
  !      TMPArray=0
  !      do i=1,TMPLT_SWF.nCells
  !          do j=1,TMPLT_SWF.nNodesPerElement
  !              ! HGS assumes 4 nodes for OLF elements but if triangles 4th value is zero so check
  !              if(TMPLT_SWF.iNode(j,i) > 0) then
  !                  ! The conversion to SWF nn from GWF nn is a global SWF nn so subtract total number of GWF nodes
  !                  TMPArray(j,i)=TMPLT_SWF.swf_nn_from_gwf_nn(TMPLT_SWF.iNode(j,i))-Modflow.GWF.nNodes
  !              end if
  !          end do
  !      end do
  !      TMPLT_SWF.iNode=TMPArray
  !      deallocate(TMPArray)
  !
  !
  !      ! Generate the modflow cell coordinates
  !      !
  !      if(TMPLT_SWF.iNode(4,1) > 0) then ! 4-node rectangles, use centroid of the iNode array coordinates
  !          do i=1,TMPLT_SWF.nCells
  !              xc=0.0
  !              yc=0.0
  !              zc=0.0
  !              do j=1,TMPLT_SWF.nNodesPerElement
  !                  xc=xc+TMPLT_SWF.x(TMPLT_SWF.iNode(j,i))
  !                  yc=yc+TMPLT_SWF.y(TMPLT_SWF.iNode(j,i))
  !                  zc=zc+TMPLT_SWF.z(TMPLT_SWF.iNode(j,i))
  !              end do
  !              TMPLT_SWF.xCell(i)=xc/4
  !              TMPLT_SWF.yCell(i)=yc/4
  !              TMPLT_SWF.zCell(i)=zc/4
  !              TMPLT_SWF.iLayer(i)=1
  !          end do
  !      else ! triangle with third node repeated, use circumcircle approach
  !
  !          !allocate(BadTri(TMPLT_SWF.nCells),stat=ialloc)
  !          !call AllocChk(ialloc,'BadTri Array')
  !          !BadTri(:)=0
  !
  !          do i=1,TMPLT_SWF.nCells
  !              ! xc and yc from circumcircles
  !              do j=1,3
  !                  x(j)=TMPLT_SWF.x(TMPLT_SWF.iNode(j,i))
  !                  y(j)=TMPLT_SWF.y(TMPLT_SWF.iNode(j,i))
  !              end do
  !              !call OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
  !              !if(bad_triangle) then
  !              !    !write(*,*) 'element ',i,' bad triangle for circumcenter'
  !              !    call InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
  !              !    BadTri(i)=1
  !              !end if
  !              !call OuterCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg,bad_triangle)
  !              !if(bad_triangle) then
  !              !    !write(*,*) 'element ',i,' bad triangle for circumcenter'
  !                  call InnerCircle(x,y,area,xc,yc,radius,lseg,aseg,dseg)
  !              !    BadTri(i)=1
  !              !end if
  !
  !              ! zc from centroid of the iNode array coordinates
  !              zc=0.0
  !              do j=1,3
  !                  zc=zc+TMPLT_SWF.z(TMPLT_SWF.iNode(j,i))
  !              end do
  !
  !              TMPLT_SWF.xCell(i)=xc
  !              TMPLT_SWF.yCell(i)=yc
  !              TMPLT_SWF.zCell(i)=zc/3
  !              TMPLT_SWF.iLayer(i)=1
  !          end do
  !      end if
  !
  !      Modflow.InnerCircles=.true.
  !
  !      allocate(TMPLT_SWF.iZone(TMPLT_SWF.nCells),stat=ialloc)
  !      call AllocChk(ialloc,'Read SWF cell property array')
  !      TMPLT_SWF.iZone(:) = hgs.iolf_id_elem(:i)
  !
  !
  !
  !
  !  end subroutine HgsOlfToMeshCenteredModflow
