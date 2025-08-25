module GB
    use GeneralRoutines
    use NumericalMesh
    use tecplot
    implicit none


    contains
    !----------------------------------------------------------------------
    subroutine ReadGridBuilderMesh(FNumMUT,GBMesh)
        implicit none
        type(mesh) gbmesh
        
        integer(i4) :: FNumMUT
        
        character(128) :: GBPathToFile

        integer(i4) :: i,j
        real(dp) :: x(3),y(3)
        real(dp) :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3)
        
        
        read(FNumMut,'(a80)') GBPathToFile
        GBMesh%Name=TRIM(GBPathToFile)

        inquire(file=trim(GBPathToFile)//'.grd',exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('File not found: '//trim(GBPathToFile)//'.grd')
        end if

        GBMesh%nNodesPerElement=3
        GBMesh%Element%Typ='triangle'
        GBMesh%TecplotTyp='fetriangle'
        
        !     NODE COORDINATES
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.xyc',form='unformatted')
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
        open(itmp,file=trim(GBPathToFile)//'.in3',form='unformatted')
        read(itmp) GBMesh%nElements

        allocate(GBMesh%Element(GBMesh%nElements), GBMesh%idNode(GBMesh%nNodesPerElement,GBMesh%nElements), stat=ialloc)
        call AllocChk(ialloc,'Read_gbldr_slice 2d element arrays')
        GBMesh%Element(:)%idZone = 0 ! automatic initialization
        GBMesh%idNode(:,:) = 0 ! automatic initialization
        read(itmp) (GBMesh%idNode(1,i),GBMesh%idNode(2,i),GBMesh%idNode(3,i), i=1,GBMesh%nElements)
	    call freeunit(itmp)

        !     Element zone numbers
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.ean',form='unformatted')  ! ean contains GB element area(aka zone) numbers 
        read(itmp) (GBMesh%Element(i)%idZone,i=1,GBMesh%nElements)
	    call freeunit(itmp)
        GBMesh%nZones=maxval(GBMesh%Element%idZone)
        
        do i=1,GBMesh%nElements
            ! xc and yc from circumcircles
            if(GBMesh%nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
            do j=1,GBMesh%nNodesPerElement
                x(j)=GBMesh%node(GBMesh%idNode(j,i))%x
                y(j)=GBMesh%node(GBMesh%idNode(j,i))%y
            end do
            call InnerCircle(x,y,GBMesh%Element(i)%xyArea,xc,yc,GBMesh%Element(i)%rCircle,lseg,aseg,dseg)
            
            GBMesh%Element(i)%SideLength(1)=lseg(1,2)
            GBMesh%Element(i)%SideLength(2)=lseg(2,3)
            GBMesh%Element(i)%SideLength(3)=lseg(3,1)
           
            GBMesh%Element(i)%xCircle=xc
            GBMesh%Element(i)%yCircle=yc
                
                
            ! zc from centroid of the idNode array coordinates
            zc=0.0
            do j=1,3
                zc=zc+GBMesh%node(GBMesh%idNode(j,i))%z
            end do
                
            GBMesh%Element(i)%x=xc
            GBMesh%Element(i)%y=yc
            GBMesh%Element(i)%z=zc/3
            GBMesh%Element(i)%zCircle=zc/3
        end do
        
        if(EnableTecplotOutput) then
            call GBToTecplot(GBMesh)
        endif

        return
    end subroutine ReadGridBuilderMesh
    
    !-------------------------------------------------------------
    subroutine GBToTecplot(GBMesh)
        implicit none
        type(mesh) gbmesh
        
        integer(i4) :: Fnum
        character(MAX_STR) :: FName
        integer(i4) :: i, j

        ! tecplot output file
        FName=trim(GBMesh%name)//'.tecplot.dat'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(GBMesh%name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","Zone","Element Area","Inner Circle Radius"'
        nVar=6

        !if(allocated(GBMesh%rCircle)) then
        !    VarSTR=trim(VarSTR)//'"'//trim(GBMesh%name)//'Inner circle radius",'
        !    nVar=nVar+1
        !end if
            
        write(FNum,'(a)') trim(VarSTR)


        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(GBMesh%name)//'"  ,N=',GBMesh%nNodes,', E=',GBMesh%nElements,&
        ', datapacking=block, zonetype='//trim(GBMesh%TecplotTyp)
        
        CellCenteredSTR=', VARLOCATION=([4,5'
        if(nVar.ge.6) then
            do j=6,nVar
                write(str2,'(i2)') j
                CellCenteredSTR=trim(CellCenteredSTR)//','//str2
            end do
        end if
        CellCenteredSTR=trim(CellCenteredSTR)//']=CELLCENTERED)'
        write(FNum,'(a)') trim(ZoneSTR)//trim(CellCenteredSTR)

        write(FNum,'(a)') '# x'
        write(FNum,'(5('//FMT_R8//'))') (GBMesh%node(i)%x,i=1,GBMesh%nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5('//FMT_R8//'))') (GBMesh%node(i)%y,i=1,GBMesh%nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5('//FMT_R8//'))') (GBMesh%node(i)%z,i=1,GBMesh%nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (GBMesh%Element(i)%idZone,i=1,GBMesh%nElements)
            
        write(FNum,'(a)') '# element area'
        write(FNum,'(5('//FMT_R8//'))') (GBMesh%Element(i)%Area,i=1,GBMesh%nElements)
                    
        write(FNum,'(a)') '# circle radius'
        write(FNum,'(5('//FMT_R8//'))') (GBMesh%Element(i)%rCircle,i=1,GBMesh%nElements)
            
        
        do i=1,GBMesh%nElements
            if(GBMesh%nNodesPerElement==3) then ! 3-node triangle
                write(FNum,'(8i8)') (GBMesh%idNode(j,i),j=1,3)
            else if(GBMesh%nNodesPerElement==4) then ! 4-node quadrilateral
                if(GBMesh%idNode(4,i) > 0) then
                    write(FNum,'(8i8)') (GBMesh%idNode(j,i),j=1,4) 
                else
                    write(FNum,'(8i8)') (GBMesh%idNode(j,i),j=1,3), GBMesh%idNode(3,i) 
                end if
            else
                write(TmpSTR,'(i2)')GBMesh%nNodesPerElement
                call ErrMsg(trim(GBMesh%name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if

        end do
       
        call FreeUnit(FNum)
        
    end subroutine GBToTecplot


    
    end module GB
