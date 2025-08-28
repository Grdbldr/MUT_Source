module GB
    use GeneralRoutines
    use NumericalMesh
    use tecplot
    implicit none


    contains
    !----------------------------------------------------------------------
    subroutine ReadGridBuilderMesh(FNumMUT,GB_TRI_2D)
        implicit none
        type(mesh) GB_TRI_2D
        
        integer(i4) :: FNumMUT
        
        character(128) :: GBPathToFile

        integer(i4) :: i,j
        real(dp) :: x(3),y(3)
        real(dp) :: xc,yc,lseg(3,3),aseg(3,3),dseg(3,3)
        
        
        read(FNumMut,'(a80)') GBPathToFile
        GB_TRI_2D%Name=TRIM(GBPathToFile)

        inquire(file=trim(GBPathToFile)//'.grd',exist=FileExists)
        if(.not. FileExists) then
            call ErrMsg('File not found: '//trim(GBPathToFile)//'.grd')
        end if

        GB_TRI_2D%nNodesPerElement=3
        GB_TRI_2D%Element%Typ='triangle'
        GB_TRI_2D%TecplotTyp='fetriangle'
        
        !     NODE COORDINATES
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.xyc',form='unformatted')
        read(itmp) GB_TRI_2D%nNodes
    
        
        allocate(GB_TRI_2D%node(GB_TRI_2D%nNodes),stat=ialloc)
        call AllocChk(ialloc,'ReadGridBuilderMesh 2d node array')
        GB_TRI_2D%node%x = 0 ! automatic initialization
        GB_TRI_2D%node%y = 0 ! automatic initialization
        GB_TRI_2D%node%z = 0 ! automatic initialization
        read(itmp) (GB_TRI_2D%node(i)%x, GB_TRI_2D%node(i)%y, i=1,GB_TRI_2D%nNodes)
	    call freeunit(itmp)

        !     ELEMENT INCIDENCES
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.in3',form='unformatted')
        read(itmp) GB_TRI_2D%nElements

        allocate(GB_TRI_2D%Element(GB_TRI_2D%nElements), GB_TRI_2D%idNode(GB_TRI_2D%nNodesPerElement,GB_TRI_2D%nElements), stat=ialloc)
        call AllocChk(ialloc,'ReadGridBuilderMesh 2d element, idNode arrays')
        GB_TRI_2D%Element(:)%idZone = 0 ! automatic initialization
        GB_TRI_2D%idNode(:,:) = 0 ! automatic initialization
        read(itmp) (GB_TRI_2D%idNode(1,i),GB_TRI_2D%idNode(2,i),GB_TRI_2D%idNode(3,i), i=1,GB_TRI_2D%nElements)
	    call freeunit(itmp)

        !     Element zone numbers
	    call getunit(itmp)
        open(itmp,file=trim(GBPathToFile)//'.ean',form='unformatted')  ! ean contains GB element area(aka zone) numbers 
        read(itmp) (GB_TRI_2D%Element(i)%idZone,i=1,GB_TRI_2D%nElements)
	    call freeunit(itmp)
        GB_TRI_2D%nZones=maxval(GB_TRI_2D%Element%idZone)
        allocate(GB_TRI_2D%Zone(GB_TRI_2D%nZones),stat=ialloc)
        call AllocChk(ialloc,'GB_TRI_2D%Zone array')
        
        do i=1,GB_TRI_2D%nElements
            ! xc and yc from circumcircles
            if(GB_TRI_2D%nNodesPerElement /= 3) call Errmsg('Currently only working for 3-node triangles')
            do j=1,GB_TRI_2D%nNodesPerElement
                x(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%x
                y(j)=GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%y
            end do
            call InnerCircle(x,y,GB_TRI_2D%Element(i)%xyArea,xc,yc,GB_TRI_2D%Element(i)%rCircle,lseg,aseg,dseg)
            
            GB_TRI_2D%Element(i)%SideLength(1)=lseg(1,2)
            GB_TRI_2D%Element(i)%SideLength(2)=lseg(2,3)
            GB_TRI_2D%Element(i)%SideLength(3)=lseg(3,1)
           
            GB_TRI_2D%Element(i)%xCircle=xc
            GB_TRI_2D%Element(i)%yCircle=yc
                
                
            ! zc from centroid of the idNode array coordinates
            zc=0.0
            do j=1,3
                zc=zc+GB_TRI_2D%node(GB_TRI_2D%idNode(j,i))%z
            end do
                
            GB_TRI_2D%Element(i)%x=xc
            GB_TRI_2D%Element(i)%y=yc
            GB_TRI_2D%Element(i)%z=zc/3
            GB_TRI_2D%Element(i)%zCircle=zc/3
        end do
        
        GB_TRI_2D%Element%is=0

        if(EnableTecplotOutput) then
            call GBToTecplot(GB_TRI_2D)
        endif

        return
    end subroutine ReadGridBuilderMesh
    
    !-------------------------------------------------------------
    subroutine GBToTecplot(GB_TRI_2D)
        implicit none
        type(mesh) GB_TRI_2D
        
        integer(i4) :: Fnum
        character(MAX_STR) :: FName
        integer(i4) :: i, j

        ! tecplot output file
        FName=trim(GB_TRI_2D%name)//'.tecplot.dat'
        
        call OpenAscii(FNum,FName)
        call Msg('  ')
        call Msg(TAB//FileCreateSTR//'Tecplot file: '//trim(FName))

        write(FNum,*) 'Title = "'//trim(GB_TRI_2D%name)//'"'

        ! static variables
        VarSTR='variables="X","Y","Z","Zone","Element Area","Inner Circle Radius"'
        nVar=6

        !if(allocated(GB_TRI_2D%rCircle)) then
        !    VarSTR=trim(VarSTR)//'"'//trim(GB_TRI_2D%name)//'Inner circle radius",'
        !    nVar=nVar+1
        !end if
            
        write(FNum,'(a)') trim(VarSTR)


        write(ZoneSTR,'(a,i8,a,i8,a)')'ZONE t="'//trim(GB_TRI_2D%name)//'"  ,N=',GB_TRI_2D%nNodes,', E=',GB_TRI_2D%nElements,&
        ', datapacking=block, zonetype='//trim(GB_TRI_2D%TecplotTyp)
        
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
        write(FNum,'(5('//FMT_R8//'))') (GB_TRI_2D%node(i)%x,i=1,GB_TRI_2D%nNodes)
        write(FNum,'(a)') '# y'
        write(FNum,'(5('//FMT_R8//'))') (GB_TRI_2D%node(i)%y,i=1,GB_TRI_2D%nNodes)
        write(FNum,'(a)') '# z'
        write(FNum,'(5('//FMT_R8//'))') (GB_TRI_2D%node(i)%z,i=1,GB_TRI_2D%nNodes)
        
        write(FNum,'(a)') '# zone'
        write(FNum,'(5i8)') (GB_TRI_2D%Element(i)%idZone,i=1,GB_TRI_2D%nElements)
            
        write(FNum,'(a)') '# element area'
        write(FNum,'(5('//FMT_R8//'))') (GB_TRI_2D%Element(i)%Area,i=1,GB_TRI_2D%nElements)
                    
        write(FNum,'(a)') '# circle radius'
        write(FNum,'(5('//FMT_R8//'))') (GB_TRI_2D%Element(i)%rCircle,i=1,GB_TRI_2D%nElements)
            
        
        do i=1,GB_TRI_2D%nElements
            if(GB_TRI_2D%nNodesPerElement==3) then ! 3-node triangle
                write(FNum,'(8i8)') (GB_TRI_2D%idNode(j,i),j=1,3)
            else if(GB_TRI_2D%nNodesPerElement==4) then ! 4-node quadrilateral
                if(GB_TRI_2D%idNode(4,i) > 0) then
                    write(FNum,'(8i8)') (GB_TRI_2D%idNode(j,i),j=1,4) 
                else
                    write(FNum,'(8i8)') (GB_TRI_2D%idNode(j,i),j=1,3), GB_TRI_2D%idNode(3,i) 
                end if
            else
                write(TmpSTR,'(i2)')GB_TRI_2D%nNodesPerElement
                call ErrMsg(trim(GB_TRI_2D%name)//': '//trim(TmpSTR)//' Nodes Per Element not supported yet')
            end if

        end do
       
        call FreeUnit(FNum)
        
    end subroutine GBToTecplot


    
    end module GB
