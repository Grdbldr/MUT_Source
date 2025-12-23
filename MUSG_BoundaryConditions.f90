module MUSG_BoundaryConditions
    !### Boundary condition management for MODFLOW-USG
    ! Handles assignment of boundary conditions: CHD, DRN, RCH, WEL, Critical Depth, etc.
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, ErrFNum, ErrMsg, Msg, FileReadSTR
    use GeneralRoutines, only: FMT_R4, FMT_R8, TmpSTR, UnitsOfLength, FileCreateSTR, MUTVersion
    use GeneralRoutines, only: bcheck, chosen, set, ConstantHead, Recharge, Drain, Well, CriticalDepth, ialloc, UnitsOfTime
    use ErrorHandling, only: ERR_LOGIC, HandleError
    use ArrayUtilities, only: AllocChk
    use FileIO, only: OpenAscii
    use MUSG_Core, only: ModflowProject, ModflowDomain, NodalControlVolume
    use NumericalMesh, only: mesh
    
    implicit none
    private
    
    public :: AssignCHDtoDomain, AssignDRNtoDomain, AssignRCHtoDomain
    public :: AssignTransientRCHtoDomain, AssignWELtoDomain
    public :: AssignCriticalDepthtoDomain, AssignCriticalDepthtoCellsSide1
    
    ! Boundary condition command strings (these would be moved from Modflow_USG.f90)
    ! For now, keeping them in the main module but documenting here
    
    contains
    
    !----------------------------------------------------------------------
    subroutine AssignCHDtoDomain(FNumMUT,modflow,domain) 
        ! Assign Constant Head boundary condition to domain
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        real(dp) :: head
        
        read(FNumMUT,*) head
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' constant head: ',head,'     '//TRIM(UnitsOfLength) 
        call Msg(trim(TmpSTR))

        if(.not. allocated(domain%ConstantHead)) then 
            allocate(domain%ConstantHead(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell constant head array')            
            domain%ConstantHead(:)=-999.d0
        end if
        
        call Msg('    Cell    Constant head')
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                call set(domain%cell(i)%is,ConstantHead)
                domain%nCHDCells=domain%nCHDCells+1
                domain%ConstantHead(i)=head
                write(TmpSTR,'(i8,2x,'//FMT_R8//',a)') i,domain%ConstantHead(i),'     '//TRIM(UnitsOfLength)
                call Msg(trim(TmpSTR))
            end if
        end do
        
        if(modflow.iCHD == 0) then ! Initialize CHD file and write data to NAM
            Modflow.FNameCHD=trim(Modflow.Prefix)//'.chd'
            call OpenAscii(Modflow.iCHD,Modflow.FNameCHD)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameCHD))
            write(Modflow.iNAM,'(a,i4,a)') 'CHD  ',Modflow.iCHD,' '//trim(Modflow.FNameCHD)
            write(Modflow.iCHD,'(a,a)') '# MODFLOW-USG CHD file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine AssignCHDtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignDRNtoDomain(FNumMUT,modflow,domain) 
        ! Assign Drain boundary condition to domain
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        real(dp) :: cond
        
        read(FNumMUT,*) cond
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//trim(domain%name)//' drain conductance: ',cond,'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
        call Msg(trim(TmpSTR))

        if(.not. allocated(domain%DrainConductance)) then 
            allocate(domain%DrainConductance(domain%nCells),domain%DrainElevation(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell drain arrays')            
            domain%DrainElevation(:)=-999.d0
            domain%DrainConductance(:)=-999.d0
        end if
        
        call Msg('        Cell      DrainElevation             DrainConductance')
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                call set(domain%cell(i)%is,Drain)
                domain%nDRNCells=domain%nDRNCells+1
                domain%DrainElevation(i)=domain%cell(i)%Top
                domain%DrainConductance(i)=cond
                write(TmpSTR,'(i8,2x,'//FMT_R8//',a,'//FMT_R8//',a)') i,domain%DrainElevation(i),'     '//TRIM(UnitsOfLength), domain%DrainConductance(i),'     '//TRIM(UnitsOfLength)//'     '//TRIM(UnitsOfTime)//'^(-1)'
                call Msg(trim(TmpSTR))
            end if
        end do
        
        if(modflow.iDRN == 0) then ! Initialize DRN file and write data to NAM
            Modflow.FNameDRN=trim(Modflow.Prefix)//'.drn'
            call OpenAscii(Modflow.iDRN,Modflow.FNameDRN)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameDRN))
            write(Modflow.iNAM,'(a,i4,a)') 'DRN  ',Modflow.iDRN,' '//trim(Modflow.FNameDRN)
            write(Modflow.iDRN,'(a,a)') '# MODFLOW-USG DRN file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine AssignDRNtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignRCHtoDomain(FNumMUT,modflow,domain) 
        ! Assign Recharge boundary condition to domain
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        real(dp) :: rech
        integer(i4) :: nRCHoption
        
        read(FNumMUT,*) rech
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' recharge: ',rech,'     '//TRIM(modflow.STR_LengthUnit)//'   '//TRIM(modflow.STR_TimeUnit)//'^(-1)'
        call Msg(trim(TmpSTR))
        read(FNumMUT,*) nRCHoption
        write(TmpSTR,'(a,'//FMT_R8//')') 'Assigning '//domain%name//' recharge option: ',nRCHoption
        call Msg(trim(TmpSTR))
        domain%nRCHoption=nRCHoption
        IF(nRCHoption.EQ.1) then
            call Msg('Option 1 -- recharge to top layer')
        else IF(nRCHoption.EQ.2) then
            call Msg('option 2 -- recharge to one specified node in each vertical column') 
        else IF(nRCHoption.EQ.3) then
            call Msg('Option 3 -- recharge to highest active node in each vertical column')
        else IF(nRCHoption.EQ.4) then
            call Msg('Option 4 -- recharge to swf domain on top of each vertical column')
        endif

        if(.not. allocated(domain%Recharge)) then 
            allocate(domain%Recharge(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell recharge array')            
            domain%Recharge(:)=-999.d0
        end if
        
        do i=1,domain%nCells
            call set(domain%cell(i)%is,Recharge)
            domain%Recharge(i)=rech
        end do

        if(modflow.iRCH == 0) then ! Initialize RCH file and write data to NAM
            Modflow.FNameRCH=trim(Modflow.Prefix)//'.rch'
            call OpenAscii(Modflow.iRCH,Modflow.FNameRCH)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameRCH))
            write(Modflow.iNAM,'(a,i4,a)') 'RCH  ',Modflow.iRCH,' '//trim(Modflow.FNameRCH)
            write(Modflow.iRCH,'(a,a)') '# MODFLOW-USG RCH file written by Modflow-User-Tools version ',trim(MUTVersion)
            write(Modflow.iRCH,*) domain%nRCHoption, domain%iCBB  
            write(modflow.iRCH,*) 1   ! inrech, defaults to read one layer of recharge values
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain%name == 'GWF') then
                do i=1,domain%nCells
                    if(Modflow%GWF%cell(i)%iLayer==1) then
                        write(Modflow.iRCH,'('//FMT_R4//')') domain%recharge(i)
                    endif
                end do
            else if(domain%name == 'SWF') then
                write(Modflow.iRCH,'(5('//FMT_R4//'))') (domain%recharge(i),i=1,domain%nCells)
            endif                

        else
            write(modflow.iRCH,*) 1   ! inrech, defaults to read one layer of recharge values
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain%name == 'GWF') then
                do i=1,domain%nCells
                    if(Modflow%GWF%cell(i)%iLayer==1) then
                        write(Modflow.iRCH,'('//FMT_R4//')') domain%recharge(i)
                    endif
                end do
            else if(domain%name == 'SWF') then
                write(Modflow.iRCH,'(5('//FMT_R4//'))') (domain%recharge(i),i=1,domain%nCells)
            endif                
        end if
    end subroutine AssignRCHtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignTransientRCHtoDomain(FNumMUT,modflow,domain) 
        ! Assign Transient Recharge boundary condition to domain
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        real(dp) :: rech
        integer(i4) :: nRCHoption
        
        read(FNumMUT,'(a)') modflow.FNameRTS
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' transient recharge from RTS file: '//TRIM(modflow.FNameRTS)
        call Msg(trim(TmpSTR))
        read(FNumMUT,*) nRCHoption
        write(TmpSTR,'(a,'//FMT_R8//')') 'Assigning '//domain%name//' recharge option: ',nRCHoption
        call Msg(trim(TmpSTR))
        domain%nRCHoption=nRCHoption
        IF(nRCHoption.EQ.1) then
            call Msg('Option 1 -- recharge to top layer')
        else IF(nRCHoption.EQ.2) then
            call Msg('option 2 -- recharge to one specified node in each vertical column') 
        else IF(nRCHoption.EQ.3) then
            call Msg('Option 3 -- recharge to highest active node in each vertical column')
        else IF(nRCHoption.EQ.4) then
            call Msg('Option 4 -- recharge to swf domain on top of each vertical column')
        endif

        if(.not. allocated(domain%Recharge)) then 
            allocate(domain%Recharge(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell recharge array')            
            domain%Recharge(:)=-999.d0
        end if
        
        ! For transient recharge, initial value is typically 0 or read from file
        rech = 0.0d0
        do i=1,domain%nCells
            call set(domain%cell(i)%is,Recharge)
            domain%Recharge(i)=rech
        end do
        
        if(modflow.iRCH == 0) then ! Initialize RCH file and write data to NAM
            Modflow.FNameRCH=trim(Modflow.Prefix)//'.rch'
            call OpenAscii(Modflow.iRCH,Modflow.FNameRCH)
            call OpenAscii(Modflow.iRTS,Modflow.FNameRTS)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameRCH))
            write(Modflow.iNAM,'(a,i4,a)') 'RCH  ',Modflow.iRCH,' '//trim(Modflow.FNameRCH)
            write(Modflow.iNAM,'(a,i4,a)') 'RTS  ',Modflow.iRTS,' '//trim(Modflow.FNameRTS)
            write(Modflow.iRCH,'(a,a)') '# MODFLOW-USG RCH file written by Modflow-User-Tools version ',trim(MUTVersion)
            write(Modflow.iRCH,*) domain%nRCHoption, domain%iCBB, 'RTS 1'
            write(modflow.iRCH,*) 1, 'INRCHZONES ',1165    ! inrech (defaults to read one layer of recharge values), 'INRCHZONES ',1165 (defaults to read current size of rainfall.rts)
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain%name == 'GWF') then
                do i=1,domain%nCells
                    if(Modflow%GWF%cell(i)%iLayer==1) then
                        write(Modflow.iRCH,'('//FMT_R4//')') domain%recharge(i)
                    endif
                end do
            else if(domain%name == 'SWF') then
                write(Modflow.iRCH,'(5('//FMT_R4//'))') (domain%recharge(i),i=1,domain%nCells)
            endif                

        else
            write(modflow.iRCH,*) 1   ! inrech, defaults to read one layer of recharge values
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  Recharge()'
            if(domain%name == 'GWF') then
                do i=1,domain%nCells
                    if(Modflow%GWF%cell(i)%iLayer==1) then
                        write(Modflow.iRCH,'('//FMT_R4//')') domain%recharge(i)
                    endif
                end do
            else if(domain%name == 'SWF') then
                write(Modflow.iRCH,'(5('//FMT_R4//'))') (domain%recharge(i),i=1,domain%nCells)
            endif                
        end if
    end subroutine AssignTransientRCHtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignWELtoDomain(FNumMUT,modflow,domain) 
        ! Assign Well boundary condition to domain
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i, itmp, itmpcln
        real(dp) :: PumpRate
        
        read(FNumMUT,*) PumpRate
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' PumpingRate: ',PumpRate,'     '//TRIM(modflow.STR_LengthUnit)//'   '//TRIM(modflow.STR_TimeUnit)//'^(-1)'
        call Msg(trim(TmpSTR))
        
        if(.not. allocated(domain%PumpingRate)) then 
            allocate(domain%PumpingRate(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell PumpingRate array')            
            domain%PumpingRate(:)=-999.d0
        end if

        itmp=0
        itmpcln=0

        if(domain%name == 'GWF') then
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    domain%nWELCells=domain%nWELCells+1
                    call set(domain%cell(i)%is,Well)
                    domain%PumpingRate(i)=PumpRate
                    itmp=itmp+1
                endif
            end do
        else if(domain%name == 'CLN') then
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    domain%nWELCells=domain%nWELCells+1
                    call set(domain%cell(i)%is,Well)
                    domain%PumpingRate(i)=PumpRate
                    itmpcln=itmpcln+1
                endif
            end do
        endif    

        !if(modflow.iWEL == 0) then ! Initialize WEL file and write data to NAM
        !    Modflow.FNameWEL=trim(Modflow.Prefix)//'.WEL'
        !    call OpenAscii(Modflow.iWEL,Modflow.FNameWEL)
        !    call Msg('  ')
        !    call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameWEL))
        !    write(Modflow.iNAM,'(a,i4,a)') 'WEL  ',Modflow.iWEL,' '//trim(Modflow.FNameWEL)
        !    write(Modflow.iWEL,'(a,a)') '# MODFLOW-USG WEL file written by Modflow-User-Tools version ',trim(MUTVersion)
        !end if
        
        if(modflow.iWEL == 0) then ! Initialize WEL file and write data to NAM
            Modflow.FNameWEL=trim(Modflow.Prefix)//'.WEL'
            call OpenAscii(Modflow.iWEL,Modflow.FNameWEL)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameWEL))
            write(Modflow.iNAM,'(a,i4,a)') 'WEL  ',Modflow.iWEL,' '//trim(Modflow.FNameWEL)
            write(Modflow.iWEL,'(a,a)') '# MODFLOW-USG WEL file written by Modflow-User-Tools version ',trim(MUTVersion)
            if(itmp>0 .AND. itmpcln>0) then
                call HandleError(ERR_LOGIC, 'WEL package can currently only be used for GWF or CLN domains but not both at the same time', 'AssignWELtoDomain')
            else if(itmp>0) then
                write(Modflow.iWEL,'(2i8)') itmp, Modflow%GWF%icbb
                write(Modflow.iWEL,'(3i8)') itmp, 0, 0 
            else if(itmpcln>0) then
                write(Modflow.iWEL,'(2i8)') itmpcln, Modflow%CLN%icbb
                write(Modflow.iWEL,'(3i8)') 0, 0, itmpcln 
            endif
                
            if(domain%name == 'GWF') then
                do i=1,domain%nCells
                    if(bcheck(domain%cell(i)%is,Well)) then
                        write(Modflow.iWEL,'(i8,('//FMT_R8//'),i8)') i,domain%PumpingRate(i),0
                    endif
                end do
            else if(domain%name == 'CLN') then
                do i=1,domain%nCells
                    if(bcheck(domain%cell(i)%is,Well)) then
                        write(Modflow.iWEL,'(i8,('//FMT_R8//'),i8)') i,domain%PumpingRate(i),0
                    endif
                end do
            endif
        !else
        !    pause 'next stress period?'
        end if

    end subroutine AssignWELtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignCriticalDepthtoDomain(modflow,domain) 
        ! Assign Critical Depth boundary condition to SWF domain
        implicit none
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i, j, k, j1, j2
        real(dp) :: AddToLength
        
        call Msg('Define all chosen '//trim(domain%name)//' Cells to be critical depth')
        call Msg('Assumes appropriate '//trim(domain%name)//' SWBC nodes flagged as boundary nodes') 
        
        if(NodalControlVolume) then
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    do k=1,domain%nElements
                        do j=1,domain%nFacesPerElement
                            if(domain%FaceNeighbour(j,k) == 0) then ! only consider faces that are on the outer boundary
                                j1=domain%idNode(domain%LocalFaceNodes(1,j),k)
                                j2=domain%idNode(domain%LocalFaceNodes(2,j),k)
                                if(j1==i) then ! this node represents a chosen cell 
                                    if(.not. bcheck(domain%cell(i)%is,CriticalDepth)) then
                                        call set(domain%cell(i)%is,CriticalDepth)
                                        domain%nSWBCCells=domain%nSWBCCells+1
                                    endif
                                    AddToLength=sqrt((domain%node(j1)%x - domain%element(k)%xSide(j))**2 + &
                                                     (domain%node(j1)%y - domain%element(k)%ySide(j))**2)
                                    domain%cell(i)%CriticalDepthLength=domain%cell(i)%CriticalDepthLength+AddToLength
                                endif
                                if(j2==i) then ! this node represents a chosen cell 
                                    if(.not. bcheck(domain%cell(i)%is,CriticalDepth)) then
                                        call set(domain%cell(i)%is,CriticalDepth)
                                        domain%nSWBCCells=domain%nSWBCCells+1
                                    endif
                                    AddToLength=sqrt((domain%node(j2)%x - domain%element(k)%xSide(j))**2 + &
                                                     (domain%node(j2)%y - domain%element(k)%ySide(j))**2)
                                    domain%cell(i)%CriticalDepthLength=domain%cell(i)%CriticalDepthLength+AddToLength
                                endif
                            endif
                        enddo 
                    enddo
                endif
            end do
        else    
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    do j=1,domain%nFacesPerElement
                        if(domain%FaceNeighbour(j,i) == 0) then ! add sidelength to CriticalDepthLength
                            if(.not. bcheck(domain%cell(i)%is,CriticalDepth)) then
                                call set(domain%cell(i)%is,CriticalDepth)
                                domain%nSWBCCells=domain%nSWBCCells+1
                            endif
                            domain%cell(i)%CriticalDepthLength=domain%cell(i)%CriticalDepthLength+domain%Element(i)%SideLength(j)
                        endif
                    enddo
                endif
            enddo
        endif
 
        if(modflow%iSWBC == 0) then ! Initialize SWBC file and write data to NAM
            modflow%FNameSWBC=trim(modflow%Prefix)//'.swbc'
            call OpenAscii(modflow%iSWBC,modflow%FNameSWBC)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(modflow%FNameSWBC))
            write(modflow%iNAM,'(a,i4,a)') 'SWBC  ',modflow%iSWBC,' '//trim(modflow%FNameSWBC)
            write(modflow%iSWBC,'(a,a)') '# MODFLOW-USG SWBC file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine AssignCriticalDepthtoDomain
    
    !----------------------------------------------------------------------
    subroutine AssignCriticalDepthtoCellsSide1(modflow,domain) 
        ! Assign Critical Depth boundary condition to cells on side 1
        implicit none
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        
        call Msg('Define all chosen '//trim(domain%name)//' Cells to be critical depth')
        call Msg('Assumes SWBC Critical Depth Length equals sqrt(cell area)') 
        
        if(NodalControlVolume) then
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    call set(domain%cell(i)%is,CriticalDepth)
                    domain%nSWBCCells=domain%nSWBCCells+1
                    domain%cell(i)%CriticalDepthLength=SQRT(domain%Element(i)%xyArea)
                endif
            end do
        else    
            do i=1,domain%nCells
                if(bcheck(domain%cell(i)%is,chosen)) then
                    call set(domain%cell(i)%is,CriticalDepth)
                    domain%nSWBCCells=domain%nSWBCCells+1
                    domain%cell(i)%CriticalDepthLength=SQRT(domain%Element(i)%xyArea)
                end if
            end do
        end if
 
        if(modflow%iSWBC == 0) then ! Initialize SWBC file and write data to NAM
            modflow%FNameSWBC=trim(modflow%Prefix)//'.swbc'
            call OpenAscii(modflow%iSWBC,modflow%FNameSWBC)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(modflow%FNameSWBC))
            write(modflow%iNAM,'(a,i4,a)') 'SWBC  ',modflow%iSWBC,' '//trim(modflow%FNameSWBC)
            write(modflow%iSWBC,'(a,a)') '# MODFLOW-USG SWBC file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine AssignCriticalDepthtoCellsSide1

end module MUSG_BoundaryConditions

