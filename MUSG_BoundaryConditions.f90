module MUSG_BoundaryConditions
    !### Boundary condition management for MODFLOW-USG
    ! Handles assignment of boundary conditions: CHD, DRN, RCH, WEL, Critical Depth, etc.
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, ErrFNum, ErrMsg, Msg, FileReadSTR
    use GeneralRoutines, only: FMT_R4, FMT_R8, TmpSTR, UnitsOfLength, FileCreateSTR, MUTVersion
    use GeneralRoutines, only: bcheck, chosen, set, ConstantHead, Recharge, Drain, Well, CriticalDepth, Evapotranspiration, ialloc, UnitsOfTime
    use ErrorHandling, only: ERR_LOGIC, HandleError
    use ArrayUtilities, only: AllocChk
    use GeneralRoutines, only: OpenAscii, FreeUnit
    use MUSG_Core, only: ModflowProject, ModflowDomain, NodalControlVolume
    use MUSG_Core, only: GSTRInstance, MAX_GSTR_INSTANCES, MAX_GSTR_SNAPS
    use NumericalMesh, only: mesh
    
    implicit none
    private
    
    public :: AssignCHDtoDomain, AssignTransientCHDtoSWF, AssignDRNtoDomain, AssignRCHtoDomain
    public :: AssignTransientRCHtoDomain, AssignWELtoDomain, AssignEVTtoDomain
    public :: AssignCriticalDepthtoDomain, AssignCriticalDepthtoCellsSide1
    public :: SetPendingCHDZoneName, SetPendingSWBCZoneName
    public :: SetPendingGSTRInstanceName, AssignGSTRtoDomain, WriteGSTRFile
    
    ! Boundary condition command strings (these would be moved from Modflow_USG.f90)
    ! For now, keeping them in the main module but documenting here
    
    contains

    !----------------------------------------------------------------------
    subroutine SetPendingCHDZoneName(FNumMUT, modflow)
        ! Read CHD zone budget name and set as pending for the next gwf constant head assign
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        character(MAX_STR) :: line
        character(16) :: zname
        integer(i4) :: i, n

        read(FNumMUT,'(a)') line
        line = adjustl(line)
        n = min(16, len_trim(line))
        if(n <= 0) then
            call ErrMsg('chd zone name: blank zone name')
        end if
        zname = ' '
        zname(1:n) = line(1:n)

        ! Reuse existing zone id if name already defined
        do i=1,modflow%nCHDZones
            if(modflow%CHDZoneName(i) == zname) then
                modflow%PendingCHDZoneID = i
                call Msg('CHD zone name (existing): '//trim(zname))
                return
            end if
        end do

        if(modflow%nCHDZones >= 100) then
            call ErrMsg('chd zone name: exceeded MAX_CHD_ZONES')
        end if
        modflow%nCHDZones = modflow%nCHDZones + 1
        modflow%CHDZoneName(modflow%nCHDZones) = zname
        modflow%PendingCHDZoneID = modflow%nCHDZones
        call Msg('CHD zone name: '//trim(zname))
    end subroutine SetPendingCHDZoneName

    !----------------------------------------------------------------------
    subroutine SetPendingSWBCZoneName(FNumMUT, modflow)
        ! Read SWBC zone budget name and set as pending for the next swf critical depth assign
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        character(MAX_STR) :: line
        character(16) :: zname
        integer(i4) :: i, n

        read(FNumMUT,'(a)') line
        line = adjustl(line)
        n = min(16, len_trim(line))
        if(n <= 0) then
            call ErrMsg('swbc zone name: blank zone name')
        end if
        if(index(line(1:n),' ') > 0) then
            call ErrMsg('swbc zone name: zone name may not contain blanks: '//line(1:n))
        end if
        zname = ' '
        zname(1:n) = line(1:n)

        ! Reuse existing zone id if name already defined
        do i=1,modflow%nSWBCZones
            if(modflow%SWBCZoneName(i) == zname) then
                modflow%PendingSWBCZoneID = i
                call Msg('SWBC zone name (existing): '//trim(zname))
                return
            end if
        end do

        if(modflow%nSWBCZones >= 100) then
            call ErrMsg('swbc zone name: exceeded MAX_SWBC_ZONES')
        end if
        modflow%nSWBCZones = modflow%nSWBCZones + 1
        modflow%SWBCZoneName(modflow%nSWBCZones) = zname
        modflow%PendingSWBCZoneID = modflow%nSWBCZones
        call Msg('SWBC zone name: '//trim(zname))
    end subroutine SetPendingSWBCZoneName

    !----------------------------------------------------------------------
    subroutine TagChosenSWBCZone(modflow,domain)
        ! Store the pending SWBC zone id on every chosen critical-depth cell, then clear it
        implicit none
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        integer(i4) :: i

        if(.not. allocated(domain%SWBCZoneID)) then
            allocate(domain%SWBCZoneID(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell SWBC zone id array')
            domain%SWBCZoneID(:)=0
        end if
        if(modflow%PendingSWBCZoneID > 0) then
            call Msg('    using SWBC zone: '//trim(modflow%SWBCZoneName(modflow%PendingSWBCZoneID)))
        end if
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen) .and. bcheck(domain%cell(i)%is,CriticalDepth)) then
                domain%SWBCZoneID(i)=modflow%PendingSWBCZoneID
            end if
        end do
        modflow%PendingSWBCZoneID = 0
    end subroutine TagChosenSWBCZone
    
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
        if(modflow%PendingCHDZoneID > 0) then
            call Msg('    using CHD zone: '//trim(modflow%CHDZoneName(modflow%PendingCHDZoneID)))
        end if

        if(.not. allocated(domain%ConstantHead)) then 
            allocate(domain%ConstantHead(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell constant head array')            
            domain%ConstantHead(:)=-999.d0
        end if
        if(.not. allocated(domain%ConstantHeadZoneID)) then
            allocate(domain%ConstantHeadZoneID(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell constant head zone id array')
            domain%ConstantHeadZoneID(:)=0
        end if
        
        call Msg('    Cell    Constant head')
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                call set(domain%cell(i)%is,ConstantHead)
                domain%nCHDCells=domain%nCHDCells+1
                domain%ConstantHead(i)=head
                domain%ConstantHeadZoneID(i)=modflow%PendingCHDZoneID
                write(TmpSTR,'(i8,2x,'//FMT_R8//',a)') i,domain%ConstantHead(i),'     '//TRIM(UnitsOfLength)
                call Msg(trim(TmpSTR))
            end if
        end do

        ! Pending zone applies only to the following CHD assignment
        modflow%PendingCHDZoneID = 0
        
        call OpenCHDFile(modflow)
    end subroutine AssignCHDtoDomain

    !----------------------------------------------------------------------
    subroutine OpenCHDFile(modflow)
        ! Initialize CHD file and write data to NAM (once)
        implicit none
        type(ModflowProject) :: modflow

        if(modflow.iCHD == 0) then
            Modflow.FNameCHD=trim(Modflow.Prefix)//'.chd'
            call OpenAscii(Modflow.iCHD,Modflow.FNameCHD)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameCHD))
            write(Modflow.iNAM,'(a,i4,a)') 'CHD  ',Modflow.iCHD,' '//trim(Modflow.FNameCHD)
            write(Modflow.iCHD,'(a,a)') '# MODFLOW-USG CHD file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine OpenCHDFile

    !----------------------------------------------------------------------
    subroutine AssignTransientCHDtoSWF(FNumMUT,modflow,domain)
        ! Assign a start/end constant head to chosen SWF cells for the current stress period.
        ! USG CHD interpolates linearly from start to end head over the stress period.
        ! Must follow a 'stress period' block; records are written by WriteCHDFile.
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain

        integer(i4) :: i, iPer, nNew, nCap
        real(dp) :: hStart, hEnd
        integer(i4), allocatable :: itmp(:)
        real(dp), allocatable :: rtmp(:)

        read(FNumMUT,*) hStart, hEnd
        iPer = max(1, modflow%nPeriods)
        write(TmpSTR,'(a,i0,a,2('//FMT_R8//'),a)') 'Assigning SWF transient constant head, stress period ',iPer, &
            ', start/end: ',hStart,hEnd,'     '//TRIM(UnitsOfLength)
        call Msg(trim(TmpSTR))
        if(modflow%PendingCHDZoneID > 0) then
            call Msg('    using CHD zone: '//trim(modflow%CHDZoneName(modflow%PendingCHDZoneID)))
        end if

        nNew = 0
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) nNew = nNew + 1
        end do
        if(nNew == 0) then
            call HandleError(ERR_LOGIC, 'swf transient constant head: no SWF cells chosen', 'AssignTransientCHDtoSWF')
        end if

        if(.not. allocated(modflow%TransCHDPeriod)) then
            nCap = max(1024, nNew)
            allocate(modflow%TransCHDPeriod(nCap), modflow%TransCHDCell(nCap), modflow%TransCHDZone(nCap), &
                modflow%TransCHDStart(nCap), modflow%TransCHDEnd(nCap), stat=ialloc)
            call AllocChk(ialloc,'Transient CHD record arrays')
        else if(modflow%nTransCHD + nNew > size(modflow%TransCHDPeriod)) then
            nCap = max(2*size(modflow%TransCHDPeriod), modflow%nTransCHD + nNew)
            allocate(itmp(nCap)); itmp(:modflow%nTransCHD) = modflow%TransCHDPeriod(:modflow%nTransCHD)
            call move_alloc(itmp, modflow%TransCHDPeriod)
            allocate(itmp(nCap)); itmp(:modflow%nTransCHD) = modflow%TransCHDCell(:modflow%nTransCHD)
            call move_alloc(itmp, modflow%TransCHDCell)
            allocate(itmp(nCap)); itmp(:modflow%nTransCHD) = modflow%TransCHDZone(:modflow%nTransCHD)
            call move_alloc(itmp, modflow%TransCHDZone)
            allocate(rtmp(nCap)); rtmp(:modflow%nTransCHD) = modflow%TransCHDStart(:modflow%nTransCHD)
            call move_alloc(rtmp, modflow%TransCHDStart)
            allocate(rtmp(nCap)); rtmp(:modflow%nTransCHD) = modflow%TransCHDEnd(:modflow%nTransCHD)
            call move_alloc(rtmp, modflow%TransCHDEnd)
        end if

        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                if(bcheck(domain%cell(i)%is,ConstantHead)) then
                    call HandleError(ERR_LOGIC, 'swf transient constant head: cell already has a static swf constant head', &
                        'AssignTransientCHDtoSWF')
                end if
                modflow%nTransCHD = modflow%nTransCHD + 1
                modflow%TransCHDPeriod(modflow%nTransCHD) = iPer
                modflow%TransCHDCell(modflow%nTransCHD) = i
                modflow%TransCHDZone(modflow%nTransCHD) = modflow%PendingCHDZoneID
                modflow%TransCHDStart(modflow%nTransCHD) = hStart
                modflow%TransCHDEnd(modflow%nTransCHD) = hEnd
            end if
        end do

        modflow%PendingCHDZoneID = 0
        call OpenCHDFile(modflow)
    end subroutine AssignTransientCHDtoSWF
    
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
        ! Assign Recharge boundary condition to domain (stress-period strategy)
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i
        real(dp) :: rech
        integer(i4) :: nRCHoption
        
        call Msg('Recharge strategy: stress-period (rates written to RCH per stress period)')
        
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
    subroutine AssignEVTtoDomain(FNumMUT,modflow,domain)
        ! Assign GWF evapotranspiration (EVT package) for the current stress period.
        ! Reads EVTR, NEVTOP, EXDP. SURF is land-surface elevation (top of layer 1).
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain

        integer(i4) :: i, nTop
        real(dp) :: evtr, exdp
        integer(i4) :: nEVToption

        if(domain%name /= 'GWF') then
            call ErrMsg('gwf evt: EVT package applies only to the GWF domain')
        end if

        read(FNumMUT,*) evtr
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' EVT max rate (EVTR): ', &
            evtr,'     '//TRIM(modflow.STR_LengthUnit)//'   '//TRIM(modflow.STR_TimeUnit)//'^(-1)'
        call Msg(trim(TmpSTR))

        read(FNumMUT,*) nEVToption
        write(TmpSTR,'(a,'//FMT_R8//')') 'Assigning '//domain%name//' EVT option (NEVTOP): ',nEVToption
        call Msg(trim(TmpSTR))
        domain%nEVToption = nEVToption
        if(nEVToption == 1) then
            call Msg('Option 1 -- evapotranspiration from top layer')
        else if(nEVToption == 2) then
            call Msg('Option 2 -- evapotranspiration from one specified node in each vertical column')
        else if(nEVToption == 3) then
            call Msg('Option 3 -- evapotranspiration from highest active node in each vertical column')
        else
            call ErrMsg('gwf evt: NEVTOP must be 1, 2, or 3')
        end if
        if(nEVToption == 2) then
            call ErrMsg('gwf evt: NEVTOP=2 (IEVT layer index) is not yet supported by MUT')
        end if

        read(FNumMUT,*) exdp
        write(TmpSTR,'(a,'//FMT_R8//',a)') 'Assigning '//domain%name//' EVT extinction depth (EXDP): ', &
            exdp,'     '//TRIM(modflow.STR_LengthUnit)
        call Msg(trim(TmpSTR))
        if(exdp <= 0.0d0) then
            call ErrMsg('gwf evt: extinction depth EXDP must be > 0')
        end if

        if(.not. allocated(domain%Evapotranspiration)) then
            allocate(domain%Evapotranspiration(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell EVTR array')
            domain%Evapotranspiration(:) = 0.0d0
        end if
        if(.not. allocated(domain%ETSurface)) then
            allocate(domain%ETSurface(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell ET surface array')
            domain%ETSurface(:) = -999.d0
        end if
        if(.not. allocated(domain%ExtinctionDepth)) then
            allocate(domain%ExtinctionDepth(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell EXDP array')
            domain%ExtinctionDepth(:) = -999.d0
        end if

        ! Reset rates each stress-period assignment; set chosen cells (typically all).
        domain%Evapotranspiration(:) = 0.0d0
        do i=1,domain%nCells
            if(bcheck(domain%cell(i)%is,chosen)) then
                call set(domain%cell(i)%is,Evapotranspiration)
                domain%Evapotranspiration(i) = evtr
                domain%ExtinctionDepth(i) = exdp
                domain%ETSurface(i) = domain%cell(i)%Top
            end if
        end do

        ! Ensure every top-layer column has SURF/EXDP even if not chosen (EVTR stays 0).
        do i=1,domain%nCells
            if(domain%cell(i)%iLayer == 1) then
                domain%ETSurface(i) = domain%cell(i)%Top
                if(domain%ExtinctionDepth(i) < 0.0d0) domain%ExtinctionDepth(i) = exdp
            end if
        end do

        nTop = domain%nCells / domain%nLayers

        if(modflow.iEVT == 0) then
            Modflow.FNameEVT = trim(Modflow.Prefix)//'.evt'
            call OpenAscii(Modflow.iEVT,Modflow.FNameEVT)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow.FNameEVT))
            write(Modflow.iNAM,'(a,i4,a)') 'EVT  ',Modflow.iEVT,' '//trim(Modflow.FNameEVT)
            write(Modflow.iEVT,'(a,a)') '# MODFLOW-USG EVT file written by Modflow-User-Tools version ', &
                trim(MUTVersion)
            write(Modflow.iEVT,*) domain%nEVToption, domain%iCBB
        end if

        ! Per stress period: INSURF, INEVTR, INEXDP (all read this period)
        write(Modflow.iEVT,*) 1, 1, 1
        write(Modflow.iEVT,'(a)') 'INTERNAL  1  (FREE)  -1  ET Surface()'
        do i=1,domain%nCells
            if(domain%cell(i)%iLayer == 1) then
                write(Modflow.iEVT,'('//FMT_R4//')') domain%ETSurface(i)
            end if
        end do
        write(Modflow.iEVT,'(a)') 'INTERNAL  1  (FREE)  -1  Evapotranspiration()'
        do i=1,domain%nCells
            if(domain%cell(i)%iLayer == 1) then
                write(Modflow.iEVT,'('//FMT_R4//')') domain%Evapotranspiration(i)
            end if
        end do
        write(Modflow.iEVT,'(a)') 'INTERNAL  1  (FREE)  -1  Extinction Depth()'
        do i=1,domain%nCells
            if(domain%cell(i)%iLayer == 1) then
                write(Modflow.iEVT,'('//FMT_R4//')') domain%ExtinctionDepth(i)
            end if
        end do

        write(TmpSTR,'(a,i8)') 'Wrote EVT arrays for top-layer cells = ', nTop
        call Msg(trim(TmpSTR))
    end subroutine AssignEVTtoDomain
    
    !----------------------------------------------------------------------
    integer(i4) function CountRTSZones(FNameRTS)
        ! Count recharge zones from first RTS data line: Tstart Tend Factor Rate1..RateN
        implicit none
        character(*), intent(in) :: FNameRTS
        integer(i4) :: iunit, ios, nvals, i
        character(len=512) :: line
        real(dp) :: vals(100)
        
        CountRTSZones = 1
        call OpenAscii(iunit, FNameRTS)
        do
            read(iunit,'(a)',iostat=ios) line
            if(ios /= 0) exit
            if(len_trim(line) == 0) cycle
            if(line(1:1) == '#' .or. line(1:1) == '!') cycle
            nvals = 0
            do i=1,100
                read(line,*,iostat=ios) vals(1:i)
                if(ios /= 0) exit
                nvals = i
            end do
            if(nvals >= 4) then
                CountRTSZones = nvals - 3
            else
                CountRTSZones = 1
            end if
            exit
        end do
        close(iunit)
        call FreeUnit(iunit)
        if(CountRTSZones < 1) CountRTSZones = 1
    end function CountRTSZones

    !----------------------------------------------------------------------
    subroutine WriteMergedRTSFile(modflow)
        ! Merge single-column RTS zone files into FNameRTS (multi-column).
        implicit none
        type(ModflowProject) :: modflow

        integer(i4), parameter :: maxrec = 5000
        integer(i4) :: iz, iunit, ios, irec, nrec, j, nvals
        integer(i4) :: nrec_ref
        character(len=512) :: line
        real(dp) :: vals(100)
        real(dp) :: tstart(maxrec), tend(maxrec), factor(maxrec)
        real(dp) :: rates(maxrec, 20)
        real(dp) :: tstart_z, tend_z, factor_z, rate_z
        integer(i4) :: iout

        if(modflow%nRTSZones < 1) return

        nrec_ref = 0
        rates = 0.0d0

        do iz = 1, modflow%nRTSZones
            call OpenAscii(iunit, modflow%FNameRTSZones(iz))
            irec = 0
            do
                read(iunit,'(a)',iostat=ios) line
                if(ios /= 0) exit
                if(len_trim(line) == 0) cycle
                if(line(1:1) == '#' .or. line(1:1) == '!') cycle
                nvals = 0
                do j = 1, 100
                    read(line,*,iostat=ios) vals(1:j)
                    if(ios /= 0) exit
                    nvals = j
                end do
                if(nvals < 4) then
                    call ErrMsg('RTS file has fewer than 4 values on a data line: '// &
                                trim(modflow%FNameRTSZones(iz)))
                end if
                if(nvals > 4) then
                    call ErrMsg('Multi-zone source RTS files must have one rate column: '// &
                                trim(modflow%FNameRTSZones(iz)))
                end if
                tstart_z = vals(1)
                tend_z = vals(2)
                factor_z = vals(3)
                rate_z = vals(4)
                irec = irec + 1
                if(irec > maxrec) then
                    call ErrMsg('RTS file exceeds maximum records in WriteMergedRTSFile')
                end if
                if(iz == 1) then
                    tstart(irec) = tstart_z
                    tend(irec) = tend_z
                    factor(irec) = factor_z
                else
                    if(irec > nrec_ref) then
                        call ErrMsg('RTS zone files have different numbers of records')
                    end if
                    if(abs(tstart_z - tstart(irec)) > 1.0d-9 .or. &
                       abs(tend_z - tend(irec)) > 1.0d-9 .or. &
                       abs(factor_z - factor(irec)) > 1.0d-9) then
                        call ErrMsg('RTS zone files have mismatched time intervals/factors')
                    end if
                end if
                rates(irec, iz) = rate_z
            end do
            close(iunit)
            call FreeUnit(iunit)
            if(iz == 1) then
                nrec_ref = irec
            else if(irec /= nrec_ref) then
                call ErrMsg('RTS zone files have different numbers of records')
            end if
        end do

        nrec = nrec_ref
        open(newunit=iout, file=trim(modflow%FNameRTS), status='replace', &
             form='formatted', iostat=ios)
        if(ios /= 0) then
            call ErrMsg('Error creating merged RTS file: '//trim(modflow%FNameRTS))
        end if
        do irec = 1, nrec
            write(iout,'(3(es16.8,1x))', advance='no') tstart(irec), tend(irec), factor(irec)
            do iz = 1, modflow%nRTSZones
                write(iout,'(es16.8,1x)', advance='no') rates(irec, iz)
            end do
            write(iout,*)
        end do
        close(iout)

        write(TmpSTR,'(a,i0,a,i0,a)') 'Wrote merged RTS with ', modflow%nRTSZones, &
            ' zones and ', nrec, ' records: '//trim(modflow%FNameRTS)
        call Msg(trim(TmpSTR))
    end subroutine WriteMergedRTSFile

    !----------------------------------------------------------------------
    subroutine WriteRTSRechargePackage(modflow, domain)
        ! Rewrite RCH package contents for current RTS zone map.
        implicit none
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain

        integer(i4) :: i, nassigned

        write(Modflow.iRCH,'(a,a)') '# MODFLOW-USG RCH file written by Modflow-User-Tools version ', &
            trim(MUTVersion)
        write(Modflow.iRCH,*) domain%nRCHoption, domain%iCBB, 'RTS', modflow%nRTSZones
        write(modflow.iRCH,*) 1, 'INRCHZONES', modflow%nRTSZones
        write(Modflow.iRCH,'(a)') 'CONSTANT         0.00000  Recharge()'

        nassigned = 0
        do i = 1, domain%nCells
            if(domain%IRTSZone(i) > 0) nassigned = nassigned + 1
        end do

        if(modflow%nRTSZones == 1 .and. nassigned == domain%nCells) then
            ! Backward-compatible uniform map when a single RTS covers all cells
            write(Modflow.iRCH,'(a)') 'CONSTANT               1  IZNRCH()'
        else
            write(Modflow.iRCH,'(a)') 'INTERNAL  1  (FREE)  -1  IZNRCH()'
            write(Modflow.iRCH,'(10(i8))') (domain%IRTSZone(i), i=1, domain%nCells)
        end if

        write(TmpSTR,'(a,i8)') 'Writing RTS-capable RCH with INRCHZONES = ', modflow%nRTSZones
        call Msg(trim(TmpSTR))
        write(TmpSTR,'(a,i8,a,i8)') 'RTS zone map: cells assigned = ', nassigned, &
            ' of ', domain%nCells
        call Msg(trim(TmpSTR))
    end subroutine WriteRTSRechargePackage
    
    !----------------------------------------------------------------------
    subroutine AssignTransientRCHtoDomain(FNumMUT,modflow,domain) 
        ! Assign Transient Recharge boundary condition to domain (RTS strategy).
        ! Repeated calls attach successive single-column RTS files to currently
        ! chosen cells; MUT merges them into one multi-column RTS for USGS.
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        
        integer(i4) :: i, nchosen, nzones_in_file
        real(dp) :: rech
        integer(i4) :: nRCHoption
        character(128) :: FNameZoneRTS
        
        call Msg('Recharge strategy: RTS (time-varying rates from transient recharge file)')
        
        read(FNumMUT,'(a)') FNameZoneRTS
        FNameZoneRTS = adjustl(FNameZoneRTS)
        write(TmpSTR,'(a)') 'Assigning '//domain%name//' transient recharge from RTS file: '// &
            TRIM(FNameZoneRTS)
        call Msg(trim(TmpSTR))

        nzones_in_file = CountRTSZones(FNameZoneRTS)
        if(nzones_in_file /= 1) then
            call ErrMsg('Each swf transient recharge RTS source must have exactly one rate column. File: '// &
                        trim(FNameZoneRTS))
        end if

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
        if(.not. allocated(domain%IRTSZone)) then
            allocate(domain%IRTSZone(domain%nCells),stat=ialloc)
            call AllocChk(ialloc,'Cell RTS zone array')
            domain%IRTSZone(:) = 0
        end if
        
        ! Base RCH array is zero; time-varying rates come from the RTS file
        rech = 0.0d0
        do i=1,domain%nCells
            call set(domain%cell(i)%is,Recharge)
            domain%Recharge(i)=rech
        end do

        if(modflow%nRTSZones >= modflow%maxRTSZones) then
            call ErrMsg('Too many swf transient recharge RTS zone files')
        end if
        modflow%nRTSZones = modflow%nRTSZones + 1
        modflow%FNameRTSZones(modflow%nRTSZones) = FNameZoneRTS

        nchosen = 0
        do i = 1, domain%nCells
            if(bcheck(domain%cell(i)%is, chosen)) then
                domain%IRTSZone(i) = modflow%nRTSZones
                nchosen = nchosen + 1
            end if
        end do
        if(nchosen == 0) then
            call ErrMsg('No chosen cells for swf transient recharge zone '// &
                        trim(FNameZoneRTS))
        end if
        write(TmpSTR,'(a,i8,a,i8)') 'RTS zone ', modflow%nRTSZones, &
            ' assigned to chosen cells: ', nchosen
        call Msg(trim(TmpSTR))

        ! Always write a merged RTS that the nam file points to
        modflow%FNameRTS = '_buildo.merged_RTS.rts'
        if(modflow%RTSNamWritten) then
            close(Modflow%iRTS)
        end if
        call WriteMergedRTSFile(modflow)

        if(modflow%iRCH == 0) then
            Modflow%FNameRCH = trim(Modflow%Prefix)//'.rch'
            call OpenAscii(Modflow%iRCH, Modflow%FNameRCH)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(Modflow%FNameRCH))
            write(Modflow%iNAM,'(a,i4,a)') 'RCH  ', Modflow%iRCH, ' '//trim(Modflow%FNameRCH)
        else
            close(Modflow%iRCH)
            open(Modflow%iRCH, file=trim(Modflow%FNameRCH), status='replace', &
                 form='formatted', iostat=ialloc)
            if(ialloc /= 0) then
                call ErrMsg('Error reopening RCH file: '//trim(Modflow%FNameRCH))
            end if
        end if

        if(.not. modflow%RTSNamWritten) then
            call OpenAscii(Modflow%iRTS, Modflow%FNameRTS)
            write(Modflow%iNAM,'(a,i4,a)') 'RTS  ', Modflow%iRTS, ' '//trim(Modflow%FNameRTS)
            modflow%RTSNamWritten = .true.
        else
            open(Modflow%iRTS, file=trim(Modflow%FNameRTS), status='old', &
                 form='formatted', iostat=ialloc)
            if(ialloc /= 0) then
                call ErrMsg('Error reopening merged RTS file: '//trim(Modflow%FNameRTS))
            end if
        end if

        call WriteRTSRechargePackage(modflow, domain)
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

        call TagChosenSWBCZone(modflow,domain)
 
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

        call TagChosenSWBCZone(modflow,domain)
 
        if(modflow%iSWBC == 0) then ! Initialize SWBC file and write data to NAM
            modflow%FNameSWBC=trim(modflow%Prefix)//'.swbc'
            call OpenAscii(modflow%iSWBC,modflow%FNameSWBC)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(modflow%FNameSWBC))
            write(modflow%iNAM,'(a,i4,a)') 'SWBC  ',modflow%iSWBC,' '//trim(modflow%FNameSWBC)
            write(modflow%iSWBC,'(a,a)') '# MODFLOW-USG SWBC file written by Modflow-User-Tools version ',trim(MUTVersion)
        end if
    end subroutine AssignCriticalDepthtoCellsSide1

    !----------------------------------------------------------------------
    subroutine SetPendingGSTRInstanceName(FNumMUT, modflow)
        ! Read GSTR instance budget name for the next gwf/swf/cln gstr assign
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        character(MAX_STR) :: line
        integer(i4) :: n

        read(FNumMUT,'(a)') line
        line = adjustl(line)
        n = min(16, len_trim(line))
        if(n <= 0) then
            call ErrMsg('gstr instance name: blank name')
        end if
        modflow%PendingGSTRName = ' '
        modflow%PendingGSTRName(1:n) = line(1:n)
        call Msg('GSTR instance name: '//trim(modflow%PendingGSTRName))
    end subroutine SetPendingGSTRInstanceName

    !----------------------------------------------------------------------
    subroutine AssignGSTRtoDomain(FNumMUT, modflow, domain, idomain)
        ! Assign a named GSTR instance to currently chosen cells on domain.
        ! Reads FAC then (time, raster) lines until "end gstr".
        implicit none
        integer(i4) :: FNumMUT
        type(ModflowProject) :: modflow
        type(ModflowDomain) :: domain
        integer(i4), intent(in) :: idomain   ! 1=GWF, 2=CLN, 3=SWF

        integer(i4) :: i, nchosen, nsnaps, inode0, ig
        real(dp) :: fac, tsnap
        character(MAX_STR) :: line, rfile
        character(16) :: iname
        real(dp) :: tbuf(MAX_GSTR_SNAPS)
        character(256) :: fbuf(MAX_GSTR_SNAPS)
        integer(i4), allocatable :: ichosen(:)

        if(modflow%nGSTRInstances >= MAX_GSTR_INSTANCES) then
            call ErrMsg('gstr: exceeded MAX_GSTR_INSTANCES')
        end if

        iname = adjustl(modflow%PendingGSTRName)
        if(len_trim(iname) == 0) then
            write(iname,'(a,i0)') 'GSTR_', modflow%nGSTRInstances + 1
        end if

        read(FNumMUT,*,iostat=ialloc) fac
        if(ialloc /= 0) then
            call ErrMsg('gstr: error reading FAC multiplier')
        end if
        write(TmpSTR,'(a,'//FMT_R8//')') 'GSTR FAC = ', fac
        call Msg(trim(TmpSTR))

        nsnaps = 0
        do
            read(FNumMUT,'(a)',iostat=ialloc) line
            if(ialloc /= 0) then
                call ErrMsg('gstr: unexpected EOF before end gstr')
            end if
            line = adjustl(line)
            if(len_trim(line) == 0) cycle
            if(line(1:1) == '!' .or. line(1:1) == '#') cycle
            if(index(line, 'end gstr') /= 0) exit
            nsnaps = nsnaps + 1
            if(nsnaps > MAX_GSTR_SNAPS) then
                call ErrMsg('gstr: exceeded MAX_GSTR_SNAPS')
            end if
            read(line,*,iostat=ialloc) tsnap, rfile
            if(ialloc /= 0) then
                call ErrMsg('gstr: expected "time rasterfile" or "end gstr"')
            end if
            tbuf(nsnaps) = tsnap
            fbuf(nsnaps) = adjustl(rfile)
            if(nsnaps > 1) then
                if(tbuf(nsnaps) < tbuf(nsnaps-1)) then
                    call ErrMsg('gstr: snapshot times must ascend')
                end if
            end if
            write(TmpSTR,'(a,i0,a,'//FMT_R8//',a)') '  snapshot ', nsnaps, &
                ' t=', tbuf(nsnaps), '  '//trim(fbuf(nsnaps))
            call Msg(trim(TmpSTR))
        end do
        if(nsnaps < 1) then
            call ErrMsg('gstr: need at least one time/raster snapshot')
        end if

        allocate(ichosen(domain%nCells), stat=ialloc)
        call AllocChk(ialloc, 'GSTR chosen cell list')
        nchosen = 0
        do i = 1, domain%nCells
            if(bcheck(domain%cell(i)%is, chosen)) then
                nchosen = nchosen + 1
                ichosen(nchosen) = i
                call set(domain%cell(i)%is, Recharge)
            end if
        end do
        if(nchosen == 0) then
            call ErrMsg('gstr: no chosen cells for instance '//trim(iname))
        end if
        write(TmpSTR,'(a,i8,a)') 'GSTR cells assigned: ', nchosen, &
            '  instance '//trim(iname)
        call Msg(trim(TmpSTR))

        ! Global node offset by domain (GWF | CLN | SWF)
        if(idomain == 1) then
            inode0 = 0
        else if(idomain == 2) then
            inode0 = modflow%GWF%nCells
        else
            inode0 = modflow%GWF%nCells + modflow%CLN%nCells
        end if

        modflow%nGSTRInstances = modflow%nGSTRInstances + 1
        ig = modflow%nGSTRInstances
        modflow%GSTRInst(ig)%name = iname
        modflow%GSTRInst(ig)%idomain = idomain
        modflow%GSTRInst(ig)%ncells = nchosen
        modflow%GSTRInst(ig)%nsnaps = nsnaps
        modflow%GSTRInst(ig)%fac = fac

        allocate(modflow%GSTRInst(ig)%inode(nchosen), &
                 modflow%GSTRInst(ig)%x(nchosen), &
                 modflow%GSTRInst(ig)%y(nchosen), &
                 modflow%GSTRInst(ig)%tsnap(nsnaps), &
                 modflow%GSTRInst(ig)%rfile(nsnaps), stat=ialloc)
        call AllocChk(ialloc, 'GSTR instance arrays')

        do i = 1, nchosen
            modflow%GSTRInst(ig)%inode(i) = inode0 + ichosen(i)
            modflow%GSTRInst(ig)%x(i) = domain%cell(ichosen(i))%x
            modflow%GSTRInst(ig)%y(i) = domain%cell(ichosen(i))%y
        end do
        do i = 1, nsnaps
            modflow%GSTRInst(ig)%tsnap(i) = tbuf(i)
            modflow%GSTRInst(ig)%rfile(i) = fbuf(i)
        end do

        modflow%PendingGSTRName = ' '

        if(modflow%iGSTR == 0) then
            modflow%FNameGSTR = trim(modflow%Prefix)//'.gstr'
            call OpenAscii(modflow%iGSTR, modflow%FNameGSTR)
            call Msg('  ')
            call Msg(FileCreateSTR//'Modflow project file: '//trim(modflow%FNameGSTR))
            write(modflow%iNAM,'(a,i4,a)') 'GSTR ', modflow%iGSTR, ' '//trim(modflow%FNameGSTR)
            modflow%GSTRNamWritten = .true.
        end if

        call WriteGSTRFile(modflow)
        deallocate(ichosen)
    end subroutine AssignGSTRtoDomain

    !----------------------------------------------------------------------
    subroutine WriteGSTRFile(modflow)
        ! Rewrite the full multi-INSTANCE GSTR package file
        implicit none
        type(ModflowProject) :: modflow
        integer(i4) :: ig, j

        if(modflow%nGSTRInstances <= 0 .or. modflow%iGSTR == 0) return

        close(modflow%iGSTR)
        open(modflow%iGSTR, file=trim(modflow%FNameGSTR), status='replace', &
             form='formatted', iostat=ialloc)
        if(ialloc /= 0) then
            call ErrMsg('Error writing GSTR file: '//trim(modflow%FNameGSTR))
        end if

        write(modflow%iGSTR,'(a,a)') '# MODFLOW-USG GSTR file written by Modflow-User-Tools version ', &
            trim(MUTVersion)
        write(modflow%iGSTR,'(2i10,a)') modflow%nGSTRInstances, modflow%IGSTRCB, &
            '     NGSTR IGSTRCB'

        do ig = 1, modflow%nGSTRInstances
            write(modflow%iGSTR,'(a,1x,a)') 'INSTANCE', trim(modflow%GSTRInst(ig)%name)
            write(modflow%iGSTR,'(2i10,i10,1x,es16.8,a)') &
                modflow%GSTRInst(ig)%idomain, &
                modflow%GSTRInst(ig)%ncells, &
                modflow%GSTRInst(ig)%nsnaps, &
                modflow%GSTRInst(ig)%fac, &
                '     IDOMAIN NCELLS NSNAPS FAC'
            do j = 1, modflow%GSTRInst(ig)%ncells
                write(modflow%iGSTR,'(i10,1x,es20.12,1x,es20.12)') &
                    modflow%GSTRInst(ig)%inode(j), &
                    modflow%GSTRInst(ig)%x(j), &
                    modflow%GSTRInst(ig)%y(j)
            end do
            do j = 1, modflow%GSTRInst(ig)%nsnaps
                write(modflow%iGSTR,'(es16.8,1x,a)') &
                    modflow%GSTRInst(ig)%tsnap(j), &
                    trim(modflow%GSTRInst(ig)%rfile(j))
            end do
        end do
    end subroutine WriteGSTRFile

end module MUSG_BoundaryConditions

