module MUSG_StressPeriods
    !### Stress period management for MODFLOW-USG
    ! Handles stress period definitions, time stepping, and temporal discretization
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, Msg, ErrMsg, TmpSTR, FMT_R4, LwrCse, ialloc
    use ErrorHandling, only: ERR_INVALID_INPUT, HandleError
    use ArrayUtilities, only: AllocChk
    use MUSG_Core, only: ModflowProject
    
    implicit none
    private
    
    public :: StressPeriod
    
    ! Stress period command strings
    character(MAX_INST), parameter :: Type_CMD = 'type'
    character(MAX_INST), parameter :: Duration_CMD = 'duration'
    character(MAX_INST), parameter :: NumberOfTimesteps_CMD = 'number of timesteps'
    character(MAX_INST), parameter :: Deltat_CMD = 'deltat'
    character(MAX_INST), parameter :: Tminat_CMD = 'tminat'
    character(MAX_INST), parameter :: Tmaxat_CMD = 'tmaxat'
    character(MAX_INST), parameter :: Tadjat_CMD = 'tadjat'
    character(MAX_INST), parameter :: Tcutat_CMD = 'tcutat'
    
    contains
    
    !----------------------------------------------------------------------
    subroutine StressPeriod(FNumMUT,modflow)
        ! Define a stress period from instructions
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Modflow
        
        integer(i4), parameter :: MAXStressPeriods=100
        character(MAX_INST) :: instruction
        integer(i4) :: status
        
        Modflow%nPeriods=Modflow%nPeriods+1  
        write(TmpSTR,'(a,i8)')'Stress period ',Modflow%nPeriods
        call Msg(trim(TmpSTR))
        
        if(Modflow%nPeriods == 1) then
            allocate(Modflow%StressPeriodDuration(MAXStressPeriods), Modflow%StressPeriodnTsteps(MAXStressPeriods), &
            Modflow%StressPeriodnTstepMult(MAXStressPeriods), Modflow%StressPeriodType(MAXStressPeriods),stat=ialloc)
            call AllocChk(ialloc,'Stress period arrays')
            Modflow%StressPeriodDuration(:)=1.0d0
            Modflow%StressPeriodnTsteps(:)=1
            Modflow%StressPeriodnTstepMult(:)=1.1d0
            Modflow%StressPeriodType(:)='TR'
        end if
        
        read_StressPeriod_instructions: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            
            if(index(instruction,'end') /=0) then
                call Msg('end stress period')
                exit read_StressPeriod_instructions
            end if

            if(index(instruction,Type_CMD) /=0) then
                read(FNumMUT,'(a)') modflow%StressPeriodType(Modflow%nPeriods)
                if(modflow%StressPeriodType(Modflow%nPeriods) /= 'SS' .and. modflow%StressPeriodType(Modflow%nPeriods) /= 'TR') then
                    call HandleError(ERR_INVALID_INPUT, 'Stress Period type must begin with either SS or TR', 'StressPeriod')
                end if
                write(TmpSTR,'(a)')'Type: '//trim(modflow%StressPeriodType(Modflow%nPeriods))
                call Msg(trim(TmpSTR))

            else if(index(instruction,Duration_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodDuration(Modflow%nPeriods)
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Duration: ',modflow%StressPeriodDuration(Modflow%nPeriods),'     '//TRIM(modflow%STR_TimeUnit)
                call Msg(trim(TmpSTR))
                
            else if(index(instruction,NumberOfTimesteps_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodnTsteps(Modflow%nPeriods)
                write(TmpSTR,'(a,i5)')'Number of time steps: ',modflow%StressPeriodnTsteps(Modflow%nPeriods)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Deltat_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodDeltat
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Starting time step size: ',modflow%StressPeriodDeltat,'     '//TRIM(modflow%STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tminat_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodTminat
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Minimum time step size: ',modflow%StressPeriodTminat,'     '//TRIM(modflow%STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tmaxat_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodTmaxat
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Maximum time step size: ',modflow%StressPeriodTmaxat,'     '//TRIM(modflow%STR_TimeUnit)
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tadjat_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodTadjat
                write(TmpSTR,'(a,'//FMT_R4//')')'Time step size adjustment factor: ',modflow%StressPeriodTadjat
                call Msg(trim(TmpSTR))

            else if(index(instruction,Tcutat_CMD) /=0) then
                read(FNumMUT,*) modflow%StressPeriodTcutat
                write(TmpSTR,'(a,'//FMT_R4//',a)')'Time step size cutting factor: ',modflow%StressPeriodTcutat
                call Msg(trim(TmpSTR))

            else
                call HandleError(ERR_INVALID_INPUT, 'Unrecognized instruction: stress period', 'StressPeriod')
            end if

        end do read_StressPeriod_instructions
    end subroutine StressPeriod

end module MUSG_StressPeriods

