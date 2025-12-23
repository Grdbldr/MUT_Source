module MUSG_OutputControl
    !### Output control for MODFLOW-USG
    ! Handles output file generation, output times, and output formatting
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, Msg, TmpSTR, FMT_R8, LwrCse, ialloc, AllocChk
    use MUSG_Core, only: ModflowProject, ModflowDomain
    
    implicit none
    private
    
    public :: GenOCFile
    
    contains
    
    !----------------------------------------------------------------------
    subroutine GenOCFile(FNumMUT,Modflow)
        ! Generate output control file from instructions
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowProject), intent(inout) :: Modflow
        
        character(MAX_INST) :: instruction
        integer(i4) :: i, status
        real(sp) :: OutputTimes(1000)
        
        modflow%nOutputTimes=0
        
        ! Read output times from instructions
        read_oc: do
            read(FNumMUT,'(a)',iostat=status) instruction
            if(status /= 0) exit

            call LwrCse(instruction)
            if(index(instruction, 'end') /=0) then
                call Msg('end generate output control file')
                exit read_oc
            else
                modflow%nOutputTimes=modflow%nOutputTimes+1
                read(instruction,*) OutputTimes(modflow%nOutputTimes)
                call Msg(instruction)
            end if  
        end do read_oc
        
        allocate(modflow%OutputTimes(modflow%nOutputTimes),stat=ialloc)
        call AllocChk(ialloc,'Output time array')
        
        modflow%OutputTimes(:modflow%nOutputTimes)=OutputTimes(:modflow%nOutputTimes)
        
        call Msg(' ')
        call Msg('   #     Output time')
        call Msg('--------------------')
        do i=1,modflow%nOutputTimes
            write(TmpSTR,'(i4,2x,'//FMT_R8//',a)') i, modflow%OutputTimes(i),'     '//TRIM(modflow%STR_TimeUnit)
            call Msg(trim(TmpSTR))
        end do
    end subroutine GenOCFile

end module MUSG_OutputControl

