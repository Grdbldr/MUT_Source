module MUSG_ObservationPoints
    !### Observation point routines for MODFLOW-USG
    ! Handles creation and management of observation points for output during simulation
    
    use KindParameters
    use GeneralRoutines, only: MAX_INST, MAX_STR, MAX_LBL, Msg, ErrMsg, TmpSTR, FMT_R8, UnitsOfLength, FileExists, status
    use GeneralRoutines, only: OpenAscii, FreeUnit
    use ErrorHandling, only: ERR_FILE_IO, HandleError
    use MUSG_Core, only: ModflowDomain
    
    implicit none
    private
    
    public :: ObservationPoint, ObservationPointsFromCSVFile
    
    contains
    
    !----------------------------------------------------------------------
    subroutine ObservationPoint(FNumMUT,domain)
        implicit none
        
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: domain

        ! Assign the cell containing XYZ input coordinate as named observation point 
        ! and output head, sat, depth, conc ...

        integer(i4) :: i
        integer(i4) :: iCell
        real(dp) :: x1,y1,z1,dist_min,f1
        
        domain%nObsPnt=domain%nObsPnt+1

        read(FNumMUT,'(a)') domain%ObsPntName(domain%nObsPnt)
        call Msg(trim(domain%name)//' observation point name: '//trim(domain%ObsPntName(domain%nObsPnt)))

        read(FNumMut,*) x1,y1,z1
        write(TMPStr,*) 'Find cell closest to user XYZ: ',x1, y1, z1
        call Msg(TMPStr)

        dist_min=1.0e20
        do i=1,domain%nCells
            f1=sqrt((x1-domain%cell(i)%x)**2+((y1-domain%cell(i)%y))**2+((z1-domain%cell(i)%z))**2)
            if(f1.lt.dist_min) then
                iCell=i
                dist_min=f1
            endif
        end do
        
        domain%ObsPntCell(domain%nObsPnt)=iCell
        write(TMPStr,'(a,i10,a)') ' Observation point cell: ',domain%ObsPntCell(domain%nObsPnt)
        call Msg(TMPStr)
        write(TMPStr,'(a,'//FMT_R8//',a)') ' Distance from user XYZ: ',dist_min,'     '//TRIM(UnitsOfLength)
        call Msg(TMPStr)
        

    end subroutine ObservationPoint
    
    !----------------------------------------------------------------------
    subroutine ObservationPointsFromCSVFile(FNumMUT,domain)
        implicit none
        integer(i4), intent(in) :: FNumMUT
        type(ModflowDomain), intent(inout) :: domain
        character(MAX_LBL) :: VarSTR
        integer(i4) :: FnumCSV
        character(MAX_STR) :: FNameCSV
        integer(i4) :: i,icell
        integer(i4) :: id
        character(MAX_LBL) :: name
        REAL(dp) :: xcoord,ycoord,zcoord
        REAL(dp) :: dist_min,f1
        
        read(FNumMUT,'(a)') FNameCSV 
        inquire(file=FNameCSV,exist=FileExists)
        if(.not. FileExists) then
            call HandleError(ERR_FILE_IO, 'File not found: '//trim(FNameCSV), 'ObservationPointsFromCSVFile')
        endif
        
        call openAscii(FnumCSV,FNameCSV)
        call Msg('CSV file : '//TRIM(FNameCSV))
        

        
        read(FnumCSV,'(a)') VarSTR
        call Msg('CSV file header: '//TRIM(VarSTR))
        ReadLoop: do 
            read(FnumCSV,*,iostat=status) id,name,xcoord,ycoord,zcoord
            if(status/=0) then
                exit ReadLoop
            else
                domain%nObsPnt=domain%nObsPnt+1
                domain%ObsPntName(domain%nObsPnt)=trim(name)

                call Msg(' ')
                call Msg('------ next observation point ')
                write(TMPStr,*) 'Find cell closest to user XYZ: ',xcoord,ycoord,zcoord
                call Msg(TMPStr)

                dist_min=1.0e20
                do i=1,domain%nCells
                    f1=sqrt((xcoord-domain%cell(i)%x)**2+((ycoord-domain%cell(i)%y))**2+((zcoord-domain%cell(i)%z))**2)
                    if(f1.lt.dist_min) then
                        iCell=i
                        dist_min=f1
                    endif
                end do
        
                domain%ObsPntCell(domain%nObsPnt)=iCell
                write(TMPStr,'(a,a)') ' Observation point name: ',trim(domain%ObsPntName(domain%nObsPnt))
                call Msg(TMPStr)

                write(TMPStr,'(a,i10)') ' Observation point cell: ',domain%ObsPntCell(domain%nObsPnt)
                call Msg(TMPStr)
                write(TMPStr,'(a,'//FMT_R8//',a)') ' Distance from user XYZ: ',dist_min,'     '//TRIM(UnitsOfLength)
                call Msg(TMPStr)
            endif
        end do ReadLoop
        
        call FreeUnit(FnumCSV)

    end subroutine ObservationPointsFromCSVFile

end module MUSG_ObservationPoints

