module FileIO
    !### File I/O utilities
    ! Provides routines for opening, closing, and managing file units
    
    use KindParameters
    use GeneralRoutines, only: MAX_STR, ErrFNum, ErrMsg, status, file_open_flag, FileExists
    use ErrorHandling, only: ERR_FILE_IO, ERR_LOGIC, HandleError
    
    implicit none
    private
    
    public :: GetUnit, FreeUnit, OpenAscii, OpenBinary, OpenDirect
    
    contains
    
    !----------------------------------------------------------------------
    subroutine GetUnit(iunit) 
        ! Return next available unit number
        use global, only: niunit
        implicit none
        integer(i4) :: iunit
	    iunit=NIUNIT+1  ! don't mess with modflow unit numbers which are less NIUNIT
	    do
		    if(iunit > 2100) stop 'iunit > 2100. Call ghostbusters.'
		    if(file_open_flag(iunit)) then
			    iunit=iunit+1
			    cycle
		    else
			    file_open_flag(iunit)=.true.
			    exit
		    end if
	    end do
    end subroutine GetUnit

    !----------------------------------------------------------------------
    subroutine FreeUnit(iunit) 
        ! Make unit number available
        implicit none
        integer(i4) :: iunit
        character(MAX_STR) :: TmpSTR
        
        if(file_open_flag(iunit)) then
		    file_open_flag(iunit)=.false.
	        close(iunit)
        else
            write(TmpSTR,'(a,i8,a)')  'Unit ',iunit,' already free'
		    call HandleError(ERR_LOGIC, trim(TmpSTR), 'FreeUnit')
	    end if
    end subroutine FreeUnit

    !----------------------------------------------------------------------
    subroutine OpenAscii(iunit,fname) 
        ! Return unit # of file or stop
        implicit none
        integer(i4) :: iunit
        character(*) :: fname
        
	    call GetUnit(iunit)
        open(iunit,file=fname,form='formatted',iostat=status)
	    if(status /= 0) then
	        call FreeUnit(iunit)
		    call HandleError(ERR_FILE_IO, 'Error opening file: '//trim(fname), 'OpenAscii')
        end if
    end subroutine OpenAscii

    !----------------------------------------------------------------------
    subroutine OpenBinary(iunit,fname) 
        ! Return unit # of file or report error and stop
        implicit none
        integer(i4) :: iunit
        character(*) :: fname

	    call GetUnit(iunit)
        open(iunit,file=fname,form='unformatted',iostat=status)
	    if(status /= 0) then
            call FreeUnit(iunit)
            inquire(file=fname,exist=FileExists)
            if(.not. FileExists) then
                call HandleError(ERR_FILE_IO, 'File not found: '//trim(fname), 'OpenBinary')
            else
                call HandleError(ERR_FILE_IO, 'Error opening file: '//trim(fname), 'OpenBinary')
            end if
        end if
    end subroutine OpenBinary

    !----------------------------------------------------------------------
    subroutine OpenDirect(iunit,fname) 
        ! Return unit # of file or stop
        implicit none
        integer(i4) :: iunit
        character(*) :: fname

	    call GetUnit(iunit)
        open(iunit,file=fname,form='formatted',access='Direct',iostat=status)
	    if(status /= 0) then
	        call FreeUnit(iunit)
		    call HandleError(ERR_FILE_IO, 'Error opening file: '//trim(fname), 'OpenDirect')
        end if
    end subroutine OpenDirect

end module FileIO

