module ErrorHandling
    !### Centralized error handling for MUT
    ! Provides standardized error codes and error handling routines
    ! Note: Uses GeneralRoutines::WarnMsg for warning reporting
    !       ErrMsg is now part of this module
    !
    ! CURRENT STATUS:
    ! The error code constants are defined but not yet integrated into the codebase.
    ! Most code still uses call ErrMsg(...) directly without error codes.
    ! These constants are available for future use when migrating to structured error handling.
    !
    ! To use error codes:
    !   call HandleError(ERR_INVALID_INPUT, 'Invalid input message', 'Context')
    !   call CheckError(ERR_FILE_IO, 'File error message', Fatal=.true.)
    !
    ! Error code meanings:
    !   ERR_SUCCESS = 0        - No error
    !   ERR_ALLOCATION = 1     - Memory allocation failure
    !   ERR_FILE_IO = 2        - File I/O error (not found, read/write error)
    !   ERR_INVALID_INPUT = 3  - Invalid user input or instruction
    !   ERR_MEMORY = 4         - Memory-related error
    !   ERR_MATH = 5           - Mathematical error (division by zero, etc.)
    !   ERR_LOGIC = 6          - Logic/programming error
    !   ERR_UNKNOWN = 99       - Unknown/uncategorized error
    
    use KindParameters
    use GeneralRoutines, only: MAX_STR, ErrFNum, WarnMsg, ErrMsg
    
    implicit none
    private
    
    ! Error code constants
    integer(i4), parameter, public :: ERR_SUCCESS = 0
    integer(i4), parameter, public :: ERR_ALLOCATION = 1
    integer(i4), parameter, public :: ERR_FILE_IO = 2
    integer(i4), parameter, public :: ERR_INVALID_INPUT = 3
    integer(i4), parameter, public :: ERR_MEMORY = 4
    integer(i4), parameter, public :: ERR_MATH = 5
    integer(i4), parameter, public :: ERR_LOGIC = 6
    integer(i4), parameter, public :: ERR_UNKNOWN = 99
    
    ! Error message buffer
    character(MAX_STR), private :: LastErrorMsg = ''
    integer(i4), private :: LastErrorCode = ERR_SUCCESS
    
    ! High-level error handling
    public :: SetError, GetLastError, GetLastErrorCode, CheckError, HandleError
    
    contains
    
    !----------------------------------------------------------------------
    subroutine SetError(ErrorCode, ErrorMessage)
        ! Set the last error code and message
        integer(i4), intent(in) :: ErrorCode
        character(*), intent(in) :: ErrorMessage
        
        LastErrorCode = ErrorCode
        LastErrorMsg = ErrorMessage
    end subroutine SetError
    
    !----------------------------------------------------------------------
    function GetLastError() result(ErrorMessage)
        ! Get the last error message
        character(MAX_STR) :: ErrorMessage
        ErrorMessage = LastErrorMsg
    end function GetLastError
    
    !----------------------------------------------------------------------
    function GetLastErrorCode() result(ErrorCode)
        ! Get the last error code
        integer(i4) :: ErrorCode
        ErrorCode = LastErrorCode
    end function GetLastErrorCode
    
    !----------------------------------------------------------------------
    subroutine CheckError(ErrorCode, ErrorMessage, Fatal)
        ! Check error code and handle appropriately
        integer(i4), intent(in) :: ErrorCode
        character(*), intent(in) :: ErrorMessage
        logical, intent(in), optional :: Fatal
        
        logical :: IsFatal
        
        IsFatal = .true.
        if(present(Fatal)) IsFatal = Fatal
        
        if(ErrorCode /= ERR_SUCCESS) then
            call SetError(ErrorCode, ErrorMessage)
            if(IsFatal) then
                call ErrMsg(ErrorMessage)
            else
                call WarnMsg(ErrorMessage)
            end if
        end if
    end subroutine CheckError
    
    !----------------------------------------------------------------------
    subroutine HandleError(ErrorCode, ErrorMessage, Context)
        ! Handle error with context information
        ! This routine:
        !   1. Tracks the error code and message via SetError()
        !   2. Formats the message with context (if provided) and error code
        !   3. Calls ErrMsg() to display and stop execution
        integer(i4), intent(in) :: ErrorCode
        character(*), intent(in) :: ErrorMessage
        character(*), intent(in), optional :: Context
        
        character(MAX_STR) :: FullMessage
        character(20) :: ErrorCodeStr
        
        ! Convert error code to string
        write(ErrorCodeStr,'(i0)') ErrorCode
        
        ! Build full message with context and error code
        if(present(Context)) then
            FullMessage = '[' // trim(ErrorCodeStr) // '] ' // trim(Context) // ': ' // trim(ErrorMessage)
        else
            FullMessage = '[' // trim(ErrorCodeStr) // '] ' // trim(ErrorMessage)
        end if
        
        ! Track error state (for GetLastError/GetLastErrorCode)
        call SetError(ErrorCode, FullMessage)
        
        ! Report error and stop (ErrMsg handles console/file output and stop)
        call ErrMsg(FullMessage)
    end subroutine HandleError

end module ErrorHandling

