module StringUtilities
    !### String manipulation utilities
    ! Provides routines for string processing and manipulation
    
    use KindParameters
    use GeneralRoutines, only: MAX_STR
    
    implicit none
    private
    
    public :: LwrCse, UprCse, StripComments
    
    contains
    
    !----------------------------------------------------------------------
    subroutine LwrCse(string) 
        ! Make string lowercase
        implicit none
        character(*) :: string
        integer(i4) :: i, n
        
        n = len(string)
        do i = 1, n
            if(string(i:i) >= 'A' .and. string(i:i) <= 'Z') then
                string(i:i) = char(ichar(string(i:i)) + 32)
            end if
        end do
    end subroutine LwrCse
    
    !----------------------------------------------------------------------
    subroutine UprCse(string) 
        ! Make string uppercase
        implicit none
        character(*) :: string
        integer(i4) :: i, n
        
        n = len(string)
        do i = 1, n
            if(string(i:i) >= 'a' .and. string(i:i) <= 'z') then
                string(i:i) = char(ichar(string(i:i)) - 32)
            end if
        end do
    end subroutine UprCse
    
    !----------------------------------------------------------------------
    subroutine StripComments(nin,nout)
        ! Strip comments and blank lines from input file
        ! This is a placeholder - the actual implementation should be moved from GeneralRoutines
        implicit none
        integer(i4) :: nin, nout
        ! TODO: Move implementation from GeneralRoutines.f90
    end subroutine StripComments

end module StringUtilities

