module KindParameters    ! Purpose: Define kind parameters for use with declaration statements
    !
    !  e.g.     real(dp)    ::  var2
    !           integer(i4) ::  ivar2
    integer, parameter :: sp = selected_real_kind(6, 37)    
	integer, parameter :: dp = selected_real_kind(15, 307)
    integer, parameter :: i2 = selected_int_kind(4)    
    integer, parameter :: i4 = selected_int_kind(9)    
end module  KindParameters