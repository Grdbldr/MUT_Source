! A generic linked list object
module list
  use KindParameters
  implicit none

  private

  public :: list_t
  public :: list_data
  public :: list_init
  public :: list_free
  public :: list_insert
  public :: list_put
  public :: list_get
  public :: list_next

  ! A public variable to use as a MOLD for transfer()
  integer(i4), dimension(:), allocatable :: list_data

  ! Linked list node data type
  type :: list_t
     private
     integer(i4), dimension(:), pointer :: data => null()
     type(list_t), pointer :: next => null()
  end type list_t

contains

  ! Initialize a head node SELF and optionally store the provided DATA.
  subroutine list_init(self, data)
    type(list_t), pointer :: self
    integer(i4), dimension(:), intent(in), optional :: data

    allocate(self)
    nullify(self%next)

    if (present(data)) then
       allocate(self%data(size(data)))
       self%data = data
    else
       nullify(self%data)
    end if
  end subroutine list_init

  ! Free the entire list and all data, beginning at SELF
  subroutine list_free(self)
    type(list_t), pointer :: self
    type(list_t), pointer :: current
    type(list_t), pointer :: next

    current => self
    do while (associated(current))
       next => current%next
       if (associated(current%data)) then
          deallocate(current%data)
          nullify(current%data)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine list_free

  ! Return the next node after SELF
  function list_next(self) result(next)
    type(list_t), pointer :: self
    type(list_t), pointer :: next
    next => self%next
  end function list_next

  ! Insert a list node after SELF containing DATA (optional)
  subroutine list_insert(self, data)
    type(list_t), pointer :: self
    integer(i4), dimension(:), intent(in), optional :: data
    type(list_t), pointer :: next

    allocate(next)

    if (present(data)) then
       allocate(next%data(size(data)))
       next%data = data
    else
       nullify(next%data)
    end if

    next%next => self%next
    self%next => next
  end subroutine list_insert

  ! Store the encoded DATA in list node SELF
  subroutine list_put(self, data)
    type(list_t), pointer :: self
    integer(i4), dimension(:), intent(in) :: data

    if (associated(self%data)) then
       deallocate(self%data)
       nullify(self%data)
    end if
    self%data = data
  end subroutine list_put

  ! Return the DATA stored in the node SELF
  function list_get(self) result(data)
    type(list_t), pointer :: self
    integer(i4), dimension(:), pointer :: data
    data => self%data
  end function list_get

end module list

    ! A derived type for storing data.
module data
  use KindParameters
  implicit none

  private
  public :: data_t
  public :: data_ptr
  public :: data_Segment

  ! Data is stored in data_t
  type :: data_t
     real(sp) :: x
  end type data_t

  ! Two xyz coordinate triples define the endpoints of a segment
  type :: data_Segment
     real(sp) :: p1x
     real(sp) :: p1y
     real(sp) :: p1z
     real(sp) :: p2x
     real(sp) :: p2y
     real(sp) :: p2z
  end type data_Segment

  ! A trick to allow us to store pointers in the list
  type :: data_ptr
     type(data_t), pointer :: p
  end type data_ptr
end module data

    
!!!! A simple generic linked list test program
!!!program list_test
!!!  use list
!!!  use data
!!!  implicit none
!!!
!!!  type(list_t), pointer :: ll => null()
!!!  type(data_t), target :: dat_a
!!!  type(data_t), target :: dat_b
!!!  type(data_ptr) :: ptr
!!!
!!!  ! Initialize two data objects
!!!  dat_a%x = 17.5
!!!  dat_b%x = 3.0
!!!
!!!  ! Initialize the list with dat_a
!!!  ptr%p => dat_a
!!!  call list_init(ll, DATA=transfer(ptr, list_data))
!!!  print *, 'Initializing list with data:', ptr%p
!!!
!!!  ! Insert dat_b into the list
!!!  ptr%p => dat_b
!!!  call list_insert(ll, DATA=transfer(ptr, list_data))
!!!  print *, 'Inserting node with data:', ptr%p
!!!
!!!  ! Get the head node
!!!  ptr = transfer(list_get(ll), ptr)
!!!  print *, 'Head node data:', ptr%p
!!!
!!!  ! Get the next node
!!!  ptr = transfer(list_get(list_next(ll)), ptr)
!!!  print *, 'Second node data:', ptr%p
!!!
!!!  ! Free the list
!!!  call list_free(ll)
!!!end program list_test 