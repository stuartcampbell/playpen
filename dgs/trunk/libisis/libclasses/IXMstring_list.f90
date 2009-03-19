!------------------------------
! MODULE: IXMstring_list
!------------------------------
!! @author Freddie Akeroyd, ISIS; expanded from IXThistory 
!! @version $Revision: 1045 $ ($Date: 2007-03-13 16:18:16 +0000 (Tue, 13 Mar 2007) $)
module IXMstring_list
  use IXMfileio
  use IXMmemory

  implicit none
!  private
  public :: IXTstring_list
  public :: IXFappend, IXFinsert, IXFreplace
  public :: IXFfile_read, IXFfile_write, IXFcheck
  type IXTstring_list
!     private
     character(len=long_len), allocatable :: list(:)
     integer(i4b) :: count = 0 !! number of elements of list(:) actually used
  end type IXTstring_list
  
  interface IXFfile_read
     module procedure file_read_string_list
  end interface
  interface IXFfile_write
     module procedure file_write_string_list
  end interface
  interface IXFcheck
     module procedure check_string_list, check_array_string_list 
  end interface
  interface IXFcheck_and_valid
     module procedure IXFcheck_and_valid_string_list,IXFcheckarray_and_valid_string_list
  end interface
  interface IXFappend
     module procedure append_string_list
  end interface
  interface IXFinsert
     module procedure insert_string_list
  end interface
  interface IXFreplace
     module procedure replace_string_list
  end interface
  integer,parameter :: IXCgrow_size = 5    !! how much to grow the array by

contains

  subroutine check_string_list(arg, status)
    implicit none
    type(IXTstring_list) :: arg
    type(IXTstatus) :: status

    if(.not. allocated(arg%list))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'string_list unallocated (IXFcheck)')
    endif    
  end subroutine check_string_list

  subroutine check_array_string_list(w1,s)
    implicit none
    type(IXTstring_list) :: w1(:)
    type(IXTstatus) :: s
    integer :: i
    do i = 1, size(w1)
       call IXFcheck(w1(i), s)
    end do
  end subroutine check_array_string_list
  
  subroutine IXFcheck_and_valid_string_list  (arg,s)
    implicit none
    type(IXTstring_list ),intent(in)::arg
    type(IXTstatus)::s
    call IXFcheck(arg,s)
  end subroutine IXFcheck_and_valid_string_list

  subroutine IXFcheckarray_and_valid_string_list  (arg,s)
    implicit none
    type(IXTstring_list ),intent(in)::arg(:)
    type(IXTstatus)::s
    call IXFcheck(arg,s)        
  end subroutine IXFcheckarray_and_valid_string_list


  
  subroutine file_read_string_list(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTstring_list) :: value
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'string list error (IXFfile_read)')
  end subroutine file_read_string_list

  subroutine file_write_string_list(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTstring_list) :: value
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'error (IXFfile_write)')
  end subroutine file_write_string_list

  subroutine IXFcreate_string_list(string_list, list, status)
    implicit none
    type(IXTstring_list) :: string_list
    type(IXTstatus) :: status
    character(len=*), allocatable, intent(in) :: list(:)

    if( .not. allocated(list))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'string_list unallocated (IXFcreate)')
    endif
    if(status == IXCseverity_error) return

    call IXFset_string_list(string_list,status,list)
  end subroutine IXFcreate_string_list


  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_string_list(string_list,status)
    implicit none
    type(IXTstring_list) :: string_list
    type(IXTstatus) :: status
    
    call IXFdeallocFortran(string_list%list, status)
    string_list%count=0

  end subroutine IXFdestroy_string_list

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_string_list(string_list,status,list,ref)
    implicit none
    type(IXTstring_list),intent(inout) :: string_list
    type(IXTstring_list),intent(in),optional:: ref
    character(len=*),optional,allocatable,intent(in)::list(:)
    type(IXTstatus),intent(inout) :: status

    if(present(ref))call IXFset_string_list(string_list,status,ref%list)

    if (present(list)) then   
       call IXFreallocDimsFortran(string_list%list, (/ size(list) /), .false., status)
       string_list%list = list
       string_list%count = size(list)
    endif
      
    call IXFcheck(string_list,status)

  end subroutine IXFset_string_list

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_alloc_string_list(string_list, status, list, wout)
    implicit none
    type(IXTstring_list),intent(in) :: string_list
    type(IXTstring_list),optional,intent(out) :: wout
    type(IXTstatus) :: status
    character(len=*),optional,allocatable,intent(out) :: list(:)

    if (present(wout)) call IXFset_string_list(wout,status,ref=string_list)

    if(present(list))then
      call IXFreallocDimsfortran(list, (/ size(string_list%list) /), .false., status)
      list = string_list%list
    endif

  end subroutine IXFget_alloc_string_list

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_string_list(string_list, list)
    implicit none
    type(IXTstring_list),target :: string_list !! it is a target since a pointer will point to a part of it
    character(len=long_len),pointer,optional::list(:)   
    
    if(present(list)) list => string_list%list

  end subroutine IXFget_ptr_string_list

  subroutine replace_string_list(sl,index,item,status)
    implicit none
    type(IXTstring_list) :: sl
    character(len=*),intent(in)::item !!entry which is added to entry(:) array
    type(IXTstatus)::status
    integer :: index
    if (index < 1 .or. index > sl%count) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'list invalid index (IXFreplace_item_string_list)')
       return
    endif
    call grow_list(sl, 0, status)
    sl%list(index) = item
    call IXFcheck(sl, status)
  end subroutine replace_string_list

  subroutine insert_string_list(sl,index,item,status)
    implicit none
    type(IXTstring_list) :: sl
    integer :: n
    type(IXTstatus) :: status
    integer :: index
    character(len=*),intent(in) :: item 
    if (index < 1 .or. index > sl%count) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'list invalid index (IXFinsert_item_string_list)')
       return
    endif
    call grow_list(sl, 1, status)
    n = sl%count
    sl%list(index+1:n+1) = sl%list(index:n)
    sl%list(index) = item
    sl%count = n + 1
    call IXFcheck(sl, status)
  end subroutine insert_string_list
  
  subroutine append_string_list(sl,item,status)
    implicit none
    type(IXTstring_list) :: sl
    character(len=*),intent(in) :: item 
    type(IXTstatus)::status
    integer :: n
    call grow_list(sl, 1, status)
    n = sl%count + 1
    sl%count = n
    sl%list(n) = item 
    call IXFcheck(sl, status)
  end subroutine append_string_list

! make sure there is space for at least n new items in the list
! n can be 0, in which will only lead to an initial allocation
  subroutine grow_list(sl, n, status)
    implicit none
    type(IXTstring_list) :: sl
    integer :: n, new_size
    type(IXTstatus) :: status
    if (.not. allocated(sl%list)) then
        call IXFallocFortran(sl%list, IXCgrow_size, status)
        sl%list = ' '
        sl%count = 0
    endif
    if ( (sl%count + n) > size(sl%list) ) then
        new_size = sl%count + max(n, IXCgrow_size) ! grow by at least grow size
        call IXFreallocDimsFortran(sl%list, (/ new_size /), .true., status)
    endif        
  end subroutine

end module IXMstring_list
