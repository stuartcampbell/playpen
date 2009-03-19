!------------------------------
! MODULE: IXMhistory
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1386 $ ($Date: 2008-05-20 04:26:17 -0400 (Tue, 20 May 2008) $)
!!
!! FORTRAN definition of IXMhistory class
module IXMhistory
  use IXMtools
  use IXMfileio
  use IXMmemory

  implicit none	
  public :: IXThistory	
  type IXThistory
     character(len=long_len),allocatable::entry(:)
     integer(i4b)::counter = 0
  end type IXThistory
  
  integer(i4b),parameter,private:: IXChist_initlength=5 !!initial length of array list elements

  interface IXFfile_read
     module procedure IXFfile_read_history
  end interface
  interface IXFfile_write
     module procedure IXFfile_write_history
  end interface
  interface IXFcheck
     module procedure IXFcheck_history,IXFcheck_array_history 
  end interface
  interface IXFcheck_and_valid
     module procedure IXFcheck_and_valid_history,IXFcheckarray_and_valid_history
  end interface
contains

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_history(arg, status)
    implicit none
    type(IXThistory) :: arg
    type(IXTstatus) :: status
    
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(arg%entry))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'entry list unallocated (IXFcheck_history)')
    endif    

    
  end subroutine IXFcheck_history

  subroutine IXFcheck_array_history(w1,s)
    implicit none
    type(IXThistory)::w1(:)
    type(IXTstatus)::s
    integer :: i
    do i = 1, size(w1)
       call IXFcheck(w1(i), s)
    end do
  end subroutine IXFcheck_array_history
  
  subroutine IXFcheck_and_valid_history  (arg,s)
    implicit none
    type(IXThistory ),intent(in)::arg
    type(IXTstatus)::s
    call IXFcheck(arg,s)
  end subroutine IXFcheck_and_valid_history

  subroutine IXFcheckarray_and_valid_history  (arg,s)
    implicit none
    type(IXThistory ),intent(in)::arg(:)
    type(IXTstatus)::s   
    call IXFcheck(arg,s)          
  end subroutine IXFcheckarray_and_valid_history

  subroutine IXFfile_read_history(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXThistory) :: value
    call IXFwrite_line(' Invalid path: error reading data source from file', status)
  end subroutine IXFfile_read_history

  subroutine IXFfile_write_history(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXThistory) :: value
    call IXFwrite_line(' Invalid path : error Writing history from file', status)
  end subroutine IXFfile_write_history

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_history(history,entry, status)
    implicit none
    type(IXThistory) :: history
    type(IXTstatus) :: status
    character(len=*),allocatable,intent(in)::entry(:)

    ! nested objects should be tested for initialisation, this shows they have been created properly   

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(entry))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'entry list unallocated (IXFcreate_history)')
    endif
    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled

    call IXFset_history(history,status,entry)

  end subroutine IXFcreate_history

!! the IXFcheck routine for IXThistory is kept in IXM operation as it is a special module which does 
!! NOT contain an IXTbase object


  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_history(history,status)
    implicit none
    type(IXThistory) :: history
    type(IXTstatus) :: status
    
    deallocate (history%entry)
    history%counter=0

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines

  end subroutine IXFdestroy_history

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_history(history,status,entry,ref)
    implicit none
    type(IXThistory),intent(inout) :: history
    type(IXThistory),intent(in),optional:: ref
    character(len=*),optional,intent(in)::entry(:)
    type(IXTstatus),intent(inout) :: status

    !no base object - specialised set subroutine

    if(present(ref))call IXFset_history(history,status,ref%entry)

    if (present(entry))then   
       call IXFreallocFortran(history%entry,size(entry),.false.,status) 
       history%counter=size(entry)
       history%entry=entry
    endif
    
    
! some extra things wrt allocatable arrays of strings    
    
    call IXFcheck_history(history,status)

  end subroutine IXFset_history

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_history(history, status,entry,wout)
    implicit none
    type(IXThistory),intent(in) :: history
    type(IXThistory),optional,intent(out)::wout
    type(IXTstatus) :: status
    character(len=*),optional,allocatable,intent(out)::entry(:)

    if (present(wout))call IXFset_history(wout,status,ref=history)

    if(present(entry))then
      call IXFreallocdimsfortran(entry,(/ size(history%entry) /),.false.,status)
      entry=history%entry
    endif
    
! some extra things wrt allocatable arrays of strings
! the equivalent of reallocfortran for strings 

  end subroutine IXFget_history


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_history(history,entry)
    implicit none
    type(IXThistory),target :: history !! it is a target since a pointer will point to a part of it
    character(len=long_len),pointer,optional::entry(:)
! some extra things wrt allocatable arrays of strings    
    
    if(present(entry))entry=>history%entry
    !may need to put a check in if it doesn't return a valid pointer

  end subroutine IXFget_ptr_history

  subroutine IXFadditem_history(hist,entry,status)
    implicit none
    type(IXThistory)::hist
    character(len=*),intent(in)::entry !!entry which is added to entry(:) array
    type(IXTstatus)::status

    !initial setting of length
    if (.not. allocated(hist%entry)) hist%counter = 0  ! just in case
    if ( hist%counter == 0 ) then
      call IXFallocdimsFortran(hist%entry, (/ IXChist_initlength /), status)
      hist%entry = ' '
    endif
    
    if ( hist%counter == size(hist%entry) )then
    ! the array is full so add some more space to the array list
      call IXFreallocdimsFortran(hist%entry,(/ hist%counter+IXChist_initlength /),.true.,status)
    endif
    
    ! increment counter and add new entries to the list
    hist%counter=hist%counter+1
    hist%entry(hist%counter)=trim(adjustl(entry))
    
    call IXFcheck(hist,status)
  
  end subroutine IXFadditem_history
  

end module IXMhistory
