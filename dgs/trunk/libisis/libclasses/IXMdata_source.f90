!------------------------------
! MODULE: IXMdata_source
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1386 $ ($Date: 2008-05-20 04:26:17 -0400 (Tue, 20 May 2008) $)
!!
!! FORTRAN definition of IXMdata_source class
module IXMdata_source
  use IXMtools
  use IXMfileio
  use IXMmemory

  implicit none	
  public :: IXTdata_source	
  type IXTdata_source
     character(len=long_len),allocatable::path(:)
     character(len=long_len),allocatable::datatype(:)
     character(len=long_len),allocatable::object_name(:)
     integer(i4b)::counter = 0
  end type IXTdata_source
  
  integer(i4b),parameter,private:: IXCdso_initlength=20 !!initial length of array list elements

  interface IXFfile_read
     module procedure IXFfile_read_data_source
  end interface
  interface IXFfile_write
     module procedure IXFfile_write_data_source
  end interface
  interface IXFcheck
     module procedure IXFcheck_data_source,IXFcheck_array_data_source 
  end interface
  interface IXFcheck_and_valid
     module procedure IXFcheck_and_valid_data_source,IXFcheckarray_and_valid_data_source
  end interface
contains

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_data_source(arg, status)
    implicit none
    type(IXTdata_source) :: arg
    type(IXTstatus) :: status
    
    if( .not. allocated(arg%path))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'path list unallocated (IXFcheck_data_source)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(arg%datatype))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'datatype list unallocated (IXFcheck_data_source)')
    endif    
    if (size (arg%datatype) /= size (arg%path))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'lengths of arrays incompatible (IXFcheck_data_source)')
    endif
    if (size (arg%datatype) /= size (arg%object_name))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'lengths of arrays incompatible (IXFcheck_data_source)')
    endif
    
  end subroutine IXFcheck_data_source

  subroutine IXFcheck_array_data_source(w1,s)
    implicit none
    type(IXTdata_source)::w1(:)
    type(IXTstatus)::s
    integer :: i
    do i = 1, size(w1)
       call IXFcheck(w1(i), s)
    end do
  end subroutine IXFcheck_array_data_source
  subroutine IXFcheck_and_valid_data_source  (arg,s)
    implicit none
    type(IXTdata_source ),intent(in)::arg
    type(IXTstatus)::s
    call IXFcheck(arg,s)
  end subroutine IXFcheck_and_valid_data_source

  subroutine IXFcheckarray_and_valid_data_source  (arg,s)
    implicit none
    type(IXTdata_source ),intent(in)::arg(:)
    type(IXTstatus)::s
    call IXFcheck(arg,s)          
  end subroutine IXFcheckarray_and_valid_data_source    
  
  
  subroutine IXFfile_read_data_source(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTdata_source) :: value
    call IXFwrite_line(' Invalid path: error reading data source from file', status)
  end subroutine IXFfile_read_data_source

  subroutine IXFfile_write_data_source(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTdata_source) :: value
    call IXFwrite_line(' Invalid path : error Writing data_source from file', status)
  end subroutine IXFfile_write_data_source

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_data_source(data_source,path,datatype,object_name, status)
    implicit none
    type(IXTdata_source) :: data_source
    type(IXTstatus) :: status
    character(len=*),allocatable,intent(in)::path(:)
    character(len=*),allocatable,intent(in)::datatype(:)
    character(len=*),allocatable,intent(in)::object_name(:)

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(path))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'path list unallocated (IXFcreate_data_source)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( .not. allocated(datatype))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'datatype list unallocated (IXFcreate_data_source)')
    endif
    if( .not. allocated(object_name))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'object_name list unallocated (IXFcreate_data_source)')
    endif
    
    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled

    call IXFset_data_source(data_source,status,path,datatype,object_name)

  end subroutine IXFcreate_data_source

!! the IXFcheck routine for IXTdata_source is kept in IXM operation as it is a special module which does 
!! NOT contain an IXTbase object


  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_data_source(data_source,status)
    implicit none
    type(IXTdata_source) :: data_source
    type(IXTstatus) :: status
    
    deallocate (data_source%path)
    deallocate (data_source%datatype)
    deallocate (data_source%object_name)
    data_source%counter=0

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines

  end subroutine IXFdestroy_data_source

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_data_source(data_source,status,path,datatype,object_name,ref)
    implicit none
    type(IXTdata_source),intent(inout) :: data_source
    type(IXTdata_source),intent(in),optional:: ref
    character(len=*),optional,intent(in)::path(:)
    character(len=*),optional,intent(in)::datatype(:)
    character(len=*),optional,intent(in)::object_name(:)
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised

    if(present(ref))call IXFset_data_source(data_source,status,ref%path,ref%datatype,ref%object_name)
    
    
! some extra things wrt allocatable arrays of strings    
    
    call IXFcheck_data_source(data_source,status)

  end subroutine IXFset_data_source

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.
! no intent due to allocatable problem
  subroutine IXFget_data_source(data_source, status,path,datatype,object_name,wout)
    implicit none
    type(IXTdata_source),intent(in) :: data_source
    type(IXTdata_source),optional,intent(out)::wout
    type(IXTstatus) :: status
    character(len=*),optional,allocatable::path(:)
    character(len=*),optional,allocatable::datatype(:)
    character(len=*),optional,allocatable::object_name(:)    

    if (present(wout))call IXFset_data_source(wout,status,ref=data_source)

    if(present(path))then
      call IXFreallocdimsfortran(path,(/ size(data_source%path) /),.false.,status)
      path=data_source%path
    endif
    if(present(datatype))then
      call IXFreallocdimsfortran(datatype,(/ size(data_source%datatype) /),.false.,status)
      datatype=data_source%datatype
    endif
    if(present(object_name))then
      call IXFreallocdimsfortran(object_name,(/ size(data_source%object_name) /),.false.,status)
      object_name=data_source%object_name
    endif    
! some extra things wrt allocatable arrays of strings
! the equivalent of reallocfortran for strings 

  end subroutine IXFget_data_source


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_data_source(data_source,path,datatype)
    implicit none
    type(IXTdata_source),target :: data_source !! it is a target since a pointer will point to a part of it
    character(len=long_len),pointer,optional::path(:)
    character(len=long_len),pointer,optional::datatype(:)
! some extra things wrt allocatable arrays of strings    
    
    if(present(path))path=>data_source%path
    if(present(datatype))datatype=>data_source%datatype
    !may need to put a check in if it doesn't return a valid pointer

  end subroutine IXFget_ptr_data_source

  !-----------------------------------------------------------------------------------------------------------------------
  !! findpath_data_source will return the path of a requested datatype from the path(:) array, after matching up with the
  !! datatype(:) array
  
  subroutine IXFfindpath_data_source(dso,dtype,rpath,object_name,found,status,index)
    implicit none
    type(IXTdata_source),intent(in) :: dso
    character(len=*), intent(out) :: rpath,object_name !!return path of requested entry in list
    character(len=*), intent(in) :: dtype !!datatype which is checked against entries in datatype(:) array
    logical, intent(out) :: found
    integer(i4b),optional,intent(out)::index
    type(IXTstatus)::status
    integer(i4b)::i
    character(len=long_len) :: dtype_lower
    found = .false.
    call IXFcheck(dso,status)
    if(status==IXCseverity_error)return
    rpath = ' '
    i=1
    dtype_lower = dtype
    call locase(dtype_lower)
    ! entries in the datatype(:) will be converted to lower case on creation 
    ! trailing and leading blanks will be removed
    do while ( (found .eqv. .false.) .and. (i <=dso%counter) )
    ! this check will be appropriate to a string comparison
      if(trim(adjustl(dtype_lower)) == dso%datatype(i) ) then
        rpath=dso%path(i)
        object_name=dso%object_name(i)
        found = .true.
        if(present(index))index=i
      endif    
      i=i+1
    end do 
    
    if (found .eqv. .false.)then
!      call IXFwrite_line('"'//trim(adjustl(dtype))//'"'//' file not found in data source',status)
       if(present(index))index=0
    endif
    
  end subroutine IXFfindpath_data_source

  !-----------------------------------------------------------------------------------------------------------------------
  !! findpaths_data_source will return the an array of paths for a multiply defined datatype
  
  subroutine IXFfindpaths_data_source(dso,dtype,rpaths,object_name,found,status)
    implicit none
    type(IXTdata_source),intent(in) :: dso
    character(len=*),allocatable :: rpaths(:),object_name(:) !!return path of requested entry in list
    character(len=*), intent(in) :: dtype !!datatype which is checked against entries in datatype(:) array
    logical, intent(out) :: found
    type(IXTstatus)::status
    integer(i4b)::i,count
    character(len=long_len) :: dtype_lower
    found = .false.
    call IXFcheck(dso,status)
    if(status==IXCseverity_error)return
    
    found = .false.    
    i=1
    count=0
    dtype_lower = dtype
    call locase(dtype_lower)
    ! entries in the datatype(:) will be converted to lower case on creation 
    ! trailing and leading blanks will be removed
    do while ( i <= dso%counter )
    ! this check will be appropriate to a string comparison
      if(trim(adjustl(dtype_lower)) == dso%datatype(i) ) then
         count=count+1
      endif    
      i=i+1
    end do 

    if (count == 0)then
      found=.false.
      return
    else
      if(allocated(rpaths))deallocate (rpaths)
      allocate(rpaths(count))
      found=.true.
      if(allocated(object_name))deallocate (object_name)
      allocate(object_name(count))            
    endif
 
    i=1
    count=1
    do while ( i <= dso%counter )
    ! this check will be appropriate to a string comparison
      if(trim(adjustl(dtype_lower)) == dso%datatype(i) ) then
         rpaths(count)=dso%path(i)
         object_name(count)=dso%object_name(i)
         count=count+1
      endif    
      i=i+1
    end do 
        
  end subroutine IXFfindpaths_data_source

  
  subroutine IXFadditem_data_source(dso,path,dtype,object_name,status)
    implicit none
    type(IXTdata_source)::dso
    character(len=*),intent(in)::path !!path of entry to be added to path(:) array
    character(len=*),intent(in)::dtype !!datatype which is added to datatype(:) array
    character(len=*),intent(in)::object_name
    character(len=long_len)::trim_dtype,trim_path,trim_object_name
    type(IXTstatus)::status
    character(len=long_len) :: dtype_lower

    dtype_lower = dtype    
    call locase(dtype_lower)
    trim_dtype=trim(adjustl(dtype_lower))
!    call locase(path)
    trim_path=trim(adjustl(path))
    trim_object_name=trim(adjustl(object_name))

    !initial setting of length
    if (.not. allocated(dso%path)) dso%counter = 0  ! just in case
    if ( dso%counter == 0 ) then
      call IXFallocdimsFortran(dso%path, (/ IXCdso_initlength /), status)
      dso%path = ' '
      call IXFallocdimsFortran(dso%datatype, (/ IXCdso_initlength /), status)
      dso%datatype = ' '
      call IXFallocdimsFortran(dso%object_name, (/ IXCdso_initlength /), status)
      dso%object_name = ' '   
    endif
    
    if ( dso%counter == size(dso%path) )then
    ! the array is full so add some more space to the array list
      call IXFreallocdimsFortran(dso%path,(/ dso%counter+IXCdso_initlength /),.true.,status)
      call IXFreallocdimsFortran(dso%datatype,(/ dso%counter+IXCdso_initlength /),.true.,status)
      call IXFreallocdimsFortran(dso%object_name,(/ dso%counter+IXCdso_initlength /),.true.,status)    
    endif
    
    ! incremnet counter and add new entries to the list
    dso%counter=dso%counter+1
    dso%path(dso%counter)=trim_path
    dso%datatype(dso%counter)=trim_dtype
    dso%object_name(dso%counter)=trim_object_name
    
    call IXFcheck(dso,status)
  
  end subroutine IXFadditem_data_source

  subroutine IXFdelitem_data_source(dso,dtype,status)
    implicit none
    type(IXTdata_source)::dso
    character(len=*),intent(in)::dtype !!dtype to be removed from array
    character(len=long_len)::trim_dtype,dtype_lower
    logical :: found
    integer:: i
    type(IXTstatus)::status
    
    found=.false.
    i=1
    if ( dso%counter == 0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'cannot delete entry from empty dso object (IXFdelitem_data_source)')
       return
    endif

    dtype_lower = dtype    
    call locase(dtype_lower)
    trim_dtype=trim(adjustl(dtype_lower))
  
    do while ( (found .eqv. .false.) .and. (i <=dso%counter) )
       ! this check will be appropriate to a string comparison
       if(trim_dtype == dso%datatype(i) ) then
          ! one could put an occupied flag to false here if we were to use one
          dso%datatype(i)=' '
          dso%object_name(i)=' '
          dso%path(i)='REMOVED'
          found = .true.  
       endif
       i=i+1
    end do

  end subroutine IXFdelitem_data_source
  
  subroutine IXFreplaceitem_data_source(dso,path,dtype,object_name,status)
    implicit none
    type(IXTdata_source)::dso
    character(len=*),intent(in)::path !!path of entry to be added to path(:) array
    character(len=*),intent(in)::dtype !!datatype which is added to datatype(:) array
    character(len=*),intent(in)::object_name !!object_name
    character(len=long_len)::trim_dtype,trim_path,trim_object_name
    type(IXTstatus)::status
    character(len=long_len) :: dummy1,dummy2
    integer(i4b)::index
    logical::found
    
    trim_path=trim(adjustl(path))
    trim_dtype=trim(adjustl(dtype))
    trim_object_name=trim(adjustl(object_name))
    !check to see if item exists, and if it does what entry number -> index
    call IXFfindpath_data_source(dso,dtype,dummy1,dummy2,found,status,index)
    
    if (found) then
    !then we need to replace its path and object_name value only
       dso%path(index)=trim_path
       dso%path(index)=trim_object_name
    else
    ! if doesn't exist then just add it
       call IXFadditem_data_source(dso,trim_path,trim_dtype,trim_object_name,status)       
    endif
    
    call IXFcheck(dso,status)
  
  end subroutine IXFreplaceitem_data_source  
end module IXMdata_source
