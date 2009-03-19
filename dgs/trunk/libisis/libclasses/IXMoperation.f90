!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMoperation
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1357 $ ($Date: 2008-05-09 10:51:55 -0400 (Fri, 09 May 2008) $)
!!
!! This module implements the IXFoperationRun interface and its sub interfaces
!! such as IXFoperationRunPrint to prints out arrays etc.
!! Note that to matlab x(5) is usually the row vector x(1,5) as all matlab arrays are at least two
!! dimensional - we print it out as x[5]
!!
!	    write(buffer, '('' '',a,'' = '')') name; 
!	     write(buffer, '('' '',10__format)') ( value(j+i),i=1,min(10,size(value)-j) ); 

module IXMoperation
  use IXMio
  use IXMfileio
  use IXMdata_source
  use IXMhistory
  use IXMstring_list
  use IXMoperation_interfaces
  use IXMwrapped_var
  implicit none

  public :: IXToperation, IXFoperationMake, IXFoperation_run, IXFoperation_run_alloc, IXFoperation_run_ptr, IXFoperationCleanup  
  interface IXFoperationStart
    module procedure operationStart_nobase
  end interface
  
  private :: operationStart_nobase
  
  type IXTop_display
     character(len=256) :: name
  end type IXTop_display
  type IXTop_matlabread
     integer(cpointer_t) :: prhs(10)
  end type IXTop_matlabread
  type IXTop_matlabwrite
     integer(cpointer_t) :: plhs(10)
     integer(cpointer_t) :: prhs = 0
  end type IXTop_matlabwrite
  type IXTop_get
     type(IXTwrapped_var), pointer :: var => NULL()
     character(len=256) :: field
  end type IXTop_get
  type IXTop_set
     type(IXTwrapped_var) :: var
     character(len=256) :: field
  end type IXTop_set
  type IXTop_fileread
     type(IXTfileio) :: fio
     character(len=256) :: path
  end type IXTop_fileread
  type IXTop_filewrite
     type(IXTfileio) :: fio
     character(len=256) :: path
  end type IXTop_filewrite
  type IXTop_init
     integer(i4b) :: i
  end type IXTop_init

  ! 10i in nameform will allow for printing of up to 10 dimensional arrays 
  character(len=*), parameter :: nameform = '('' '',A,''['',i,''] = '')'
  character(len=*), parameter :: realform = '('' '',10(G9.3,1X))'
  character(len=*), parameter :: intform = '('' '',10(I5,1X))'
  character(len=*), parameter :: charform = '('' '',(A))'
  
  character(len=*), parameter :: fieldnameformat = '(A,''_'',I5.5)' ! in NeXus file

  type IXToperation
     integer :: level = 0
     integer :: count(10) !! number of  operations done at level
     integer :: array_index(10) !! array offset at that level
     type(IXTop_display), pointer :: display => NULL()
     type(IXTop_matlabread), pointer :: matlabread => NULL()
     type(IXTop_matlabwrite), pointer :: matlabwrite => NULL()
     type(IXTop_set), pointer :: set => NULL()
     type(IXTop_get), pointer :: get => NULL()
     type(IXTop_fileread), pointer :: fileread => NULL()
     type(IXTop_filewrite), pointer :: filewrite => NULL()
     type(IXTop_init), pointer :: init => NULL()
  end type IXToperation


  interface IXFoperationMake
     module procedure makeOperationDisplay, makeOperationMatlabRead, &
          makeOperationMatlabWrite, makeOperationGet, makeOperationSet, &
          makeOperationFileRead, makeOperationFileWrite, makeOperationInit
  end interface

  interface IXFoperation_run
     module procedure runOperationReal, &
          runOperationCharacter, runOperationInteger, &
          runOperationLogical
  end interface

  interface IXFoperation_run
     module procedure runOperation1d, runOperation2d, runOperation3d, runOperation4d
     module procedure runOperation1i, runOperation2i, runOperation3i, runOperation4i
     module procedure runOperation1c
  end interface

  interface IXFoperation_run_ptr
     module procedure runOperationPtr1d, runOperationPtr2d, runOperationPtr3d, runOperationPtr4d
     module procedure runOperationPtr1i, runOperationPtr2i, runOperationPtr3i, runOperationPtr4i
  end interface

  interface IXFoperation_run_alloc
     module procedure runOperationAlloc1d, runOperationAlloc2d, runOperationAlloc3d, runOperationAlloc4d
     module procedure runOperationAlloc1i, runOperationAlloc2i, runOperationAlloc3i, runOperationAlloc4i
     module procedure runOperationAlloc1c
  end interface

  interface IXFoperationPrint
     module procedure runOperationPrint1d, runOperationPrint2d, runOperationPrint3d, runOperationPrint4d
     module procedure runOperationPrint1i, runOperationPrint2i, runOperationPrint3i, runOperationPrint4i
     module procedure runOperationPrint1c, runOperationPrint2c, runOperationPrint3c, runOperationPrint4c
  end interface

  interface IXFoperation_run
     module procedure IXFoperation_run_fileio, IXFoperation_run_array_fileio
     module procedure IXFoperation_run_path, IXFoperation_run_array_path
     module procedure IXFoperation_run_array_data_source,IXFoperation_run_data_source
     module procedure IXFoperation_run_array_history,IXFoperation_run_history
     module procedure IXFoperation_run_string_list, IXFoperation_run_array_string_list
  end interface
  

  interface IXFdisplay
     module procedure IXFdisplay_fileio,IXFdisplay_array_fileio 
     module procedure IXFdisplay_path,IXFdisplay_array_path
     module procedure IXFdisplay_data_source,IXFdisplay_array_data_source
     module procedure IXFdisplay_history,IXFdisplay_array_history
     module procedure IXFdisplay_string_list, IXFdisplay_array_string_list      
  end interface
  

  interface IXFcheck
     module procedure IXFcheck_fileio,IXFcheck_array_fileio 
  end interface
  interface IXFcheck_and_valid
     module procedure IXFcheckarray_and_valid_fileio,IXFcheck_and_valid_fileio 
  end interface
  
  interface IXFfile_read
     module procedure IXFfile_read_path
  end interface
  interface IXFfile_write
     module procedure IXFfile_write_path
  end interface
  


contains


  subroutine IXFfile_read_path(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTpath) :: value
    call IXFwrite_line(' Invalid path: error reading IXTpath from file', status)
  end subroutine IXFfile_read_path

  subroutine IXFfile_write_path(value, fio, name, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    type(IXTpath) :: value
    call IXFwrite_line(' Invalid path : error Writing IXTpath from file', status)
  end subroutine IXFfile_write_path

  pure function IXFfile_op(op) result(r)
    implicit none
    logical :: r
    type(IXToperation), intent(in) :: op
    r = .false.
    if (associated(op%fileread) .or. associated(op%filewrite)) then
       r = .true.
    endif
  end function IXFfile_op

  subroutine initialiseOperation(op)
    implicit none
    type(IXToperation) :: op
    if (associated(op%display)) deallocate(op%display)
    if (associated(op%matlabread)) deallocate(op%matlabread)
    if (associated(op%matlabwrite)) deallocate(op%matlabwrite)
    if (associated(op%set)) deallocate(op%set)
    if (associated(op%get)) deallocate(op%get)
    if (associated(op%fileread)) deallocate(op%fileread)
    if (associated(op%filewrite)) deallocate(op%filewrite)
    if (associated(op%init)) deallocate(op%init)
    op%level = 0
  end subroutine initialiseOperation

  ! interface IXFoperationRun

#define IXD_NAME		1c
#define IXD_TYPE		character(len=*)
#define IXD_DIMS		:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_char
#define IXD_INITIALISE(__val)  __val = IXCundef_char
#include "IXMoperation_routines2.f90"

#define IXD_NAME		1d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	__val = IXCundef_dp
#include "IXMoperation_routines2.f90"

#define IXD_NAME		2d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	__val = IXCundef_dp
#include "IXMoperation_routines2.f90"

#define IXD_NAME		3d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	__val = IXCundef_dp
#include "IXMoperation_routines2.f90"

#define IXD_NAME		4d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	__val = IXCundef_dp
#include "IXMoperation_routines2.f90"

#define IXD_NAME		1i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	__val = IXCundef_i4b
#include "IXMoperation_routines2.f90"

#define IXD_NAME		2i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	__val = IXCundef_i4b
#include "IXMoperation_routines2.f90"

#define IXD_NAME		3i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	__val = IXCundef_i4b
#include "IXMoperation_routines2.f90"

#define IXD_NAME		4i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX	
#define IXD_QUALIFIER	target
#define IXD_CHECK(__val)	.true.
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	__val = IXCundef_i4b
#include "IXMoperation_routines2.f90"

  ! interface IXFoperationRunPtr

#define IXD_NAME		Ptr1d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr2d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr3d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr4d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr1i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr2i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr3i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Ptr4i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX		Ptr
#define IXD_QUALIFIER	pointer
#define IXD_CHECK(__val)		associated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	nullify(__val)
#include "IXMoperation_routines2.f90"

! interface IXFoperationRunAlloc

#define IXD_NAME		Alloc1c
#define IXD_TYPE		character(len=*)
#define IXD_DIMS		:
#define IXD_PREFIX	    Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)	allocated(__val)
#define IXD_UNDEF		IXCundef_char
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc1d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc2d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc3d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc4d
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_dp
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc1i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc2i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc3i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"

#define IXD_NAME		Alloc4i
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:,:,:
#define IXD_PREFIX		Alloc
#define IXD_QUALIFIER	allocatable
#define IXD_CHECK(__val)		allocated(__val)
#define IXD_UNDEF		IXCundef_i4b
#define IXD_INITIALISE(__val)	!allocate(__val(1));deallocate(__val)
#include "IXMoperation_routines2.f90"


  ! interface IXFoperationRunPrint

#define IXD_NAME    c
#define IXD_TYPE    character(len=*)
#define IXD_FORMAT  charform
#define IXD_NUMPRINT    1
#include "IXMoperation_routines.f90"

#define IXD_NAME    d
#define IXD_TYPE    real(dp)
#define IXD_FORMAT  realform
#define IXD_NUMPRINT    10
#include "IXMoperation_routines.f90"

#define IXD_NAME    i
#define IXD_TYPE    integer(i4b)
#define IXD_FORMAT  intform
#define IXD_NUMPRINT    10
#include "IXMoperation_routines.f90"

  subroutine removeBlanks(buffer)
    implicit none
    character(len=*), intent(inout) :: buffer
    character(len=1) :: c
    integer :: i, j
    j = 0
    ! buffer(j:j) = buffer(i:i) gave memcpy source and destination overlap
    ! error with valgrind
    do i=1,len_trim(buffer)
       c = buffer(i:i)
       if (c /= ' ') then
          j = j + 1
          buffer(j:j) = c
       endif
    enddo
    buffer(j+1:) = ' '
  end subroutine removeBlanks

  subroutine makeArrayNameString(buffer, name, dims_array)
    implicit none
    integer :: i, dims_array(:), nl
    character(len=*) :: buffer, name
    buffer = ' '
    nl = len_trim(name)
    ! insert space before name
    write(buffer, '('' '',a,''['')') name(:nl)
    do i = 1,size(dims_array)
       nl = len_trim(buffer)
       if (i < size(dims_array)) then
          write(buffer(nl+1:),'(i9,'','')') dims_array(i)
       else    
          write(buffer(nl+1:),'(i9,'']'')') dims_array(i)
       endif
    enddo
    ! buffer(2:) so leading blank inserted before name is retained
    call removeBlanks(buffer(2:))
  end subroutine makeArrayNameString

  function IXFop_matlabreadMake(prhs) result(r)
    implicit none
    integer(cpointer_t) :: prhs
    type(IXTop_matlabread) :: r
    r%prhs(1) = prhs
  end function IXFop_matlabreadMake

  function IXFop_matlabwriteMake(plhs) result(r)
    implicit none
    integer(cpointer_t) :: plhs
    type(IXTop_matlabwrite) :: r
    r%plhs(1) = plhs
    r%prhs = 0
  end function IXFop_matlabwriteMake

  function IXFop_filereadMake(fio, path) result(r)
    implicit none
    character(len=*) :: path
    type(IXTfileio) :: fio
    type(IXTop_fileread) :: r
    r%path = path
    r%fio = fio
  end function IXFop_filereadMake

  function IXFop_filewriteMake(fio,path) result(r)
    implicit none
    character(len=*) :: path
    type(IXTfileio) :: fio
    type(IXTop_filewrite) :: r
    r%path = path
    r%fio = fio
  end function IXFop_filewriteMake

  function IXFop_getMake(wrapped_var) result(r)
    implicit none
    type(IXTwrapped_var) :: wrapped_var
    type(IXTop_get) :: r
    r%var = wrapped_var
  end function IXFop_getMake

  function IXFop_setMake(wrapped_var) result(r)
    implicit none
    type(IXTwrapped_var) :: wrapped_var
    type(IXTop_set) :: r
    r%var = wrapped_var
  end function IXFop_setMake

  function IXFop_initMake(i) result(r)
    implicit none
    integer :: i
    type(IXTop_init) :: r
    r%i = i
  end function IXFop_initMake

  subroutine IXFoperationArrayInit(op, name, field, n, status)
    implicit none
    type(IXToperation) :: op
    type(IXTstatus) :: status
    logical :: found
    character(len=*) :: field
    character(len=*) :: name
    integer,intent(inout) :: n
    character(len=256) :: t_field, t_name
    integer(cpointer_t) :: marray, IXBcreateBindingFieldIfNeeded, val
    integer :: fnum
    character(len=50) field_names(1)
    external IXBcreateBindingFieldIfNeeded
    if (associated(op%fileread)) then
       n = 0
       if (field /= ' ') then
          t_field = ' '
!          t_name = name
          write(t_field, fieldnameformat) field(1:len_trim(field)),n+1
          call IXBfindGroup(op%fileread%fio, t_field(1:len_trim(t_field)), t_name, found, status)
          do while(found)
             n = n + 1
             t_field = ' '
             write(t_field, fieldnameformat) field(1:len_trim(field)),n+1
             call IXBfindGroup(op%fileread%fio, t_field(1:len_trim(t_field)), t_name, found, status)
          end do
       endif
    endif
    if (associated(op%matlabread)) then
       if (field /= ' ') then
          call IXBgetFieldFromBinding(op%matlabread%prhs(op%level), &
               field, 0, op%array_index(op%level), op%count(op%level), &
               marray, status)
       else
          marray = op%matlabread%prhs(op%level)
       endif
       n = IXBgetNumberOfElements(marray, status) 
    endif
    if (associated(op%matlabwrite)) then
       val = IXBcreateClassArray(name, n, status)
       if (field /= ' ') then
          call IXBsendFieldToBinding(op%matlabwrite%plhs(op%level), field, 0, &
                   op%array_index(op%level), op%count(op%level), val, status)
!       else
!          marray = op%matlabwrite%plhs(op%level)
       endif
!!!!       n = IXBgetNumberOfElements(marray, status) ! should be set on input
    endif
  end subroutine IXFoperationArrayInit

  subroutine operationStart_nobase(op, name, field, status)
    implicit none
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    character(len=*) :: name
    character(len=512) :: buffer
    integer(cpointer_t) :: marray, IXBcreateBindingFieldIfNeeded
    integer :: fnum
    character(len=50) field_names(1)
    external IXBcreateBindingFieldIfNeeded
    if (field /= ' ') then
       op%count(op%level) = op%count(op%level) + 1
    endif
    if (associated(op%display)) then
       if (field /= ' ') then
          write(buffer, '('' '',A,'' = '',A)') field, name
          if (op%level <= 1) call IXFwrite_line_indent(buffer,op%level,'-',status)
          op%level = op%level + 1
       else
          if (op%level <= 1) call IXFwrite_line_indent(name,op%level,' ',status)
       endif
    endif
    if (associated(op%matlabread)) then
       if (field /= ' ') then
          call IXBgetFieldFromBinding(op%matlabread%prhs(op%level), &
               field, 0, op%array_index(op%level), op%count(op%level), &
               marray, status)
          op%level = op%level + 1
          op%matlabread%prhs(op%level) = marray
       endif
    endif
    if (associated(op%matlabwrite)) then
       if (field /= ' ') then
          marray = IXBcreateBindingFieldIfNeeded(op%matlabwrite%plhs(op%level), field, op%array_index(op%level), status)
          op%level = op%level + 1
          op%matlabwrite%plhs(op%level) = marray
       endif
    endif
    if (associated(op%set)) then
       if (field /= ' ') then
          op%level = op%level + 1
       endif
    endif
    if (associated(op%get)) then
       if (field /= ' ') then
          op%level = op%level + 1
       endif
    endif
    if (associated(op%filewrite)) then
       !       if (op%filewrite%file_id == 0)  call IXBfileOpen(op%filewrite%file_name, op%filewrite%file_id)
       if (field /= ' ') then
          op%level = op%level + 1
          !op%array_index(op%level)
          call IXBfileMakeGroup(op%filewrite%fio, field, name)
          call IXBfileOpenGroup(op%filewrite%fio, field, name)
       endif
    endif
    if (associated(op%fileread)) then
       !       if (op%fileread%file_id == 0)  call IXBfileOpen(op%fileread%file_name, op%fileread%file_id)
       if (field /= ' ') then
          op%level = op%level + 1
          call IXBfileOpenGroup(op%fileread%fio, field, name)
       endif
    endif
    if (field /= ' ') then
       op%count(op%level) = 0
       op%array_index(op%level) = 1
    endif
  end subroutine operationStart_nobase

  subroutine IXFoperationFinish(op, field, status)
    implicit none
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    if (associated(op%display)) then
       if (field /= ' ') then
          op%level = op%level - 1
       endif
    endif
    !   if (allocated(op%display)) deallocate(op%display)
    if (associated(op%matlabread)) then
       if (field /= ' ') then
          op%level = op%level - 1
       endif
       !		deallocate(op%matlabread)
    endif
    if (associated(op%matlabwrite)) then
       if (field /= ' ') then
          op%level = op%level - 1
       endif
       !		deallocate(op%matlabread)
    endif
    if (associated(op%set)) then
       if (field /= ' ') then
          op%level = op%level - 1
       endif
       !		deallocate(op%matlabread)
    endif
    if (associated(op%get)) then
       if (field /= ' ') then
          op%level = op%level - 1
       endif
       !		deallocate(op%matlabread)
    endif
    if (associated(op%filewrite)) then
       if (field /= ' ') then
          op%level = op%level - 1
          call IXBfileCloseGroup(op%filewrite%fio, field)
       endif
       !       if (op%level == 1)  call IXBfileClose(op%filewrite%fio)
    endif
    if (associated(op%fileread)) then
       if (field /= ' ') then
          op%level = op%level - 1
          call IXBfileCloseGroup(op%fileread%fio, field)
       endif
       !       if (op%level == 1)  call IXBfileClose(op%fileread%fio)
    endif
    !   if (allocated(op%writematlab)) deallocate(op%writematlab)
    !   if (allocated(op%readmatlabvargin)) deallocate(op%readmatlabvargin)
  end subroutine IXFoperationFinish

  subroutine makeOperationDisplay(op, op_display, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_display) :: op_display
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%display)
    op%display = op_display
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
    !   op%display => op_display
  end subroutine makeOperationDisplay

  subroutine makeOperationSet(op, op_set, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_set) :: op_set
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%set)
    op%set = op_set
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
  end subroutine makeOperationSet

  subroutine makeOperationGet(op, op_get, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_get) :: op_get
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%get)
    op%get = op_get
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
  end subroutine makeOperationGet

  subroutine makeOperationMatlabRead(op, op_matlabread, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_matlabread) :: op_matlabread
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%matlabread)
    op%matlabread = op_matlabread
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
    !   op%display => op_display
  end subroutine makeOperationMatlabRead

  subroutine makeOperationMatlabWrite(op, op_matlabwrite, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_matlabwrite) :: op_matlabwrite
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%matlabwrite)
    op%matlabwrite = op_matlabwrite
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
    !   op%display => op_display
  end subroutine makeOperationMatlabWrite

  subroutine makeOperationFileRead(op, op_fileread, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_fileread) :: op_fileread
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%fileread)
    op%fileread = op_fileread
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
  end subroutine makeOperationFileRead

  subroutine makeOperationFileWrite(op, op_filewrite, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_filewrite) :: op_filewrite
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%filewrite)
    op%filewrite = op_filewrite
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
  end subroutine makeOperationFileWrite

  subroutine makeOperationInit(op, op_init, status)
    implicit none
    type(IXToperation) :: op
    type(IXTop_init) :: op_init
    type(IXTstatus) :: status
    call initialiseOperation(op)
    allocate(op%init)
    op%init = op_init
    op%level = 1
    op%count(op%level) = 0
    op%array_index(op%level) = 1
  end subroutine makeOperationInit

  subroutine runOperationCharacter(op, name, value, status)
    implicit none
    type(IXToperation) :: op
    character(len=*) :: name
    character(len=*) :: value
    character(len=1320) :: buffer
    type(IXTstatus) :: status
    integer str_end, lb
    op%count(op%level) = op%count(op%level) + 1
    if (associated(op%display)) then
       buffer = ' '
       if (len_trim(value) > 1300) then
          str_end=1300
       else
          str_end=len_trim(value)
       endif
       if (str_end == 0) str_end = 1
       write(buffer, '('' '',a,'' = '''''',a,'''''''')') name, value(:str_end)
       lb = str_end + len(name) + 6 ! initial space, two single quotes and ' = '
       if (op%level <= 1) call IXFwrite_line_indent(buffer(:lb),op%level,'-',status)
    endif
    if (associated(op%matlabread)) then
       call IXBgetFromBinding(op%matlabread%prhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%matlabwrite)) then
       call IXBcreateBindingPLHS(op%matlabwrite%plhs(op%level), op%matlabwrite%prhs, op%array_index(op%level), status)
       call IXBsendToBinding(op%matlabwrite%plhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%get)) then
       if (name == op%get%field) then
          call IXFwrap_var(value, op%get%var, status)
       endif
    endif
    if (associated(op%set)) then
       if (name == op%set%field) then
          call IXFunwrap_var(op%set%var, value, status)
       endif
    endif
    if (associated(op%filewrite)) then
       call IXBfileWrite(op%filewrite%fio, name, value, status)
    endif
    if (associated(op%fileread)) then
       call IXBfileRead(op%fileread%fio, name, value, status)
    endif
    if (associated(op%init)) then
       value = IXCundef_char
    endif
  end subroutine runOperationCharacter

  subroutine runOperationReal(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    real(dp) :: value
    character(len=1320) :: buffer
    type(IXTstatus) :: status
    integer lb
    op%count(op%level) = op%count(op%level) + 1
    if (associated(op%display)) then
       buffer = ' '
       write(buffer, '('' '',a,'' = '',g9.3)') name, value
       lb = len_trim(buffer)
       if (op%level <= 1) call IXFwrite_line_indent(buffer(:lb),op%level,'-',status)
    endif
    if (associated(op%matlabread)) then
       call IXBgetFromBinding(op%matlabread%prhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%matlabwrite)) then
       call IXBcreateBindingPLHS(op%matlabwrite%plhs(op%level), op%matlabwrite%prhs,  status)
       call IXBsendToBinding(op%matlabwrite%plhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%get)) then
       if (name == op%get%field) then
          call IXFwrap_var(value, op%get%var, status)
       endif
    endif
    if (associated(op%set)) then
       if (name == op%set%field) then
          call IXFunwrap_var(op%set%var, value, status)
       endif
    endif
    if (associated(op%filewrite)) then
       call IXBfileWrite(op%filewrite%fio, name, value, status)
    endif
    if (associated(op%fileread)) then
       call IXBfileRead(op%fileread%fio, name, value, status)
    endif
    if (associated(op%init)) then
       value = IXCundef_dp
    endif
  end subroutine runOperationReal

  subroutine runOperationLogical(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    logical :: value
    character(len=1320) :: buffer
    type(IXTstatus) :: status
    integer lb
    op%count(op%level) = op%count(op%level) + 1
    if (associated(op%display)) then
       buffer = ' '
       if (value) then
          write(buffer, '('' '',a,'' = true'')') name
       else
          write(buffer, '('' '',a,'' = false'')') name
       endif
       lb = len_trim(buffer)
       if (op%level <= 1) call IXFwrite_line_indent(buffer(:lb),op%level,'-',status)
    endif
    if (associated(op%matlabread)) then
       call IXBgetFromBinding(op%matlabread%prhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%matlabwrite)) then
       call IXBcreateBindingPLHS(op%matlabwrite%plhs(op%level), op%matlabwrite%prhs, status)
       call IXBsendToBinding(op%matlabwrite%plhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%get)) then
       if (name == op%get%field) then
          call IXFwrap_var(value, op%get%var, status)
       endif
    endif
    if (associated(op%set)) then
       if (name == op%set%field) then
          call IXFunwrap_var(op%set%var, value, status)
       endif
    endif
    if (associated(op%filewrite)) then
       call IXBfileWrite(op%filewrite%fio, name, value, status)
    endif
    if (associated(op%fileread)) then
       call IXBfileRead(op%fileread%fio, name, value, status)
    endif
    if (associated(op%init)) then
       value = IXCundef_logical
    endif
  end subroutine runOperationLogical

  subroutine IXFoperationCleanup(op, status)
    type(IXToperation) :: op
    type(IXTstatus) :: status
    call initialiseOperation(op)
  end subroutine

  subroutine runOperationInteger(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    integer(i4b) :: value, lb
    type(IXTstatus) :: status
    character(len=1320) :: buffer
    op%count(op%level) = op%count(op%level) + 1
    if (associated(op%display)) then
       buffer = ' '
       write(buffer, '('' '',a,'' = '',i8)') name, value
       lb = len_trim(buffer)
       if (op%level <= 1) call IXFwrite_line_indent(buffer(:lb),op%level,'-',status)
    endif
    if (associated(op%matlabread)) then
       call IXBgetFromBinding(op%matlabread%prhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%matlabwrite)) then
       call IXBcreateBindingPLHS(op%matlabwrite%plhs(op%level), op%matlabwrite%prhs,  status)
       call IXBsendToBinding(op%matlabwrite%plhs(op%level), name, op%array_index(op%level), &
            op%count(op%level), value, status)
    endif
    if (associated(op%get)) then
       if (name == op%get%field) then
          call IXFwrap_var(value, op%get%var, status)
       endif
    endif
    if (associated(op%set)) then
       if (name == op%set%field) then
          call IXFunwrap_var(op%set%var, value, status)
       endif
    endif
    if (associated(op%filewrite)) then
       call IXBfileWrite(op%filewrite%fio, name, value, status)
    endif
    if (associated(op%fileread)) then
       call IXBfileRead(op%fileread%fio, name, value, status)
    endif
    if (associated(op%init)) then
       value = IXCundef_i4b
    endif
  end subroutine runOperationInteger


    !-----------------------------------------------------------------------------------------------------------------------

  recursive subroutine IXFoperation_run_data_source(op, field, arg, status)
    implicit none
    type(IXTdata_source) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdata_source', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run_alloc(op,'path', arg%path, status)
    call IXFoperation_run_alloc(op,'datatype',arg%datatype,status)
    call IXFoperation_run_alloc(op,'object_name',arg%object_name,status)    
    call IXFoperation_run(op,'counter',arg%counter,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_data_source

  recursive subroutine IXFoperation_run_array_data_source(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXTdata_source) :: value(:)
    type(IXTstatus) :: status
    integer :: i
    !    call IXFoperationStart(op, IXD_DESCRIPTION , field, status)
    do i = 1, size(value)
       op%array_index(op%level) = i
       call IXFoperation_run(op, name, value(i), status)
    enddo
    !    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_array_data_source


  subroutine IXFdisplay_data_source(value, status)
    type(IXTdata_source) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
    call IXFoperationMake(op,IXTop_display('IXTdata_source class'),status)
    call IXFoperation_run(op,' ',value,status)
    call IXFoperationCleanup(op,status)
  end subroutine IXFdisplay_data_source

  subroutine IXFdisplay_array_data_source(w1,s)
    implicit none
    type(IXTdata_source)::w1(:)
    type(IXTstatus)::s
    integer :: i
    if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
    endif
    do i = 1, min(size(w1), 5)
       call IXFdisplay(w1(i), s)
    end do
  end subroutine IXFdisplay_array_data_source



  recursive subroutine IXFoperation_run_fileio(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXTfileio) :: value
    type(IXTstatus) :: status
    call IXFoperationStart(op, 'IXTfileio', name, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'file_name', value%file_name, status)
    call IXFoperation_run(op,'file_id', value%file_id, status)
    call IXFoperation_run(op,'mode', value%mode, status)
    !    call IXFoperation_run(op,'file_id', value%file_id, status)
    call IXFoperationFinish(op, name, status)
  end subroutine IXFoperation_run_fileio


  recursive subroutine IXFoperation_run_array_fileio(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXTfileio) :: value(:)
    type(IXTstatus) :: status
    integer :: i
    !    call IXFoperationStart(op, IXD_DESCRIPTION , field, status)
    do i = 1, size(value)
       op%array_index(op%level) = i
       call IXFoperation_run(op, name, value(i), status)
    enddo
    !    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_array_fileio

  subroutine IXFcheck_fileio(value, status)
    type(IXTfileio) :: value
    type(IXTstatus) :: status
  end subroutine IXFcheck_fileio

  subroutine IXFdisplay_fileio(value, status)
    type(IXTfileio) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
    call IXFoperationMake(op,IXTop_display('IXTfileio class'),status)
    call IXFoperation_run(op,' ',value,status)
    call IXFoperationCleanup(op,status)
  end subroutine IXFdisplay_fileio

  subroutine IXFdisplay_array_fileio(w1,s)
    implicit none
    type(IXTfileio)::w1(:)
    type(IXTstatus)::s
    integer :: i
    if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
    endif
    do i = 1, min(size(w1), 5)
       call IXFdisplay(w1(i), s)
    end do
  end subroutine IXFdisplay_array_fileio

  subroutine IXFcheck_array_fileio(w1,s)
    implicit none
    type(IXTfileio)::w1(:)
    type(IXTstatus)::s
    integer :: i
    do i = 1, size(w1)
       call IXFcheck(w1(i), s)
    end do
  end subroutine IXFcheck_array_fileio
  
  subroutine IXFcheck_and_valid_fileio  (arg,s)
    implicit none
    type(IXTfileio),intent(in)::arg
    type(IXTstatus)::s
    call IXFcheck(arg,s)
  end subroutine IXFcheck_and_valid_fileio

  subroutine IXFcheckarray_and_valid_fileio(arg,s)
    implicit none
    type(IXTfileio),intent(in)::arg(:)
    type(IXTstatus)::s
    call IXFcheck(arg,s)          
  end subroutine IXFcheckarray_and_valid_fileio    

    !-----------------------------------------------------------------------------------------------------------------------

  subroutine IXFdisplay_path(value, status)
    type(IXTpath) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
    call IXFoperationMake(op,IXTop_display('IXTpath class'),status)
    call IXFoperation_run(op,' ',value,status)
    call IXFoperationCleanup(op,status)
  end subroutine IXFdisplay_path

  subroutine IXFdisplay_array_path(w1,s)
    implicit none
    type(IXTpath)::w1(:)
    type(IXTstatus)::s
    integer :: i
    if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
    endif
    do i = 1, min(size(w1), 5)
       call IXFdisplay(w1(i), s)
    end do
  end subroutine IXFdisplay_array_path

  

  recursive subroutine IXFoperation_run_path(op, field, arg, status)
    implicit none
    type(IXTpath) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTpath', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run_alloc(op,'directory',arg%directory,status)
    call IXFoperation_run(op,'counter',arg%counter,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_path
  

  recursive subroutine IXFoperation_run_array_path(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXTpath) :: value(:)
    type(IXTstatus) :: status
    integer :: i
    !    call IXFoperationStart(op, IXD_DESCRIPTION , field, status)
    do i = 1, size(value)
       op%array_index(op%level) = i
       call IXFoperation_run_path(op, name, value(i), status)
    enddo
    !    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_array_path  
    !-----------------------------------------------------------------------------------------------------------------------

  recursive subroutine IXFoperation_run_history(op, field, arg, status)
    implicit none
    type(IXThistory) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXThistory', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run_alloc(op,'entry', arg%entry, status)
    call IXFoperation_run(op,'counter',arg%counter,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_history

  recursive subroutine IXFoperation_run_array_history(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXThistory) :: value(:)
    type(IXTstatus) :: status
    integer :: i
    !    call IXFoperationStart(op, IXD_DESCRIPTION , field, status)
    do i = 1, size(value)
       op%array_index(op%level) = i
       call IXFoperation_run(op, name, value(i), status)
    enddo
    !    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_array_history

  subroutine IXFdisplay_history(value, status)
    type(IXThistory) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
    call IXFoperationMake(op,IXTop_display('IXThistory class'),status)
    call IXFoperation_run(op,' ',value,status)
    call IXFoperationCleanup(op,status)
  end subroutine IXFdisplay_history

  subroutine IXFdisplay_array_history(w1,s)
    implicit none
    type(IXThistory)::w1(:)
    type(IXTstatus)::s
    integer :: i
    if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
    endif
    do i = 1, min(size(w1), 5)
       call IXFdisplay(w1(i), s)
    end do
  end subroutine IXFdisplay_array_history

!----

  recursive subroutine IXFoperation_run_string_list(op, field, arg, status)
    implicit none
    type(IXTstring_list) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTstring_list', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run_alloc(op,'list', arg%list, status)
    call IXFoperation_run(op,'count',arg%count,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_string_list

  recursive subroutine IXFoperation_run_array_string_list(op, name, value, status)
    type(IXToperation) :: op
    character(len=*) :: name
    type(IXTstring_list) :: value(:)
    type(IXTstatus) :: status
    integer :: i
    !    call IXFoperationStart(op, IXD_DESCRIPTION , field, status)
    do i = 1, size(value)
       op%array_index(op%level) = i
       call IXFoperation_run(op, name, value(i), status)
    enddo
    !    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_array_string_list

  subroutine IXFdisplay_string_list(value, status)
    type(IXTstring_list) :: value
    type(IXTstatus) :: status
    type(IXToperation)::op
    call IXFoperationMake(op,IXTop_display('IXTstring_list class'),status)
    call IXFoperation_run(op,' ',value,status)
    call IXFoperationCleanup(op,status)
  end subroutine IXFdisplay_string_list

  subroutine IXFdisplay_array_string_list(w1,s)
    implicit none
    type(IXTstring_list)::w1(:)
    type(IXTstatus)::s
    integer :: i
    if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
    endif
    do i = 1, min(size(w1), 5)
       call IXFdisplay(w1(i), s)
    end do
  end subroutine IXFdisplay_array_string_list

end module IXMoperation
