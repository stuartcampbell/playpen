! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-----------------------------------------------------------------------------------------------------------------------------------
! bindings placeholders/stubs for LIBCORE and LIBCLASSES
!-----------------------------------------------------------------------------------------------------------------------------------
! This file contains the low level IXB* routines that will need to be substituted
! by any external bindings (e.g. MATLAB or PYTHON)
!
! You should not normally need to worry about the contents of this file
!

!! This routine is only used by the IXMlibcore module
!!
!! It writes a line of text to the default standard output device
!!
!! If external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!! @see IXMlibcore
!!
subroutine IXBwrite_line(line,status)
  use IXMtype_definitions
  use IXMstatus
  implicit none
  character(len=*) line !! text to write to the output device (screen)
  type(IXTstatus) :: status
! In theory we should check the IOSTAT of the WRITE and then
! call IXFadd_status() ... however IXFadd_status will probably try to
! write the error to the screen via this function and so we would get stuck in an infinite loop
  write(6,'(A)') line
end subroutine IXBwrite_line

!! This routine is only used by the IXMmemory module
!!
!! It returns a pointer to external memory, the dimensions of which
!! are given by dims_array and the data type given by the string array_type.
!! While this function will not generally be called by the stand-alone FORTRAN
!! library, having it here allows the memory interface to be used for e.g. debugging 
!! or testing of routines that will ultimately be called by a binding layer. 
!!
!! If external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!! @see IXMmemory
!!
function IXBallocArrayDescriptor(ndims, dims_array, array_type) result(external_ptr)
  use IXMtype_definitions
  implicit none
  integer ndims, array_size, i, dims_array(ndims)
  integer(cpointer_t) :: external_ptr, IXIcalloc !! pointer to external array descriptor
  external IXIcalloc !! C language memory allocator (libcore/utils.c)
  character(len=*) :: array_type !! 'double', 'int32' etc.
  array_size = 1
  do i=1,ndims
    array_size = array_size * dims_array(i)
  enddo
  external_ptr = IXIcalloc(array_size*8) ! assume 'double' for the moment
end function IXBallocArrayDescriptor

!! This routine is only used by the IXMmemory module
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!! @see IXMmemory
!!
subroutine IXBdeallocArrayDescriptor(external_ptr)
  use IXMtype_definitions
  implicit none
  integer IXIcdealloc, stat
  integer(cpointer_t) :: external_ptr !! pointer to external array descriptor
  external IXIcdealloc !! C language memory deallocator (libcore/utils.c)
  if (external_ptr /= 0) stat = IXIcdealloc(external_ptr)
  external_ptr = 0
end subroutine IXBdeallocArrayDescriptor

!! This routine is only used by the IXMmemory module
!!
!! It return a pointer to array data given an external pointer to the array descriptor
!! While this function will not generally be called by the stand-alone FORTRAN
!! library, having it here allows the memory interface to be used for e.g. debugging 
!! or testing of routines that will ultimately be called by a binding layer. 
!!
!! If external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!! @see IXMmemory
!!
function IXBgetArrayData(external_ptr) result(array_ptr)
  use IXMtype_definitions
  implicit none
  integer(cpointer_t) :: external_ptr !! pointer to array descriptor
  integer(cpointer_t) :: array_ptr	!! pointer to array data
  array_ptr = external_ptr ! if we have used the allocators in this file, then these are the same
end function IXBGetArrayData


!! This routine is only used by the IXMmemory module 
!!
!! It creates a result version of an external object that has been held on the memory
!! stack. With some bindings you cannot return an object that was passed into fortran by the
!! bindings layer back to the original program as a result.
!! While this function will not generally be called by the stand-alone FORTRAN
!! library, having it here allows the memory interface to be used for e.g. debugging 
!! or testing of routines that will ultimately be called by a binding layer. 
!!
!! If external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!! @see IXMmemory
!!
function IXBexternalMakeResult(external_ptr, fortran_alloc) result(result_ptr)
  use IXMtype_definitions
  use IXMstatus
  implicit none
  integer(cpointer_t) :: external_ptr !! pointer to external object memory
!! true if the external memory was allocated in FORTRAN as opposed
!! to being passed in from outside by the bindings layer
  logical fortran_alloc 
  integer(cpointer_t) :: result_ptr
  type(IXTstatus) :: status
  result_ptr = external_ptr ! if we have used the allocators in this file, then nothing special is needed
end function IXBexternalMakeResult

!! This routine is purely a placeholder and is not used in a stand-alone FORTRAN program
!!
!! When external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!!
subroutine IXBcreateBindingPLHS(plhs,prhs,s)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer(cpointer_t)::prhs;
  integer(cpointer_t),target::plhs
  type(IXTstatus)::s
  call IXFwrite_line('IXBcreateBindingPLHS called in error',s)
  return
end subroutine

!! This routine is purely a placeholder and is not used in a stand-alone FORTRAN program
!!
!! When external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!!
function IXBcreateBindingFieldIfNeeded(plhs, field, array_index, s) result(array_ptr)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer array_index
  integer(cpointer_t) :: plhs, array_ptr
  character(len=*) :: field
  type(IXTstatus)::s
  array_ptr = 0
  call IXFwrite_line('IXBcreateBindingFieldIfNeeded called in error',s)
end function

!! This routine is purely a placeholder and is not used in a stand-alone FORTRAN program
!!
!! When external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 692 $ ($Date: 2006-05-08 18:44:57 -0400 (Mon, 08 May 2006) $)
!!
subroutine IXBgetFieldFromBinding(prhs, field, field_num, array_index, op_count, array_ptr, status)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer(cpointer_t) :: prhs, array_ptr
  character(len=*) :: field
  integer field_num, array_index, op_count
  type(IXTstatus)::status
  array_ptr = 0
  call IXFwrite_line('IXBgetFieldFromBinding called in error',status)
end subroutine

subroutine IXBsendFieldToBinding(plhs, field, field_num, array_index, op_count, marray, status)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer(cpointer_t) :: plhs, marray
  character(len=*) :: field
  integer field_num, array_index, op_count
  type(IXTstatus)::status
  call IXFwrite_line('IXBsendFieldToBinding called in error',status)
end subroutine

function IXBgetNumberOfElements(external_ptr, status) result(n)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer(cpointer_t) :: external_ptr
  type(IXTstatus)::status
  integer :: n
  n = 0
  call IXFwrite_line('IXBgetNumberOfElements called in error',status)
end function

function IXBcreateClassArray(class_name, n, s) result(marray)
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  implicit none
  integer(cpointer_t) :: marray
  character(len=*) :: class_name
  type(IXTstatus)::s
  integer :: n
  marray = 0
  call IXFwrite_line('IXBcreateClassArray called in error',s)
end function

! this needs to be tidied up
!
! for IXMoperation_interfaces


! interface IXBgetFromBinding
 
#define IXD_NAME    Char
#define IXD_TYPE    character(len=*)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i4b
#define IXD_TYPE    integer(i4b)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp
#define IXD_TYPE    real(dp)
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Logical
#define IXD_TYPE    logical
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp1
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp2
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp3
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    dp4
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i1
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i2
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i3
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    i4
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    c1
#define IXD_TYPE    character(len=*)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

! interface IXBgetFromBindingPtr

#define IXD_NAME    Ptrdp1
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp2
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp3
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptrdp4
#define IXD_TYPE    real(dp),pointer
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri1
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri2
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri3
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Ptri4
#define IXD_TYPE    integer(i4b),pointer
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"


! interface IXBgetFromBindingAlloc

#define IXD_NAME    Allocdp1
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp2
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp3
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocdp4
#define IXD_TYPE    real(dp),allocatable
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci1
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci2
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci3
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Alloci4
#define IXD_TYPE    integer(i4b),allocatable
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_get.f90"

#define IXD_NAME    Allocc1
#define IXD_TYPE    character(len=*),allocatable
#define IXD_DIMS    :
#include "IXMoperation_interfaces_get.f90"

! interface IXBsendToBinding

#define IXD_NAME    Char
#define IXD_TYPE    character(len=*)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i4b
#define IXD_TYPE    integer(i4b)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp
#define IXD_TYPE    real(dp)
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    Logical
#define IXD_TYPE    logical
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp1
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp2
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp3
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    dp4
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i1
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i2
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i3
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    i4
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:,:
#include "IXMoperation_interfaces_send.f90"

#define IXD_NAME    c1
#define IXD_TYPE    character(len=*)
#define IXD_DIMS    :
#include "IXMoperation_interfaces_send.f90"

