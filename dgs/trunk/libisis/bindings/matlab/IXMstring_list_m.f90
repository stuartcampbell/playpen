module IXMm_string_list

use IXMoperation

#define IXD_TYPE string_list
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE string_list
#include "bindings_base.f90"

end module IXMm_string_list

#define IXD_TYPE string_list
#include "bindings_extra.f90"

subroutine IXBappend_string_list(nlhs, plhs, nrhs, prhs, status)
  use IXMm_string_list
  implicit none
  integer :: nlhs, nrhs
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTstring_list) :: sl
  character(len=long_len) :: item
  call IXBgetFromBinding(prhs(1),' ', 1, 0, sl, status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, item, status)
  if (status == IXCseverity_error) return
  call IXFappend(sl, item, status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, sl, status)
end subroutine IXBappend_string_list

subroutine IXBinsert_string_list(nlhs, plhs, nrhs, prhs, status)
  use IXMm_string_list
  implicit none
  integer :: nlhs, nrhs
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTstring_list) :: sl
  integer(i4b) :: index
  character(len=long_len) :: item
  call IXBgetFromBinding(prhs(1),' ', 1, 0, sl, status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, index, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, item, status)
  if (status == IXCseverity_error) return
  call IXFinsert(sl, index, item, status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, sl, status)
end subroutine IXBinsert_string_list

subroutine IXBreplace_string_list(nlhs, plhs, nrhs, prhs, status)
  use IXMm_string_list
  implicit none
  integer :: nlhs, nrhs
  integer(i4b) :: index
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTstring_list) :: sl
  character(len=long_len) :: item
  call IXBgetFromBinding(prhs(1),' ', 1, 0, sl, status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, index, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, item, status)
  if (status == IXCseverity_error) return
  call IXFreplace(sl, index, item, status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, sl, status)
end subroutine IXBreplace_string_list
