module IXMm_axis

#define IXD_TYPE axis
#include "bindings_header.f90"

contains

#define IXD_TYPE axis
#include "bindings_base.f90"

end module IXMm_axis

#define IXD_TYPE axis
#include "bindings_extra.f90"

subroutine IXBmake_label_1d_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: x_axis,s_axis
  logical :: x_dist
  character(len=long_len),allocatable :: x_label(:), s_label(:)
  call IXBgetFromBinding(prhs(1),' ', 1, 0, x_axis,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, s_axis, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, x_dist, status)
  if (status == IXCseverity_error) return
  call IXFmake_label_axis(x_axis,s_axis,x_dist,x_label,s_label,status)  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), ' ', 1, 0, x_label, status)
    call IXBsendToBinding(plhs(2), ' ', 1, 0, s_label, status)
  endif
  end subroutine IXBmake_label_1d_axis

subroutine IXBmake_label_2d_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: x_axis,y_axis,s_axis
  logical :: x_dist,y_dist
  character(len=long_len),allocatable :: x_label(:), y_label(:),s_label(:)
  call IXBgetFromBinding(prhs(1),' ', 1, 0, x_axis,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, y_axis,status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, s_axis, status)
  call IXBgetFromBinding(prhs(4),' ', 1, 0, x_dist, status)
  call IXBgetFromBinding(prhs(5),' ', 1, 0, y_dist, status)
  if (status == IXCseverity_error) return
  call IXFmake_label_axis(x_axis,y_axis,s_axis,x_dist,y_dist,x_label,y_label,s_label,status)  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), ' ', 1, 0, x_label, status)
    call IXBsendToBinding(plhs(2), ' ', 1, 0, y_label, status)
    call IXBsendToBinding(plhs(3), ' ', 1, 0, s_label, status)
  endif
  end subroutine IXBmake_label_2d_axis

subroutine IXBcreate_code_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: axis
  character(len=5)::code
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, axis, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, code, status)

  if (status == IXCseverity_error) return

  call IXFcreate_code_axis(axis,code,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, axis, status)
  endif
end subroutine  IXBcreate_code_axis

subroutine IXBcreate_caption_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: axis
  character(len=long_len),allocatable::caption(:)
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, axis, status)
  call IXBgetFromBindingalloc(prhs(2), ' ', 1, 0, caption, status)

  if (status == IXCseverity_error) return

  call IXFcreate_caption_axis(axis,caption,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, axis, status)
  endif
end subroutine  IXBcreate_caption_axis


subroutine IXBcreate_caption_units_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: axis
  character(len=long_len),allocatable::u_caption(:)
  character(len=long_len)::u_units
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, axis, status)
  call IXBgetFromBindingalloc(prhs(2), ' ', 1, 0, u_caption, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, u_units, status)

  if (status == IXCseverity_error) return

  call IXFcreate_caption_units_axis(axis,u_caption,u_units,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, axis, status)
  endif
end subroutine  IXBcreate_caption_units_axis

subroutine IXBcreate_caption_units_code_axis(nlhs, plhs, nrhs, prhs, status)
  use IXMm_axis
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTaxis) :: axis
  character(len=long_len),allocatable::u_caption(:)
  character(len=long_len)::u_units
  character(len=5)::code
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, axis, status)
  call IXBgetFromBindingalloc(prhs(2), ' ', 1, 0, u_caption, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, u_units, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, code, status)
  ! code can be standard or user_defined

  if (status == IXCseverity_error) return

  call IXFcreate_axis(axis,u_caption,u_units,code,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, axis, status)
  endif
end subroutine  IXBcreate_caption_units_code_axis