module IXMm_raw_file

#define IXD_TYPE raw_file
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE raw_file
#include "bindings_base.f90"

end module IXMm_raw_file

#define IXD_TYPE raw_file
#include "bindings_extra.f90"

subroutine IXBopen_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  if (status == IXCseverity_error) return
  call IXFopen_raw_handle(rf, status)
  if (status == IXCseverity_error) return
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf, status)
end subroutine

subroutine IXBgeti_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs,n
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  integer, pointer :: value(:)
  integer(i4b)::errcode
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, name, status)
  if (status == IXCseverity_error) return
  call IXFsize_raw(rf, name, n, status)
  call IXFalloc(value, n, status)
  call IXFget_raw(rf, name, value, status,errcode)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, value, status)
end subroutine

subroutine IXBgetr_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs,n
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  real(dp), pointer :: value(:)
  integer(i4b)::errcode
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, name, status)
  if (status == IXCseverity_error) return
  call IXFsize_raw(rf, name, n, status)
  call IXFalloc(value, n, status)  
  call IXFget_raw(rf, name, value, status,errcode)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, value, status)
end subroutine

subroutine IXBgetc_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs,n
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  character(len=256),allocatable :: value(:)
  integer(i4b)::errcode  
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, name, status)
  if (status == IXCseverity_error) return
  call IXFsize_raw(rf, name, n, status)
  allocate(value(n))
  call IXFget_raw(rf, name, value,status, errcode)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, value, status)
end subroutine

subroutine IXBhead_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs,n
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)  
  type(IXTraw_file),allocatable :: rf(:)
  type(IXTstatus) :: status
  n = ixGetNumberOfElements(prhs(1))
  if(n >= 1)then
    allocate(rf(n))
  endif    
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)  
  if (status == IXCseverity_error) return
  call IXFhead_raw_file(rf,status)
end subroutine

subroutine IXBget_generic_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  implicit none
  integer :: nlhs,nrhs,n
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  character(len=256),allocatable :: value_c(:)
  real(dp),pointer::value_r(:)
  integer(i4b),pointer::value_i(:)
  integer(i4b)::index

  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, name, status)
  if (status == IXCseverity_error) return    
  call IXFget_generic(rf,name,value_r,value_i,value_c,index,status)
  if (status == IXCseverity_error)return
  
  select case(index)
    case(1)
      call IXBsendToBinding(plhs(1), ' ', 1, 0, value_r, status)
    case(2)
      call IXBsendToBinding(plhs(1), ' ', 1, 0, value_i, status)
    case(3)
      call IXBsendToBinding(plhs(1), ' ', 1, 0, value_c, status)
  end select
end subroutine IXBget_generic_raw_file

subroutine IXBgetspectrum_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs,period
  integer, pointer :: spec_num(:)
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  type(IXTdataset_1d), allocatable :: value(:)
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, spec_num, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, period, status)
  if (status == IXCseverity_error) return
  allocate(value(size(spec_num)))
  call IXFget_raw(rf, spec_num, value,period, status)
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(4))
  else
    call IXBsendToBinding(plhs(1), prhs(4), ' ', 1, 0,value, status)
  endif
  deallocate(value)
end subroutine

subroutine IXBgetspectra_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs,n
  integer, pointer :: spec_nums(:)
  integer, pointer :: periods(:)
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  type(IXTdataset_2d),allocatable :: d2(:)
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, spec_nums, status)  
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, periods, status)
  allocate(d2(size(periods)))
  if (status == IXCseverity_error) return  
  call IXFget_raw(rf, spec_nums, d2, periods,status)
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(4))
  else
    call IXBsendToBinding(plhs(1), prhs(4), ' ', 1, 0, d2, status)
  endif
end subroutine
  
subroutine IXBsumspec_raw_file(nlhs, plhs, nrhs, prhs, status)
  use IXMm_raw_file
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs,n
  integer, pointer :: spec_nums(:)
  integer :: period
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  character(len=256) :: name
  type(IXTraw_file) :: rf
  type(IXTstatus) :: status
  real(dp)::xmin,xmax
  type(IXTdataset_1d) :: d1
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, rf, status)
  call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, spec_nums, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, xmin, status)  
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, xmax, status)    
  call IXBgetFromBinding(prhs(5), ' ', 1, 0, period, status)
  if (status == IXCseverity_error) return  
  call IXFget_raw(rf, spec_nums, d1, xmin,xmax, period,status)
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(6))
  else
    call IXBsendToBinding(plhs(1), prhs(6), ' ', 1, 0, d1, status)
  endif
      
end subroutine IXBsumspec_raw_file
