module IXMm_data_source

use IXMoperation

#define IXD_TYPE data_source
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE data_source
#include "bindings_base.f90"

end module IXMm_data_source

#define IXD_TYPE data_source
#include "bindings_extra.f90"

subroutine IXBfindpath_data_source(nlhs, plhs, nrhs, prhs, status)
  use IXMm_data_source
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdata_source) :: dso
  logical :: found
  character(len=long_len) :: rpath, dtype,object_name
  call IXBgetFromBinding(prhs(1),' ', 1, 0, dso,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, dtype, status)
  if (status == IXCseverity_error) return
  call IXFfindpath_data_source(dso,dtype,rpath,object_name,found,status)
  call IXBsendToBinding(plhs(1), ' ', 1, 0, rpath, status)
  call IXBsendToBinding(plhs(2), ' ', 1, 0, found, status)
end subroutine IXBfindpath_data_source

subroutine IXBfindpaths_data_source(nlhs, plhs, nrhs, prhs, status)
  use IXMm_data_source
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdata_source) :: dso
  logical :: found
  character(len=long_len) ::  dtype
  character(len=long_len),allocatable::rpaths(:),object_name(:)
  call IXBgetFromBinding(prhs(1),' ', 1, 0, dso,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, dtype, status)
  if (status == IXCseverity_error) return
  call IXFfindpaths_data_source(dso,dtype,rpaths,object_name,found,status)
  call IXBsendToBinding(plhs(2), ' ', 1, 0, found, status)
  if(found)then
    call IXBsendToBinding(plhs(1), ' ', 1, 0, rpaths, status)
  else
    allocate(rpaths(1))
    rpaths(1)=' '
    call IXBsendToBinding(plhs(1), ' ', 1, 0, rpaths, status)
  endif    
end subroutine IXBfindpaths_data_source


subroutine IXBadditem_data_source(nlhs, plhs, nrhs, prhs, status)
  use IXMm_data_source
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdata_source) :: dso
  character(len=long_len) :: path, dtype,object_name
  call IXBgetFromBinding(prhs(1),' ', 1, 0, dso,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, path, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, dtype, status)
  if(nrhs == 4)then
    call IXBgetFromBinding(prhs(4),' ', 1, 0, object_name, status)
  else
    object_name=IXCundef_char
  endif
  if (status == IXCseverity_error) return
  call IXFadditem_data_source(dso,path,dtype,object_name,status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, dso, status)
end subroutine IXBadditem_data_source

subroutine IXBreplaceitem_data_source(nlhs, plhs, nrhs, prhs, status)
  use IXMm_data_source
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdata_source) :: dso
  character(len=long_len) :: path, dtype,object_name
  call IXBgetFromBinding(prhs(1),' ', 1, 0, dso,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, path, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, dtype, status)
  if(nrhs == 4)then
    call IXBgetFromBinding(prhs(4),' ', 1, 0, object_name, status)
  else
    object_name=IXCundef_char
  endif  
  if (status == IXCseverity_error) return
  call IXFreplaceitem_data_source(dso,path,dtype,object_name,status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, dso, status)
end subroutine IXBreplaceitem_data_source


subroutine IXBdelitem_data_source(nlhs, plhs, nrhs, prhs, status)
  use IXMm_data_source
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdata_source) :: dso
  character(len=long_len) :: dtype
  call IXBgetFromBinding(prhs(1),' ', 1, 0, dso,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, dtype, status)
  if (status == IXCseverity_error) return
  call IXFdelitem_data_source(dso,dtype,status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, dso, status)
end subroutine IXBdelitem_data_source
