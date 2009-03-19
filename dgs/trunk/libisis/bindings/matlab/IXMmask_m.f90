module IXMm_mask

#define IXD_TYPE mask
#include "bindings_header.f90"

contains

#define IXD_TYPE mask
#include "bindings_base.f90"

end module IXMm_mask

#define IXD_TYPE mask
#include "bindings_extra.f90"


subroutine IXBfileread_mask(nlhs,plhs,nrhs,prhs,status)
  use IXMm_mask
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTmask) :: mask
  character(len=long_len)::file1
  type(IXTstatus)::status
  integer(i4b):: n

    
  call IXBgetFromBinding(prhs(3),' ',1,0,file1,status)

  call IXBgetFromBinding(prhs(2),' ',1,0,mask,status)
  
  call IXFfileread_mask(mask,file1,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, mask, status)
  endif

  
end subroutine IXBfileread_mask
