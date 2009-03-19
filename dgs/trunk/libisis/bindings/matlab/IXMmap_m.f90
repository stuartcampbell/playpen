module IXMm_map

#define IXD_TYPE map
#include "bindings_header.f90"

contains

#define IXD_TYPE map
#include "bindings_base.f90"

end module IXMm_map

#define IXD_TYPE map
#include "bindings_extra.f90"

subroutine IXBfileread_map(nlhs,plhs,nrhs,prhs,status)
  use IXMm_map
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTmap) :: map
  character(len=long_len)::file1
  type(IXTstatus)::status
  integer(i4b):: n

    
  call IXBgetFromBinding(prhs(3),' ',1,0,file1,status)

  call IXBgetFromBinding(prhs(2),' ',1,0,map,status)
  
  call IXFfileread_map(map,file1,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, map, status)
  endif

  
end subroutine IXBfileread_map
