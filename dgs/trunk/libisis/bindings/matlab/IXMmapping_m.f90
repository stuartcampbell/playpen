module IXMm_mapping

#define IXD_TYPE mapping
#include "bindings_header.f90"

contains

#define IXD_TYPE mapping
#include "bindings_base.f90"

end module IXMm_mapping

#define IXD_TYPE mapping
#include "bindings_extra.f90"

subroutine IXBfileread_mapping(nlhs,plhs,nrhs,prhs,status)
  use IXMm_mapping
  implicit none
  integer::nlhs,nrhs
  integer(cpointer_t)::plhs(nlhs),prhs(nrhs)
  type(IXTmapping) :: mapping
  character(len=short_len)::file1
  type(IXTstatus)::status
  integer(i4b):: n

    
  call IXBgetFromBinding(prhs(3),' ',1,0,file1,status)

  call IXBgetFromBinding(prhs(2),' ',1,0,mapping,status)
  
  call IXFfileread_mapping(mapping,file1,status)
  
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, mapping, status)
  endif

  
end subroutine IXBfileread_mapping
