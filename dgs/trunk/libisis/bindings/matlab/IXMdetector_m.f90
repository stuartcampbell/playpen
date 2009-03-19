module IXMm_detector

#define IXD_TYPE detector
#include "bindings_header.f90"

contains

#define IXD_TYPE detector
#include "bindings_base.f90"

end module IXMm_detector

#define IXD_TYPE detector
#include "bindings_extra.f90"

subroutine IXBfilecreator_detector(nlhs, plhs, nrhs, prhs, status)
  use IXMm_detector  
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus)::status
  character(len=long_len)::file1,file2,file3,file4,file5
  type(IXTdetector)::det_out
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(1),' ', 1, 0, file1, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, file2, status)
  call IXBgetFromBinding(prhs(3),' ', 1, 0, file3, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, file4, status)
  call IXBgetFromBinding(prhs(5),' ', 1, 0, file5, status)  
  
  if (status == IXCseverity_error) return
  
  if(nlhs == 1)then    
    call IXBgetFromBinding(prhs(6),' ', 1, 0, det_out, status) 
    if (status == IXCseverity_error) return
    call IXFreference_file_create_detector(file1,file2,file3,file4,file5,status,det_out)
    if (status == IXCseverity_error) then
      plhs(1)=ixDuplicateArray(prhs(1))
    else
      call IXBsendToBinding(plhs(1), prhs(6), ' ', 1, 0, det_out, status)
    endif
  else
    call IXFreference_file_create_detector(file1,file2,file3,file4,file5,status)
  endif
end subroutine IXBfilecreator_detector
