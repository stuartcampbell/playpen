module IXMm_history

use IXMoperation

#define IXD_TYPE history
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE history
#include "bindings_base.f90"

end module IXMm_history

#define IXD_TYPE history
#include "bindings_extra.f90"


subroutine IXBadditem_history(nlhs, plhs, nrhs, prhs, status)
  use IXMm_history
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXThistory) :: hist
  character(len=long_len) ::command
  call IXBgetFromBinding(prhs(1),' ', 1, 0, hist,status)
  call IXBgetFromBinding(prhs(2),' ', 1, 0, command, status)
  if (status == IXCseverity_error) return
  call IXFadditem_history(hist,command,status)
  call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0,hist, status)
end subroutine IXBadditem_history
