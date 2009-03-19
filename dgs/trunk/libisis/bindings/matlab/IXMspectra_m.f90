module IXMm_spectra

#define IXD_TYPE spectra
#include "bindings_header.f90"

contains

#define IXD_TYPE spectra
#include "bindings_base.f90"

end module IXMm_spectra

#define IXD_TYPE spectra
#include "bindings_extra.f90"

subroutine IXBfilecreator_spectra(nlhs, plhs, nrhs, prhs, status)
  use IXMm_spectra  
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus)::status
  character(len=long_len)::file1,file2
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(1),' ', 1, 0, file1, status)
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, file2, status)
  
  if (status == IXCseverity_error) return

  call spectrum_table_main(file1,file2,status)

!  if (status == IXCseverity_error) then
!     plhs(1)=ixDuplicateArray(prhs(1))
!  else
!     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, ires, status)
!  endif
end subroutine IXBfilecreator_spectra
