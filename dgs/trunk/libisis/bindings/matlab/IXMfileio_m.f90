module IXMm_fileio

use IXMoperation

#define IXD_TYPE fileio
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE fileio
#include "bindings_base.f90"

end module IXMm_fileio

#define IXD_TYPE fileio
#include "bindings_extra.f90"

subroutine IXBopen_fileio(nlhs, plhs, nrhs, prhs, status)
    use IXMm_fileio
	implicit none    
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
	character(len=long_len) :: file_name
	integer mode
! internal declarations
	type(IXTfileio) :: fio
    call IXBgetFromBinding(prhs(1),' ', 1, 0, fio, status)
    call IXBgetFromBinding(prhs(2),' ', 1, 0, file_name, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, mode, status)
    call IXFfile_open(fio, file_name, mode, status)
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, fio, status)
end subroutine

subroutine IXBclose_fileio(nlhs, plhs, nrhs, prhs, status)
    use IXMm_fileio
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
	character(len=long_len) :: file_name
! internal declarations
	type(IXTfileio) :: fio
    call IXBgetFromBinding(prhs(1),' ', 1, 0, fio, status)
    call IXFfile_close(fio, status)
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, fio, status)
end subroutine

