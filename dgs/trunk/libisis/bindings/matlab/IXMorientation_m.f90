module IXMm_orientation

#define IXD_TYPE orientation
#include "bindings_header.f90"

contains

#define IXD_TYPE orientation
#include "bindings_base.f90"

end module IXMm_orientation

#define IXD_TYPE orientation
#include "bindings_extra.f90"

!----------------------------------------------------------------------------------------------------------------------
! Specific matlab functions
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBsetgen_orientation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_orientation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manual says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTorientation) :: or, or_res
	real(dp) :: rotvec(3)

    if (nrhs == 3) then ! rotation vector
        call IXBgetFromBinding(prhs(2),' ', 1, 0, or, status)
        call IXBgetFromBinding(prhs(3),' ', 1, 0, rotvec, status)
	    if (status == IXCseverity_error) return
!	    call IXFsetgen_orientation (or_res, status, rotvec)
	    if (status == IXCseverity_error) return
        call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, or_res, status)
    else
        call IXFadd_status(status, IXCfacility_bindings, IXCseverity_error, IXCerr_invparam, &
			 & 'Invalid number of arguments to IXBsetgen_orientation')
    endif

end subroutine
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBget_rotvec_orientation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_orientation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manual says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTorientation) :: or
	real(dp) :: rotvec(3)

    call IXBgetFromBinding(prhs(1),' ', 1, 0, or, status)
    if (status == IXCseverity_error) return
!	call IXFget_rotvec_orientation (or, status, rotvec)
    if (status == IXCseverity_error) return
	call IXBsendToBinding(plhs(1), ' ', 1, 0, rotvec, status)

  end subroutine

!----------------------------------------------------------------------------------------------------------------------

subroutine IXBtimes_orientation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_orientation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manual says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTorientation) :: or_1, or_2, or_res

    call IXBgetFromBinding(prhs(2),' ', 1, 0, or_1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, or_2, status)
	if (status == IXCseverity_error) return
!	or_res = or_1 * or_2
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, or_res, status)

end subroutine
