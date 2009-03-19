module IXMm_translation

#define IXD_TYPE translation
#include "bindings_header.f90"

contains

#define IXD_TYPE translation
#include "bindings_base.f90"

end module IXMm_translation

#define IXD_TYPE translation
#include "bindings_extra.f90"

!----------------------------------------------------------------------------------------------------------------------
! Specific matlab functions
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBplus_translation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTtranslation) :: t1, t2, tres

    call IXBgetFromBinding(prhs(2),' ', 1, 0, t1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, t2, status)
	if (status == IXCseverity_error) return
!	tres = t1 + t2
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, tres, status)

end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBminus_translation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTtranslation) :: t1, t2, tres

    call IXBgetFromBinding(prhs(2),' ', 1, 0, t1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, t2, status)
	if (status == IXCseverity_error) return
!	tres = t1 - t2
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, tres, status)

end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBnorm_translation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTtranslation) :: t1
	real(dp) ans

    call IXBgetFromBinding(prhs(1),' ', 1, 0, t1, status)
	if (status == IXCseverity_error) return
!	ans = IXFnorm (t1)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, ans, status)

end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBdot_translation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTtranslation) :: t1, t2
	real(dp) ans

    call IXBgetFromBinding(prhs(1),' ', 1, 0, t1, status)
    call IXBgetFromBinding(prhs(2),' ', 1, 0, t2, status)
	if (status == IXCseverity_error) return
!	ans = IXFdot (t1, t2)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, ans, status)

end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBcross_translation (nlhs, plhs, nrhs, prhs, status)
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTtranslation) :: t1, t2, tres

    call IXBgetFromBinding(prhs(2),' ', 1, 0, t1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, t2, status)
	if (status == IXCseverity_error) return
!	tres = IXFcross (t1, t2)
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, tres, status)

end subroutine



