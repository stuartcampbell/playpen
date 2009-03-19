module IXMm_shape

#define IXD_TYPE shape
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE shape
#include "bindings_base.f90"

end module IXMm_shape

#define IXD_TYPE shape
#include "bindings_extra.f90"


!----------------------------------------------------------------------------------------------------------------------
! Specific matlab functions
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBvolume_shape (nlhs, plhs, nrhs, prhs, status)
	use IXMm_shape
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTshape) :: shape
	real(dp) :: volume

    call IXBgetFromBinding(prhs(1),' ', 1, 0, shape, status)
!	volume = IXFvolume (shape)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, volume, status)

end subroutine


subroutine IXBsolid_angle_shape (nlhs, plhs, nrhs, prhs, status)
	use IXMm_shape
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTshape) :: shape
	real(dp) :: vp(3), omega

    call IXBgetFromBinding(prhs(1),' ', 1, 0, shape, status)
    call IXBgetFromBinding(prhs(2),' ', 1, 0, vp, status)
!	omega = IXFsolid_angle (shape, vp)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, omega, status)

end subroutine
