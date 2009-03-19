module IXMm_fermi_chopper

#define IXD_TYPE fermi_chopper
#include "bindings_header.f90"

contains

#define IXD_TYPE fermi_chopper
#include "bindings_base.f90"

end module IXMm_fermi_chopper

#define IXD_TYPE fermi_chopper
#include "bindings_extra.f90"
!----------------------------------------------------------------------------------------------------------------------
! Create the default matlab mex files for LIBISISEXC infrastructure
!----------------------------------------------------------------------------------------------------------------------


!----------------------------------------------------------------------------------------------------------------------
! Specific matlab functions
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBtransmission_fermi_chopper (nlhs, plhs, nrhs, prhs, status)
	use IXMm_fermi_chopper
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTfermi_chopper) :: fermi_chopper
	real(dp) :: transmission_scalar
	real(dp), pointer :: energy(:), transmission(:)		! for use with LIBISISEXC binding routines
! Matlab class determination (if wanted)
 
    call IXBgetFromBinding(prhs(1), ' ', 1, 0, fermi_chopper, status)    ! generated function to get fermi_chopper class
	if (nrhs==1) then	! no energy argument passed in argument list
		transmission_scalar = IXFtransmission_fermi_chopper (fermi_chopper, status)
		call IXBsendToBinding(plhs(1), ' ', 1, 0, transmission_scalar, status)
	else				! energy (scalar or array) passed as an argument
		call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, energy, status)		    ! declare energy as pointer to share fortran and matlab memory
		call IXFalloc (transmission, size(energy), status)				        ! create output pointer
		transmission = IXFtransmission_fermi_chopper (fermi_chopper, energy, status)
	    call IXBsendToBinding(plhs(1), ' ', 1, 0, transmission, status)
	endif

!  this is how you return a class object:
!     call IXBmatlabWriteFermi_chopper(fermi_chopper,plhs(1),prhs(1),status)
end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBtransmission_odd_fermi_chopper (nlhs, plhs, nrhs, prhs, status)
	use IXMm_fermi_chopper
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTfermi_chopper) :: fermi_chopper
	real(dp) :: transmission_scalar
	real(dp), pointer :: energy(:), transmission(:)		! for use with LIBISISEXC binding routines
! Matlab class determination (if wanted)
 
    call IXBgetFromBinding(prhs(1), ' ', 1, 0, fermi_chopper, status)    ! generated function to get fermi_chopper class
	if (nrhs==1) then	! no energy argument passed in argument list
		transmission_scalar = IXFtransmission_odd_fermi_chopper (fermi_chopper, status)
		call IXBsendToBinding(plhs(1), ' ', 1, 0, transmission_scalar, status)
	else				! energy (scalar or array) passed as an argument
		call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, energy, status)		    ! declare energy as pointer to share fortran and matlab memory
		call IXFalloc (transmission, size(energy), status)				        ! create output pointer
		transmission = IXFtransmission_odd_fermi_chopper (fermi_chopper, energy, status)
	    call IXBsendToBinding(plhs(1), ' ', 1, 0, transmission, status)
	endif

!  this is how you return a class object:
!     call IXBmatlabWriteFermi_chopper(fermi_chopper,plhs(1),prhs(1),status)
end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBvariance_fermi_chopper (nlhs, plhs, nrhs, prhs, status)
	use IXMm_fermi_chopper
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTfermi_chopper) :: fermi_chopper
	real(dp) variance_scalar
	real(dp), pointer :: energy(:), variance(:)		! for use with LIBISISEXC binding routines
! Matlab class determination (if wanted)
 
    call IXBgetFromBinding(prhs(1), ' ', 1, 0, fermi_chopper, status)    ! generated function to get fermi_chopper class
	if (nrhs==1) then	! no energy argument passed in argument list
		variance_scalar = IXFvariance_fermi_chopper (fermi_chopper, status)
		call IXBsendToBinding(plhs(1), ' ', 1, 0, variance_scalar, status)
	else				! energy (scalar or array) passed as an argument
		call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, energy, status)		    ! declare energy as pointer to share fortran and matlab memory
		call IXFalloc (variance, size(energy), status)				        ! create output pointer
		variance = IXFvariance_fermi_chopper (fermi_chopper, energy, status)
	    call IXBsendToBinding(plhs(1), ' ', 1, 0, variance, status)
	endif

!  this is how you return a class object:
!     call IXBmatlabWriteFermi_chopper(fermi_chopper,plhs(1),prhs(1),status)
end subroutine

!----------------------------------------------------------------------------------------------------------------------
subroutine IXBvariance_odd_fermi_chopper (nlhs, plhs, nrhs, prhs, status)
	use IXMm_fermi_chopper
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTfermi_chopper) :: fermi_chopper
	real(dp) :: variance_scalar
	real(dp), pointer :: energy(:), variance(:)		! for use with LIBISISEXC binding routines
! Matlab class determination (if wanted)
 
    call IXBgetFromBinding(prhs(1), ' ', 1, 0, fermi_chopper, status)    ! generated function to get fermi_chopper class
	if (nrhs==1) then	! no energy argument passed in argument list
		variance_scalar = IXFvariance_odd_fermi_chopper (fermi_chopper, status)
		call IXBsendToBinding(plhs(1), ' ', 1, 0, variance_scalar, status)
	else				! energy (scalar or array) passed as an argument
		call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, energy, status)		    ! declare energy as pointer to share fortran and matlab memory
		call IXFalloc (variance, size(energy), status)				        ! create output pointer
		variance = IXFvariance_odd_fermi_chopper (fermi_chopper, energy, status)
	    call IXBsendToBinding(plhs(1), ' ', 1, 0, variance, status)
	endif

!  this is how you return a class object:
!     call IXBmatlabWriteFermi_chopper(fermi_chopper,plhs(1),prhs(1),status)
end subroutine
