! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-----------------------------------------------------------------------------------------------------------------------------------
! bindings placeholders/stubs for MATLAB
! DO NOT use mxCopyInteger4ToPtr etc. here - they claim only to work with sparse matrices
!-----------------------------------------------------------------------------------------------------------------------------------
! This file contains the low level IXB* routines that will to be substituted
! for those provided in LIBCORE / LIBCLASSES
!


subroutine IXBdofortrancall(func, nlhs, plhs, nrhs, prhs, errcode)
	use IXMtype_definitions
    use IXMstatus
	use IXMlibcore
!    use IXMmatlab_interface
   	implicit none
	integer :: nlhs, nrhs, errcode
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
    interface 
	  subroutine func(nlhs, plhs, nrhs, prhs, status)
       use IXMstatus
	   use IXMtype_definitions
	   implicit none
	   integer :: nlhs, nrhs
	   integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	   type(IXTstatus) :: status
	  end subroutine
	end interface
	type(IXTstatus) :: status
	! initialse status and set the current subroutine name
	call IXFlibrary_init()
!	call IXFstatus_init('domatlabcall', status)
!	call IXBmatlab_init()
	! the work
	call func(nlhs, plhs, nrhs, prhs, status)
    ! flush out any informational messages and clean up status
	if (status == IXCseverity_error) then
	    errcode = 1
	else
		errcode = 0
	endif 
!	call IXBmatlab_cleanup()
	call IXFlibrary_finish(status)
end subroutine

