module IXMm_testclass

#define IXD_TYPE testclass
#include "bindings_header.f90"

contains

#define IXD_TYPE testclass
#include "bindings_base.f90"

end module IXMm_testclass

#define IXD_TYPE testclass
#include "bindings_extra.f90"

!!! This is a general purpose function for user testing.
!!! It is called from matlab with the syntax:
!!!
!!!   [res1, res2, ...] = testfunc(IXTtestclass, arg1, arg2, arg3, ...)
!!!
  subroutine IXBtestfunc_testclass(nlhs, plhs, nrhs, prhs, status)
    use IXMm_testclass
    use IXMunspike ! Add our module for testing here
    implicit none
    integer :: nlhs, nrhs !! number of matlab arguments
    integer(cpointer_t) :: plhs(nlhs), prhs(nrhs) !! pointers to matlab arguments
    type(IXTstatus) :: status
    character*256 :: message

! define variables we will need for our test function call
! arrays that will share memory with matlab need are declared as pointers
    real(dp), pointer :: x_in(:), y_in(:), e_in(:) ! inputs passed in from matlab
    real(dp), pointer :: y_out(:), e_out(:) ! output variables to be allocated here and returned

! nlhs == 1 always as we return a single "varargout" cell array in plhs(1)
    write(message, '(A,I2,A,I2)') 'IXTtestclass->testfunc() called with nrhs = ',nrhs 
    call IXFwrite_line(message, status)

! matlab arg1, arg2, arg3 ... are in prhs(1), prhs(2), prhs(3), ...
! Use IXBgetFromBindingPtr for pointers, IXBgetFromBinding otherwise
!
! NOTE: prhs() input arguments such as these are strictly READ ONLY 
! and should not be modified; output variables should be allocated from
! within the code using IXFalloc() and then assigned to a plhs() with
! A call tot IXBsendToinding() (There is one excepiton to this READ ONLY 
! rule for the specific case of    a = func(a)  where a is a non class/strcuture
! type; in this case A can be modified in place and returned directly 
! as a plhs()) 

    call IXBgetFromBindingPtr(prhs(1),' ', 1, 0, x_in, status)
    call IXBgetFromBindingPtr(prhs(2),' ', 1, 0, y_in, status)
    call IXBgetFromBindingPtr(prhs(3),' ', 1, 0, e_in, status)

! check read went OK - report errors and return if not
    if (status == IXCseverity_error) return

! now allocate matlab memory for the output result arrays
    call IXFalloc(y_out, size(y_in), status)
    call IXFalloc(e_out, size(e_in), status)

! call our function
    call IXFunspike_1d (status, x_in, y_in, e_in, y_out, e_out)

! return the results; first create a 1 dimensional "varargout" cell array 
! of the correct length
    plhs(1) = ixCreateCellArray(1, (/ 2 /)) ! args are (num_dimensions, dimensions_array)
! now add values to cell array plhs(1)    
    call IXBsendToBinding(plhs(1), ' ', 1, 0, y_out, status)   ! 1 is cell index
    call IXBsendToBinding(plhs(1), ' ', 2, 0, e_out, status)   ! 2 is cell index
  end subroutine
!
  subroutine IXBplus_testclass(nlhs, plhs, nrhs, prhs, status)
    use IXMm_testclass
   	implicit none
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type (IXTtestclass) :: wres, w1, w2
	real(dp),pointer::array(:)
	type(IXTstatus) :: status
	! read in two structures which will be added together
	! wres has been created empty with type statement
    call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, w2, status)
    call IXBgetFromBindingPtr(prhs(4),' ', 1, 0, array, status)
	! check read went OK - report errors and return if not
	if (status == IXCseverity_error) return
	! do adding operation
	call IXFplus_testclass(wres, w1, w2, array,status)
	! wres has now been filled and needs to be entered into the matlab memory
    if (status == IXCseverity_error) then
        plhs(1)=ixDuplicateArray(prhs(1))
    else
        call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
    endif
  end subroutine IXBplus_testclass
!
!!! This is a general purpose function for user testing.
!!! It is called from matlab with the syntax:
!!!
!!!   [res1, res2, ...] = testfunc(IXTtestclass, arg1, arg2, arg3, ...)
!!!
  subroutine IXBtestfaa_testclass(nlhs, plhs, nrhs, prhs, status)
    use IXMm_testclass
    implicit none
    integer :: nlhs, nrhs !! number of matlab arguments
    integer(cpointer_t) :: plhs(nlhs), prhs(nrhs) !! pointers to matlab arguments
    type(IXTstatus) :: status
	type(IXTtestclass) :: w
    character*256 :: message

! nlhs == 1 always as we return a single "varargout" cell array in plhs(1)
    write(message, '(A,I2,A,I2)') 'IXTtestclass->testfaa() called with nrhs = ',nrhs 
    call IXFwrite_line(message, status)

! matlab arg1, arg2, arg3 ... are in prhs(1), prhs(2), prhs(3), ...
! Use IXBgetFromBindingPtr for pointers, IXBgetFromBinding otherwise
!
! NOTE: prhs() input arguments such as these are strictly READ ONLY 
! and should not be modified; output variables should be allocated from
! within the code using IXFalloc() and then assigned to a plhs() with
! A call tot IXBsendToinding() (There is one excepiton to this READ ONLY 
! rule for the specific case of    a = func(a)  where a is a non class/strcuture
! type; in this case A can be modified in place and returned directly 
! as a plhs()) 

    call IXBgetFromBinding(prhs(1),' ', 1, 0, w, status)
    call IXFdisplay(w, status)
! check read went OK - report errors and return if not
    if (status == IXCseverity_error) then
        plhs(1)=ixDuplicateArray(prhs(1))
    else
        call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w, status)
    endif

  end subroutine
