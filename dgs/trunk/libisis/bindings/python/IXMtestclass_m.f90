#define IXD_TYPE testclass
#include "class_base.f90"

! matlab calling
  subroutine IXBplus_Testclass(nlhs, plhs, nrhs, prhs, status)
    use IXMm_testclass
	use IXMbinding_interface
   	implicit none
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type (IXTtestclass) :: wres, w1, w2
	type(IXTstatus) :: status
	! read in two structures which will be added together
	! wres has been created empty with type statement
    call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, w2, status)
	! check read went OK - report errors and return if not
	if (status == IXCseverity_error) return
	! do adding operation
	call IXFtestclassPlus(wres, w1, w2, status)
	! wres has now been filled and needs to be entered into the matlab memory
    if (status == IXCseverity_error) then
        plhs(1)=IXBduplicateObject(prhs(1))
    else
        call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
    endif
  end subroutine
