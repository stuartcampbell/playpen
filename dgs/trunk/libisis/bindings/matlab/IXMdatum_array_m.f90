module IXMm_datum_array

#define IXD_TYPE datum_array
#include "bindings_header.f90"

contains

#define IXD_TYPE datum_array
#include "bindings_base.f90"

end module IXMm_datum_array

#define IXD_TYPE datum_array
#include "bindings_extra.f90"

subroutine IXBplusXYZdatum_array(nlhs, plhs, nrhs, prhs, status)
  use IXMm_datum_array
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdatum_array) :: wres,w1,w2
  type(IXTstatus) :: status
  real(dp) :: scalar

  !this function checks for the types of argument it has been passed
  !it is assumed that if neither of the arguments are scalar then it must have been passed structures
  !this should have been taken care of by the matlab overloading
  ! if it has been passed a scalar then it calls the overloaded function -> IXFclassOperation

  if(ixIsClass(prhs(2), 'double') .ne. 0)then
     ! scalar is first element 
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, scalar, status)
     ! read datum_array class  
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, status)
     if (status == IXCseverity_error) return

     call IXFplus_datum_array(wres,scalar,w2,status)
  elseif(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, scalar, status)
     ! read datum_array class
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFplus_datum_array(wres,w1,scalar,status)
  else
     !there are no scalars so make the usual read function
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, status)
     if (status == IXCseverity_error) return
     call IXFplus_datum_array(wres,w1,w2,status)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBplusXYZdatum_array


#define IXD_TYPE	datum_array
#define IXD_NAME	Log_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Exp_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Sin_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Cos_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Tan_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Sinh_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Cosh_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Tanh_Datum_array
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Plus_Datum_array
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Minus_Datum_array
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Times_Datum_array
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Divide_Datum_array
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Power_Datum_array
#include "binary_ops.f90"
