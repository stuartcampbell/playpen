module IXMm_dataset_1d

#define IXD_TYPE dataset_1d
#include "bindings_header.f90"

contains

#define IXD_TYPE dataset_1d
#include "bindings_base.f90"

end module IXMm_dataset_1d

#define IXD_TYPE dataset_1d
#include "bindings_extra.f90"

subroutine IXBintegrate_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_datum
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdatum) :: ires
  type(IXTdataset_1d) :: w1
  real(dp) :: x1, x2
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, x1, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, x2, status)
  if (status == IXCseverity_error) return

  call IXFintegrate_dataset_1d(ires,w1,x1,x2,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, ires, status)
  endif
end subroutine IXBintegrate_dataset_1d

subroutine IXBrebunch_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: w1, wres
  integer(i4b)::nbins !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, nbins, status)

  if (status == IXCseverity_error) return

  call IXFrebunch_dataset_1d(wres,w1,nbins,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBrebunch_dataset_1d

subroutine IXBcreatexye_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: w1
  real(dp),pointer::x(:),signal(:),error(:) 
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, w1, status)
  call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, x, status)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, signal, status)
  call IXBgetFromBindingPtr(prhs(4), ' ', 1, 0, error, status)

  if (status == IXCseverity_error) return

  call IXFcreatexye_dataset_1d(w1,x,signal,error,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, status)
  endif
end subroutine  IXBcreatexye_dataset_1d


subroutine IXBshift_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: w1, wres
  real(dp)::shift 
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, shift, status)

  if (status == IXCseverity_error) return

  call IXFshift_dataset_1d(wres,w1,status,shift)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBshift_dataset_1d



subroutine IXBrebin_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_1d) :: wres,w1,wref
  type(IXTstatus) :: status
  real(dp),pointer :: rebin_params(:)

  !this function checks for the types of argument it has been passed
  !it is assumed that if neither of the arguments are scalar then it must have been passed structures
  !this should have been taken care of by the matlab overloading


  if(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,rebin_params, status)
     ! read dataset_1d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFrebin_dataset_1d(wres,status,w1,Xdesc=rebin_params)
  else
     !there are no scalars so make the usual read function
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     call IXBgetFromBinding(prhs(3),' ', 1, 0, wref, status)
     if (status == IXCseverity_error) return
     call IXFrebin_dataset_1d(wres,status,w1,Xref=wref)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBrebin_dataset_1d

subroutine IXBmake_label_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: d1d
  character(len=long_len),allocatable :: x_label(:), s_label(:)
  call IXBgetFromBinding(prhs(1),' ', 1, 0, d1d,status)
  if (status == IXCseverity_error) return
  call IXFmake_label_dataset_1d(d1d,x_label,s_label,status)
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), ' ', 1, 0, x_label, status)
    call IXBsendToBinding(plhs(2), ' ', 1, 0, s_label, status)
  endif    
end subroutine IXBmake_label_dataset_1d

subroutine IXBunspike_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: w1, wres
  real(dp)::ymin,ymax,fac,sfac !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, ymin, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, ymax, status)
  call IXBgetFromBinding(prhs(5), ' ', 1, 0, fac, status)
  call IXBgetFromBinding(prhs(6), ' ', 1, 0, sfac, status)

  if (status == IXCseverity_error) return

  call IXFunspike_dataset_1d(wres,w1,ymin,ymax,fac,sfac,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBunspike_dataset_1d

subroutine IXBregroup_dataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_1d) :: wres,w1
  type(IXTstatus) :: status
  real(dp),pointer :: params(:)


     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, params, status)
     ! read dataset_1d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFregroup_dataset_1d(wres,w1,params,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBregroup_dataset_1d


#define IXD_TYPE	dataset_1d
#define IXD_NAME	Log_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Log10_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Exp_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Sin_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Cos_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Tan_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Sinh_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Cosh_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Tanh_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Plus_Dataset_1d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Minus_Dataset_1d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Times_Dataset_1d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Divide_Dataset_1d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Power_Dataset_1d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Deriv1_Dataset_1d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_1d
#define IXD_NAME	Deriv2_Dataset_1d
#include "unary_ops.f90"


!! and 1-D array operations (IXFarray_operation_dataset_1d)
#define IXD_AANAME	times_dataset_1d
#define IXD_TYPE    dataset_1d
#define IXD_DIM     :
#include "binary_ops.f90"

#define IXD_AANAME	divide_dataset_1d
#define IXD_TYPE    dataset_1d
#define IXD_DIM     :
#include "binary_ops.f90"

#define IXD_AANAME	plus_dataset_1d
#define IXD_TYPE    dataset_1d
#define IXD_DIM     :
#include "binary_ops.f90"

#define IXD_AANAME	minus_dataset_1d
#define IXD_TYPE    dataset_1d
#define IXD_DIM     :
#include "binary_ops.f90"

#define IXD_AANAME	power_dataset_1d
#define IXD_TYPE    dataset_1d
#define IXD_DIM     :
#include "binary_ops.f90"

subroutine IXBplusXYZDataset_1d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_1d) :: wres,w1,w2
  type(IXTstatus) :: status
  real(dp) :: scalar

  !this function checks for the types of argument it has been passed
  !it is assumed that if neither of the arguments are scalar then it must have been passed structures
  !this should have been taken care of by the matlab overloading
  ! if it has been passed a scalar then it calls the overloaded function -> IXFclassOperation

  if(ixIsClass(prhs(2), 'double') .ne. 0)then
     ! scalar is first element 
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, scalar, status)
     ! read dataset_1d class  
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, status)
     if (status == IXCseverity_error) return

     call IXFplus_dataset_1d(wres,scalar,w2,status)
  elseif(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, scalar, status)
     ! read dataset_1d class
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFPlus_dataset_1d(wres,w1,scalar,status)
  else
     !there are no scalars so make the usual read function
     call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, status)
     if (status == IXCseverity_error) return
     call IXFPlus_dataset_1d(wres,w1,w2,status)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBplusXYZdataset_1d









