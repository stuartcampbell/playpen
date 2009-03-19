module IXMm_dataset_2d

#define IXD_TYPE dataset_2d
#include "bindings_header.f90"

contains

#define IXD_TYPE dataset_2d
#include "bindings_base.f90"

end module IXMm_dataset_2d

#define IXD_TYPE dataset_2d
#include "bindings_extra.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Log_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Log10_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Exp_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Sin_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Cos_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Tan_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Sinh_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Cosh_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Tanh_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Deriv1x_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Deriv2x_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Deriv1y_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Deriv2y_Dataset_2d
#include "unary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Plus_Dataset_2d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Minus_Dataset_2d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Times_Dataset_2d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Divide_Dataset_2d
#include "binary_ops.f90"

#define IXD_TYPE	dataset_2d
#define IXD_NAME	Power_Dataset_2d
#include "binary_ops.f90"



!! array operations on dataset_2d
!! 1-D array operations (IXFarray_X_operation_dataset_2d)
#define IXD_ANAME	times_dataset_2d
#define IXD_TYPE    dataset_2d
#include "binary_ops.f90"

#define IXD_ANAME	divide_dataset_2d
#define IXD_TYPE    dataset_2d
#include "binary_ops.f90"

#define IXD_ANAME	plus_dataset_2d
#define IXD_TYPE    dataset_2d
#include "binary_ops.f90"

#define IXD_ANAME	minus_dataset_2d
#define IXD_TYPE    dataset_2d
#include "binary_ops.f90"

#define IXD_ANAME	power_dataset_2d
#define IXD_TYPE    dataset_2d
#include "binary_ops.f90"

!! and 2-D array operations (IXFarray_operation_dataset_2d)
#define IXD_AANAME	times_dataset_2d
#define IXD_TYPE    dataset_2d
#define IXD_DIM     :,:
#include "binary_ops.f90"

#define IXD_AANAME	divide_dataset_2d
#define IXD_TYPE    dataset_2d
#define IXD_DIM     :,:
#include "binary_ops.f90"

#define IXD_AANAME	plus_dataset_2d
#define IXD_TYPE    dataset_2d
#define IXD_DIM     :,:
#include "binary_ops.f90"

#define IXD_AANAME	minus_dataset_2d
#define IXD_TYPE    dataset_2d
#define IXD_DIM     :,:
#include "binary_ops.f90"

#define IXD_AANAME	power_dataset_2d
#define IXD_TYPE    dataset_2d
#define IXD_DIM     :,:
#include "binary_ops.f90"
! dataset_2d and dataset_1d operations
! IXFdataset_1d_{XY}_{operation}_dataset_2d
#define IXD_F1NAME	_X_times_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_X_divide_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_X_plus_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_X_minus_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_X_power_dataset_2d
#include "binaryd1d.f90"


#define IXD_F1NAME	_Y_times_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_Y_divide_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_Y_plus_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_Y_minus_dataset_2d
#include "binaryd1d.f90"

#define IXD_F1NAME	_Y_power_dataset_2d
#include "binaryd1d.f90"

!!  array of dataset_1d operations (IXFoperation_dataset_2d)
#define IXD_F2NAME	Plus_Dataset_2d
#include "binaryd1d.f90"

#define IXD_F2NAME	Minus_Dataset_2d
#include "binaryd1d.f90"

#define IXD_F2NAME	Times_Dataset_2d
#include "binaryd1d.f90"

#define IXD_F2NAME	Divide_Dataset_2d
#include "binaryd1d.f90"

#define IXD_F2NAME	Power_Dataset_2d
#include "binaryd1d.f90"

subroutine IXBcreatexyze_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1
  real(dp),pointer::x(:),y(:),signal(:,:),error(:,:) 
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(1), ' ', 1, 0, w1, status)
  call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, x, status)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, y, status)
  call IXBgetFromBindingPtr(prhs(4), ' ', 1, 0, signal, status)
  call IXBgetFromBindingPtr(prhs(5), ' ', 1, 0, error, status)

  if (status == IXCseverity_error) return

  call IXFcreatexyze_dataset_2d(w1,x,y,signal,error,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, status)
  endif
end subroutine  IXBcreatexyze_dataset_2d

subroutine IXBintegrate_x_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: wres
  type(IXTdataset_2d) :: w1
  real(dp) :: x1, x2
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, x1, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, x2, status)
  if (status == IXCseverity_error) return

  call IXFintegrate_x_dataset_2d(wres,w1,x1,x2,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBintegrate_x_dataset_2d

subroutine IXBexpand_arrayd1d_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d),allocatable :: arrayd1d(:)
  type(IXTdataset_2d) :: d2d
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, d2d, status)
  if (status == IXCseverity_error) return

  call IXFexpand_arrayd1d_dataset_2d(d2d,status,arrayd1d)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, arrayd1d, status)
  endif
end subroutine IXBexpand_arrayd1d_dataset_2d

subroutine IXBexpand_arrayd2d_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d),allocatable :: arrayd2d(:)
  type(IXTdataset_2d) :: d2d
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, d2d, status)
  if (status == IXCseverity_error) return

  call IXFexpand_arrayd2d_dataset_2d(d2d,status,arrayd2d)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, arrayd2d, status)
  endif
end subroutine IXBexpand_arrayd2d_dataset_2d

subroutine IXBexpand_listd1d_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  integer(i4b),pointer::list(:)
  type(IXTstatus) :: status
  type(IXTdataset_1d),allocatable :: arrayd1d(:)
  type(IXTdataset_2d) :: d2d
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, d2d, status)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, list, status)

  if (status == IXCseverity_error) return

  call IXFexpand_arrayd1d_dataset_2d(d2d,status,arrayd1d,list)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, arrayd1d, status)
  endif
end subroutine IXBexpand_listd1d_dataset_2d

subroutine IXBexpand_listd2d_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  integer(i4b),pointer::list(:)
  type(IXTstatus) :: status
  type(IXTdataset_2d),allocatable :: arrayd2d(:)
  type(IXTdataset_2d) :: d2d
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, d2d, status)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, list, status)
  if (status == IXCseverity_error) return

  call IXFexpand_arrayd2d_dataset_2d(d2d,status,arrayd2d,list)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, arrayd2d, status)
  endif
end subroutine IXBexpand_listd2d_dataset_2d

subroutine IXBcontract_arrayd2d_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d),allocatable :: arrayd2d(:)
  type(IXTdataset_2d) :: d2d
  integer(i4b)::n
  ! read in parameters from Matlab

  n = ixGetNumberOfElements(prhs(2))
  if(n >= 1)then
    allocate(arrayd2d(n))
  endif
  if (status == IXCseverity_error) return

  call IXBgetFromBinding(prhs(2), ' ', 1, 0, arrayd2d, status)
  if (status == IXCseverity_error) return

  call IXFcontract_arrayd2d_dataset_2d(arrayd2d,d2d,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, d2d, status)
  endif
end subroutine IXBcontract_arrayd2d_dataset_2d

subroutine IXBintegrate_y_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_1d
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_1d) :: wres
  type(IXTdataset_2d) :: w1
  real(dp) :: y1, y2
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, y1, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, y2, status)
  if (status == IXCseverity_error) return

  call IXFintegrate_y_dataset_2d(wres,w1,y1,y2,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBintegrate_y_dataset_2d

subroutine IXBintegrate_xy_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  use IXMm_datum
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdatum) :: ires
  type(IXTdataset_2d) :: w2d
  real(dp) :: x1,x2,y1,y2
  ! read in parameters from Matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w2d, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, x1, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, x2, status)
  call IXBgetFromBinding(prhs(5), ' ', 1, 0, y1, status)
  call IXBgetFromBinding(prhs(6), ' ', 1, 0, y2, status)
  if (status == IXCseverity_error) return

  call IXFintegrate_xy_dataset_2d(ires,w2d,x1,x2,y1,y2,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, ires, status)
  endif
end subroutine IXBintegrate_xy_dataset_2d


subroutine IXBshift_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer(i4b) :: nlhs,nrhs,iflag
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1, wres
  real(dp)::shift1,shift2 
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)

  ! get first shift variable, is it an integer
  iflag=0
  if(ixIsClass(prhs(3), 'double').eq. 0)iflag=1

  ! is there a y variable
  if (nrhs > 3) then
     ! either y only or xy
     !get y
     call IXBgetFromBinding(prhs(4), ' ', 1, 0, shift2, status)

     if(iflag == 1 )then
        ! must be y shift only
        call IXFshift_dataset_2d(wres,w1,status,shift_y=shift2)
     else
        ! must be xy
        call IXBgetFromBinding(prhs(3), ' ', 1, 0, shift1, status)
        call IXFshift_dataset_2d(wres,w1,status,shift1,shift2)
     endif

  else   
     ! x only
     call IXBgetFromBinding(prhs(3), ' ', 1, 0, shift1, status)
     call IXFshift_dataset_2d(wres,w1,status,shift_x=shift1)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBshift_dataset_2d


subroutine IXBrebunch_x_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1, wres
  integer(i4b)::nbins !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, nbins, status)

  if (status == IXCseverity_error) return

  call IXFrebunch_x_dataset_2d(wres,w1,nbins,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBrebunch_x_dataset_2d

subroutine IXBunspike_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1, wres
  real(dp)::ymin,ymax,fac,sfac !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, ymin, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, ymax, status)
  call IXBgetFromBinding(prhs(5), ' ', 1, 0, fac, status)
  call IXBgetFromBinding(prhs(6), ' ', 1, 0, sfac, status)
  if (status == IXCseverity_error) return

  call IXFunspike_dataset_2d(wres,w1,ymin,ymax,fac,sfac,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBunspike_dataset_2d


subroutine IXBrebunch_y_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1, wres
  integer(i4b)::nbins !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, nbins, status)

  if (status == IXCseverity_error) return

  call IXFrebunch_y_dataset_2d(wres,w1,nbins,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBrebunch_y_dataset_2d


subroutine IXBrebunch_xy_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: w1, wres
  integer(i4b)::xbins,ybins !the number of bins each to group together
  ! read in parameters from matlab
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0, xbins, status)
  call IXBgetFromBinding(prhs(4), ' ', 1, 0, ybins, status)
  
  if (status == IXCseverity_error) return

  call IXFrebunch_xy_dataset_2d(wres,w1,xbins,ybins,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine  IXBrebunch_xy_dataset_2d


subroutine IXBrebin_x_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1,wref
  type(IXTstatus) :: status
  real(dp),pointer :: rebin_params(:)

  !this function checks for the types of argument it has been passed
  !it is assumed that if neither of the arguments are scalar then it must have been passed structures
  !this should have been taken care of by the matlab overloading
  ! if it has been passed a scalar then it calls the overloaded function -> IXFdataset_1dRebin


  if(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,rebin_params, status)
     ! read dataset_2d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFrebin_x_dataset_2d(wres,status,w1,Xdesc=rebin_params)
  else
     !there are no scalars so make the usual read function
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     call IXBgetFromBinding(prhs(3),' ', 1, 0, wref, status)
     if (status == IXCseverity_error) return
     call IXFrebin_x_dataset_2d(wres,status,w1,Xref=wref)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBrebin_x_dataset_2d

subroutine IXBrebin_y_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1,wref
  type(IXTstatus) :: status
  real(dp),pointer :: rebin_params(:)

  !this function checks for the types of argument it has been passed
  !it is assumed that if neither of the arguments are scalar then it must have been passed structures
  !this should have been taken care of by the matlab overloading
  ! if it has been passed a scalar then it calls the overloaded function -> IXFdataset_1dRebin


  if(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,rebin_params, status)
     ! read dataset_2d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFrebin_y_dataset_2d(wres,status,w1,Ydesc=rebin_params)
  else
     !there are no scalars so make the usual read function
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     call IXBgetFromBinding(prhs(3),' ', 1, 0, wref, status)
     if (status == IXCseverity_error) return
     call IXFrebin_y_dataset_2d(wres,status,w1,Yref=wref)
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBrebin_y_dataset_2d


subroutine IXBrebin_xy_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1,xref,yref
  type(IXTstatus) :: status
  real(dp),pointer :: Xparams(:),Yparams(:)

  ! read dataset_2d class
  call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
  if (status == IXCseverity_error) return

  ! this function checks for the types of argument it has been passed in sequence
  ! it is assumed that if the arguments are not scalar then it must have been passed a structure
  ! this should have been taken care of by the matlab overloading

! sorting if blocks to determine permutation of descriptors (XYdesc) and reference (XYref) workspaces

  if(ixIsClass(prhs(3), 'double') .ne. 0)then ! xdesc present
     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,Xparams, status)
     if(ixIsClass(prhs(4), 'double') .ne. 0)then !ydesc present
        call IXBgetFromBindingPtr(prhs(4), ' ', 1, 0,Yparams, status)
        if (status == IXCseverity_error) return
        call IXFrebin_xy_dataset_2d(wres,status,w1,Xdesc=Xparams,Ydesc=Yparams)
     else !yref must be present
        call IXBgetFromBinding(prhs(4), ' ', 1, 0,yref, status)
        if (status == IXCseverity_error) return
        call IXFrebin_xy_dataset_2d(wres,status,w1,Xdesc=Xparams,Yref=yref)
     endif
  else ! xref is present
     call IXBgetFromBinding(prhs(3), ' ', 1, 0,xref, status)
     if(ixIsClass(prhs(4), 'double') .ne. 0)then !ydesc present
        call IXBgetFromBindingPtr(prhs(4), ' ', 1, 0,Yparams, status)
        if (status == IXCseverity_error) return
        call IXFrebin_xy_dataset_2d(wres,status,w1,Xref=xref,Ydesc=Yparams)
     else !yref present
        call IXBgetFromBinding(prhs(4), ' ', 1, 0,yref, status)
        if (status == IXCseverity_error) return
        call IXFrebin_xy_dataset_2d(wres,status,w1,Xref=xref,Yref=yref)
     endif
   endif
        

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBrebin_xy_dataset_2d


subroutine IXBregroup_x_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1
  type(IXTstatus) :: status
  real(dp),pointer :: params(:)


     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, params, status)
     ! read dataset_2d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFregroup_x_dataset_2d(wres,w1,params,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBregroup_x_dataset_2d

subroutine IXBregroup_y_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1
  type(IXTstatus) :: status
  real(dp),pointer :: params(:)


     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, params, status)
     ! read dataset_2d class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
     if (status == IXCseverity_error) return

     call IXFregroup_y_dataset_2d(wres,w1,params,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBregroup_y_dataset_2d

subroutine IXBregroup_xy_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: wres,w1
  type(IXTstatus) :: status
  real(dp),pointer :: xparams(:),yparams(:)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, xparams, status)
  call IXBgetFromBindingPtr(prhs(4), ' ', 1, 0, yparams, status)
     ! read dataset_2d class
  call IXBgetFromBinding(prhs(2),' ', 1, 0, w1, status)
  if (status == IXCseverity_error) return

  call IXFregroup_xy_dataset_2d(wres,w1,xparams,yparams,status)

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, status)
  endif
end subroutine IXBregroup_xy_dataset_2d

subroutine IXBmake_label_dataset_2d(nlhs, plhs, nrhs, prhs, status)
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTdataset_2d) :: d2d
  character(len=long_len),allocatable :: x_label(:), y_label(:),s_label(:)
  call IXBgetFromBinding(prhs(1),' ', 1, 0, d2d,status)
  if (status == IXCseverity_error) return
  call IXFmake_label_dataset_2d(d2d,x_label,y_label,s_label,status)
  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), ' ', 1, 0, x_label, status)
    call IXBsendToBinding(plhs(2), ' ', 1, 0, y_label, status)
    call IXBsendToBinding(plhs(3), ' ', 1, 0, s_label, status)
  endif
    
end subroutine IXBmake_label_dataset_2d


