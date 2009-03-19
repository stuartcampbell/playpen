! IXD_NAME IXD_DIMS

#if defined(IXD_NAME) && defined(IXD_DIMS)

!! subroutine to check IXD_NAME arrays before core array manipulation routines
subroutine arrayCheck_&/**/
                      &IXD_NAME (wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status)
  implicit none
  real(dp),intent(in)::wres_v( IXD_DIMS )  !! output value array
  real(dp),intent(in):: wres_e( IXD_DIMS ) !! output error array
  real(dp),intent(in)::w1_v( IXD_DIMS ) !! input value array
  real(dp),intent(in):: w1_e(IXD_DIMS ) !! input error array
  real(dp),intent(in),optional::w2_v( IXD_DIMS ) !! input value array
  real(dp),intent(in),optional:: w2_e(IXD_DIMS ) !! input error array
  type(IXTstatus),intent(inout)::status !! error status object
  if(sum(abs(shape(wres_v) - shape(wres_e)))/=0 )then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of output arrays passed incompatible for array operation')
  endif

  if(sum(abs(shape(w1_v) - shape(w1_e)))/=0 )then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays (w1) passed incompatible for array operation')
  endif
  if(sum(abs(shape(wres_v) - shape(w1_v)))/=0 )then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
  endif
  if (present(w2_v) )then
     if(sum(abs(shape(w1_v) - shape(w2_v)))/=0 )then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
			IXCerr_invparam, 'lengths of input arrays (w1,w2) passed incompatible for array operation')
     endif
     if(present(w2_e))then     
       if(sum(abs(shape(w2_v) - shape(w2_e)))/=0 )then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		  	  IXCerr_invparam, 'lengths of input arrays (w2) incompatible for array operation')
       endif
     endif
  endif
end subroutine 

!! subroutine to add two IXD_NAME arrays
subroutine arrayPlusDD_&/**/
                       &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array

  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v + arg2_v
  wres_e = sqrt(arg1_e**2+arg2_e**2)
end subroutine 

!! subroutine to subtract a IXD_NAME array from another IXD_NAME array
subroutine arrayMinusDD_&/**/
                        &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v - arg2_v
  wres_e = sqrt(arg1_e**2+arg2_e**2)
end subroutine 

!! subroutine to multiply a IXD_NAME array by another IXD_NAME array
subroutine arrayTimesDD_&/**/
                        &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v * arg2_v
  wres_e = wres_v*sqrt((arg1_e/arg1_v)**2+(arg2_e/arg2_v)**2)
end subroutine 

!! subroutine to divide a IXD_NAME array by another IXD_NAME array
subroutine arrayDivideDD_&/**/
                         &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v / arg2_v
  wres_e = wres_v*sqrt((arg1_e/arg1_v)**2+(arg2_e/arg2_v)**2)
end subroutine 

!! subroutine to raise a IXD_NAME array to the power of another IXD_NAME array
subroutine arrayPowerDD_&/**/
                        &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v ** arg2_v
  wres_e = sqrt((arg2_v*(arg1_v**((arg2_v)-1))*arg1_e)**2+(wres_v*log(arg2_v)*arg2_e)**2)
end subroutine 

!****************
!****************

!! subroutine to add a IXD_NAME array to a scalar 
subroutine arrayPlusDS_&/**/
                       &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v + arg2
  wres_e=arg1_e
end subroutine 

!! subroutine to subtract a scalar from a IXD_NAME array
subroutine arrayMinusDS_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v - arg2
  wres_e = arg1_e
end subroutine 


!! subroutine to multiply a IXD_NAME by a scalar
subroutine arrayTimesDS_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v * arg2
  wres_e=arg1_e *arg2
end subroutine 

!! subroutine to divide a IXD_NAME array by a scalar
subroutine arrayDivideDS_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v / arg2
  wres_e=arg1_e / arg2
end subroutine 

!! subroutine to raise a IXD_NAME array to the power of a scalar
subroutine arrayPowerDS_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v = arg1_v ** arg2
  wres_e=arg2*(arg1_v**(arg2-1))*arg1_e
end subroutine 

!****************
!****************

!! subroutine to add a scalar to a IXD_NAME array
subroutine arrayPlusSD_&/**/
                       &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=arg1+arg2_v
  wres_e=arg2_e
end subroutine 

!! subroutine to subtract a IXD_NAME from a scalar
subroutine arrayMinusSD_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=arg1-arg2_v
  wres_e=arg2_e
end subroutine 


!! subroutine to multiply a scalar by a IXD_NAME array
subroutine arrayTimesSD_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=arg1*arg2_v
  wres_e=arg1*arg2_e
end subroutine 


!! subroutine to divide a scalar by a IXD_NAME array
subroutine arrayDivideSD_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=arg1/arg2_v
  wres_e=wres_v*arg2_e/arg2_v
end subroutine 

!! subroutine to raise a scalar to the power of a IXD_NAME array
subroutine arrayPowerSD_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1 !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=arg1 ** arg2_v
  wres_e=wres_v*log(arg2_v)*arg2_e
end subroutine 

!****************
!****************

!! subroutine to take the exponential of a IXD_NAME array
subroutine arrayExp_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=exp(w1_v)
  wres_e=wres_v*w1_e
end subroutine 

!! subroutine to take the natural logarithm of a IXD_NAME array
subroutine arrayLog_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=log(w1_v)
  wres_e = w1_e / w1_v
end subroutine 

!! subroutine to take the logarithm base 10 of a IXD_NAME array
subroutine arrayLog10_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=log10(w1_v)
  wres_e = w1_e * (1.0d0/log(10.0))* (1.0d0/w1_v)
end subroutine 

!! subroutine to take the sine of a IXD_NAME array
subroutine arraySin_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
 type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=sin(w1_v)
  wres_e = cos(w1_v)*w1_e
end subroutine 

!! subroutine to take the cosine of a IXD_NAME array
subroutine arrayCos_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=cos(w1_v)
  wres_e = sin(w1_v) * w1_e
end subroutine 

!! subroutine to take the tangent of a IXD_NAME array
subroutine arrayTan_&/**/
                    &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=tan(w1_v)
  wres_e = w1_e / ( cos(w1_v)**2)
end subroutine 

!! subroutine to take the hyperbolic sine of a IXD_NAME array
subroutine arraySinh_&/**/
                     &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=sinh(w1_v)
  wres_e = cosh(w1_v) * w1_e
end subroutine 

!! subroutine to take the hyperbolic cosine of a IXD_NAME array
subroutine arrayCosh_&/**/
                     &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=cosh(w1_v)
  wres_e = sinh(w1_v) * w1_e
end subroutine 

!! subroutine to take the hyperbolic tangent of a IXD_NAME array
subroutine arrayTanh_&/**/
                     &IXD_NAME (wres_v,wres_e, w1_v,w1_e,status)
  implicit none
  real(dp),intent(in) :: w1_v( IXD_DIMS )!!input value array
  real(dp),intent(in) :: w1_e( IXD_DIMS )!!input error array
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,w1_v,w1_e,status=status)
  if(status == IXCseverity_error)  return
  wres_v=tanh(w1_v)
  wres_e = w1_e / ( cosh(w1_v)**2)
end subroutine 

!the array operations
!*******************************************************************************
!! subroutine to add a IXD_NAME array to a 2d array 
subroutine arrayPlusDA_&/**/
                       &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(sum(abs(shape(arg1_v)-shape(arg2)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif
  if(status == IXCseverity_error)  return
  wres_v = arg1_v + arg2
  wres_e=arg1_e
end subroutine 

!! subroutine to subtract a 2d array from a IXD_NAME array
subroutine arrayMinusDA_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2( IXD_DIMS )!!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(sum(abs(shape(arg1_v)-shape(arg2)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v = arg1_v - arg2
  wres_e = arg1_e
end subroutine 


!! subroutine to multiply a IXD_NAME by a 2d array
subroutine arrayTimesDA_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2( IXD_DIMS )!!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(sum(abs(shape(arg1_v)-shape(arg2)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif
  if(status == IXCseverity_error)  return
  wres_v = arg1_v * arg2
  wres_e=arg1_e *arg2
end subroutine 

!! subroutine to divide a IXD_NAME array by a 2d array
subroutine arrayDivideDA_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(sum(abs(shape(arg1_v)-shape(arg2)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v = arg1_v / arg2
  wres_e=arg1_e / arg2
end subroutine 

!! subroutine to power a IXD_NAME array by a 2d array
subroutine arrayPowerDA_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
  implicit none
  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg2( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
  if(sum(abs(shape(arg1_v)-shape(arg2)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v = arg1_v ** arg2
  wres_e= arg2*(arg1_v**(arg2-1))*arg1_e
end subroutine 
!****************

!! subroutine to add a 2d array to a IXD_NAME array
subroutine arrayPlusAD_&/**/
                       &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(sum(abs(shape(arg1)-shape(arg2_v)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v=arg1+arg2_v
  wres_e=arg2_e
end subroutine 

!! subroutine to subtract a IXD_NAME from a 2d array
subroutine arrayMinusAD_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(sum(abs(shape(arg1)-shape(arg2_v)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif    
  if(status == IXCseverity_error)  return
  wres_v=arg1-arg2_v
  wres_e=arg2_e
end subroutine 


!! subroutine to multiply a 2d array by a IXD_NAME array
subroutine arrayTimesAD_&/**/
                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(sum(abs(shape(arg1)-shape(arg2_v)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v=arg1*arg2_v
  wres_e=arg1*arg2_e
end subroutine 


!! subroutine to divide a 2d array by a IXD_NAME array
subroutine arrayDivideAD_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(sum(abs(shape(arg1)-shape(arg2_v)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v=arg1/arg2_v
  wres_e=wres_v*arg2_e/arg2_v
end subroutine 

!! subroutine to divide a 2d array by a IXD_NAME array
subroutine arrayPowerAD_&/**/
                         &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
  implicit none
  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
  real(dp),intent(in) :: arg1( IXD_DIMS ) !!input - scalar
  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
  type(IXTstatus),intent(inout) :: status !! error status flag
  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
  if(sum(abs(shape(arg1)-shape(arg2_v)))/=0)then
     call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input arrays passed incompatible for array operation')
  endif  
  if(status == IXCseverity_error)  return
  wres_v= arg1 ** arg2_v
  wres_e= wres_v*log(arg2_v)*arg2_e
end subroutine 



#undef IXD_NAME
#undef IXD_DIMS

#endif /* defined(IXD_NAME) && defined(IXD_DIMS) */

#if defined(IXD_NAME) && defined(IXD_PM)
  subroutine d2d1array_error_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_err_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_err_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarray&/**/
            &IXD_NAME (res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,vec_err_21,status)
    enddo
  end subroutine
  
  subroutine d2d1array_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=arr_val(:,i)&/**/
                              &IXD_PM vec_val_21
      res_err(:,i)=arr_err(:,i)
    enddo
  end subroutine
  
  subroutine d2d1array_error_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_err_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_err_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_21))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    if( size(vec_err_21) /= size(vec_val_21))then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'lengths of 1D input vector arrays passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
    do i = 1,nsy
      res_val(:,i)=arr_val(:,i) &/**/
                                &IXD_PM vec_val_21(i)
      res_err(:,i)=sqrt(arr_err(:,i)**2  +  vec_err_21(i)**2)
    enddo
  end subroutine   
  
  subroutine d2d1array_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy   
    nsy=size(arr_val,2)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_21))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    do i=1,nsy
      res_val(:,i)=arr_val(:,i) &/**/
                                &IXD_PM vec_val_21(i)
      res_err(:,i)=arr_err(:,i)      
    enddo
  end subroutine     
  
!**************************************
  subroutine d1d2array_error_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarray&/**/
            &IXD_NAME (res_val(:,i),res_err(:,i),vec_val_12,vec_err_12,arr_val(:,i),arr_err(:,i),status)
    enddo
  end subroutine
  
  subroutine d1d2array_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_12,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=vec_val_12 &/**/
                              &IXD_PM arr_val(:,i)
      res_err(:,i)=arr_err(:,i)
    enddo
  end subroutine
  
  subroutine d1d2array_error_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy
    nsy=size(arr_val,2)

 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_12))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    if( size(vec_err_12) /= size(vec_val_12))then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'lengths of 1D input vector arrays passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
    do i = 1,nsy
      res_val(:,i)= vec_val_12(i) &/**/
                                &IXD_PM arr_val(:,i)
      res_err(:,i)=sqrt(arr_err(:,i)**2  +  vec_err_12(i)**2)
    enddo
  end subroutine   
  
  subroutine d1d2array_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy  
    nsy=size(arr_val,2)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_12))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    do i=1,nsy
      res_val(:,i)=vec_val_12(i) &/**/
                                &IXD_PM arr_val(:,i) 
      res_err(:,i)=arr_err(:,i)      
    enddo
  end subroutine      
  
#undef IXD_NAME
#undef IXD_PM

#endif /* defined(IXD_NAME) && defined(IXD_PM) */




#if defined(IXD_NAME) && defined(IXD_TD)
  subroutine d2d1array_error_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_errl_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_errl_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
          call IXFarray&/**/
                  &IXD_NAME (res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,vec_errl_21,status)
    enddo
  end subroutine

  subroutine d2d1array_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=arr_val(:,i)&/**/
                               &IXD_TD vec_val_21
      res_err(:,i)=arr_err(:,i)&/**/
                               &IXD_TD vec_val_21
    enddo
  end subroutine
  
  subroutine d2d1array_error_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_errl_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_errl_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    real(dp),allocatable::temp_err(:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy,nsx    
    nsy=size(arr_val,2)
    nsx=size(arr_val,1)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_21))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    if( size(vec_errl_21) /= size(vec_val_21))then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
      IXCerr_invparam, 'lengths of 1D input vector arrays passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
    allocate(temp_err(nsx))
    do i = 1,nsy
      res_val(:,i)=arr_val(:,i) &/**/
                                &IXD_TD vec_val_21(i)
      temp_err=(vec_errl_21(i)/vec_val_21(i))**2                                  
      res_err(:,i)=res_val(:,i) * sqrt( (arr_err(:,i)/arr_val(:,i))**2 + temp_err)
    enddo
    deallocate(temp_err)
  end subroutine   

  subroutine d2d1array_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_21))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    do i=1,nsy
      res_val(:,i)=arr_val(:,i) &/**/
                                &IXD_TD vec_val_21(i)
      res_err(:,i)=arr_err(:,i) &/**/
                                &IXD_TD vec_val_21(i)      
    enddo
  end subroutine   
!************************************8
  subroutine d1d2array_error_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
          call IXFarray&/**/
                  &IXD_NAME (res_val(:,i),res_err(:,i),vec_val_12,vec_err_12,arr_val(:,i),arr_err(:,i),status)
    enddo
  end subroutine

  subroutine d1d2array_X_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_12,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=vec_val_12 &/**/
                               &IXD_TD arr_val(:,i)
      res_err(:,i)=vec_val_12 &/**/
                               &IXD_TD arr_err(:,i)
    enddo
  end subroutine
  
  subroutine d1d2array_error_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    real(dp),allocatable::temp_err(:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy,nsx    
    nsy=size(arr_val,2)
    nsx=size(arr_val,1)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_12))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    if( size(vec_err_12) /= size(vec_val_12))then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
      IXCerr_invparam, 'lengths of 1D input vector arrays passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
    allocate(temp_err(nsx))
    do i = 1,nsy
      res_val(:,i)=vec_val_12(i) &/**/
                                &IXD_TD arr_val(:,i)
!this order can stay the same as d2d1 subroutine                             
      temp_err=(vec_err_12(i)/vec_val_12(i))**2                                  
      res_err(:,i)=res_val(:,i) * sqrt( (arr_err(:,i)/arr_val(:,i))**2 + temp_err)
    enddo
    deallocate(temp_err)
  end subroutine   

  subroutine d1d2array_Y_2d&/**/
                         &IXD_NAME (status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
 
    if(sum(abs(shape(arr_val) - shape(arr_err)))/=0 )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	   	  IXCerr_invparam, 'lengths of 2D input arrays incompatible for array operation')
    endif        
    if(sum(abs(shape(res_val) - shape(arr_val)))/=0 )then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
		IXCerr_invparam, 'lengths of input/output arrays incompatible for array operation')
    endif        
    if(size(arr_val,2) /= size(vec_val_12))then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
	  IXCerr_invparam, 'lengths of input arrays  passed incompatible for array operation')
    endif
    if (status == IXCseverity_error) return
        
    do i=1,nsy
      res_val(:,i)=vec_val_12(i) &/**/
                                &IXD_TD arr_val(:,i)
      res_err(:,i)=vec_val_12(i) &/**/
                                &IXD_TD arr_err(:,i)      
    enddo
  end subroutine   
#undef IXD_NAME
#undef IXD_TD

#endif /* defined(IXD_NAME) && defined(IXD_TD) */

!!! subroutine to raise a scalar to the power of a IXD_NAME array
!subroutine arrayPowerSD_&/**/
!                        &IXD_NAME (wres_v,wres_e,arg1,arg2_v,arg2_e,status)
!  implicit none
!  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input1 value array
!  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input1 error array
!  real(dp),intent(in) :: arg1 !!input - scalar
!  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
!  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
!  type(IXTstatus),intent(inout) :: status !! error status flag
!  call IXFarrayCheck(wres_v,wres_e,arg2_v,arg2_e,status=status)
!  if(status == IXCseverity_error)  return
!  wres_v=arg1 ** arg2_v
!  wres_e=wres_v*log(arg2_v)*arg2_e
!end subroutine 
!
!!! subroutine to raise a IXD_NAME array to the power of another IXD_NAME array
!subroutine arrayPowerDD_&/**/
!                        &IXD_NAME (wres_v,wres_e, arg1_v,arg1_e, arg2_v,arg2_e, status)
!  implicit none
!  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
!  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
!  real(dp),intent(in) :: arg2_v( IXD_DIMS )!!input2 value array
!  real(dp),intent(in) :: arg2_e( IXD_DIMS )!!input2 error array
!  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
!  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
!  type(IXTstatus),intent(inout) :: status !! error status flag
!  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,arg2_v,arg2_e,status)
!  if(status == IXCseverity_error)  return
!  wres_v = arg1_v ** arg2_v
!  wres_e = sqrt((arg2_v*(arg1_v**((arg2_v)-1))*arg1_e)**2+(wres_v*log(arg2_v)*arg2_e)**2)
!end subroutine 
!
!!! subroutine to raise a IXD_NAME array to the power of a scalar
!subroutine arrayPowerDS_&/**/
!                        &IXD_NAME (wres_v,wres_e,arg1_v,arg1_e,arg2,status)
!  implicit none
!  real(dp),intent(in) :: arg1_v( IXD_DIMS )!!input1 value array
!  real(dp),intent(in) :: arg1_e( IXD_DIMS )!!input1 error array
!  real(dp),intent(in) :: arg2 !!input - scalar
!  real(dp),intent(out) :: wres_v( IXD_DIMS )!!output value array
!  real(dp),intent(out) :: wres_e( IXD_DIMS )!!output error array
!  type(IXTstatus),intent(inout) :: status !! error status flag
!  call IXFarrayCheck(wres_v,wres_e,arg1_v,arg1_e,status=status)
!  if(status == IXCseverity_error)  return
!  wres_v = arg1_v ** arg2
!  wres_e=arg2*(arg1_v**(arg2-1))*arg1_e
!end subroutine 
