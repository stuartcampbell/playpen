
module IXMarraymanips
  use IXMstatus
  use IXMtype_definitions
  implicit none

! all the following internal routines are private, they can only be called through the generic interface procedure
! they do not have the IXF prefix

private ::arrayCheck_1D, arrayCheck_2D, arrayCheck_3D
private :: arrayPlusDD_1D, arrayPlusDS_1D,arrayPlusSD_1D,   &
          arrayPlusDD_2D, arrayPlusDS_2D,arrayPlusSD_2D,   &
          arrayPlusDD_3D, arrayPlusDS_3D,arrayPlusSD_3D
private :: arrayTimesDD_1D, arrayTimesDS_1D, arrayTimesSD_1D,   &
          arrayTimesDD_2D, arrayTimesDS_2D, arrayTimesSD_2D,   &
          arrayTimesDD_3D, arrayTimesDS_3D, arrayTimesSD_3D
private :: arrayMinusDD_1D, arrayMinusDS_1D, arrayMinusSD_1D,   &
          arrayMinusDD_2D, arrayMinusDS_2D, arrayMinusSD_2D,   &
          arrayMinusDD_3D, arrayMinusDS_3D, arrayMinusSD_3D
private ::  arrayDivideDD_1D, arrayDivideDS_1D, arrayDivideSD_1D,   &
          arrayDivideDD_2D, arrayDivideDS_2D, arrayDivideSD_2D,   &
          arrayDivideDD_3D, arrayDivideDS_3D, arrayDivideSD_3D
private ::  arrayPowerDD_1D, arrayPowerDS_1D, arrayPowerSD_1D,   &
          arrayPowerDD_2D, arrayPowerDS_2D, arrayPowerSD_2D,   &
          arrayPowerDD_3D, arrayPowerDS_3D, arrayPowerSD_3D
private :: arrayExp_1D, arrayExp_2D, arrayExp_3D
private :: arrayLog_1D, arrayLog_2D, arrayLog_3D
private :: arraySin_1D, arraySin_2D, arraySin_3D
private :: arrayCos_1D, arrayCos_2D, arrayCos_3D
private :: arrayTan_1D, arrayTan_2D, arrayTan_3D
private :: arraySinh_1D, arraySinh_2D, arraySinh_3D
private :: arrayCosh_1D, arrayCosh_2D, arrayCosh_3D
private :: arrayTanh_1D, arrayTanh_2D, arrayTanh_3D
private ::  arrayPlusDA_1D,arrayPlusAD_1D,   &
          arrayPlusDA_2D,arrayPlusAD_2D,   &
           arrayPlusDA_3D,arrayPlusAD_3D
private ::  arrayTimesDA_1D, arrayTimesAD_1D,   &
          arrayTimesDA_2D, arrayTimesAD_2D,   &
           arrayTimesDA_3D, arrayTimesAD_3D
private ::  arrayMinusDA_1D, arrayMinusAD_1D,   &
           arrayMinusDA_2D, arrayMinusAD_2D,   &
          arrayMinusDA_3D, arrayMinusAD_3D
private ::   arrayDivideDA_1D, arrayDivideAD_1D,   &
           arrayDivideDA_2D, arrayDivideAD_2D,   &
           arrayDivideDA_3D, arrayDivideAD_3D
 
private :: d1d2array_error_X_2dplus , d1d2array_error_Y_2dplus
private :: d2d1array_error_X_2dplus , d2d1array_error_Y_2dplus
private :: d1d2array_error_X_2dminus , d1d2array_error_Y_2dminus
private :: d2d1array_error_X_2dminus , d2d1array_error_Y_2dminus
private :: d1d2array_error_X_2dtimes , d1d2array_error_Y_2dtimes
private :: d2d1array_error_X_2dtimes , d2d1array_error_Y_2dtimes
private :: d1d2array_error_X_2ddivide , d1d2array_error_Y_2ddivide
private :: d2d1array_error_X_2ddivide , d2d1array_error_Y_2ddivide

private :: d1d2array_X_2dplus , d1d2array_Y_2dplus
private :: d2d1array_X_2dplus , d2d1array_Y_2dplus
private :: d1d2array_X_2dminus , d1d2array_Y_2dminus
private :: d2d1array_X_2dminus , d2d1array_Y_2dminus
private :: d1d2array_X_2dtimes , d1d2array_Y_2dtimes
private :: d2d1array_X_2dtimes , d2d1array_Y_2dtimes
private :: d1d2array_X_2ddivide , d1d2array_Y_2ddivide
private :: d2d1array_X_2ddivide , d2d1array_Y_2ddivide


interface IXFarray_X_2dplus
   module procedure d1d2array_error_X_2dplus, d1d2array_X_2dplus
   module procedure d2d1array_error_X_2dplus,  d2d1array_X_2dplus
end interface

interface IXFarray_Y_2dplus
   module procedure d1d2array_error_Y_2dplus, d1d2array_Y_2dplus
   module procedure d2d1array_error_Y_2dplus, d2d1array_Y_2dplus
end interface

interface IXFarray_X_2dminus
   module procedure d1d2array_error_X_2dminus, d1d2array_X_2dminus
   module procedure d2d1array_error_X_2dminus, d2d1array_X_2dminus
end interface

interface IXFarray_Y_2dminus
   module procedure d1d2array_error_Y_2dminus, d1d2array_Y_2dminus
   module procedure d2d1array_error_Y_2dminus, d2d1array_Y_2dminus
end interface

interface IXFarray_X_2dtimes
   module procedure d1d2array_error_X_2dtimes, d1d2array_X_2dtimes
   module procedure d2d1array_error_X_2dtimes, d2d1array_X_2dtimes
end interface

interface IXFarray_Y_2dtimes
   module procedure d1d2array_error_Y_2dtimes, d1d2array_Y_2dtimes
   module procedure d2d1array_error_Y_2dtimes, d2d1array_Y_2dtimes
end interface

interface IXFarray_X_2ddivide
   module procedure d1d2array_error_X_2ddivide, d1d2array_X_2ddivide
   module procedure d2d1array_error_X_2ddivide, d2d1array_X_2ddivide
end interface

interface IXFarray_Y_2ddivide
   module procedure d1d2array_error_Y_2ddivide, d1d2array_Y_2ddivide
   module procedure d2d1array_error_Y_2ddivide, d2d1array_Y_2ddivide
end interface

interface IXFarray_X_2dpower
   module procedure d1d2array_error_X_2dpower, d1d2array_X_2dpower
   module procedure d2d1array_error_X_2dpower, d2d1array_X_2dpower
end interface

interface IXFarray_Y_2dpower
   module procedure d1d2array_error_Y_2dpower, d1d2array_Y_2dpower
   module procedure d2d1array_error_Y_2dpower, d2d1array_Y_2dpower
end interface  

!*******************************************************************************************************
!*******************************************************************************************************
!*******************************************************************************************************


!! interface for subroutine to check input arrays to the core array manipulation routines
!! checks array lengths correspond for input and output arrays etc.
!! overloaded for 1D, 2D and 3D cases
 interface IXFarrayCheck
     module procedure arrayCheck_1D, arrayCheck_2D, arrayCheck_3D
  end interface



!! Interface for subroutine to add one datum to another (1D, 2D or 3D), or a datum to a scalar or vice versa
!! a datum refers to a value/error pair of arrays
!!n naming convention -> DD = datum,datum , DS= datum,scalar , SD=scalar,datum
!!n Subroutines overloaded for these different cases (as well as 1D, 2D & 3D cases):
!!
!! Calling convention:
!!n call IXFarrayPlus(wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status) Datum/Datum 
!!n call IXFarrayPlus(wres_v,wres_e,w1_v,w1_e,arg2,status) Datum/Scalar
!!n call IXFarrayPlus(wres_v,wres_e,arg1,w2_v,w2_e,status) Scalar/Datum
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input scalar
!! -status	-> IXTstatus object 
  interface IXFarrayPlus
     module procedure arrayPlusDD_1D, arrayPlusDS_1D,arrayPlusSD_1D,   &
          arrayPlusDD_2D, arrayPlusDS_2D,arrayPlusSD_2D,   &
          arrayPlusDD_3D, arrayPlusDS_3D,arrayPlusSD_3D  
  end interface

!! Interface for subroutine to multiply one datum by another (1D, 2D or 3D), or a datum by a scalar or vice versa
!! a datum refers to a value/error pair of arrays
!!n naming convention -> DD = datum,datum , DS= datum,scalar , SD=scalar,datum
!!n Subroutines overloaded for these different cases (as well as 1D, 2D & 3D cases):
!!
!! Calling convention:
!!n call IXFarrayTimes(wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status) Datum/Datum 
!!n call IXFarrayTimes(wres_v,wres_e,w1_v,w1_e,arg2,status) Datum/Scalar
!!n call IXFarrayTimes(wres_v,wres_e,arg1,w2_v,w2_e,status) Scalar/Datum
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input scalar
!! -status	-> IXTstatus object 
  interface IXFarrayTimes
     module procedure arrayTimesDD_1D, arrayTimesDS_1D, arrayTimesSD_1D,   &
          arrayTimesDD_2D, arrayTimesDS_2D, arrayTimesSD_2D,   &
          arrayTimesDD_3D, arrayTimesDS_3D, arrayTimesSD_3D
  end interface

!! Interface for subroutine to subtract one datum from another (1D, 2D or 3D), or a datum from a scalar or vice versa
!! a datum refers to a value/error pair of arrays
!!n naming convention -> DD = datum,datum , DS= datum,scalar , SD=scalar,datum
!!n Subroutines overloaded for these different cases (as well as 1D, 2D & 3D cases):
!!
!! Calling convention:
!!n call IXFarrayMinus(wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status) Datum/Datum 
!!n call IXFarrayMinus(wres_v,wres_e,w1_v,w1_e,arg2,status) Datum/Scalar
!!n call IXFarrayMinus(wres_v,wres_e,arg1,w2_v,w2_e,status) Scalar/Datum
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input scalar
!! -status	-> IXTstatus object 
  interface IXFarrayMinus
     module procedure arrayMinusDD_1D, arrayMinusDS_1D, arrayMinusSD_1D,   &
          arrayMinusDD_2D, arrayMinusDS_2D, arrayMinusSD_2D,   &
          arrayMinusDD_3D, arrayMinusDS_3D, arrayMinusSD_3D
  end interface

!! Interface for subroutine to divide one datum to by the other (1D, 2D or 3D), or a datum by a scalar or vice versa
!! a datum refers to a value/error pair of arrays
!!n naming convention -> DD = datum,datum , DS= datum,scalar , SD=scalar,datum
!!n Subroutines overloaded for these different cases (as well as 1D, 2D & 3D cases):
!!
!! Calling convention:
!!n call IXFarrayDivide(wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status) Datum/Datum 
!!n call IXFarrayDivide(wres_v,wres_e,w1_v,w1_e,arg2,status) Datum/Scalar
!!n call IXFarrayDivide(wres_v,wres_e,arg1,w2_v,w2_e,status) Scalar/Datum
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input scalar
!! -status	-> IXTstatus object 
  interface IXFarrayDivide
     module procedure arrayDivideDD_1D, arrayDivideDS_1D, arrayDivideSD_1D,   &
          arrayDivideDD_2D, arrayDivideDS_2D, arrayDivideSD_2D,   &
          arrayDivideDD_3D, arrayDivideDS_3D, arrayDivideSD_3D
  end interface

!! Interface for subroutine to raise one datum to the power of the other (1D, 2D or 3D), or a datum to a scalar or vice versa
!! a datum refers to a value/error pair of arrays
!!n naming convention -> DD = datum,datum , DS= datum,scalar , SD=scalar,datum
!!n Subroutines overloaded for these different cases (as well as 1D, 2D & 3D cases):
!!
!! Calling convention:
!!n call IXFarrayPower(wres_v,wres_e,w1_v,w1_e,w2_v,w2_e,status) Datum/Datum 
!!n call IXFarrayPower(wres_v,wres_e,w1_v,w1_e,arg2,status) Datum/Scalar
!!n call IXFarrayPower(wres_v,wres_e,arg1,w2_v,w2_e,status) Scalar/Datum
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input scalar
!! -status	-> IXTstatus object 
  interface IXFarrayPower
     module procedure arrayPowerDD_1D, arrayPowerDS_1D, arrayPowerSD_1D,   &
          arrayPowerDD_2D, arrayPowerDS_2D, arrayPowerSD_2D,   &
          arrayPowerDD_3D, arrayPowerDS_3D, arrayPowerSD_3D
  end interface

!! Interface for subroutine to take the exponential of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayExp(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object
  interface IXFarrayExp
     module procedure arrayExp_1D, arrayExp_2D, arrayExp_3D
  end interface

!! Interface for subroutine to take the natural logarithm of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayLog(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayLog
     module procedure arrayLog_1D, arrayLog_2D, arrayLog_3D
  end interface

!! Interface for subroutine to take the logarithm (base 10) of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayLog10(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayLog10
     module procedure arrayLog10_1D, arrayLog10_2D, arrayLog10_3D
  end interface

!! Interface for subroutine to take the sine of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarraySin(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarraySin
     module procedure arraySin_1D, arraySin_2D, arraySin_3D
  end interface

!! Interface for subroutine to take the cosine of aof a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayCos(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayCos
     module procedure arrayCos_1D, arrayCos_2D, arrayCos_3D
  end interface

!! Interface for subroutine to take the tangent of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling Convention:
!!n call IXFarrayTan(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayTan
     module procedure arrayTan_1D, arrayTan_2D, arrayTan_3D
  end interface

!! Interface for subroutine to take the hyperbolic sine of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarraySinh(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarraySinh
     module procedure arraySinh_1D, arraySinh_2D, arraySinh_3D
  end interface

!! Interface for subroutine to take the hyperbolic cosine of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayCosh(wres_v,wres_e,w1_v,w1_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayCosh
     module procedure arrayCosh_1D, arrayCosh_2D, arrayCosh_3D
  end interface

!! Interface for subroutine to take the hyperbolic tangent of of a datum(1D, 2D or 3D)
!! a datum refers to a value/error pair of arrays
!! Calling convention:
!!n call IXFarrayTanh(wres_v,wres_e,w1_v,w1_e,status)
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! -w1_v	-> input value array
!! -w1_e	-> input ereror array
!! -status  -> IXTstatus object	
  interface IXFarrayTanh
     module procedure arrayTanh_1D, arrayTanh_2D, arrayTanh_3D
  end interface


!! Interface for subroutine to add a datum to an array of the same shape or vice versa
!!n naming convention:   DA= datum,array , AD=array,datum
!!
!!Calling convention:
!!n call IXFarrayPlusAD(wres_v,wres_e,w1_v,w1_e,arg2,status)  
!!n call IXFarrayPlusDA(wres_v,wres_e,arg1,w2_v,w2_e,status)
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! a datum refers to a value/error pair of arrays
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input array
!! -status  -> IXTstatus object	 
  interface IXFarrayPlusDA
     module procedure  arrayPlusDA_1D,arrayPlusDA_2D,arrayPlusDA_3D
  end interface
  interface IXFarrayPlusAD
     module procedure  arrayPlusAD_1D,arrayPlusAD_2D,arrayPlusAD_3D  
  end interface
  
!! Interface for subroutine to multiply a datum with an array of the same shape or vice versa
!!n naming convention:   DA= datum,array , AD=array,datum
!!
!!Calling convention:
!!n call IXFarrayTimesAD(wres_v,wres_e,w1_v,w1_e,arg2,status)
!!n call IXFarrayTimesDA(wres_v,wres_e,arg1,w2_v,w2_e,status) 
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! a datum refers to a value/error pair of arrays
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input array
!! -status  -> IXTstatus object	 
  interface IXFarrayTimesDA
     module procedure  arrayTimesDA_1D,arrayTimesDA_2D,arrayTimesDA_3D
  end interface
  interface IXFarrayTimesAD
     module procedure  arrayTimesAD_1D,arrayTimesAD_2D,arrayTimesAD_3D  
  end interface

!! Interface for subroutine to subtract a datum from an array of the same shape or vice versa
!!n naming convention:   DA= datum,array , AD=array,datum
!!
!!Calling convention:
!!n call IXFarrayMinusAD(wres_v,wres_e,w1_v,w1_e,arg2,status)
!!n call IXFarrayMinusDA(wres_v,wres_e,arg1,w2_v,w2_e,status) 
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! a datum refers to a value/error pair of arrays
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input array
!! -status  -> IXTstatus object	 
  interface IXFarrayMinusDA
     module procedure  arrayMinusDA_1D,arrayMinusDA_2D,arrayMinusDA_3D
  end interface
  interface IXFarrayMinusAD
     module procedure  arrayMinusAD_1D,arrayMinusAD_2D,arrayMinusAD_3D  
  end interface

!! Interface for subroutine to divide a datum by an array of the same shape or vice versa
!!n naming convention:   DA= datum,array , AD=array,datum
!!
!!Calling convention:
!!n call IXFarrayMinusAD(wres_v,wres_e,w1_v,w1_e,arg2,status)
!!n call IXFarrayMinusDA(wres_v,wres_e,arg1,w2_v,w2_e,status) 
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! a datum refers to a value/error pair of arrays
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input array
!! -status  -> IXTstatus object	 
  interface IXFarrayDivideDA
     module procedure  arrayDivideDA_1D,arrayDivideDA_2D,arrayDivideDA_3D
  end interface
  interface IXFarrayDivideAD
     module procedure  arrayDivideAD_1D,arrayDivideAD_2D,arrayDivideAD_3D  
  end interface

!! Interface for subroutine to raise a datum to the power of an array of the same shape or vice versa
!!n naming convention:   DA= datum,array , AD=array,datum
!!
!!Calling convention:
!!n call IXFarrayPowerAD(wres_v,wres_e,w1_v,w1_e,arg2,status)
!!n call IXFarrayPowerDA(wres_v,wres_e,arg1,w2_v,w2_e,status) 
!!
!! -wres_v	-> output value array
!! -wres_e	-> output error array
!! a datum refers to a value/error pair of arrays
!! -w#_v	-> input value array
!! -w#_e	-> input error array
!! -arg#	-> input array
!! -status  -> IXTstatus object	 
  interface IXFarrayPowerDA
     module procedure  arrayPowerDA_1D,arrayPowerDA_2D,arrayPowerDA_3D
  end interface
  interface IXFarrayPowerAD
     module procedure  arrayPowerAD_1D,arrayPowerAD_2D,arrayPowerAD_3D  
  end interface


	
contains
	
#define IXD_NAME	1D
#define IXD_DIMS	:
#include "IXMarraymanips_routines.f90"
  !*************************************************************************************************

#define IXD_NAME	2D
#define IXD_DIMS	:,:
#include "IXMarraymanips_routines.f90"
!*************************************************************************************************

#define IXD_NAME	3D
#define IXD_DIMS	:,:,:
#include "IXMarraymanips_routines.f90"

!*************************************************************************************************
!IXFvecX1d_2dPlus & IXFvecY1d_2dPlus
#define IXD_NAME	Plus
#define IXD_PM      +
#include "IXMarraymanips_routines.f90"
!*************************************************************************************************
!IXFvecX1d_2dMinus & IXFvecY1d_2dMinus

#define IXD_NAME	Minus
#define IXD_PM      -
#include "IXMarraymanips_routines.f90"
!*************************************************************************************************
!IXFvecX1d_2dTimes & IXFvecY1d_2dTimes
#define IXD_NAME	Times
#define IXD_TD      *
#include "IXMarraymanips_routines.f90"
  !*************************************************************************************************
!IXFvecX1d_2dDivide & IXFvecY1d_2dDivide
#define IXD_NAME	Divide
#define IXD_TD      /
#include "IXMarraymanips_routines.f90"
  !*************************************************************************************************
  !*************************************************************************************************
  ! no generalised error functions for raising to power so routine written implicitly here
  
  subroutine d2d1array_error_X_2dPower(status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_errl_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_errl_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
          call IXFarrayPower(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,vec_errl_21,status)
    enddo
  end subroutine

  subroutine d2d1array_X_2dPower (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_21,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=arr_val(:,i) ** vec_val_21
      res_err(:,i)=   vec_val_21*(arr_val(:,i)**(vec_val_21-1))*arr_err(:,i)
    enddo
  end subroutine
  
  subroutine d2d1array_error_Y_2dPower (status,res_val,res_err,arr_val,arr_err,vec_val_21,vec_errl_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:),vec_errl_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsx        
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
          
    do i=1,nsx
       call IXFarrayPower(res_val(i,:),res_err(i,:),arr_val(i,:),arr_err(i,:),vec_val_21,vec_errl_21,status)
    enddo

  end subroutine   

  subroutine d2d1array_Y_2dPower (status,res_val,res_err,arr_val,arr_err,vec_val_21)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_21(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)    
    type(IXTstatus)::status
    integer(i4b)::i,nsx        
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
        
    do i=1,nsx
      res_val(i,:)=arr_val(i,:) ** vec_val_21
      res_err(i,:)=vec_val_21*(arr_val(i,:)**(vec_val_21-1))*arr_err(i,:)      
    enddo
  end subroutine   
!************************************8
  subroutine d1d2array_error_X_2dPower (status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
          call IXFarrayPower (res_val(:,i),res_err(:,i),vec_val_12,vec_err_12,arr_val(:,i),arr_err(:,i),status)
    enddo
  end subroutine

  subroutine d1d2array_X_2dPower(status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsy    
    nsy=size(arr_val,2)
    do i=1,nsy
      call IXFarrayCheck(res_val(:,i),res_err(:,i),arr_val(:,i),arr_err(:,i),vec_val_12,status=status)
      if (status == IXCseverity_error) return
      res_val(:,i)=vec_val_12 ** arr_val(:,i)
      res_err(:,i)=   res_val(:,i)*log(arr_val(:,i))*arr_err(:,i)
      
    enddo
  end subroutine
  
  subroutine d1d2array_error_Y_2dPower(status,res_val,res_err,vec_val_12,vec_err_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:),vec_err_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)
    type(IXTstatus)::status
    integer(i4b)::i,nsx    
    
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
    


do i=1,nsx
 call IXFarrayPower(res_val(i,:),res_err(i,:),vec_val_12,vec_err_12,arr_val(i,:),arr_err(i,:),status)
enddo
    

  end subroutine   

  subroutine d1d2array_Y_2dPower(status,res_val,res_err,vec_val_12,arr_val,arr_err)
    real(dp),intent(in)::arr_val(:,:),arr_err(:,:),vec_val_12(:)
    real(dp),intent(out)::res_val(:,:),res_err(:,:)

    type(IXTstatus)::status
    integer(i4b)::i,nsx    

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
        
    do i=1,nsx
      res_val(i,:)=vec_val_12(i) ** arr_val(i,:)
      res_err(i,:)= res_val(i,:)*log(arr_val(i,:))*arr_err(i,:)
    enddo
  end subroutine   


!! subroutine to multiply a section of every column of a 2d array(real, signal/error array) by a 1d array (real, binwidth array)
  subroutine IXFcreatebinfac_D1(s,e,x,mx_u,mx_l,binfacV_D1,binfacE_D1)
    implicit none
    real(dp),intent(in):: s(:,:), e(:,:)!! input 2D arrays
	real(dp),intent(in):: x(:) !! 1D array to be multiplied along the 1st dimension of the 2D arrays
    real(dp),intent(out):: binfacV_D1(:,:),binfacE_D1(:,:)!! output 2D arrays
    integer(i4b)::i,mx_u,mx_l,ny

    ny=size(s,2)
    do i=1,ny
       binfacV_D1(:,i)=s(mx_l:mx_u-1,i)*(x(mx_l+1:mx_u)-x(mx_l:mx_u-1))
       binfacE_D1(:,i)=e(mx_l:mx_u-1,i)*(x(mx_l+1:mx_u)-x(mx_l:mx_u-1))
    enddo

  end subroutine IXFcreatebinfac_D1
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

!! subroutine to multiply a section of every column of a 2d array(real, signal/error array) by a 1d array (real, binwidth array)
  subroutine IXFcreatePfac_D1(s,e,x,mx_u,mx_l,PfacV_D1,PfacE_D1)
    implicit none
    real(dp),intent(in):: s(:,:), e(:,:)!! input 2D arrays
	real(dp),intent(in):: x(:) !! 1D array to be multiplied along the 1st dimension of the 2D arrays
    real(dp),intent(out):: PfacV_D1(:,:),PfacE_D1(:,:)!! output 2D arrays
    integer(i4b)::i,mx_u,mx_l,ny

    ny=size(s,2)

    do i=1,ny
       PfacV_D1(:,i)=(s(mx_l:mx_u-1,i)+s(mx_l+1:mx_u,i))*(x(mx_l+1:mx_u)-x(mx_l:mx_u-1))
       PfacE_D1(:,i)=e(mx_l+1:mx_u-1,i)  *  (x(mx_l+2:mx_u)-x(mx_l:mx_u-2))
    enddo

  end subroutine IXFcreatePfac_D1
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  
!! subroutine to multiply a section of every row of a 2d array(real, signal/error array) by a 1d array (real, binwidth array), 
!! performed as a double loop to prevent the creation of a copy
  subroutine IXFcreatebinfac_D2(s,e,y,my_u,my_l,binfacV_D2,binfacE_D2)
    implicit none
    real(dp),intent(in):: s(:,:), e(:,:) !! input 2D arrays
	real(dp),intent(in):: y(:) !! 1D array to be multiplied along the 2nd dimension of the 2D arrays
    real(dp),intent(out):: binfacV_D2(:,:),binfacE_D2(:,:) !! output 2D arrays
    integer(i4b)::j,i,my_u,my_l,nx
 
    nx=size(s,1)
    do i=1,nx
       do j=1,size(binfacV_D2,2)	
          binfacV_D2(i,j)=s(i,my_l+j-1)  *(y(my_l+j)-y(my_l+j-1))
          binfacE_D2(i,j)=e(i,my_l+j-1)*(y(my_l+j)-y(my_l+j-1))
       enddo
    enddo

  end subroutine IXFcreatebinfac_D2
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

!! subroutine to multiply a section of every row of a 2d array(real, signal/error array) by a 1d array (real, binwidth array), 
!! performed as a double loop to prevent the creation of a copy
  subroutine IXFcreatePfac_D2(s,e,y,my_u,my_l,PfacV_D2,PfacE_D2)
    implicit none
    real(dp),intent(in):: s(:,:), e(:,:) !! input 2D arrays
	real(dp),intent(in):: y(:) !! 1D array to be multiplied along the 2nd dimension of the 2D arrays
    real(dp),intent(out):: PfacV_D2(:,:),PfacE_D2(:,:) !! output 2D arrays
    integer(i4b)::j,i,my_u,my_l,nx
 
    nx=size(s,1)
    do i=1,nx
       do j=1,size(PfacV_D2,2)	
          PfacV_D2(i,j)=(s(i,my_l+j-1)+s(i,my_l+j))*(y(my_l+j)-y(my_l+j-1))
       enddo
       
       do j=1,size(PfacE_D2,2)
          PfacE_D2(i,j)=e(i,my_l+j)*(y(my_l+j+1)-y(my_l+j-1))
       enddo
    enddo

  end subroutine IXFcreatePfac_D2

end module IXMarraymanips





