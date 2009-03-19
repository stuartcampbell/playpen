!------------------------------
! MODULE: IXMdatum
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1131 $ ($Date: 2007-05-21 10:17:25 -0400 (Mon, 21 May 2007) $)
!!
!! FORTRAN definition of  IXMdatum object 



module IXMdatum
  use IXMtype_definitions
  use IXMbase
  implicit none
  public :: IXTdatum
  type IXTdatum
     !	private
     real(dp) :: val=0.0_dp 	!! value 
     real(dp) :: err=0.0_dp	!! associated error
  end type IXTdatum

#define IXD_TYPE datum
#define IXD_NO_BASE	1
#include "class_header.f90"

  private datumPlusWS, datumPlusWW, datumPlusSW
  private datumMinusWS, datumMinusWW, datumMinusSW
  private datumTimesWS, datumTimesWW, datumTimesSW
  private datumDivideWS, datumDivideWW, datumDivideSW
  private datumPowerWS, datumPowerWW, datumPowerSW


  interface IXFplus_Datum  
     module procedure datumPlusWS, datumPlusWW, datumPlusSW
  end interface
  interface IXFplus  
     module procedure datumPlusWS, datumPlusWW, datumPlusSW
  end interface

  interface IXFminus_Datum
     module procedure datumMinusWS, datumMinusWW, datumMinusSW
  end interface
  interface IXFminus
     module procedure datumMinusWS, datumMinusWW, datumMinusSW
  end interface

  interface IXFtimes_Datum
     module procedure datumTimesWS, datumTimesWW, datumTimesSW
  end interface
  interface IXFtimes
     module procedure datumTimesWS, datumTimesWW, datumTimesSW
  end interface

  interface IXFdivide_Datum
     module procedure datumDivideWS, datumDivideWW, datumDivideSW
  end interface
  interface IXFdivide
     module procedure datumDivideWS, datumDivideWW, datumDivideSW
  end interface

  interface IXFpower_Datum
     module procedure datumPowerWS, datumPowerWW, datumPowerSW
  end interface
  interface IXFpower
     module procedure datumPowerWS, datumPowerWW, datumPowerSW
  end interface

  interface IXFexp
     module procedure IXFexp_datum
  end interface

  interface IXFlog
     module procedure IXFlog_datum
  end interface

  interface IXFsin
     module procedure IXFsin_datum
  end interface

  interface IXFcos
     module procedure IXFcos_datum
  end interface

  interface IXFtan
     module procedure IXFtan_datum
  end interface

  interface IXFsinh
     module procedure IXFsinh_datum
  end interface

  interface IXFcosh
     module procedure IXFcosh_datum
  end interface

  interface IXFtanh
     module procedure IXFtanh_datum
  end interface


contains

#define IXD_DESCRIPTION	"IXTdatum class"
#define IXD_TYPE datum
#define IXD_SQTYPE 'datum'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_datum(arg, status)
    implicit none
    type(IXTdatum) :: arg
    type(IXTstatus) :: status
  end subroutine IXFdestroy_datum

  recursive subroutine IXFoperation_run_datum(op, field, arg, status)
    implicit none
    type(IXTdatum) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdatum', field, status)
    ! this order must match the declarion order in matlab as it is
    ! used when parsing arguments passed in class creation with varargin

    call IXFoperation_run(op,'val', arg%val, status)
    call IXFoperation_run(op,'err', arg%err, status)

    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_datum

  subroutine IXFget_datum(datum,status,val,err,wout)
    type(IXTdatum),intent(in)::datum
    type(IXTdatum),optional,intent(out)::wout
    real(dp),optional,intent(out)::val
    real(dp),optional,intent(out)::err
    type(IXTstatus),intent(inout)::status

    if(present(wout))then ! need to create a copy, use set with ref=datum
       call IXFset_datum(wout,status,ref=datum)
    endif

    if(present(val))val=datum%val  
    if(present(err))err=datum%err

  end subroutine IXFget_datum

  subroutine IXFcreate_datum(datum,value,error,status)
    type(IXTdatum),intent(out)::datum
    real(dp),intent(in)::value
    real(dp),intent(in)::error
    type(IXTstatus),intent(inout)::status

    call IXFset_datum(datum,status,value,error)

  end subroutine IXFcreate_datum

  recursive subroutine IXFset_datum(datum,status,value,error,ref)
    type(IXTdatum),intent(inout)::datum
    type(IXTdatum),intent(in),optional::ref
    real(dp),optional,intent(in)::value
    real(dp),optional,intent(in)::error
    type(IXTstatus),intent(inout)::status

    if(present(ref))call IXFset_datum(datum,status,ref%val,ref%err)

    if(present(value))datum%val=value  
    if(present(error))datum%err=error

  end subroutine IXFset_datum

  subroutine IXFcheck_datum(w1, status)
    implicit none
    type(IXTdatum) :: w1
    type(IXTStatus) :: status

    !    if (size(w1%val) /= size(w1%err)) then
    !       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    !            IXCerr_invparam, 'length of value and error arrays incompatible')
    !    endif
    ! there are no valid checks i can think of
    !if checks failed return to calling routine
    if (status == IXCseverity_error) return

  end subroutine IXFcheck_datum

  subroutine datumPlusWW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, arg2, wres
    type(IXTstatus) :: status

    wres%val= arg1%val + arg2%val
    !	if(associated(arg1%val,arg2%val) )then
    ! if arg1=arg2 identically -> correlated errors
    !	wres%err = 2.0 * arg1%err
    !	else
    wres%err = sqrt(arg1%err**2+arg2%err**2)
    !	endif

  end subroutine datumPlusWW

  subroutine datumPlusWS(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, wres
    type(IXTstatus) :: status
    real(dp) :: arg2
    wres%val= arg1%val + arg2
    wres%err = arg1%err

  end subroutine datumPlusWS

  subroutine datumPlusSW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg2, wres
    type(IXTstatus) :: status
    real(dp) :: arg1
    wres%val= arg1 + arg2%val
    wres%err = arg2%err

  end subroutine datumPlusSW

  subroutine datumMinusWW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, arg2, wres
    type(IXTstatus) :: status

    wres%val= arg1%val - arg2%val

    !	if(associated(arg1%val,arg2%val) )then
    ! if arg1=arg2 identically -> correlated errors
    !	wres%err = 0.0
    !	else
    wres%err = sqrt(arg1%err**2+arg2%err**2)
    !	endif
  end subroutine datumMinusWW

  subroutine datumMinusSW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) ::  arg2, wres
    type(IXTstatus) :: status
    real(dp) :: arg1
    wres%val= arg1 - arg2%val
    wres%err = arg2%err

  end subroutine datumMinusSW

  subroutine datumMinusWS(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) ::  arg1, wres
    type(IXTstatus) :: status
    real(dp) :: arg2
    wres%val= arg1%val - arg2
    wres%err = arg1%err

  end subroutine datumMinusWS

  subroutine datumTimesWW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, arg2, wres
    type(IXTstatus) :: status

    wres%val = arg1%val* arg2%val

    !	if(associated(arg1%val,arg2%val) )then
    ! if arg1=arg2 identically -> correlated errors
    !	wres%err = wres%val*sqrt(4.0*(arg1%err**2)/(arg1%val**2))
    !	else
    wres%err = wres%val*sqrt((arg1%err/arg1%val)**2+(arg2%err/arg2%val)**2)
    !	endif
  end subroutine datumTimesWW

  subroutine datumTimesWS(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, wres
    type(IXTstatus) :: status
    real(dp) :: arg2

    wres%val = arg1%val * arg2
    wres%err = arg1%err * arg2

  end subroutine datumTimesWS

  subroutine datumTimesSW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg2, wres
    type(IXTstatus) :: status
    real(dp) :: arg1

    wres%val =  arg1 * arg2%val
    wres%err = arg1 * arg2%err

  end subroutine datumTimesSW

  subroutine datumDivideWW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1, arg2, wres
    type(IXTstatus) :: status

    wres%val = arg1%val / arg2%val

    !	if(associated(arg1%val,arg2%val) )then
    ! if arg1=arg2 identically -> correlated errors
    wres%err = 0.0
    !	else
    wres%err = wres%val*sqrt((arg1%err/arg1%val)**2+(arg2%err/arg2%val)**2)
    !	endif

  end subroutine datumDivideWW

  subroutine datumDivideSW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) ::  arg2, wres
    type(IXTstatus) :: status
    real(dp) :: arg1

    wres%val = arg1 / arg2%val
    wres%err = wres%val*arg2%err/arg2%val

  end subroutine datumDivideSW

  subroutine datumDivideWS(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) ::  arg1, wres
    type(IXTstatus) :: status
    real(dp) :: arg2

    wres%val = arg1%val / arg2
    wres%err = arg1%err / arg2

  end subroutine datumDivideWS

  subroutine datumPowerWW(wres, arg1, arg2, status)
    !raises one workspace to the power of another workspace and assoc. errors
    implicit none
    type(IXTdatum) :: arg1, arg2, wres
    type(IXTstatus) :: status

    wres%val = arg1%val ** arg2%val

    !	if(associated(arg1%val,arg2%val) )then
    ! if arg1=arg2 identically -> correlated errors
    !	wres%err = wres%val*arg1%err*(1.0+log(arg1%val))
    !	else
    wres%err = sqrt(																&
         (  arg2%val * 	(arg1%val**((arg2%val)-1) ) * arg1%err)**2 +		&
         (  wres%val  *  log(arg2%val) * arg2%err ) **2						&
         )
    !	endif
  end subroutine datumPowerWW

  subroutine datumPowerWS(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg1,  wres
    type(IXTstatus) :: status
    real(dp) :: arg2

    wres%val = arg1%val ** arg2

    wres%err = arg2*(arg1%val**(arg2-1))*arg1%err

  end subroutine datumPowerWS

  subroutine datumPowerSW(wres, arg1, arg2, status)
    implicit none
    type(IXTdatum) :: arg2,  wres
    type(IXTstatus) :: status
    real(dp) :: arg1

    wres%val = arg1**arg2%val 

    wres%err = wres%val*log(arg2%val)*arg2%err

  end subroutine datumPowerSW

  subroutine IXFexp_Datum(wres,w1,status)
    implicit none
    type(IXTdatum) :: w1,wres
    type(IXTstatus) :: status

    wres%val=exp(w1%val)
    !	wres%err=exp(w1%val)*w1%err
    !	since wres%val=exp(w1%val)	->	already calculated
    wres%err=wres%val*w1%err
  end subroutine IXFexp_Datum

  subroutine IXFlog_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=log(w1%val)
    wres%err= w1%err / w1%val

  end subroutine IXFlog_Datum

  subroutine IXFsin_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=sin(w1%val)
    wres%err = cos(w1%val)*w1%err

  end subroutine IXFsin_Datum

  subroutine IXFcos_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=cos(w1%val)
    wres%err = sin(w1%val)*w1%err

  end subroutine IXFcos_Datum

  subroutine IXFtan_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=tan(w1%val)
    wres%err = w1%err /(cos(w1%val)**2)

  end subroutine IXFtan_Datum

  subroutine IXFsinh_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=sinh(w1%val)
    wres%err = cosh(w1%val)*w1%err

  end subroutine IXFsinh_Datum

  subroutine IXFcosh_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status

    wres%val=cosh(w1%val)
    wres%err = sinh(w1%val)*w1%err

  end subroutine IXFcosh_Datum

  subroutine IXFtanh_Datum(wres, w1, status)
    implicit none
    type(IXTdatum) :: w1, wres
    type(IXTstatus) :: status


    wres%val=tanh(w1%val)

    wres%err= w1%err / ( cosh(w1%val)**2)

  end subroutine IXFtanh_Datum


end module IXMdatum

