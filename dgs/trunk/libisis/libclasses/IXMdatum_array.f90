!------------------------------
! MODULE: IXMdatum_array
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1284 $ ($Date: 2007-11-27 09:46:33 -0500 (Tue, 27 Nov 2007) $)
!!
!! FORTRAN definition of IXMdatum_array object 


module IXMdatum_array
  use IXMtype_definitions
  use IXMarraymanips
  use IXMbase
  implicit none
  public :: IXTdatum_array
  type IXTdatum_array
     !     private
     type(IXTbase)::base
     real(dp), pointer :: signal(:)=>NULL() 	!! value array
     real(dp), pointer :: error(:)=>NULL()		!! associated error array
  end type IXTdatum_array

#define IXD_TYPE datum_array
#include "class_header.f90"

  private setup_unary_op_datum_array,setup_binary_op_datum_array,finish_op_datum_array
  private ts_Plus_datum_array, tt_Plus_datum_array, st_Plus_datum_array
  private ts_Minus_datum_array, tt_Minus_datum_array, st_Minus_datum_array
  private ts_Times_datum_array, tt_Times_datum_array, st_Times_datum_array
  private ts_Divide_datum_array, tt_Divide_datum_array, st_Divide_datum_array
  private ts_Power_datum_array, tt_Power_datum_array, st_Power_datum_array

  !***********************************
  interface IXFplus_Datum_array  
     module procedure ts_Plus_datum_array, tt_Plus_datum_array, st_Plus_datum_array
  end interface
  interface IXFplus   
     module procedure ts_Plus_datum_array, tt_Plus_datum_array, st_Plus_datum_array
  end interface

  interface IXFminus_datum_array
     module procedure ts_Minus_datum_array, tt_Minus_datum_array, st_Minus_datum_array
  end interface
  interface IXFminus 
     module procedure ts_Minus_datum_array, tt_Minus_datum_array, st_Minus_datum_array
  end interface

  interface IXFtimes_Datum_array
     module procedure ts_Times_datum_array, tt_Times_datum_array, st_Times_datum_array
  end interface
  interface IXFtimes 
     module procedure ts_Times_datum_array, tt_Times_datum_array, st_Times_datum_array
  end interface

  interface IXFdivide_Datum_array
     module procedure ts_Divide_datum_array, tt_Divide_datum_array, st_Divide_datum_array
  end interface
  interface IXFdivide 
     module procedure ts_Divide_datum_array, tt_Divide_datum_array, st_Divide_datum_array
  end interface

  interface IXFpower_Datum_array
     module procedure ts_Power_datum_array, tt_Power_datum_array, st_Power_datum_array
  end interface
  interface IXFpower 
     module procedure ts_Power_datum_array, tt_Power_datum_array, st_Power_datum_array
  end interface

  interface IXFexp
     module procedure IXFexp_datum_array
  end interface

  interface IXFlog
     module procedure IXFlog_datum_array
  end interface

  interface IXFsin
     module procedure IXFsin_datum_array
  end interface

  interface IXFcos
     module procedure IXFcos_datum_array
  end interface

  interface IXFtan
     module procedure IXFtan_datum_array
  end interface

  interface IXFsinh
     module procedure IXFsinh_datum_array
  end interface

  interface IXFcosh
     module procedure IXFcosh_datum_array
  end interface

  interface IXFtanh
     module procedure IXFtanh_datum_array
  end interface

contains

#define IXD_DESCRIPTION	"IXTdatum_array class"
#define IXD_TYPE datum_array
#define IXD_SQTYPE 'datum_array'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_datum_array(arg, status)
    implicit none
    type(IXTdatum_array) :: arg
    type(IXTstatus) :: status
    
    call IXFdestroy(arg%base,status)
    call IXFdealloc(arg%signal,status)
    call IXFdealloc(arg%error,status)
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_datum_array

  recursive subroutine IXFoperation_run_datum_array(op, field, arg, status)
    implicit none
    type(IXTdatum_array) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdatum_array', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declation order in matlab as it is
    ! used when parsing arguments passed in class creation with varargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'signal', arg%signal, status)
    call IXFoperation_run_ptr(op,'error', arg%error, status)

    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_datum_array


  subroutine IXFcheck_datum_array(w1, status)
    implicit none
    type(IXTdatum_array) :: w1
    type(IXTStatus) :: status

    if (size(w1%signal) /= size(w1%error)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length of signal and error arrays incompatible (IXFcheck_datum_array)')
    endif

    !if checks failed return to calling routine
    if (status == IXCseverity_error) return

  end subroutine IXFcheck_datum_array

  !!generic create routine which calls a specific option of 'set'
  !! and must pass all arguments required to create the object
  subroutine IXFcreate_datum_array(array,signal,error,status)
    implicit none
    type(IXTdatum_array),intent(out) :: array
    real(dp),intent(in) :: signal(:)!!input: signal ARRAY passed
    real(dp),intent(in):: error(:) !!input: error ARRAY passed
    type(IXTstatus) :: status
 
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(array)
    call IXFset_datum_array(array,status,signal,error)
  
  end subroutine IXFcreate_datum_array

  recursive subroutine IXFset_datum_array(array,status,signal,error,ref)
    implicit none
    type(IXTdatum_array),intent(inout)::array
    type(IXTdatum_array),intent(in),optional::ref
    real(dp),optional,intent(in) :: signal(:)!!input: signal ARRAY passed
    real(dp),optional,intent(in):: error(:) !!input: error ARRAY passed
    type(IXTstatus),intent(inout)::status !! error status object

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_datum_array)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(array)
    else    
       if(IXFvalid(array) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_datum_array)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref)) call IXFset_datum_array(array,status,ref%signal,ref%error)

    call IXFset_real_array(array%signal,status,signal)
    call IXFset_real_array(array%error,status,error)

    call IXFcheck_datum_array(array,status)

  end subroutine IXFset_datum_array


  subroutine IXFget_datum_array(array,status,signal,error,wout)
    type(IXTdatum_array),intent(in)::array
    type(IXTdatum_array),optional,intent(out)::wout !!input:: object passed if a copy of a datum_array is required
    real(dp),optional,intent(out) :: signal(:)!!input: signal ARRAY passed
    real(dp),optional,intent(out):: error(:) !!input: error ARRAY passed
    type(IXTstatus),intent(inout)::status !! error status object

    if(present(wout))then ! need to create a copy, use set with ref=dataset_1d
       call IXFcopy(array,wout,status)
    endif

    call IXFget_real_array(array%signal,status,signal)
    call IXFget_real_array(array%error,status,error)

  end subroutine IXFget_datum_array

  subroutine IXFget_ptr_datum_array(w1,signal,error)
    real(dp),pointer,optional::signal(:) !!output: signal array pointer
    real(dp),pointer,optional::error(:) !!output: error array pointer
    type(IXTdatum_array),intent(in)::w1 !!input dataset_1d 

    if (present(signal))signal=>w1%signal
    if (present(error))error=>w1%error

  end subroutine IXFget_ptr_datum_array


  subroutine IXFget_alloc_datum_array(w1,status,signal,error,wout)
    type(IXTdatum_array),optional,intent(out)::wout !!input:: object passed if a copy of a datum_array is required
    real(dp),allocatable,optional::signal(:) !!signal array pointer
    real(dp),allocatable,optional::error(:) !!error array pointer
    type(IXTdatum_array),intent(in)::w1 !!input dataset_1d 
    type(IXTstatus),intent(inout)::status !! error status object

    if (present(signal))then
      call IXFreallocdimsFortran(signal,shape(w1%signal),.false.,status)
    endif
    if (present(error))then
      call IXFreallocdimsFortran(error,shape(w1%error),.false.,status)
    endif
    call IXFget_datum_array(w1,status,signal,error)

  end subroutine IXFget_alloc_datum_array

#define IXD_TYPE	datum_array
#define IXD_NAME	Log_Datum_array
#define IXD_OPERATION   Log
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Exp_Datum_array
#define IXD_OPERATION   Exp
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Sin_Datum_array
#define IXD_OPERATION   Sin
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Cos_Datum_array
#define IXD_OPERATION   Cos
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Tan_Datum_array
#define IXD_OPERATION   Tan
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Sinh_Datum_array
#define IXD_OPERATION   Sinh
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Cosh_Datum_array
#define IXD_OPERATION   Cosh
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Tanh_Datum_array
#define IXD_OPERATION   Tanh
#include "unary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Plus_Datum_array
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Minus_Datum_array
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Times_Datum_array
#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Divide_Datum_array
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_TYPE	datum_array
#define IXD_NAME	Power_Datum_array
#define IXD_OPERATION   Power
#include "binary_ops.f90"


  subroutine setup_binary_op_datum_array(wres, w1, w2, status)
    implicit none
    type(IXTdatum_array) :: wres,w1,w2
    type(IXTstatus) :: status
    ! make checks on the lengths of the arrays

    if (size(w1%signal) /= size(w2%signal)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'length of signal and error arrays incompatible') 
    endif

    if (status == IXCseverity_error) return  
    !if check passed create memory for output
    call IXFalloc(wres%signal, size(w1%signal), status)
    call IXFalloc(wres%error, size(w1%error), status)

  end subroutine setup_binary_op_datum_array

  subroutine setup_unary_op_datum_array(wres, w1, status)
    implicit none
    type(IXTdatum_array) :: wres,w1
    type(IXTstatus) :: status
    ! no checks to make

    !  if (status == IXCseverity_error) return  

    !if create memory for output
    call IXFalloc(wres%signal, size(w1%signal), status)
    call IXFalloc(wres%error, size(w1%error), status)

  end subroutine setup_unary_op_datum_array

  subroutine finish_op_datum_array(wres,w1,status)
    implicit none
    type(IXTdatum_array) :: wres,w1
    type(IXTstatus) :: status
    call IXFcheck_datum_array(wres,status)
    return
  end subroutine finish_op_datum_array


  pure function IXFSize_Datum_array(w1) result(x)
    !this function will return the length of the datum array passed to it
    type(IXTdatum_array),intent(in) :: w1
    integer(i4b) :: x
    x=size(w1%signal)
  end function IXFSize_Datum_array

end module IXMdatum_array

