!------------------------------
! MODULE: IXMdataset_2d
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMdataset_2d object 
module IXMdataset_2d
  use IXMdataset_common
  use IXMdataset_1d
  implicit none
  public :: IXTdataset_2d
  type IXTdataset_2d
     private
     type(IXTbase) :: base
     character(len=long_len),allocatable ::title(:) !! Title of dataset for plotting purposes
     real(dp), pointer :: signal(:,:)=>NULL()
     real(dp), pointer:: error(:,:)=>NULL()
     type(IXTaxis):: s_axis !! Signal axis title (e.g. 'Counts')
     real(dp), pointer :: x(:)=>NULL()  !! x-axis (either histogram boundaries or point data)
     type(IXTaxis):: x_axis !! axis along x-axis e.g. 'meV' or 'microseconds' [NeXus: axis attribute]
     !! x-distribution
     !! - If X_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If X_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x_distribution=.false.
     real(dp), pointer :: y(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: y_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: y_distribution=.false.
  end type IXTdataset_2d
#define IXD_TYPE	dataset_2d
#include "class_header.f90"

  private ::  ts_plus_dataset_2d, tt_plus_dataset_2d, st_plus_dataset_2d 
  private ::  ts_minus_dataset_2d, tt_minus_dataset_2d, st_minus_dataset_2d
  private ::  ts_times_dataset_2d, tt_times_dataset_2d, st_times_dataset_2d
  private ::  ts_divide_dataset_2d, tt_divide_dataset_2d, st_divide_dataset_2d
  private ::  ts_power_dataset_2d, tt_power_dataset_2d, st_power_dataset_2d
  private ::  setup_unary_op_dataset_2d,setup_binary_op_dataset_2d,finish_op_dataset_2d
  private ::  x_hist,y_hist, setup_binary_common_dataset_2d
  private ::  setup_binary_X_special_dataset_2d,setup_binary_Y_special_dataset_2d
  private :: units_single_array, units_array_array, units_array, units_common
  private :: integrate_x_dataset_2d,integrate_x_arr_dataset_2d,integrate_xd2d_dataset_2d

  private ::  ta_plus_dataset_2d, at_plus_dataset_2d 
  private ::  ta_minus_dataset_2d, at_minus_dataset_2d
  private ::  ta_times_dataset_2d, at_times_dataset_2d
  private ::  ta_divide_dataset_2d, at_divide_dataset_2d
  private ::  ta_power_dataset_2d, at_power_dataset_2d

  private ::  t_at_dataset1d_plus_dataset_2d, at_t_dataset1d_plus_dataset_2d 
  private ::  t_at_dataset1d_minus_dataset_2d, at_t_dataset1d_minus_dataset_2d
  private ::  t_at_dataset1d_times_dataset_2d, at_t_dataset1d_times_dataset_2d
  private ::  t_at_dataset1d_divide_dataset_2d, at_t_dataset1d_divide_dataset_2d
  private ::  t_at_dataset1d_power_dataset_2d, at_t_dataset1d_power_dataset_2d

  private ::  t_at_dataset2d_plus_dataset_2d, at_t_dataset2d_plus_dataset_2d 
  private ::  t_at_dataset2d_minus_dataset_2d, at_t_dataset2d_minus_dataset_2d
  private ::  t_at_dataset2d_times_dataset_2d, at_t_dataset2d_times_dataset_2d
  private ::  t_at_dataset2d_divide_dataset_2d, at_t_dataset2d_divide_dataset_2d
  private ::  t_at_dataset2d_power_dataset_2d, at_t_dataset2d_power_dataset_2d


  private :: d1d2_dataset_1d_X_plus_dataset_2d,d2d1_dataset_1d_X_plus_dataset_2d
  private :: d1d2_dataset_1d_Y_plus_dataset_2d,d2d1_dataset_1d_Y_plus_dataset_2d
  private :: d1d2_dataset_1d_X_minus_dataset_2d,d2d1_dataset_1d_X_minus_dataset_2d
  private :: d1d2_dataset_1d_Y_minus_dataset_2d,d2d1_dataset_1d_Y_minus_dataset_2d
  private :: d1d2_dataset_1d_X_times_dataset_2d,d2d1_dataset_1d_X_times_dataset_2d
  private :: d1d2_dataset_1d_Y_times_dataset_2d,d2d1_dataset_1d_Y_times_dataset_2d
  private :: d1d2_dataset_1d_X_divide_dataset_2d,d2d1_dataset_1d_X_divide_dataset_2d
  private :: d1d2_dataset_1d_Y_divide_dataset_2d,d2d1_dataset_1d_Y_divide_dataset_2d
  private :: d1d2_dataset_1d_X_power_dataset_2d,d2d1_dataset_1d_X_power_dataset_2d
  private :: d1d2_dataset_1d_Y_power_dataset_2d,d2d1_dataset_1d_Y_power_dataset_2d

  interface IXFdataset_1d_X_plus_dataset_2d
     module procedure d1d2_dataset_1d_X_plus_dataset_2d,d2d1_dataset_1d_X_plus_dataset_2d
  end interface
  interface IXFplus_X_dataset_2d
     module procedure IXFarray_X_plus_dataset_2d,d1d2_dataset_1d_X_plus_dataset_2d,d2d1_dataset_1d_X_plus_dataset_2d
     module procedure IXFdataset_2d_X_plus_dataset_2d      
  end interface
  interface IXFplus_X
     module procedure IXFarray_X_plus_dataset_2d,d1d2_dataset_1d_X_plus_dataset_2d,d2d1_dataset_1d_X_plus_dataset_2d
     module procedure IXFdataset_2d_X_plus_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_Y_plus_dataset_2d
     module procedure d1d2_dataset_1d_Y_plus_dataset_2d,d2d1_dataset_1d_Y_plus_dataset_2d
  end interface
  interface IXFplus_Y_dataset_2d
     module procedure IXFarray_Y_plus_dataset_2d,d1d2_dataset_1d_Y_plus_dataset_2d,d2d1_dataset_1d_Y_plus_dataset_2d
     module procedure IXFdataset_2d_Y_plus_dataset_2d      
  end interface
  interface IXFplus_Y
     module procedure IXFarray_Y_plus_dataset_2d,d1d2_dataset_1d_Y_plus_dataset_2d,d2d1_dataset_1d_Y_plus_dataset_2d
     module procedure IXFdataset_2d_Y_plus_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_X_minus_dataset_2d
     module procedure d1d2_dataset_1d_X_minus_dataset_2d,d2d1_dataset_1d_X_minus_dataset_2d
  end interface

  interface IXFminus_X_dataset_2d
     module procedure IXFarray_X_minus_dataset_2d,d1d2_dataset_1d_X_minus_dataset_2d,d2d1_dataset_1d_X_minus_dataset_2d
     module procedure IXFdataset_2d_X_minus_dataset_2d      
  end interface
  interface IXFminus_X
     module procedure IXFarray_X_minus_dataset_2d,d1d2_dataset_1d_X_minus_dataset_2d,d2d1_dataset_1d_X_minus_dataset_2d
     module procedure IXFdataset_2d_X_minus_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_Y_minus_dataset_2d
     module procedure d1d2_dataset_1d_Y_minus_dataset_2d,d2d1_dataset_1d_Y_minus_dataset_2d
  end interface
  interface IXFminus_Y_dataset_2d
     module procedure IXFarray_Y_minus_dataset_2d,d1d2_dataset_1d_Y_minus_dataset_2d,d2d1_dataset_1d_Y_minus_dataset_2d
     module procedure IXFdataset_2d_Y_minus_dataset_2d      
  end interface
  interface IXFminus_Y
     module procedure IXFarray_Y_minus_dataset_2d,d1d2_dataset_1d_Y_minus_dataset_2d,d2d1_dataset_1d_Y_minus_dataset_2d
     module procedure IXFdataset_2d_Y_minus_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_X_times_dataset_2d
     module procedure d1d2_dataset_1d_X_times_dataset_2d,d2d1_dataset_1d_X_times_dataset_2d
  end interface
  interface IXFtimes_X_dataset_2d
     module procedure IXFarray_X_times_dataset_2d,d1d2_dataset_1d_X_times_dataset_2d,d2d1_dataset_1d_X_times_dataset_2d
     module procedure IXFdataset_2d_X_times_dataset_2d      
  end interface
  interface IXFtimes_X
     module procedure IXFarray_X_times_dataset_2d,d1d2_dataset_1d_X_times_dataset_2d,d2d1_dataset_1d_X_times_dataset_2d
     module procedure IXFdataset_2d_X_times_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_Y_times_dataset_2d
     module procedure d1d2_dataset_1d_Y_times_dataset_2d,d2d1_dataset_1d_Y_times_dataset_2d
  end interface
  interface IXFtimes_Y_dataset_2d
     module procedure IXFarray_Y_times_dataset_2d,d1d2_dataset_1d_Y_times_dataset_2d,d2d1_dataset_1d_Y_times_dataset_2d
     module procedure IXFdataset_2d_Y_times_dataset_2d      
  end interface
  interface IXFtimes_Y
     module procedure IXFarray_Y_times_dataset_2d,d1d2_dataset_1d_Y_times_dataset_2d,d2d1_dataset_1d_Y_times_dataset_2d
     module procedure IXFdataset_2d_Y_times_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_X_divide_dataset_2d
     module procedure d1d2_dataset_1d_X_divide_dataset_2d,d2d1_dataset_1d_X_divide_dataset_2d
  end interface
  interface IXFdivide_X_dataset_2d
     module procedure IXFarray_X_divide_dataset_2d,d1d2_dataset_1d_X_divide_dataset_2d,d2d1_dataset_1d_X_divide_dataset_2d
     module procedure IXFdataset_2d_X_divide_dataset_2d      
  end interface
  interface IXFdivide_X
     module procedure IXFarray_X_divide_dataset_2d,d1d2_dataset_1d_X_divide_dataset_2d,d2d1_dataset_1d_X_divide_dataset_2d
     module procedure IXFdataset_2d_X_divide_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_Y_divide_dataset_2d
     module procedure d1d2_dataset_1d_Y_divide_dataset_2d,d2d1_dataset_1d_Y_divide_dataset_2d
  end interface
  interface IXFdivide_Y_dataset_2d
     module procedure IXFarray_Y_divide_dataset_2d,d1d2_dataset_1d_Y_divide_dataset_2d,d2d1_dataset_1d_Y_divide_dataset_2d
     module procedure IXFdataset_2d_Y_divide_dataset_2d      
  end interface
  interface IXFdivide_Y
     module procedure IXFarray_Y_divide_dataset_2d,d1d2_dataset_1d_Y_divide_dataset_2d,d2d1_dataset_1d_Y_divide_dataset_2d
     module procedure IXFdataset_2d_Y_divide_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_X_power_dataset_2d
     module procedure d1d2_dataset_1d_X_power_dataset_2d,d2d1_dataset_1d_X_power_dataset_2d
  end interface
  interface IXFpower_X_dataset_2d
     module procedure IXFarray_X_power_dataset_2d,d1d2_dataset_1d_X_power_dataset_2d,d2d1_dataset_1d_X_power_dataset_2d
     module procedure IXFdataset_2d_X_power_dataset_2d      
  end interface
  interface IXFpower_X
     module procedure IXFarray_X_power_dataset_2d,d1d2_dataset_1d_X_power_dataset_2d,d2d1_dataset_1d_X_power_dataset_2d
     module procedure IXFdataset_2d_X_power_dataset_2d      
  end interface
  !*****************
  interface IXFdataset_1d_Y_power_dataset_2d
     module procedure d1d2_dataset_1d_Y_power_dataset_2d,d2d1_dataset_1d_Y_power_dataset_2d
  end interface
  interface IXFpower_Y_dataset_2d
     module procedure IXFarray_Y_power_dataset_2d,d1d2_dataset_1d_Y_power_dataset_2d,d2d1_dataset_1d_Y_power_dataset_2d
     module procedure IXFdataset_2d_Y_power_dataset_2d      
  end interface
  interface IXFpower_Y
     module procedure IXFarray_Y_power_dataset_2d,d1d2_dataset_1d_Y_power_dataset_2d,d2d1_dataset_1d_Y_power_dataset_2d
     module procedure IXFdataset_2d_Y_power_dataset_2d      
  end interface
    !*****************
  interface IXFintegrate_x_dataset_2d
     module procedure integrate_x_dataset_2d,integrate_x_arr_dataset_2d,integrate_xd2d_dataset_2d
  end interface

  !! Interface for subroutine to add two dataset_2d objects, a dataset_2d to an array of dataset_1d's or vice versa,
  !!a dataset_2d to a 2d array or vice versa or a dataset_2d to a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFplus_dataset_2d(wres,w1,w2,status) Type/Type 
  !!n call IXFplus_dataset_2d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFplus_dataset_2d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFplus_dataset_2d(wres,w1,arg2,status) Type/Array 
  !!n call IXFplus_dataset_2d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_2d object
  !! -w#	    -> input dataset_2d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	 
  interface IXFplus_dataset_2d
     module procedure t_at_dataset1d_plus_dataset_2d,at_t_dataset1d_plus_dataset_2d
     module procedure ta_Plus_dataset_2d , at_plus_dataset_2d
     module procedure ts_plus_dataset_2d, tt_plus_dataset_2d, st_plus_dataset_2d
     module procedure t_at_dataset2d_plus_dataset_2d,at_t_dataset2d_plus_dataset_2d 
  end interface
  interface IXFplus
     module procedure t_at_dataset1d_plus_dataset_2d,at_t_dataset1d_plus_dataset_2d  
     module procedure ts_plus_dataset_2d, tt_plus_dataset_2d, st_plus_dataset_2d
     module procedure at_Plus_dataset_2d, ta_Plus_dataset_2d
     module procedure t_at_dataset2d_plus_dataset_2d,at_t_dataset2d_plus_dataset_2d 
  end interface

  !! Interface for subroutine to subtract two dataset_2d objects, a dataset_2d from an array of dataset1d's or vice versa,
  !! a dataset_2d from a 2d array or vice versa or a dataset_2d from a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFminus_dataset_2d(wres,w1,w2,status) Type/Type 
  !!n call IXFminus_dataset_2d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFminus_dataset_2d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFminus_dataset_2d(wres,w1,arg2,status) Type/Array 
  !!n call IXFminus_dataset_2d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_2d object
  !! -w#	    -> input dataset_2d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	 
  interface IXFminus_dataset_2d
     module procedure t_at_dataset1d_minus_dataset_2d,at_t_dataset1d_minus_dataset_2d  
     module procedure at_minus_dataset_2d , ta_minus_dataset_2d
     module procedure ts_minus_dataset_2d, tt_minus_dataset_2d, st_minus_dataset_2d
     module procedure t_at_dataset2d_minus_dataset_2d,at_t_dataset2d_minus_dataset_2d      
  end interface
  interface IXFminus
     module procedure t_at_dataset1d_minus_dataset_2d,at_t_dataset1d_minus_dataset_2d
     module procedure at_minus_dataset_2d , ta_minus_dataset_2d
     module procedure ts_minus_dataset_2d, tt_minus_dataset_2d, st_minus_dataset_2d
     module procedure t_at_dataset2d_minus_dataset_2d,at_t_dataset2d_minus_dataset_2d 
  end interface

  !! Interface for subroutine to multiply two dataset_2d objects, a dataset_2d with an array of dataset1d's or vice versa,
  !! a dataset_2d with a 2d array or vice versa or a dataset_2d with a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFtimes_dataset_2d(wres,w1,w2,status) Type/Type 
  !!n call IXFtimes_dataset_2d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFtimes_dataset_2d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFtimes_dataset_2d(wres,w1,arg2,status) Type/Array 
  !!n call IXFtimes_dataset_2d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_2d object
  !! -w#	    -> input dataset_2d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	 
  interface IXFtimes_dataset_2d
     module procedure t_at_dataset1d_times_dataset_2d,at_t_dataset1d_times_dataset_2d
     module procedure at_times_dataset_2d, ta_times_dataset_2d
     module procedure ts_times_dataset_2d, tt_times_dataset_2d, st_times_dataset_2d
     module procedure t_at_dataset2d_times_dataset_2d,at_t_dataset2d_times_dataset_2d 
  end interface
  interface IXFtimes
     module procedure t_at_dataset1d_times_dataset_2d,at_t_dataset1d_times_dataset_2d
     module procedure at_times_dataset_2d, ta_times_dataset_2d
     module procedure ts_times_dataset_2d, tt_times_dataset_2d, st_times_dataset_2d
     module procedure t_at_dataset2d_times_dataset_2d,at_t_dataset2d_times_dataset_2d 
  end interface

  !! Interface for subroutine to divide two dataset_2d objects, a dataset_2d by an array of dataset1d's or vice versa,
  !! a dataset_2d by a 2d array or vice versa or a dataset_2d by a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFdivide_dataset_2d(wres,w1,w2,status) Type/Type 
  !!n call IXFdivide_dataset_2d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFdivide_dataset_2d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFdivide_dataset_2d(wres,w1,arg2,status) Type/Array 
  !!n call IXFdivide_dataset_2d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_2d object
  !! -w#	    -> input dataset_2d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object
  interface IXFdivide_dataset_2d
     module procedure t_at_dataset1d_divide_dataset_2d,at_t_dataset1d_divide_dataset_2d
     module procedure at_divide_dataset_2d, ta_divide_dataset_2d  
     module procedure ts_divide_dataset_2d, tt_divide_dataset_2d, st_divide_dataset_2d
     module procedure t_at_dataset2d_divide_dataset_2d,at_t_dataset2d_divide_dataset_2d 
  end interface
  interface IXFdivide
     module procedure t_at_dataset1d_divide_dataset_2d,at_t_dataset1d_divide_dataset_2d
     module procedure at_divide_dataset_2d, ta_divide_dataset_2d  
     module procedure ts_divide_dataset_2d, tt_divide_dataset_2d, st_divide_dataset_2d
     module procedure t_at_dataset2d_divide_dataset_2d,at_t_dataset2d_divide_dataset_2d 
  end interface


  !! Interface for subroutine to raise one dataset_2d object to the power of another, or a dataset_2d to a scalar or vice versa.   
  !!n naming convention:  tt = type,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFpower_dataset_2d(wres,w1,w2,status) Type/Type 
  !!n call IXFpower_dataset_2d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFpower_dataset_2d(wres,arg1,w2,status) Scalar/Type 
  !!
  !! -wres	-> output dataset_2d object
  !! -w#	    -> input dataset_2d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	
  interface IXFpower_dataset_2d
     module procedure t_at_dataset1d_power_dataset_2d,at_t_dataset1d_power_dataset_2d
     module procedure at_power_dataset_2d, ta_power_dataset_2d  
     module procedure ts_power_dataset_2d, tt_power_dataset_2d, st_power_dataset_2d
     module procedure t_at_dataset2d_power_dataset_2d,at_t_dataset2d_power_dataset_2d 
  end interface
  interface IXFpower
     module procedure t_at_dataset1d_power_dataset_2d,at_t_dataset1d_power_dataset_2d
     module procedure at_power_dataset_2d, ta_power_dataset_2d  
     module procedure ts_power_dataset_2d, tt_power_dataset_2d, st_power_dataset_2d
     module procedure t_at_dataset2d_power_dataset_2d,at_t_dataset2d_power_dataset_2d 
  end interface
  !


  interface IXFexp
     module procedure IXFexp_dataset_2d
  end interface

  interface IXFlog
     module procedure IXFlog_dataset_2d
  end interface

  interface IXFlog10
     module procedure IXFlog10_dataset_2d
  end interface

  interface IXFsin
     module procedure IXFsin_dataset_2d
  end interface

  interface IXFcos
     module procedure IXFcos_dataset_2d
  end interface

  interface IXFtan
     module procedure IXFtan_dataset_2d
  end interface

  interface IXFsinh
     module procedure IXFsinh_dataset_2d
  end interface

  interface IXFcosh
     module procedure IXFcosh_dataset_2d
  end interface

  interface IXFtanh
     module procedure IXFtanh_dataset_2d
  end interface
  
  interface IXFderiv1x
     module procedure IXFderiv1x_dataset_2d
  end interface

  interface IXFderiv2x
     module procedure IXFderiv2x_dataset_2d
  end interface

  interface IXFunits_dataset_2d
     module procedure units_single_array, units_array_array, units_array
  end interface

  interface IXFunits
     module procedure units_single_array, units_array_array, units_array
  end interface

  interface IXFunspike
     module procedure IXFunspike_dataset_2d
  end interface

contains

#define IXD_DESCRIPTION	"IXTdataset_2d class"
#define IXD_TYPE dataset_2d
#define IXD_SQTYPE 'dataset_2d'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_dataset_2d(op, field, arg, status)
    implicit none
    type(IXTdataset_2d),intent(inout) :: arg
    type(IXToperation),intent(inout) :: op
    type(IXTstatus),intent(inout) :: status
    character(len=*),intent(in) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdataset_2d', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_alloc(op,'title', arg%title, status)
    call IXFoperation_run_ptr(op,'signal', arg%signal, status)
    call IXFoperation_run_ptr(op,'error',arg%error,status)
    call IXFoperation_run(op,'s_axis', arg%s_axis, status)
    call IXFoperation_run_ptr(op,'x', arg%x, status)
    call IXFoperation_run(op,'x_axis', arg%x_axis, status)
    call IXFoperation_run(op,'x_distribution', arg%x_distribution, status)
    call IXFoperation_run_ptr(op,'y', arg%y, status)
    call IXFoperation_run(op,'y_axis', arg%y_axis, status)
    call IXFoperation_run(op,'y_distribution', arg%y_distribution, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_dataset_2d(w1,x,y,signal,error,x_axis,y_axis,s_axis)
    implicit none
    real(dp),pointer,optional::signal(:,:) !!output: signal array pointer
    real(dp),pointer,optional::error(:,:) !!output: error array pointer
    real(dp),pointer,optional::x(:) !!output: x-array pointer
    real(dp),pointer,optional::y(:) !!output: y-array pointer
    type(IXTaxis),pointer,optional::x_axis,y_axis,s_axis
    type(IXTdataset_2d),intent(in),target::w1 !!input dataset_2d 

    if (present(signal))signal=>w1%signal
    if (present(error))error=>w1%error
    if (present(x))x=>w1%x
    if (present(y))y=>w1%y
    if (present(x_axis))x_axis=>w1%x_axis
    if (present(y_axis))y_axis=>w1%y_axis
    if (present(s_axis))s_axis=>w1%s_axis
    
  end subroutine IXFget_ptr_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc can be called with all the same arguments as IXFget, but the pointer array elements can be allocatable arrays.
  !! the arrays are allocated to the appropriate length and IXF get is called underneath to fill them up.

  subroutine IXFget_alloc_dataset_2d(w1,status,title,signal,error,s_axis,x,&
       x_axis,x_distribution,y,y_axis,y_distribution,wout)
    implicit none
    type(IXTdataset_2d),optional,intent(out):: wout !!input:: object passed if a copy of a dataset is required
    character(len=*),optional,allocatable :: title(:) !!input: title passed    
    ! intent for arguments below would normally be OUT but for compiler issues
    real(dp),allocatable,optional::signal(:,:) !!signal array 
    real(dp),allocatable,optional::error(:,:) !!error array 
    real(dp),allocatable,optional::x(:) !!x-array 
    real(dp),allocatable,optional::y(:) !!y-array
    type(IXTaxis),optional,intent(out) :: s_axis !!!!input: s_axis passed
    type(IXTaxis),optional,intent(out) :: x_axis !!!!input: s_axis passed
    logical,optional,intent(out) :: x_distribution    !!input: x distribution flag 
    type(IXTaxis),optional,intent(out) :: y_axis !!!!input: s_axis passed
    logical,optional,intent(out) :: y_distribution    !!input:y distribution flag 
    type(IXTstatus),intent(inout)::status
    type(IXTdataset_2d),intent(in)::w1 !!input dataset_2d 

    if (present(title))then
       call IXFreallocdimsfortran(title,(/ size(w1%title) /),.false.,status)
    endif

    if (present(signal))then
       call IXFreallocdimsFortran(signal,shape(w1%signal),.false.,status)
    endif

    if (present(error))then
       call IXFreallocdimsFortran(error,shape(w1%error),.false.,status)
    endif

    if (present(x))then
       call IXFreallocdimsFortran(x,shape(w1%x),.false.,status)
    endif
    if (present(y))then
       call IXFreallocdimsFortran(y,shape(w1%y),.false.,status)
    endif
    call IXFget_dataset_2d(w1,status,title,signal,error,s_axis,x,&
         x_axis,x_distribution,y,y_axis,y_distribution,wout)

  end subroutine IXFget_alloc_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_dataset_2d(dataset_2d,status,title,signal,error,s_axis,x,&
       x_axis,x_distribution,y,y_axis,y_distribution,wout)
    implicit none
    type(IXTdataset_2d),intent(in)::dataset_2d !!input: dataset_2d used to fill output
    type(IXTdataset_2d),optional,intent(out):: wout !!input:: object passed if a copy of a dataset is required
    character(len=*),optional,intent(out) :: title(:) !!input: title passed
    real(dp),optional,intent(out) :: signal(:,:)!!input: signal ARRAY passed
    real(dp),optional,intent(out):: error(:,:) !!input: error ARRAY passed
    type(IXTaxis),optional,intent(out) :: s_axis !!!!input: s_axis passed
    real(dp), optional,intent(out) :: x(:) !!input: x ARRAY passed    
    type(IXTaxis),optional,intent(out) :: x_axis !!!!input: s_axis passed
    logical,optional,intent(out) :: x_distribution    !!input: x distribution flag 
    real(dp), optional,intent(out) :: y(:) !!input: y ARRAY passed
    type(IXTaxis),optional,intent(out) :: y_axis !!!!input: s_axis passed
    logical,optional,intent(out) :: y_distribution    !!input:y distribution flag 
    type(IXTstatus),intent(inout)::status

    if(present(wout))call IXFcopy(dataset_2d,wout,status)

    call IXFget_real_array(dataset_2d%x,status,x)
    call IXFget_real_array(dataset_2d%y,status,y)
    call IXFget_real_array(dataset_2d%signal,status,signal)
    call IXFget_real_array(dataset_2d%error,status,error)

    if (present(title))title=dataset_2d%title
    if (present(x_distribution))x_distribution=dataset_2d%x_distribution

    if (present(y_distribution))y_distribution=dataset_2d%y_distribution
    if (present(s_axis))call IXFcopy(dataset_2d%s_axis,s_axis,status)
    if (present(x_axis))call IXFcopy(dataset_2d%x_axis,x_axis,status)
    if (present(y_axis))call IXFcopy(dataset_2d%y_axis,y_axis,status)

  end subroutine IXFget_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which when given the appropriate characteristics of a dataset_2d object 
  !! (lengths x and signal array, histogram and distribution data flags) will assign memory for its elements,
  !! the object may then be filled with data in another function/subroutine
  subroutine IXFmake_dataset_2d(dataset_2d,nx,ny,xdist,xhist,ydist,yhist,status)
    implicit none
    integer(i4b),intent(in)::nx !! input: length of x array of dataset_2d object to be created
    integer(i4b),intent(in)::ny !! input: length of y array of dataset_2d object to be created
    type(IXTdataset_2d),intent(out)::dataset_2d  !! dataset_2d object to be created to be created
    logical,intent(in)::xhist !! x_histogram flag of dataset_2d object to be created
    logical,intent(in)::xdist !! x_distribtion of dataset_2d object to be created
    logical,intent(in)::yhist !! y_histogram flag of dataset_2d object to be created
    logical,intent(in)::ydist !! y_distribtion of dataset_2d object to be created

    type(IXTstatus),intent(inout)::status !! error status object
    integer(i4b)::sx,sy ! size of error/signal array along each dimension

    if(xhist)then
       sx=nx-1
    else
       sx=nx
    endif

    if(yhist)then
       sy=ny-1
    else
       sy=ny
    endif

    call IXFrealloc(dataset_2d%x,nx,.false.,status)
    call IXFrealloc(dataset_2d%y,ny,.false.,status)
    call IXFreallocdims(dataset_2d%signal,(/ sx,sy /),.false.,status)
    call IXFreallocdims(dataset_2d%error,(/ sx,sy /),.false.,status)

    dataset_2d%x_distribution=xdist

    dataset_2d%y_distribution=ydist

    call IXFmark_valid(dataset_2d)

  end subroutine IXFmake_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_dataset_2d(dataset_2d,title,signal,error,s_axis,x,x_axis,  &
       x_distribution,y,y_axis,y_distribution,status)
    implicit none
    type(IXTdataset_2d),intent(out)::dataset_2d  !! the dataset_1d object to be created
    character(len=*),intent(in) :: title(:) !!input: title passed
    real(dp),intent(in) :: signal(:,:)!!input: signal ARRAY passed
    real(dp),intent(in):: error(:,:) !!input: error ARRAY passed
    real(dp),intent(in) :: x(:),y(:) !!input: x/y ARRAY passed
    type(IXTaxis),intent(in) :: s_axis,x_axis,y_axis	 !!input: x/y_axis passed
    logical,intent(in) :: x_distribution,y_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status !! error status object

    ! nested objects should be tested for initialisation, this shows they have been created properly

    if( IXFvalid(s_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFcreate_dataset_2d)')
    endif

    if( IXFvalid(x_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFcreate_dataset_2d)')
    endif

    if( IXFvalid(y_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFcreate_dataset_2d)')
    endif


    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(dataset_2d)

    call IXFset_dataset_2d(dataset_2d,status,title,signal,error,s_axis,x,x_axis,  &
         x_distribution,y,y_axis,y_distribution)

  end subroutine IXFcreate_dataset_2d

  !! IXFcreate_xyze_dataset_2d, will create an IXTdataset_2d object using an x,y,signal,error array as input, 
  !! the rest of the values will be default 
  subroutine IXFcreatexyze_dataset_2d(dataset_2d,x,y,signal,error,status)
    implicit none
    type(IXTdataset_2d),intent(out)::dataset_2d
    real(dp),intent(in)::x(:),y(:),signal(:,:),error(:,:) !! arrays to populate the object with
    type(IXTstatus),intent(inout)::status
    type(IXTaxis):: s_axis,x_axis,y_axis !! default axis labels
    logical :: x_dist,y_dist
    character(long_len),allocatable::nullcaption(:)
    character(len=long_len) ::  title(1)	!! default title
    title(1)='title'
    x_dist=.false.
    y_dist=.false.
    allocate(nullcaption(1))
    nullcaption=' '
    call IXFcreate_axis(s_axis,nullcaption,IXCnullunits,IXCnullcode,status)
    call IXFcreate_axis(x_axis,nullcaption,IXCnullunits,IXCnullcode,status)
    call IXFcreate_axis(y_axis,nullcaption,IXCnullunits,IXCnullcode,status)
    deallocate(nullcaption)
    call IXFcreate_dataset_2d(dataset_2d,title,signal,error,s_axis,x,x_axis,  &
         x_dist,y,y_axis,y_dist,status)

  end subroutine IXFcreatexyze_dataset_2d
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_dataset_2d(dataset_2d,status,title,signal,error,s_axis,  &
       x,x_axis,x_distribution,y,y_axis,y_distribution,ref)
    implicit none
    type(IXTdataset_2d),intent(inout)::dataset_2d
    type(IXTdataset_2d),intent(in),optional::ref  !! reference dataset_2d
    character(len=*),optional,intent(in) :: title(:)	
    real(dp),optional,intent(in) :: signal(:,:), error(:,:) 
    real(dp), optional,intent(in) :: x(:),y(:)
    type(IXTaxis),optional,intent(in) :: s_axis,x_axis,y_axis	

    logical,optional,intent(in) :: x_distribution, y_distribution    
    type(IXTstatus),intent(inout)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_dataset_2d)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(dataset_2d)
    else    
       if(IXFvalid(dataset_2d) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_dataset_2d)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_dataset_2d(dataset_2d,status,ref%title,ref%signal,ref%error, &
         ref%s_axis,ref%x,ref%x_axis,ref%x_distribution,ref%y,ref%y_axis,ref%y_distribution)

    if (present(title))then
       call IXFreallocFortran(dataset_2d%title,size(title),.false.,status)
       dataset_2d%title=title
    endif
    if (present(x_distribution))dataset_2d%x_distribution=x_distribution
    if (present(y_distribution))dataset_2d%y_distribution=y_distribution

    if (present(s_axis))call IXFcopy(s_axis,dataset_2d%s_axis,status)
    if (present(x_axis))call IXFcopy(x_axis,dataset_2d%x_axis,status)
    if (present(y_axis))call IXFcopy(y_axis,dataset_2d%y_axis,status)

    call IXFset_real_array(dataset_2d%x,status,x)
    call IXFset_real_array(dataset_2d%y,status,y)    
    call IXFset_real_array(dataset_2d%signal,status,signal)
    call IXFset_real_array(dataset_2d%error,status,error)

    call IXFcheck_dataset_2d(dataset_2d,status)

  end subroutine IXFset_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_dataset_2d(w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status !! error status object
    type(IXTdataset_2d),intent(in) :: w1 !! dataset_2d to be checked
    integer :: xflag, yflag, sxflag, syflag 

    call IXFcheck(w1%x_axis,status)
    call IXFcheck(w1%s_axis,status)
    call IXFcheck(w1%y_axis,status)

    if( .not. associated(w1%y))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%y must be associated with an array (IXFcheck_dataset_2d)')
    endif
    if( .not. associated(w1%x))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%x must be associated with an array (IXFcheck_dataset_2d)')
    endif
    if( .not. associated(w1%signal))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%signal must be associated with an array (IXFcheck_dataset_2d)')
    endif
    if( .not. associated(w1%error))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%error must be associated with an array (IXFcheck_dataset_2d)')
    endif

    if( .not. allocated(w1%title))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%title array must be allocated (IXFcheck_dataset_2d)')
    endif


    if(status == IXCseverity_error)return
    sxflag=size(w1%signal,1)
    syflag=size(w1%signal,2)    
    xflag=size(w1%x)
    yflag=size(w1%y)

    !check for empty array
    if(size(w1%signal) < 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'must have at least one element in signal array  (IXFcheck_dataset_2d)')
    endif

    !check no. of error == no. of signal in 2D respect
    if (sum(abs(shape(w1%signal) - shape(w1%error)))/=0  )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'signal and error data incompatible  (IXFcheck_dataset_2d) ')
    endif

    ! check it is either point or histogram data
    if(xflag /= sxflag)then
       if(xflag /= sxflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined along x-axis (IXFcheck_dataset_2d)')
       endif
    endif

    if(yflag /= syflag)then
       if(yflag /= syflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined along y-axis (IXFcheck_dataset_2d)')
       endif
    endif


  end subroutine IXFcheck_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to perform integration along X dimension of a dataset_2d to give a dataset_1d output. Integration
  !! will be performed irrespective of point or histogram data.
  subroutine integrate_x_dataset_2d(wres,w1,x1,x2,status)
    use IXMintegrate
    implicit none
    type(IXTdataset_1d),intent(out)::wres !!output: dataset_1d
    type(IXTdataset_2d),intent(in)::w1 !! input: dataset_2d
    type(IXTstatus),intent(inout)::status !! error status object
    real(dp),intent(in)::x1,x2 !! integration limits along first dimension 
    real(dp),pointer::x_ptr(:),s_ptr(:),e_ptr(:)!!temporary pointers used to fill output dataset_1d object
    logical::hist,dist !!local variables to describe histogram and distribution data characteristics


    ! here we describe the characteristics of the output dataset_1d
    ! it will have the dataset_2d y properties
    hist=y_hist(w1)
    ! in all cases when we integrate in 1-dimension the output(wres) will not be a distribution
    dist=.false.
    ! create the dtaset_1d with the appropriate characteristics length, nx, etc...
    !    call IXFinitialise(wres,status)!need a default dataset_1d which is initialised
    call IXFmake_dataset_1d(wres,size(w1%y),dist,hist,status)

    ! allocate output dataset_1d memory space

    call IXFget_ptr_dataset_1d(wres,x_ptr,s_ptr,e_ptr)


    call IXFintegrate_2d_hist(w1%signal,w1%error,status,x=w1%x,y=w1%y,xdist=w1%x_distribution,xhist=x_hist(w1),xmin_in=x1,&
         xmax_in=x2,val_ar_in=s_ptr,err_ar_in=e_ptr,x_ar=x_ptr)
    call IXFset_dataset_1d(wres,status,title=w1%title,x_axis=w1%y_axis,s_axis=w1%s_axis,x_distribution=.false.)
    ! not necessary set calls a check
    !    call IXFcheck_dataset_1d(wres,status)

  end subroutine integrate_x_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to perform integration along X dimension of a dataset_2d to give a dataset_2d output. Integration
  !! will be performed irrespective of point or histogram data.
  subroutine integrate_xd2d_dataset_2d(wres,w1,x1,x2,status)
    use IXMintegrate
    implicit none
    type(IXTdataset_2d),intent(out)::wres !!output: dataset_1d
    type(IXTdataset_2d),intent(in)::w1 !! input: dataset_2d
    type(IXTstatus),intent(inout)::status !! error status object
    real(dp),intent(in)::x1,x2 !! integration limits along first dimension 
    real(dp)::setx1,setx2

    ! here we describe the characteristics of the output dataset_2d
    ! it will have the dataset_2d y properties
    ! in all cases when we integrate in 1-dimension the output(wres) will not be a distribution
    wres%y_distribution=w1%y_distribution
    wres%x_distribution=.false.
    ! create the dataset_2d with the appropriate characteristics length, nx, etc...

    call IXFallocdims(wres%signal, (/ 1 , size(w1%signal,2) /), status)
    call IXFallocdims(wres%error, (/ 1 , size(w1%signal,2) /), status)
    call IXFalloc(wres%x,2,status)

    if(isnan(x1))then
       setx1=w1%x(1)
    else
       setx1=x1
    endif
    if(isnan(x2))then
       setx2=w1%x(size(w1%x))
    else
       setx2=x2
    endif

    call IXFintegrate_2d_hist(w1%signal,w1%error,status,x=w1%x,y=(/ setx1, setx2 /),xdist=w1%x_distribution,xhist=x_hist(w1),xmin_in=x1,&
         xmax_in=x2,val_ar_in=wres%signal(1,:),err_ar_in=wres%error(1,:) ,x_ar=wres%x)

    call IXFmark_valid(wres)


    call IXFset_dataset_2d(wres,status,y=w1%y,y_axis=w1%y_axis,y_distribution=.false.,&
         x_distribution=.false.,title=w1%title,s_axis=w1%s_axis,x_axis=w1%x_axis)


  end subroutine integrate_xd2d_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to perform integration along X dimension of a dataset_2d to give a dataset_1d output. Integration
  !! will be performed irrespective of point or histogram data, the limits are supplied in two arrays, and can be different
  !! for each (:,i)
  subroutine integrate_x_arr_dataset_2d(wres,d2d,x1,x2,status)
    use IXMintegrate
    implicit none
    type(IXTdataset_1d),intent(out)::wres !!output: dataset_1d
    type(IXTdataset_2d),intent(in)::d2d !! input: dataset_2d
    type(IXTstatus),intent(inout)::status !! error status object
    real(dp),intent(in)::x1(:),x2(:) !! integration limits along first dimension 
    real(dp),pointer::x_ptr(:),s_ptr(:),e_ptr(:)!!temporary pointers used to fill output dataset_1d object
    logical::hist,dist !!local variables to describe histogram and distribution data characteristics
    integer(i4b)::i,nsy

    if(size(x1) /= size(x2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'limits arrays (x1,x2) are different lengths (integrate_x_arr_dataset_2d)')
    endif
    if(size(x1) /= size(d2d%signal,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'limits arrays (x1,x2) are incompatible with dataset_2d (integrate_x_arr_dataset_2d)')
    endif

    if(status == IXCseverity_error)return
    ! here we describe the characteristics of the output dataset_1d
    ! it will have the dataset_2d y properties
    hist=y_hist(d2d)
    ! in all cases when we integrate in 1-dimension the output(wres) will not be a distribution
    dist=.false.
    ! create the dtaset_1d with the appropriate characteristics length, nx, etc...
    !    call IXFinitialise(wres,status)!need a default dataset_1d which is initialised
    call IXFmake_dataset_1d(wres,size(d2d%y),dist,hist,status)

    ! allocate output dataset_1d memory space

    call IXFget_ptr_dataset_1d(wres,x_ptr,s_ptr,e_ptr)

    !loop over 2nd dimension
    nsy=size(d2d%signal,2)
    do i=1,nsy
       if(hist)then
          x_ptr(i)=d2d%y(i)
          !not necessary to treat as a 2d histogram
          call IXFintegrate_1d_hist(s_ptr(i),e_ptr(i),d2d%x,d2d%signal(:,i),d2d%error(:,i),d2d%x_distribution,x1(i),x2(i),status)
       else
          x_ptr(i)=d2d%y(i)
          call IXFintegrate_1d_points(s_ptr(i),e_ptr(i),d2d%x,d2d%signal(:,i),d2d%error(:,i),x1(i),x2(i),status)
       endif
    enddo

    call IXFset_dataset_1d(wres,x_axis=d2d%y_axis,s_axis=d2d%s_axis,status=status)

  end subroutine integrate_x_arr_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to perform integration along Y dimension of a dataset_2d to give a dataset_1d output.  Integration
  !! will be performed irrespective of point or histogram data.
  subroutine IXFintegrate_y_dataset_2d(wres,w1,y1,y2,status)
    use IXMintegrate
    use IXMdataset_1d
    implicit none
    type(IXTdataset_1d),intent(out)::wres !!output: dataset_1d
    type(IXTdataset_2d),intent(in)::w1 !! input: dataset_2d
    type(IXTstatus),intent(inout)::status !! error status object
    real(dp),intent(in)::y1,y2 !! input integration limits along second dimension 
    real(dp),pointer::x_ptr(:),s_ptr(:),e_ptr(:)!!temporary pointers used to fill output dataset_1d object
    logical::hist,dist !!local variables to describe histogram and distribution data characteristics

    ! here we describe the characteristics of the output dataset_1d
    ! it will have the dataset_2d x properties
    hist=x_hist(w1)
    ! in all cases when we integrate in 1-dimension the output(wres) will not be a distribution
    dist=.false.
    ! create the dtaset_1d with the appropriate characteristics length, nx, etc...
    !    call IXFinitialise(wres,status)!need a default dataset_1d which is initialised
    call IXFmake_dataset_1d(wres,size(w1%x),dist,hist,status)

    ! allocate output dataset_1d memory space
    call IXFget_ptr_dataset_1d(wres,x_ptr,s_ptr,e_ptr)


    call IXFintegrate_2d_hist(w1%signal,w1%error,status,x=w1%x,y=w1%y,ydist=w1%y_distribution,yhist=y_hist(w1),ymin_in=y1,&
         ymax_in=y2,val_ar_in=s_ptr,err_ar_in=e_ptr,x_ar=x_ptr)

    call IXFset_dataset_1d(wres,status,title=w1%title,x_axis=w1%x_axis,s_axis=w1%s_axis)


  end subroutine IXFintegrate_y_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to integrate across 2 dimensions to give an IXTdatum object.  Integration
  !! will be performed irrespective of point or histogram data.
  subroutine IXFintegrate_xy_dataset_2d(ires,w2D,xmin,xmax,ymin,ymax,status)
    use IXMdatum
    use IXMintegrate
    implicit none
    type(IXTdatum),intent(out)::ires !!output:: IXTdatum object
    type(IXTdataset_2d),intent(in)::w2D !! input: dataset_2d object
    real(dp),intent(in)::xmin,xmax !!x integration limit
    real(dp),intent(in)::ymin,ymax !! y integration limit
    type(IXTstatus),intent(inout)::status !! error status object

    call IXFintegrate_2d_hist(w2D%signal,w2D%error,status,x=w2D%x,xdist=w2D%x_distribution,xhist=x_hist(w2D), &
         xmin_in=xmin,xmax_in=xmax, val_pt=ires%val,err_pt=ires%err,ymin_in=ymin,ymax_in=ymax,y=w2D%y, &
         ydist=w2D%y_distribution,yhist=y_hist(w2D))

  end subroutine IXFintegrate_xy_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which acts on a dataset_2d object with histogram data along the x axis and point data
  !! along the y-axis. it integrates along the x axis and then sums the output between two specified 
  !! limits along the y-axis.
  subroutine IXFintXsumY_dataset_2d(ires,w2D,xmin,xmax,y_lo,y_hi,status)
    use IXMdatum
    use IXMintegrate
    implicit none
    type(IXTdatum),intent(out)::ires !!output:: IXTdatum object
    type(IXTdataset_2d),intent(in)::w2D !! input: dataset_2d object
    real(dp),intent(in)::xmin,xmax !!x integration limit
    integer(i4b),intent(in)::y_lo,y_hi !! y-axis summing limit
    type(IXTstatus),intent(inout)::status !! error status object

    !in the following call y is sent even though it is not used since it is a non-optional argument
    call IXFintegrate_2d_hist(w2D%signal,w2D%error,status,x=w2D%x,y=w2D%y,xdist=w2D%x_distribution,xhist=x_hist(w2D), &
         xmin_in=xmin,xmax_in=xmax,val_pt=ires%val,err_pt=ires%err,spec_lo=y_lo,spec_hi=y_hi)

  end subroutine IXFintXsumY_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !!subroutine to shift the x and/or y array of a dataset_2d object
  subroutine IXFshift_dataset_2d(wres,w1,status,shift_x,shift_y)
    use IXMshift
    implicit none
    type(IXTdataset_2d), intent(in) :: w1 !! input: dataset_2d object to have its array shifted
    type(IXTdataset_2d),intent(out) :: wres !! output: dataset_2d object
    real(dp),intent(in),optional::shift_x !!input: optional amount to shift x_array
    real(dp),intent(in),optional::shift_y !!input: optional amount to shift Y_array
    type(IXTstatus),intent(inout) :: status !! error status object

    call IXFalloc(wres%x,size(w1%x),status)
    call IXFalloc(wres%y,size(w1%y),status)
    call IXFallocdims(wres%signal,shape(w1%signal),status)
    call IXFallocdims(wres%error,shape(w1%error),status)
    if (status == IXCseverity_error) return

    if(present(shift_x))then
       call IXFshift(w1%x,wres%x,shift_x)
       if(present(shift_y))then
          call IXFshift(w1%y,wres%y,shift_y)
       else
          wres%y=w1%y
       endif
    else
       !shift_y only present
       call IXFshift(w1%y,wres%y,shift_y)
       wres%x=w1%x
    endif
    call IXFallocFortran(wres%title,size(w1%title),status)
    wres%title=w1%title

    wres%signal=w1%signal
    wres%error=w1%error
    call IXFcopy(w1%s_axis,wres%s_axis,status)

    call IXFcopy(w1%x_axis,wres%x_axis,status)
    wres%x_distribution=w1%x_distribution

    call IXFcopy(w1%y_axis,wres%y_axis,status)
    wres%y_distribution=w1%y_distribution
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFshift_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_binary_op_dataset_2d(wres,w1,w2, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(out) :: wres
    type(IXTdataset_2d),intent(in)::w1,w2

    call setup_binary_common_dataset_2d(w1,w2, status)
    if (status == IXCseverity_error) return

    if (size(w1%y) /= size(w2%y) ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of y array elements incompatible(setup_binary_op_dataset_2d)') 
       return
    endif

    !check all elements of y_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
    if (sum(abs(w1%y - w2%y))/=0 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'y array elements incompatible(setup_binary_op_dataset_2d)') 
       return
    endif


    ! since now must be both same type, only have to check number of elements
    if (size(w1%x) /= size(w2%x) ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of x array elements incompatible(setup_binary_op_dataset_2d)') 
       return   
    endif

    ! if length test passed then make final checks on x/y-array values 

    !check all elements of x_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
    if (sum(abs(w1%x - w2%x))/=0 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array elements incompatible(setup_binary_op_dataset_2d)') 
       return
    endif


    ! we will allocate one array with standard fortran, the other with
    ! MATLAB memory. When we write the object back to matlab we will get an informational
    ! message telling us that some additional MATLAB memory had to be created for
    ! the fortran memory
    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%y, size(w1%y),.false., status)
    call IXFreallocdims(wres%signal, shape(w1%signal),.false., status)
    call IXFreallocdims(wres%error, shape(w1%error),.false., status)

  end subroutine setup_binary_op_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  ! w1 is necessarily the full 2d dataset_2d
  subroutine setup_binary_X_special_dataset_2d(wres,w1,w2, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(out) :: wres
    type(IXTdataset_2d),intent(in)::w1,w2

    call setup_binary_common_dataset_2d(w1,w2, status)
    if (status == IXCseverity_error) return
    ! since now must be both same type, only have to check number of elements
    if (size(w1%x) /= size(w2%x) ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of x array elements incompatible(setup_binary_X_special_dataset_2d)') 
       return   
    endif

    ! if length test passed then make final checks on x/y-array values 

    !check all elements of x_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
    if (sum(abs(w1%x - w2%x))/=0 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array elements incompatible (setup_binary_X_special_dataset_2d)') 
       return
    endif
    ! it is assumed that it will be predetermined which dataset is used to set the size of the output array

    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%y, size(w1%y),.false., status)
    call IXFreallocdims(wres%signal, shape(w1%signal),.false., status)   
    call IXFreallocdims(wres%error, shape(w1%error),.false., status)

  end subroutine setup_binary_X_special_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_binary_Y_special_dataset_2d(wres,w1,w2, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(out) :: wres
    type(IXTdataset_2d),intent(in)::w1,w2


    call setup_binary_common_dataset_2d(w1,w2, status)
    if (status == IXCseverity_error) return

    if (size(w1%y) /= size(w2%y) ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of y array elements incompatible (setup_binary_Y_special_dataset_2d)') 
       return
    endif

    !check all elements of y_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
    if (sum(abs(w1%y - w2%y))/=0 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'y array elements incompatible (setup_binary_Y_special_dataset_2d)') 
       return
    endif
    ! it is assumed that it will be predetermined which dataset is used to set the size of the output array

    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%y, size(w1%y),.false., status)
    call IXFreallocdims(wres%signal, shape(w1%signal),.false., status)   
    call IXFreallocdims(wres%error, shape(w1%error),.false., status)

  end subroutine setup_binary_Y_special_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_binary_common_dataset_2d(w1,w2, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(in)::w1,w2
    logical:: xhist_w1,xhist_w2,yhist_w1,yhist_w2

    xhist_w1=x_hist(w1)
    xhist_w2=x_hist(w2)
    yhist_w1=y_hist(w1)
    yhist_w2=y_hist(w2)

    !checks in data compatibility 
    !check x-axis 
    ! need a axis comparison    
    if(IXFcompare_units(w1%x_axis,w2%x_axis) .neqv. .true.) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x_axis incompatible for desired operation(setup_binary_common_dataset_2d)') 
       return
    endif

    !check y-axis 
    ! need a units comparison    
    if(IXFcompare_units(w1%y_axis,w2%y_axis) .neqv. .true.) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'y_axis incompatible for desired operation(setup_binary_common_dataset_2d)') 
       return
    endif

    ! need a units comparison    
    if(IXFcompare_units(w1%s_axis,w2%s_axis) .neqv. .true.) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 's_axis incompatible for desired operation(setup_binary_common_dataset_2d)') 
       return
    endif

    ! check x histogram compatibility
    if( xhist_w1 .neqv. xhist_w2 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x histogram flags incompatible(setup_binary_common_dataset_2d)') 
       return
    endif
    ! check x distribution compatibilty
    if( w1%x_distribution .neqv. w2%x_distribution ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x distribution flags incompatible(setup_binary_common_dataset_2d)') 
       return
    endif

    ! check y histogram compatibility
    if(yhist_w1 .neqv. yhist_w2  ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'y histogram flags incompatible(setup_binary_common_dataset_2d)') 
       return
    endif
    ! check distribution compatibilty
    if( w1%y_distribution .neqv. w2%y_distribution ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'y distribution flags incompatible(setup_binary_common_dataset_2d)') 
       return
    endif

  end subroutine setup_binary_common_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_unary_op_dataset_2d(wres,w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(out) :: wres
    type(IXTdataset_2d),intent(in)::w1

    ! no checks need to be made - dataset should have been created properly, it is a unary operation
    ! allocate memory to output dataset
    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%y, size(w1%y),.false., status)
    call IXFreallocdims(wres%signal, shape(w1%signal),.false., status)
    call IXFreallocdims(wres%error, shape(w1%error),.false., status)

  end subroutine setup_unary_op_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine finish_op_dataset_2d(wres,w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status
    type(IXTdataset_2d),intent(inout)::wres
    type(IXTdataset_2d),intent(in) :: w1

    wres%x = w1%x
    call IXFcopy(w1%x_axis,wres%x_axis,status)

    wres%y=w1%y
    !all foll variables are necessarily the same 
    call IXFcopy(w1%y_axis,wres%y_axis,status)

    call IXFcopy(w1%s_axis,wres%s_axis,status)
    wres%x_distribution=w1%x_distribution
    wres%y_distribution=w1%y_distribution
    call IXFallocFortran(wres%title,size(w1%title),status)
    ! could be changed to show combination
    wres%title=w1%title
    wres%base=w1%base
    call IXFmark_valid(wres)

  end subroutine finish_op_dataset_2d


#define IXD_NAME        plus_dataset_2d
#define IXD_TYPE        dataset_2d
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        minus_dataset_2d
#define IXD_TYPE        dataset_2d
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        times_dataset_2d
#define IXD_TYPE        dataset_2d
#define IXD_OPERATION   Times
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        divide_dataset_2d
#define IXD_TYPE        dataset_2d
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        power_dataset_2d
#define IXD_TYPE        dataset_2d
#define IXD_OPERATION   Power
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	log_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Log
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	log10_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Log10
#include "unary_ops.f90"


  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	exp_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Exp
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	sin_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Sin
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	cos_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Cos
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	tan_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Tan
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	sinh_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Sinh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	cosh_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Cosh
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	tanh_dataset_2d
#define IXD_TYPE	dataset_2d
#define IXD_OPERATION   Tanh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which will rebin a dataset_2d object along the X dimension, provided it is histogram data.
  !! It will accept a reference dataset_2d object or a descriptor array to calculate the new bin boundaries
  subroutine IXFrebin_x_dataset_2d(wres,status,w2,Xdesc,Xref)
    use IXMrebin
    implicit none
    type (IXTdataset_2d),intent(out):: wres !! output: rebinned dataset_2d object
    type (IXTdataset_2d),intent(in):: w2 !! input: dataset_2d to be rebinned
    type (IXTdataset_2d),intent(in),optional::Xref !!optional input - reference dataset  
    real(dp),intent(in),optional::Xdesc(:) !! optional input - descriptor array
    type (IXTstatus),intent(inout)::status
    logical::xhist
    integer(i4b)::nx_out !new length of x-array

    xhist=x_hist(w2)

    if(.not.(xhist))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'rebinning of data only permitted on histogram data ') 
    endif

    if(present(Xref) .and. present(Xdesc) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'a reference and descriptor variable passed for X dimension') 
    endif

    if(status == IXCseverity_error)return

    if(present(Xdesc))then 
       ! call subroutine to find the length of the output x array
       call IXFrebin_1d_hist_get_arr(Xdesc,x_in=w2%x,n_out=nx_out,status=status)
       ! assign memory space to wres
       call IXFalloc(wres%x,nx_out,status)
       call IXFallocdims(wres%signal, (/nx_out-1, size(w2%signal,2)  /) ,status)
       call IXFallocdims(wres%error,  (/nx_out-1, size(w2%signal,2)  /) ,status)
       ! 
       ! now fill the wres%x with data (it is called using x_out, but it is just a list of numbers no real reference to "x")

       call IXFrebin_1d_hist_get_arr(Xdesc,x_in=w2%x,x_out=wres%x,status=status)
       ! perform rebinning with new output x_arrax
       call IXFrebinX_2d_hist( w2%x,w2%signal,w2%error,wres%x ,wres%signal ,wres%error ,w2%x_distribution ,status)

    endif

    if(present(Xref))then
       call IXFalloc(wres%x,size(Xref%x),status)

       call IXFallocdims(wres%signal, (/ size(Xref%x)-1  , size(w2%signal,2)  /) ,status)
       call IXFallocdims(wres%error,  (/ size(Xref%x)-1  , size(w2%signal,2)  /) ,status)
       ! create the output x array with the reference dataset_2d x array -> Xref%x
       wres%x=Xref%x

       call IXFrebinX_2d_hist( w2%x ,w2%signal,w2%error,wres%x ,wres%signal ,wres%error ,w2%x_distribution ,status)

    endif

    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)


    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    ! allocate memory for unchanged output y-array 
    call  IXFalloc(wres%y,size(w2%y),status)
    wres%y=w2%y

    !everything is now properly filled
    call IXFmark_valid(wres)

    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFrebin_x_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which will rebin a dataset_2d object along the Y dimension, provided it is histogram data.
  !! It will accept a reference dataset_2d object or a descriptor array to calculate the new bin boundaries
  subroutine IXFrebin_y_dataset_2d(wres,status,w2,Ydesc,Yref)
    use IXMrebin
    implicit none
    type (IXTdataset_2d),intent(out):: wres !! output: rebinned dataset_2d object
    type (IXTdataset_2d),intent(in):: w2 !! input: dataset_2d to be rebinned
    type (IXTdataset_2d),intent(in),optional::Yref !!optional input - reference dataset  
    real(dp),intent(in),optional::Ydesc(:) !! optional input - descriptor array
    type (IXTstatus),intent(inout)::status
    logical::yhist
    integer(i4b)::ny_out ! new length of y array

    yhist=y_hist(w2)

    if(.not.(yhist))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'rebinning of data only permitted on histogram data ') 
    endif

    if(present(Yref) .and. present(Ydesc) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'a reference AND descriptor variable passed for Y dimension') 
    endif

    if(status == IXCseverity_error)return

    if(present(Ydesc))then 
       ! call subroutine to find the length of the output y array
       call IXFrebin_1d_hist_get_arr(Ydesc,x_in=w2%y,n_out=ny_out,status=status)
       ! assign memory space to wres
       call IXFalloc(wres%y,ny_out,status)
       call IXFallocdims(wres%signal, (/ size(w2%signal,1), ny_out-1 /) ,status)
       call IXFallocdims(wres%error,  (/ size(w2%signal,1), ny_out-1 /) ,status)
       ! fill the wres%y with data (it is called using x_out, but it is just a list of numbers no real reference to "x")
       call IXFrebin_1d_hist_get_arr(Ydesc,x_in=w2%y,x_out=wres%y,status=status)
       ! perform rebinning with new output y_array
       call IXFrebinY_2d_hist( w2%y,w2%signal,w2%error,wres%y ,&
            wres%signal ,wres%error ,w2%y_distribution ,status)
    endif

    if(present(Yref))then
       call IXFalloc(wres%y,size(Yref%y),status)
       call IXFallocdims(wres%signal,(/ size(w2%signal,1),size(Yref%x)-1 /), status)
       call IXFallocdims(wres%error, (/ size(w2%signal,1),size(Yref%x)-1 /), status)
       ! create the output y array with the reference dataset_2d y array -> Yref%y
       wres%y=Yref%y
       call IXFrebinY_2d_hist( w2%y ,w2%signal,w2%error,wres%y ,wres%signal ,wres%error ,w2%y_distribution ,status)

    endif

    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)
    ! allocate memory for unchanged output x-array 
    call  IXFalloc(wres%x,size(w2%x),status)

    wres%x=w2%x
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres, status)
  end subroutine IXFrebin_y_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which will rebin along the X and Y dimensions provided both dimensions are histogram type data. 
  !! It rebins first along the X-axis and outputs to a temporary signal and error array. These temporary arrays
  !! are then rebinned along the Y axis to output to the wres dataset_2d object. A descriptor array or a reference dataset_2d object can
  !! be used to calculate the new bin boundaries along each dimension.
  subroutine IXFrebin_xy_dataset_2d(wres,status,w2,Xdesc,Xref,Ydesc,Yref)
    use IXMrebin
    implicit none
    type (IXTdataset_2d),intent(out):: wres !! output: rebinned dataset_2d object
    type (IXTdataset_2d),intent(in):: w2 !! input: dataset_2d to be rebinned
    type (IXTdataset_2d),intent(in),optional::Xref,Yref !!optional input - reference dataset  
    real(dp),intent(in),optional::Xdesc(:),Ydesc(:) !! optional input - descriptor array
    type (IXTstatus),intent(inout)::status !! error status object
    real(dp),allocatable::e_temp(:,:),s_temp(:,:)!!temporary arrays to hold signal/error arrays after first rebin on X dimension
    !! they are then fed into the Y rebin part
    integer(i4b)::nx_out,ny_out !new length of x/y-array

    if((.not. x_hist(w2)) .or. (.not. y_hist(w2)) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'rebinning of data only permitted on histogram data') 
    endif

    if(present(Xref) .and. present(Xdesc) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'a reference AND descriptor variable passed for X dimension') 
    endif

    if(present(Yref) .and. present(Ydesc) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'a reference AND descriptor variable passed for Y dimension') 
    endif


    if(status == IXCseverity_error)return

    if(present(Xdesc))then 

       ! call subroutine to find the length of the output x array
       call IXFrebin_1d_hist_get_arr(Xdesc,x_in=w2%x,n_out=nx_out,status=status)

       call IXFalloc(wres%x,nx_out,status)
       ! assign memory to temporary output 
       allocate(s_temp(nx_out-1,size(w2%signal,2)))
       allocate(e_temp(nx_out-1,size(w2%signal,2)))
       ! 
       ! now fill the wres%x with data (it is called using x_out, but it is just a list of numbers no real reference to "x")

       call IXFrebin_1d_hist_get_arr(Xdesc,x_in=w2%x,x_out=wres%x,status=status)
       ! perform rebinning with new output x_arrax
       call IXFrebinX_2d_hist( w2%x,w2%signal,w2%error,wres%x ,s_temp ,e_temp ,w2%x_distribution ,status)

    endif

    if(present(Xref))then

       call IXFalloc(wres%x,size(Xref%x),status)
       ! assign memory to temporary output            
       allocate(s_temp(nx_out-1,size(w2%signal,2)))       
       allocate(e_temp(nx_out-1,size(w2%signal,2)))

       ! create the output x array with the reference dataset_2d x array -> Xref%x
       wres%x=Xref%x

       call IXFrebinX_2d_hist( w2%x ,w2%signal,w2%error,wres%x ,s_temp ,e_temp ,w2%x_distribution ,status)

    endif

    if (status == IXCseverity_error) return

    ! now rebin along the Y dimension
    if(present(Ydesc))then 

       ! call subroutine to find the length of the output y array
       call IXFrebin_1d_hist_get_arr(Ydesc,x_in=w2%y,n_out=ny_out,status=status)

       ! assign memory space to wres
       call IXFalloc(wres%y,ny_out,status)

       call IXFallocdims(wres%signal, (/ size(s_temp,1), ny_out-1 /) ,status)
       call IXFallocdims(wres%error,  (/ size(s_temp,1), ny_out-1 /) ,status)
       ! 
       ! now fill the wres%y with data (it is called using x_out, but it is just a list of numbers no real reference to "x")
       call IXFrebin_1d_hist_get_arr(Ydesc,x_in=w2%y,x_out=wres%y,status=status)
       ! perform rebinning with new output y_array
       call IXFrebinY_2d_hist( w2%y,s_temp,e_temp,wres%y ,wres%signal ,wres%error ,w2%y_distribution ,status)

    endif

    if(present(Yref))then

       call IXFalloc(wres%y,size(Yref%y),status)

       call IXFallocdims(wres%signal,(/ size(s_temp,1),size(Yref%x)-1 /), status)
       call IXFallocdims(wres%error, (/ size(s_temp,1),size(Yref%x)-1 /), status)
       ! create the output y array with the reference dataset_2d y array -> Yref%y
       wres%y=Yref%y

       call IXFrebinY_2d_hist( w2%y ,s_temp,e_temp,wres%y ,wres%signal ,wres%error ,w2%y_distribution ,status)

    endif

    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres, status)

    deallocate(s_temp,e_temp)

  end subroutine IXFrebin_xy_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFrebunch_x_dataset_2d(wres,w2,nbunch,status)
    use IXMrebunch
    implicit none
    type(IXTdataset_2d),intent(in)::w2 !! input: dataset_2d object to be rebunched
    type(IXTdataset_2d),intent(out)::wres !!output: rebunched dataset-2d
    integer(i4b),intent(in)::nbunch !! input: number of bins to be grouped together
    type(IXTstatus),intent(inout) :: status !! eroor status object


    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)
    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    call IXFalloc(wres%y,size(w2%y),status)
    wres%y=w2%y
    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    if(x_hist(w2))then
       call IXFrebunchHistX(w2%x,w2%signal,w2%error,wres%x,wres%signal,wres%error,w2%x_distribution,nbunch,status)
    else
       call IXFrebunchPointsX(w2%x,w2%signal,w2%error,wres%x,wres%signal,wres%error,nbunch,status)
    endif

    if (status == IXCseverity_error) return

    call IXFmark_valid(wres)
    !has it written back a valid dataset_1d ?
    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFrebunch_x_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFrebunch_y_dataset_2d(wres,w2,nbunch,status)
    use IXMrebunch
    implicit none
    type(IXTdataset_2d),intent(in)::w2 !! input: dataset_2d object to be rebunched
    type(IXTdataset_2d),intent(out)::wres !!output: rebunched dataset-2d
    integer(i4b),intent(in)::nbunch !! input: number of bins to be grouped together
    type(IXTstatus),intent(inout) :: status !! eroor status object


    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)
    call IXFalloc(wres%x,size(w2%x),status)
    wres%x=w2%x

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    if(y_hist(w2))then
       call IXFrebunchHistY(w2%y,w2%signal,w2%error,wres%y,wres%signal,wres%error,w2%y_distribution,nbunch,status)
    else
       call IXFrebunchPointsY(w2%y,w2%signal,w2%error,wres%y,wres%signal,wres%error,nbunch,status)
    endif

    if (status == IXCseverity_error) return
    call IXFmark_valid(wres)
    !has it written back a valid dataset_2d ?
    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFrebunch_y_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFrebunch_xy_dataset_2d(wres,w2,Xbunch,Ybunch,status)
    use IXMrebunch
    implicit none
    type(IXTdataset_2d),intent(in)::w2 !! input: dataset_2d object to be rebunched
    type(IXTdataset_2d),intent(out)::wres !!output: rebunched dataset-2d
    integer(i4b),intent(in)::Xbunch,Ybunch !! input: number of bins to be grouped together
    type(IXTstatus),intent(inout) :: status !! error status object
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title    
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    call IXFrebunchXY(w2%x,w2%y,w2%signal,w2%error,wres%x,wres%y,wres%signal,wres%error,  &
         w2%x_distribution,w2%y_distribution,x_hist(w2),y_hist(w2),Xbunch,Ybunch,status)

    if (status == IXCseverity_error) return
    call IXFmark_valid(wres)

    !has it written back a valid dataset_1d ?
    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFrebunch_xy_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to regroup a dataset_2d object with bin boundaries to ensure that bins have minimum width 
  !! determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
  !! bin boundaries. dx is defined in the input array param[xlo,dx,xhi], where xlo and xhi are the minimum
  !! and maximum array bin boundaries.
  subroutine IXFregroup_x_dataset_2d(wres,w2,param,status)
    use IXMregroup
    implicit none

    type(IXTdataset_2d),intent(out)::wres !!output: dataset_1d object
    type(IXTdataset_2d),intent(in)::w2 !!input: dataset_1d to be regrouped
    type(IXTstatus),intent(inout) :: status !! error status object
    real(dp),intent(in) :: param(3) !!input: param array defines regrouping parameters -> (xlo,dx,xhi)
    integer(i4b) :: nx_out !no. of output xbins

    if(.not.(x_hist(w2)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'regrouping of data only permitted on histogram data ') 
    endif

    if (status == IXCseverity_error) return

    ! call subroutine to find the no.of bins of the output y array
    ! this operation is independent of dimension of array
    call IXFregroup_1d_hist(param(1),param(2),param(3),w2%x_distribution,w2%x,nout=nx_out,status=status)
    if (status == IXCseverity_error) return

    ! assign memory space to wres
    call IXFalloc(wres%x,nx_out+1,status)
    call IXFallocdims(wres%signal,(/ nx_out, size(w2%signal,2) /),status)
    call IXFallocdims(wres%error,(/ nx_out, size(w2%signal,2) /),status)

    if (status == IXCseverity_error) return

    call IXFregroupX_2d_hist(param(1),param(2),param(3),w2%x_distribution,&
         w2%x,w2%signal,w2%error,wres%x,wres%signal,wres%error,status=status)

    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)

    call IXFalloc(wres%y,size(w2%y),status)
    wres%y=w2%y
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFregroup_x_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to regroup a dataset_2d object with bin boundaries to ensure that bins have minimum width 
  !! determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
  !! bin boundaries. dx is defined in the input array param[xlo,dx,xhi], where xlo and xhi are the minimum
  !! and maximum array bin boundaries.
  subroutine IXFregroup_y_dataset_2d(wres,w2,param,status)
    use IXMregroup
    implicit none

    type(IXTdataset_2d),intent(out)::wres !!output: dataset_1d object
    type(IXTdataset_2d),intent(in)::w2 !!input: dataset_1d to be regrouped
    type(IXTstatus),intent(inout) :: status !! error status object
    real(dp),intent(in) :: param(3) !!input: param array defines regrouping parameters -> (xlo,dx,xhi)
    integer(i4b) :: ny_out !no. of output xbins

    if(.not.(y_hist(w2)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'regrouping of data only permitted on histogram data ') 
    endif
    if (status == IXCseverity_error) return

    ! call subroutine to find the no.of bins of the output y array
    ! this operation is independent of dimension of array
    call IXFregroup_1d_hist(param(1),param(2),param(3),w2%y_distribution,&
         w2%y,nout=ny_out,status=status)
    if (status == IXCseverity_error) return

    ! assign memory space to wres
    call IXFalloc(wres%y,ny_out+1,status)
    call IXFallocdims(wres%signal,(/ size(w2%signal,1) , ny_out /),status)
    call IXFallocdims(wres%error,(/ size(w2%signal,1) , ny_out /),status)
    if (status == IXCseverity_error) return

    call IXFregroupY_2d_hist(param(1),param(2),param(3),w2%y_distribution,&
         w2%y,w2%signal,w2%error,wres%y,wres%signal,wres%error,status=status)

    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    call IXFalloc(wres%x,size(w2%x),status)
    wres%x=w2%x

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)
    call IXFmark_valid(wres)

    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFregroup_y_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to regroup a dataset_2d object with bin boundaries to ensure that bins have minimum width 
  !! determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
  !! bin boundaries. dx is defined in the input array param[xlo,dx,xhi], where xlo and xhi are the minimum
  !! and maximum array bin boundaries.
  subroutine IXFregroup_xy_dataset_2d(wres,w2,Xparam,Yparam,status)
    use IXMregroup
    implicit none
    type(IXTdataset_2d),intent(out)::wres !!output: dataset_1d object
    type(IXTdataset_2d),intent(in)::w2 !!input: dataset_1d to be regrouped
    type(IXTstatus),intent(inout) :: status !! error status object
    real(dp),intent(in) :: Xparam(3), Yparam(3) !!input: param array defines regrouping parameters -> (xlo,dx,xhi)
    integer(i4b) :: nx_out, ny_out !no. of output xbins
    real(dp),allocatable::stemp(:,:),etemp(:,:)

    if((.not.(x_hist(w2))).or.(.not.(y_hist(w2))))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'regrouping of data only permitted on histogram data ') 
    endif

    if (status == IXCseverity_error) return

    ! call subroutine to find the no.of bins of the output y array
    ! this operation is independent of dimension of array
    call IXFregroup_1d_hist(Xparam(1),Xparam(2),Xparam(3),w2%x_distribution,w2%x,nout=nx_out,status=status)
    if (status == IXCseverity_error) return

    ! assign memory space to wres
    call IXFalloc(wres%x,nx_out+1,status)

    allocate(stemp(nx_out, size(w2%signal,2)))
    allocate(etemp(nx_out, size(w2%signal,2)))

    if (status == IXCseverity_error) return

    call IXFregroupX_2d_hist(Xparam(1),Xparam(2),Xparam(3),w2%x_distribution,&
         w2%x,w2%signal,w2%error,wres%x,stemp,etemp,status=status)

    if (status == IXCseverity_error) return

    ! call subroutine to find the no.of bins of the output y array
    ! this operation is independent of dimension of array
    call IXFregroup_1d_hist(Yparam(1),Yparam(2),Yparam(3),w2%y_distribution,w2%y,nout=ny_out,status=status)
    if (status == IXCseverity_error) return

    ! assign memory space to wres
    call IXFalloc(wres%y,ny_out+1,status)
    call IXFallocdims(wres%signal,(/ size(stemp,1) , ny_out /),status)
    call IXFallocdims(wres%error,(/ size(etemp,1) , ny_out /),status)

    if (status == IXCseverity_error) return

    call IXFregroupY_2d_hist(Yparam(1),Yparam(2),Yparam(3),w2%y_distribution,&
         w2%y,stemp,etemp,wres%y,wres%signal,wres%error,status=status)

    if (status == IXCseverity_error) return

    deallocate(stemp)
    deallocate(etemp)
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)

    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)

    wres%y_distribution=w2%y_distribution
    call IXFcopy(w2%y_axis,wres%y_axis,status)
    call IXFmark_valid(wres)

    call IXFcheck_dataset_2d(wres, status)

  end subroutine IXFregroup_xy_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv1x_dataset_2d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_2d),intent(in) :: w1
    type (IXTdataset_2d),intent(out) :: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx,ny,i

    call setup_unary_op_dataset_2d(wres,w1,status)

    ny=size(w1%signal,2)
    nx=size(w1%x)

    if (status == IXCseverity_error) return

    if(x_hist(w1))then !determine bincentres of histogram data
       allocate(bincentres(nx-1))    
       bincentres=w1%x(2:nx)+w1%x(1:nx-1)/2.0d0
       do i=1,ny                
          call IXFderiv_1_1d (bincentres,w1%signal(:,i),w1%error(:,i),wres%signal(:,i),wres%error(:,i),status)       
          if (status == IXCseverity_error) return 
       enddo
       deallocate(bincentres)
    else !point data so use xarray
       do i=1,ny
          call IXFderiv_1_1d (w1%x,w1%signal(:,i),w1%error(:,i),wres%signal(:,i),wres%error(:,i),status)       
          if (status == IXCseverity_error) return       
       enddo
    endif
    call finish_op_dataset_2d(wres, w1, status)

    call IXFmark_valid(wres)
    call IXFreallocFortran(wres%title,size(w1%title),.false.,status)       
    wres%title=w1%title       

    call IXFcheck_dataset_2d(wres,status)

  end subroutine IXFderiv1x_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv1y_dataset_2d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_2d),intent(in) :: w1
    type (IXTdataset_2d),intent(out) :: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx,ny,i

    call setup_unary_op_dataset_2d(wres,w1,status)

    nx=size(w1%signal,1)
    ny=size(w1%y)

    if (status == IXCseverity_error) return

    if(y_hist(w1))then !determine bincentres of histogram data
       allocate(bincentres(ny-1))    
       bincentres=w1%x(2:ny)+w1%x(1:ny-1)/2.0d0
       do i=1,nx                
          call IXFderiv_1_1d (bincentres,w1%signal(i,:),w1%error(i,:),wres%signal(i,:),wres%error(i,:),status)       
          if (status == IXCseverity_error) return
       enddo
       deallocate(bincentres)
    else !point data so use xarray
       do i=1,nx
          call IXFderiv_1_1d (w1%y,w1%signal(i,:),w1%error(i,:),wres%signal(i,:),wres%error(i,:),status)       
          if (status == IXCseverity_error) return
       enddo
    endif
    call finish_op_dataset_2d(wres, w1, status)    
    call IXFmark_valid(wres)
    call IXFreallocFortran(wres%title,size(w1%title),.false.,status)       
    wres%title=w1%title           
    call IXFcheck_dataset_2d(wres,status)

  end subroutine IXFderiv1y_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv2x_dataset_2d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_2d),intent(in) :: w1
    type (IXTdataset_2d),intent(out) :: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx,ny,i

    call setup_unary_op_dataset_2d(wres,w1,status)

    ny=size(w1%signal,2)
    nx=size(w1%x)

    if (status == IXCseverity_error) return

    if(x_hist(w1))then !determine bincentres of histogram data
       allocate(bincentres(nx-1))    
       bincentres=w1%x(2:nx)+w1%x(1:nx-1)/2.0d0
       do i=1,ny                
          call IXFderiv_2_1d (bincentres,w1%signal(:,i),w1%error(:,i),wres%signal(:,i),wres%error(:,i),status)       
          if (status == IXCseverity_error) return
       enddo
       deallocate(bincentres)
    else !point data so use xarray
       do i=1,ny
          call IXFderiv_2_1d (w1%x,w1%signal(:,i),w1%error(:,i),wres%signal(:,i),wres%error(:,i),status)       
          if (status == IXCseverity_error) return
       enddo
    endif
    call finish_op_dataset_2d(wres, w1, status)
    call IXFreallocFortran(wres%title,size(w1%title),.false.,status)       
    wres%title=w1%title           
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres,status)

  end subroutine IXFderiv2x_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv2y_dataset_2d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_2d),intent(in) :: w1
    type (IXTdataset_2d),intent(out) :: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx,ny,i

    call setup_unary_op_dataset_2d(wres,w1,status)

    nx=size(w1%signal,1)
    ny=size(w1%y)

    if (status == IXCseverity_error) return

    if(y_hist(w1))then !determine bincentres of histogram data
       allocate(bincentres(ny-1))    
       bincentres=w1%x(2:ny)+w1%x(1:ny-1)/2.0d0
       do i=1,nx                
          call IXFderiv_2_1d (bincentres,w1%signal(i,:),w1%error(i,:),wres%signal(i,:),wres%error(i,:),status)       
          if (status == IXCseverity_error) return
       enddo
       deallocate(bincentres)
    else !point data so use xarray
       do i=1,nx
          call IXFderiv_2_1d (w1%y,w1%signal(i,:),w1%error(i,:),wres%signal(i,:),wres%error(i,:),status)       
          if (status == IXCseverity_error) return
       enddo
    endif
    call finish_op_dataset_2d(wres, w1, status)    
    call IXFreallocFortran(wres%title,size(w1%title),.false.,status)       
    wres%title=w1%title           
    call IXFmark_valid(wres)
    call IXFcheck_dataset_2d(wres,status)

  end subroutine IXFderiv2y_dataset_2d
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFcorr_dataset_2d(d2d,atms,Ei,emode,status)
    use IXMefficiency
    implicit none
    type(IXTdataset_2d),intent(inout)::d2d
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::emode
    real(dp),intent(in)::atms,Ei    
    real(dp),allocatable::efficiency(:),Ef(:)
    type(IXTdataset_2d)::effic
    integer(i4b)::i,nx

    ! check for units in mev/energy transfer/or just energy....

    if (emode /= 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'efficiency calculation currently only possible with emode=1(IXFcorr_dataset2d)')
       return
    endif

    nx=size(d2d%signal,1)
    allocate(efficiency(nx))
    allocate(Ef(nx))

    if(x_hist(d2d))then   
       Ef=Ei-((d2d%x(1:nx)+d2d%x(2:nx+1))/2.0d0)
       !calculate bincentres to get energy
       do i=1,nx
          efficiency(i)=EFF(Ef(i),atms)
       enddo
    else  
       !point data
       Ef=Ei+d2d%x(1:nx) 
       do i=1,nx
          efficiency(i)=EFF(Ef(i),atms)
       enddo
    endif

    ! the sqrt function converts Ef and Ei to kf and ki respectively
    select case(emode)
    case(0)
       !      call IXFarray_X_Divide_dataset_2d(effic,d2d,efficiency,status)
    case (1)
       call IXFarray_X_Divide_dataset_2d(effic,d2d,efficiency*sqrt(Ef/Ei),status)
    case (2)
       !      call IXFarray_X_Divide_dataset_2d(effic,d2d,efficiency*sqrt(Ei/Ef),status)
    end select
    call IXFset_dataset_2d(d2d,status,signal=effic%signal,error=effic%error)
    call IXFdestroy(effic,status)
    deallocate(Ef,efficiency)
  end subroutine  IXFcorr_dataset_2d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFdestroy_dataset_2d(w2d,status)
    type(IXTdataset_2d),intent(inout)::w2d
    type(IXTstatus),intent(inout)::status
    call IXFdestroy(w2d%base,status)
    call IXFdealloc(w2d%x,status)
    call IXFdealloc(w2d%y,status)
    call IXFdealloc(w2d%signal,status)
    call IXFdealloc(w2d%error,status)
    if(allocated(w2d%title))deallocate(w2d%title)
    !    w2d%title='title'
    call IXFdestroy(w2d%x_axis,status)
    call IXFdestroy(w2d%s_axis,status)
    call IXFdestroy(w2d%y_axis,status)
    w2d%x_distribution=.false.
    w2d%y_distribution=.false.
    call IXFclear_valid(w2d)
  end subroutine IXFdestroy_dataset_2d

  !! IXFexpand_arrayd1d_dataset_2d this subroutine will take a whole IXTdataset_2d and convert it to an array of IXTdataset_1d objects.
  !! If it is given a list array of valid indices, only those indices in the y-array of the IXTdataset_2d will be extracted. This function
  !! is valid for histogram and point data as no information is lost
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFexpand_arrayd1d_dataset_2d(d2d,status,arrayd1d,list)
    use IXMdataset_1d
    implicit none
    type(IXTstatus),intent(inout)::status
    integer(i4b),optional,intent(in)::list(:)    
    type(IXTdataset_2d),intent(in)::d2d !!dataset_2d structure to be expanded out
    ! this would be intent OUT but for compiler issues
    type(IXTdataset_1d),allocatable::arrayd1d(:) !!array of dataset_1d structures
    real(dp),allocatable::s_in(:),e_in(:)
    integer(i4b)::i,sizey,sizex
    logical::list_p
    list_p=present(list)

    if(list_p)then
       sizey=size(list)
       if((sizey > size(d2d%y)) .or. (maxval(list) > size(d2d%y)) .or. (minval(list) < 1) )then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Invalid dataset_2d indices to extract (IXFexpand_arrayd1d_dataset_2d)') 
          return  
       endif
    else
    ! this is now the length of the signal array along y dimension to allow for histogram data in y-axis    
       sizey=size(d2d%signal,2)
    endif

    ! see if allocatable array of structures is correct length and reallocate if not 
    if (allocated(arrayd1d))then    
       call IXFrealloc(arrayd1d,sizey,.false.,status)
    else
       call IXFalloc(arrayd1d,sizey,status)
    endif

    sizex=size(d2d%signal,1)
    allocate(s_in(sizex))
    allocate(e_in(sizex))  

    do i=1,sizey
       if (list_p)then
          s_in=d2d%signal(:,list(i))
          e_in=d2d%error(:,list(i))
       else
          s_in=d2d%signal(:,i)
          e_in=d2d%error(:,i)
       endif
       call IXFcreate_dataset_1d(arrayd1d(i),d2d%title,s_in,e_in,d2d%s_axis,&
            d2d%x,d2d%x_axis,d2d%x_distribution,status)
    enddo
    deallocate(s_in,e_in)
  end subroutine IXFexpand_arrayd1d_dataset_2d

  !! IXFexpand_arrayd2d_dataset_2d this subroutine will take a whole IXTdataset_2d and convert it to an array of IXTdataset_2d objects.
  !! If it is given a list array of valid indices, only those indices in the y-array of the IXTdataset_2d will be extracted. for histogram
  !! objects in the y -dimension the centre of the bin boundary will be determined
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFexpand_arrayd2d_dataset_2d(d2d,status,arrayd2d,list)
    use IXMdataset_1d
    implicit none
    type(IXTstatus),intent(inout)::status
    type(IXTdataset_2d),intent(in)::d2d !!dataset_2d structure to be expanded out
    ! this would be intent(out) but for compiler issues
    type(IXTdataset_2d),allocatable::arrayd2d(:) !!array of dataset_2d structures
    integer(i4b),optional,intent(in)::list(:)
    integer(i4b)::i=0,sizey,j
    real(dp)::y_in(1)
    real(dp),allocatable::s_in(:,:),e_in(:,:)
    logical::list_p,yhist
    list_p=present(list)
    yhist=y_hist(d2d)
    if(list_p)then
       sizey=size(list)
       if((sizey > size(d2d%signal,2)) .or. (maxval(list) > size(d2d%signal,2)) .or. (minval(list) < 1) )then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Invalid dataset_2d indices to extract (IXFexpand_arrayd2d_dataset_2d)') 
          return  
       endif
    else
       sizey=size(d2d%signal,2)
    endif
    ! see if allocatable array of structures is correct length and reallocate if not 
    if (allocated(arrayd2d))then    
       call IXFrealloc(arrayd2d,sizey,.false.,status)
    else
       call IXFalloc(arrayd2d,sizey,status)
    endif

    j=size(d2d%signal,1)
    allocate(s_in(j,1))
    allocate(e_in(j,1))      
    do i=1,sizey
       if(list_p)then
          if(yhist)then
              y_in(1)=(d2d%y(list(i)) + d2d%y(list(i)+1) )/2.0_dp
          else
            y_in(1)=d2d%y(list(i)) ! to set y as an array in the array of structures
         endif
          s_in(:,1)=d2d%signal(:,list(i))
          e_in(:,1)=d2d%error(:,list(i))
       else
          if(yhist)then            
              y_in(1)=(d2d%y(i) +d2d%y(i+1) )/2.0_dp                 
          else
            y_in(1)=d2d%y(i) ! to set y as an array in the array of structures          
          endif
          s_in(:,1)=d2d%signal(:,i)
          e_in(:,1)=d2d%error(:,i)      
       endif
       call IXFcreate_dataset_2d(arrayd2d(i),d2d%title,s_in,e_in,d2d%s_axis,&
            d2d%x,d2d%x_axis,d2d%x_distribution,y_in,d2d%y_axis,.false.,status)
    enddo
    deallocate(e_in,s_in)

  end subroutine IXFexpand_arrayd2d_dataset_2d
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine units_single_array(d2d,arrayd2d,status,emode,efixed,L1,L2,theta,delay,axis_out)
    implicit none
    type(IXTdataset_2d),intent(inout)::d2d
    !would be intent(out) but for compiler issues
    type(IXTdataset_2d),allocatable::arrayd2d(:)
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::emode
    real(dp),intent(in)::theta(:),delay(:),L2(:),L1,efixed
    type(IXTaxis),intent(in)::axis_out
    integer(i4b)::ny,i

    !histogram only for the moment
    if ((x_hist(d2d) .neqv. .true.) .and. (y_hist(d2d) .eqv. .false.))then    
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'units change only permitted on histogram data (IXFunits_dataset_2d)') 
    endif

    ny=size(d2d%y)

    if(size(L2) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'L2 input must be length of d2d array (IXFunits_dataset_2d)') 
    endif
    if(size(theta) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'theta input must be length of d2d array (IXFunits_dataset_2d)') 
    endif
    if(size(delay) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'delaytime input must be length of d2d array (IXFunits_dataset_2d)') 
    endif

    if(status == IXCseverity_error) return

    if(allocated(arrayd2d))then
       call IXFdealloc(arrayd2d,status)
    endif

    call IXFalloc(arrayd2d,ny,status)

    do i=1,ny
       call units_common(d2d,i,arrayd2d(i),emode,efixed,L1,L2(i),theta(i),delay(i),axis_out,status)
       if (status == IXCseverity_error) return
    enddo
    ! kept for retesting purposes    
    !    call IXFdestroy(d2d,status)
  end subroutine units_single_array

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine units_array_array(d2d_in,d2d_out,status,emode,efixed,L1,L2,theta,delay,axis_out)
    implicit none
    type(IXTdataset_2d),allocatable,intent(in)::d2d_in(:)
    ! would be intent(out) but for compiler issues
    type(IXTdataset_2d),allocatable::d2d_out(:)    
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::emode
    real(dp),intent(in)::theta(:),delay(:),L2(:),L1,efixed
    type(IXTaxis),intent(in)::axis_out
    integer(i4b)::ny,i

    ny=size(d2d_in)
    if(size(L2) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'L2 input must be length of d2d array') 
    endif
    if(size(theta) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'theta input must be length of d2d array') 
    endif
    if(size(delay) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'delaytime input must be length of d2d array') 
    endif
    if(status == IXCseverity_error) return

    if(allocated(d2d_out))then
       call IXFdealloc(d2d_out,status)
    endif

    do i=1,ny
       !histogram only for the moment
       if ((x_hist(d2d_in(i)) .neqv. .true.) .and. (y_hist(d2d_in(i)) .eqv. .false.))then    
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'units change only permitted on histogram data ') 
          call IXFdealloc(d2d_out,status)
          return
       endif
       call units_common(d2d_in(i),i,d2d_out(i),emode,efixed,L1,L2(i),theta(i),delay(i),axis_out,status)
       if (status == IXCseverity_error) return
    enddo

  end subroutine units_array_array
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine units_common(d2d_in,i,d2d_out,emode,efixed,L1,L2,theta,delay,axis_out,status)  
    implicit none
    type(IXTdataset_2d),intent(in)::d2d_in
    type(IXTdataset_2d),intent(out)::d2d_out
    integer(i4b),intent(in)::i,emode
    real(dp),intent(in)::L1,L2,theta,delay,efixed
    type(IXTaxis),intent(in)::axis_out
    type(IXTstatus),intent(inout)::status
    integer(i4b)::qopt_in,qopt_out,sgn_in,sgn_out,ilo,ihi,len_o
    real(dp)::shift_in,shift_out,ctot,gtot
    character(len=5)::units_O,units_I

    call IXFget_axis(axis_out,status,code=units_O)
    call IXFget_axis(d2d_in%x_axis,status,code=units_I)
    !    call IXFinitialise(d2d_out,status)
    call IXFmark_valid(d2d_out)

    call IXFunits_get_len_arr (units_I,	d2d_in%x, units_O, emode, delay,	L1,L2, theta, efixed, &
         ilo, ihi, ctot, gtot, sgn_in,	shift_in, qopt_in, sgn_out, shift_out,	qopt_out,status)
    if(status == IXCseverity_error)return
    len_o=ihi-ilo+1

    call IXFrealloc(d2d_out%x,len_o,.false.,status)
    call IXFrealloc(d2d_out%y,1,.false.,status)

    call IXFallocdims(d2d_out%signal,(/ len_o-1,1 /),status)
    call IXFallocdims(d2d_out%error,(/ len_o-1,1 /),status)
    d2d_out%y(1)=d2d_in%y(i)

    call IXFunits_convert (d2d_in%x,	d2d_in%signal(:,i),	d2d_in%error(:,i), emode, theta,	efixed, &
         d2d_in%x_distribution,ilo, ihi, ctot, gtot, sgn_in,	shift_in, qopt_in, sgn_out, shift_out,	qopt_out, &
         d2d_out%x, d2d_out%signal(:,1), d2d_out%error(:,1),status)	
    if(status == IXCseverity_error)return

    d2d_out%y_distribution=d2d_in%y_distribution
    d2d_out%x_distribution=d2d_in%x_distribution
    call IXFallocFortran(d2d_out%title,size(d2d_in%title),status)
    d2d_out%title=d2d_in%title
    call IXFcopy(axis_out,d2d_out%x_axis,status)
    call IXFcopy(d2d_in%s_axis,d2d_out%s_axis,status)
    call IXFcopy(d2d_in%y_axis,d2d_out%y_axis,status)

    call IXFcheck (d2d_out,status)

  end subroutine units_common
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine units_array(arrayd2d,status,emode,efixed,L1,L2,theta,delay,axis_out)
    implicit none
    type(IXTdataset_2d),allocatable,intent(inout)::arrayd2d(:)
    integer(i4b),intent(in)::emode
    real(dp),intent(in)::L1,L2(:),theta(:),delay(:),efixed
    type(IXTaxis),intent(in)::axis_out
    type(IXTstatus),intent(inout)::status
    integer(i4b)::qopt_in,qopt_out,sgn_in,sgn_out,ilo,ihi,len_o,ny,i
    real(dp)::shift_in,shift_out,ctot,gtot
    real(dp),allocatable::x_arr(:),s_arr(:),e_arr(:)
    character(len=5)::units_O,units_I

    ny=size(arrayd2d)
    if(size(L2) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'L2 input must be length of d2d array') 
    endif
    if(size(theta) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'theta input must be length of d2d array') 
    endif
    if(size(delay) /= ny)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'delaytime input must be length of d2d array') 
    endif

    if(status == IXCseverity_error) return
    call IXFget_axis(axis_out,status,code=units_O)

    do i=1,ny
       if ((x_hist(arrayd2d(i)) .neqv. .true.) .and. (y_hist(arrayd2d(i)) .eqv. .false.))then    
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'units change only permitted on histogram data ') 
       endif

       call IXFget_axis(arrayd2d(i)%x_axis,status,code=units_I)

       call IXFunits_get_len_arr (units_I, arrayd2d(i)%x, units_O, emode, delay(i),	L1,L2(i), theta(i), efixed, &
            ilo, ihi, ctot, gtot, sgn_in,	shift_in, qopt_in, sgn_out, shift_out,	qopt_out,status)
       if(status == IXCseverity_error)return

       len_o=ihi-ilo+1

       call IXFreallocdimsFortran(x_arr,(/ len_o /) , .false., status)
       call IXFreallocdimsFortran(s_arr,(/ len_o-1 /) , .false., status)
       call IXFreallocdimsFortran(e_arr,(/ len_o-1 /) , .false., status)

       call IXFunits_convert (arrayd2d(i)%x,arrayd2d(i)%signal(:,1),arrayd2d(i)%error(:,1), emode, theta(i),	efixed, &
            arrayd2d(i)%x_distribution,ilo, ihi, ctot, gtot, sgn_in,	shift_in, qopt_in, sgn_out, shift_out,	qopt_out, &
            x_arr, s_arr, e_arr,status)	
       if(status == IXCseverity_error)return

       call IXFrealloc(arrayd2d(i)%x,len_o,.false.,status)
       call IXFreallocdims(arrayd2d(i)%signal,(/ len_o-1,1 /),.false.,status)
       call IXFreallocdims(arrayd2d(i)%error,(/ len_o-1,1 /),.false.,status)

       arrayd2d(i)%x=x_arr
       arrayd2d(i)%signal(:,1)=s_arr
       arrayd2d(i)%error(:,1)=e_arr

       call IXFcheck(arrayd2d(i),status)
    enddo
    deallocate(x_arr,e_arr,s_arr)


  end subroutine units_array
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFcontract_arrayd2d_dataset_2d(arrayd2d,d2d,status)
    implicit none
    type(IXTdataset_2d),intent(in)::arrayd2d(:)
    type(IXTdataset_2d),intent(out)::d2d
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,sizey,sizex,wk_st,wk_end

    call IXFmark_valid(d2d)

    sizex=size(arrayd2d(1)%x)        

    ! allocate space in resultant d2d structure using information from all dataset_2d's in array
    ! need to assume that all x arrays will be the same length
    ! this routine will not work if there is a histogram along any of the y-axes
    !write a check....


    call IXFalloc(d2d%x,sizex,status)

    sizey=0
    do i=1,size(arrayd2d)
       sizey=sizey+size(arrayd2d(i)%y)
    enddo

    call IXFalloc(d2d%y,sizey,status)

    call IXFalloc(d2d%signal,size(arrayd2d(1)%signal,1),sizey,status)
    call IXFalloc(d2d%error,size(arrayd2d(1)%error,1),sizey,status) 

    d2d%x_distribution=arrayd2d(1)%x_distribution
    d2d%y_distribution=arrayd2d(1)%y_distribution

    call IXFallocFortran(d2d%title,size(arrayd2d(1)%title),status)
    d2d%title=arrayd2d(1)%title

    call IXFcopy(arrayd2d(1)%s_axis,d2d%s_axis,status)
    call IXFcopy(arrayd2d(1)%x_axis,d2d%x_axis,status)
    call IXFcopy(arrayd2d(1)%y_axis,d2d%y_axis,status)
    call IXFset_dataset_2d(d2d,status,x=arrayd2d(1)%x)


    wk_st=1
    wk_end=size(arrayd2d(1)%y)
    do i=1,size(arrayd2d)
       if(i>1)then
          wk_st=wk_end+1
          wk_end=wk_end+size(arrayd2d(i)%y)
       endif
       d2d%signal(:,wk_st:wk_end)=arrayd2d(i)%signal
       d2d%error(:,wk_st:wk_end)=arrayd2d(i)%error
       d2d%y(wk_st:wk_end)=arrayd2d(i)%y
    enddo
    ! set calls check function
    !    call IXFcheck(d2d,status)
  end subroutine IXFcontract_arrayd2d_dataset_2d

  subroutine IXFgetei_dataset_2d(d2d,Ei,ei_extras,monitor_no,Lm1,Lm2,charge,status)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d
    real(dp),intent(in)::Lm1,Lm2,charge 
    real(dp),intent(out)::Ei !starts as estimate, finishes as actual value determined
    real(dp),intent(out)::ei_extras(6)
    integer(i4b),intent(in)::monitor_no(2)
    type(IXTstatus),intent(inout)::status
    real(dp)::Tmin,Tmax,T_M(2),cc,A_M(2)
    real(dp),parameter::m1_tol=0.1,m2_tol=0.05
    integer(i4b)::i_M
    character(len=15)::number1, number2,number3
    character(len=2)::number
    ! ====== Perform manipulation:


    cc=sqrt(c_t_to_emev)

    ! code is understandably very sensitive to Lm1 and Lm2, to reproduce VMS homer values for energy, 
    ! may need to frig data to be the same as VMS input in IXFgetei_data

    ! since Lm_1 = L1 + L2_m1 /= Lm_1_VMS, where L2_m1 is populated by value in RAW file

    if(size(d2d%signal,2)< 2)then !either too many or too few spectra in datadset
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam,  &
            'at least two spectra must be in dataset, getei command called on inappropriate dataset_2d (IXFgetei_dataset_2d)') 
       Ei=0.0
       return
    endif

    do i_M=1,2
       if(i_M == 1)then
         if( Ei > 0.0)then
          !first estimated time of peak using initial Ei
           T_M(1)=cc*Lm1/sqrt(Ei) !in microseconds
           Tmin=(1.0-m1_tol)*T_M(1)  
           Tmax=(1.0+m1_tol)*T_M(1)
         else
       ! if no ei given a default range to search is defined, these are the values used in the VMS homer GET_EI.FOR
       ! when the first peak is found then it is carried through to the 2nd monitor spectrum
           Tmin=400
           Tmax=12000
         endif
       endif
       ! this method uses the first moment of the monitor peak to determine the energy
       ! if this is changed then it will also have to be changed in the method defined in
       ! IXFgetei_moments       
       call IXFmoments_dataset_2d(d2d,monitor_no(i_M),Tmin,Tmax,T_M(i_M),A_M(i_M),status) 
       if(status == IXCseverity_error)then
          Ei=0.0_dp
          return
       endif
       if(i_M == 1)then
          T_M(2)=T_M(1) * Lm2 / Lm1
          Tmin=(1.0-m2_tol)*T_M(2)
          Tmax=(1.0+m2_tol)*T_M(2)
       endif
    enddo
    Ei = ((cc*(Lm2-Lm1))/(T_M(2)-T_M(1)))**2
    write(number1,'(f15.3)')Ei

    call IXFwrite_line('Incident Energy = '//number1//' meV',status)
    write(number,'(i2)')monitor_no(1)
    write(number1,'(f15.3)')T_M(1)
    write(number2,'(f15.3)')A_M(1)
    write(number3,'(f15.3)')A_M(1)/charge
    call IXFwrite_line('M'//trim(adjustl(number))//' :  t = '//trim(adjustl(number1))//' us    I = '//trim(adjustl(number2))//' I / uAhr = '//trim(adjustl(number3)),status)
    write(number,'(i2)')monitor_no(2)
    write(number1,'(f15.3)')T_M(2)
    write(number2,'(f15.3)')A_M(2)    
    write(number3,'(f15.3)')A_M(2)/charge
    call IXFwrite_line('M'//trim(adjustl(number))//' :  t = '//trim(adjustl(number1))//' us    I = '//trim(adjustl(number2))//' I / uAhr = '//trim(adjustl(number3)),status)
    ei_extras(1)=T_M(1)
    ei_extras(2)=A_M(1)
    ei_extras(3)=A_M(1)/charge
    ei_extras(4)=T_M(2)
    ei_extras(5)=A_M(2)
    ei_extras(6)=A_M(2)/charge    
  end subroutine IXFgetei_dataset_2d

  !will calculate the moments of a desired spectrum in a dataset_2d object defined by index argument
  subroutine IXFmoments_dataset_2d(d2d,index,Tmin,Tmax,T_M,A_M,status,&
       & sig_area_o,B_M_o,sig_bkgd_o,c_o,c_fwhm_o,w_o,sig_xbar_o,sig_o,sig_sig_o,gam1_o,sig_gam1_o,gam2_o,sig_gam2_o)
    use IXMmoments_utils
    use IXMunspike
    use IXMregroup
    use IXMindex  
    implicit none
    type(IXTdataset_2d),intent(in)::d2d
    real(dp),intent(in)::Tmin,Tmax
    real(dp),intent(out)::T_M,A_M
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::index  ! spectrum index to use

    real(dp),intent(out),optional:: sig_area_o,B_M_o,sig_bkgd_o,c_o,c_fwhm_o,w_o
    real(dp),intent(out),optional:: sig_gam1_o, sig_gam2_o, gam1_o, gam2_o,sig_xbar_o, sig_o,sig_sig_o


    real(dp):: sig_area,B_M,sig_bkgd,c,c_fwhm,w,sig_gam1, sig_gam2, gam1, gam2,sig_xbar, sig,sig_sig
    real(dp),allocatable:: x_p(:),x_usp(:),y_usp(:),e_usp(:)
    real(dp),allocatable:: xh_rgp(:),x_rgp(:),y_rgp(:),e_rgp(:)
    real(dp):: prominence,bmin
    real(dp)::  h,pk_min, pk_max, bkgd_min, bkgd_max
    integer(i4b)::xh,n_rgp,imin,imax,i
    real(dp),parameter::npnts=8.0_dp,fac=1.2_dp,sfac=5.0_dp   
    character(len=4)::index_char
    ! ====== Perform manipulation:

    write(index_char,'(i4)')index

    !  convert x array to point data - for get moments routine
    xh=size(d2d%x)
    allocate(x_p(xh-1))
    forall(i=2:xh)x_p(i-1)=(d2d%x(i)+d2d%x(i-1))/2

    allocate(x_usp(xh-1),y_usp(xh-1),e_usp(xh-1))
    x_usp = x_p

    call IXFunspike_1d (status, x_p, d2d%signal(:,index), d2d%error(:,index), y_usp, e_usp,  fac=fac, sfac=sfac)

    prominence=4_dp
    ! to start prominence=4
    call get_moments (x_usp, y_usp, e_usp, Tmin, Tmax, prominence, status, &
         & A_M, B_M, c, c_fwhm, h, w, T_M, sig, gam1, gam2, &
         sig_area,sig_bkgd, sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max)

    if(c_fwhm > 0.0)then
       bmin=w/(1.5*npnts)
       !there was then some other stuff looking at prominence=2 if nothing found first time
    else
       !add status call  !!
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam,'no valid peak found, check Tmin & Tmax :Monitor index:'//index_char//' (IXFmoments_dataset_2d)')
       return
    endif
    !regroup function acts on histogram data

    call IXFregroup_1d_hist(d2d%x(1),bmin,d2d%x(xh),d2d%x_distribution,d2d%x,nout=n_rgp,status=status)

    allocate(xh_rgp(n_rgp+1),y_rgp(n_rgp),e_rgp(n_rgp),x_rgp(n_rgp))

    call IXFregroup_1d_hist(d2d%x(1),bmin,d2d%x(xh),d2d%x_distribution,d2d%x,y_usp,e_usp,&
         xh_rgp,y_rgp,e_rgp,status=status)

    forall(i=2:(n_rgp+1))x_rgp(i-1)=(xh_rgp(i)+xh_rgp(i-1))/2
    ! call get moments again, prominence=4 still
    call get_moments (x_rgp, y_rgp, e_rgp, Tmin, Tmax, prominence, status, &
         & A_M, B_M, c, c_fwhm, h, w, T_M, sig, gam1, gam2, &
         sig_area,sig_bkgd,sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max)
    if (c_fwhm == 0.0)then
       prominence=2_dp
       call get_moments (x_rgp, y_rgp, e_rgp, Tmin, Tmax, prominence, status, &
            & A_M, B_M, c, c_fwhm, h, w, T_M, sig, gam1, gam2, &
            sig_area,sig_bkgd,sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max)
    endif

    if (present(sig_area_o))sig_area_o=sig_area
    if (present(B_M_o))B_M_o=B_M
    if (present(sig_bkgd_o))sig_bkgd_o=sig_bkgd
    if (present(c_o))c_o=c
    if (present(c_fwhm_o))c_fwhm_o=c_fwhm
    if (present(w_o))w_o=w
    if (present(sig_gam1_o))sig_gam1_o=sig_gam1
    if (present(sig_gam2_o))sig_gam2_o=sig_gam2
    if (present(gam1_o))gam1_o=gam1
    if (present(gam2_o))gam2_o=gam2
    if (present(sig_xbar_o))sig_xbar_o=sig_xbar
    if (present(sig_sig_o))sig_sig_o=sig_sig
    if (present(sig_o))sig_o=sig      

    if((c == 0.0) .or. (w > (0.2*c_fwhm)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam,'no valid peak found, check initial Tmin & Tmax :Monitor index:'//index_char//' (IXFmoments_dataset_2d)')
       return
    endif
    ! find time channels across the peak
    imin=IXFlower_index(x_p,c_fwhm-(0.5*w))
    imax=IXFupper_index(x_p,c_fwhm+(0.5*w))
    if(imax-imin < 6) call IXFwrite_line('check data Monitor index '//index_char//' (IXFmoments_dataset_2d)',status)

    deallocate(x_usp,y_usp,e_usp,xh_rgp,x_rgp,y_rgp,e_rgp)  

  end subroutine IXFmoments_dataset_2d
  !! IXFunspike_dataset_2d, will unspike data from the signal array in an, row by row
  subroutine IXFunspike_dataset_2d(d2dout,d2d,ymin,ymax,fac,sfac,status)
    use IXMunspike
    implicit none
    type(IXTdataset_2d),intent(out)::d2dout
    type(IXTdataset_2d),intent(in)::d2d
    type(IXTstatus),intent(inout)::status
    real(dp),intent(in),optional::fac,sfac,ymin,ymax
    real(dp),allocatable::x_pt(:)
    integer(i4b)::i,j    

    call setup_unary_op_dataset_2d(d2dout,d2d,status)
    if(x_hist(d2d))then
       j=size(d2d%signal,1)
       allocate(x_pt(j))      
       x_pt=(d2d%x(1:j)+d2d%x(2:j+1))/2
       do i=1,size(d2d%signal,2)
          call IXFunspike_1d(status,x_pt,d2d%signal(:,i),d2d%error(:,i),&
               d2dout%signal(:,i),d2dout%error(:,i),ymin,ymax,fac,sfac)
       enddo
    else
       do i=1,size(d2d%signal,2)
          call IXFunspike_1d(status,d2d%x,d2d%signal(:,i),d2d%error(:,i),&
               d2dout%signal(:,i),d2dout%error(:,i),ymin,ymax,fac,sfac)
       enddo
    endif
    call finish_op_dataset_2d (d2dout,d2d,status)
  end subroutine IXFunspike_dataset_2d

  !! this will create an x_label and an s_label from an IXTdataset_2d object
  subroutine IXFmake_label_dataset_2d(d2d,x_label,y_label,s_label,status)
    implicit none
    type(IXTdataset_2d),intent(in)::d2d
    character(len=long_len),allocatable::x_label(:),y_label(:),s_label(:)
    type(IXTstatus)::status  
    call IXFmake_label_axis(d2d%x_axis,d2d%y_axis,d2d%s_axis,d2d%x_distribution, &
    d2d%y_distribution,x_label,y_label,s_label,status)
  end subroutine IXFmake_label_dataset_2d


  !-----------------------------------------------------------------------------------------------------------------------  
  !one dimensional array operations with dataset_2d, error array optional
#define IXD_DIM        IXFarray_X_
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_X_
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_X_
#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_X_
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_X_
#define IXD_OPERATION   Power
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_Y_
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_Y_
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_Y_
#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_Y_
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_DIM        IXFarray_Y_
#define IXD_OPERATION   Power
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !dataset_1d operations with dataset_2d, similar to above
  ! and operations between dataset_2d(n,1) with dataset_2d(n,n)
#define IXD_DIMXY        X_
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_DIMXY        X_
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_DIMXY        X_
#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_DIMXY        X_
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_DIMXY        X_
#define IXD_OPERATION   Power
#include "binary_ops.f90"


#define IXD_DIMXY        Y_
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_DIMXY        Y_
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_DIMXY        Y_
#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_DIMXY        Y_
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_DIMXY        Y_
#define IXD_OPERATION   Power
#include "binary_ops.f90"


  !-----------------------------------------------------------------------------------------------------------------------
  !!array of dataset_1d operations on a dataset_2d
  !operations between dataset_2d(n,1) with dataset_2d(n,n)
  !operations between d2d(m)(n,1) and d2d(1)[n,m]

#define IXD_OPERATION   Plus
#include "binary_ops.f90"

#define IXD_OPERATION   Minus
#include "binary_ops.f90"

#define IXD_OPERATION   Times
#include "binary_ops.f90"

#define IXD_OPERATION   Divide
#include "binary_ops.f90"

#define IXD_OPERATION   Power
#include "binary_ops.f90"
  !
  !-----------------------------------------------------------------------------------------------------------------------
  !!array/type binary operations on a dataset_2d
#define IXD_OPERATION   Plus
#define IXD_TYPE dataset_2d
#define IXD_DIMS :,:
#include "binary_ops.f90"

#define IXD_OPERATION   Minus
#define IXD_TYPE dataset_2d
#define IXD_DIMS :,:
#include "binary_ops.f90"

#define IXD_OPERATION   Times
#define IXD_TYPE dataset_2d
#define IXD_DIMS :,:
#include "binary_ops.f90"

#define IXD_OPERATION   Divide
#define IXD_TYPE dataset_2d
#define IXD_DIMS :,:
#include "binary_ops.f90"

#define IXD_OPERATION   Power
#define IXD_TYPE dataset_2d
#define IXD_DIMS :,:
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !**********PRIVATE FUNCTIONS  TO THE MODULE
  function x_hist(w1d)result(xhist)
    type (IXTdataset_2d),intent(in)::w1d
    logical:: xhist

    if(size(w1d%x) == size(w1d%signal,1)+1)then
       xhist=.true.
    else
       xhist=.false.
    endif

  end function x_hist

  function y_hist(w1d)result(yhist)
    type (IXTdataset_2d),intent(in)::w1d
    logical:: yhist

    if(size(w1d%y) == size(w1d%signal,2)+1)then
       yhist=.true.
    else
       yhist=.false.
    endif

  end function y_hist
  !******************test arrya subroutines

  pure integer(i4b) function IXFsize_x(w2d)result(len_x)
    implicit none
    type(IXTdataset_2d),intent(in)::w2d        
    if(associated(w2d%x))then
       len_x=size(w2d%x)
    else
       len_x=0
    endif
  end function IXFsize_x
  pure integer(i4b) function IXFsize_y(w2d)result(len_y)
    implicit none
    type(IXTdataset_2d),intent(in)::w2d    
    if(associated(w2d%y))then
       len_y=size(w2d%y)
    else
       len_y=0
    endif
  end function IXFsize_y


end module IXMdataset_2d




!  subroutine IXFdataset_1d_X_Plus_dataset_2dZZZZ(d2d,status,d1d)
!    use IXMdataset_1d
!    implicit none
!    type(IXTdataset_2d)::d2d
!    type(IXTdataset_1d)::d1d
!    real(dp),allocatable::res_v(:,:),res_e(:,:)
!    real(dp),pointer::v_s(:),v_e(:)
!    integer(i4b)::nsy,nsx
!    type(IXTstatus)::status
!
!    nsy=size(d2d%signal,2)
!    nsx=size(d2d%signal,1)
!
!    call IXFget_ptr_dataset_1d(d1d,signal=v_s,error=v_e)
!
!    call IXFallocdimsfortran(res_v,(/ nsx,nsy /),status)
!    call IXFallocdimsfortran(res_e,(/ nsx,nsy /),status)
!
!    call IXFarray_X_2dPlus(status,res_v,res_e,d2d%signal,d2d%error,v_s,v_e)
!    if (status == IXCseverity_error)return
!    call IXFset_dataset_2d(d2d,status,signal=res_v,error=res_e)
!
!    deallocate(res_v,res_e)
!
!  end subroutine IXFdataset_1d_X_Plus_dataset_2dZZZZ
