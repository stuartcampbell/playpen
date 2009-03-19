!------------------------------
! MODULE: IXMdataset_1d
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMdataset_1d object 

module IXMdataset_1d
  use IXMdataset_common
  use IXMarraymanips
  use IXMaxis
  use IXMunits_utils
  implicit none
  public :: IXTdataset_1d
  type IXTdataset_1d
     private
     type(IXTbase) :: base
     character(len=long_len),allocatable ::  title(:)	!! Title of dataset for plotting purposes
     real(dp),pointer :: signal(:)=>NULL() !! Pointer to signal array
     real(dp),pointer :: error(:)=>NULL()  ! Pointer to associated error array
     type(IXTaxis)::s_axis
     real(dp), pointer :: x(:)=>NULL() !! Pointer to x array
     type(IXTaxis)::x_axis
     !! x-distribution
     !! - If X_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If X_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical ::  x_distribution=.false.
  end type IXTdataset_1d

#define IXD_TYPE	dataset_1d
#include "class_header.f90"

  private :: ts_plus_dataset_1d, tt_plus_dataset_1d, st_plus_dataset_1d 
  private :: ts_minus_dataset_1d, tt_minus_dataset_1d, st_minus_dataset_1d
  private :: ts_times_dataset_1d, tt_times_dataset_1d, st_times_dataset_1d
  private :: ts_divide_dataset_1d, tt_divide_dataset_1d, st_divide_dataset_1d
  private ::  ta_plus_dataset_1d, at_plus_dataset_1d 
  private ::  ta_minus_dataset_1d, at_minus_dataset_1d
  private ::  ta_times_dataset_1d, at_times_dataset_1d
  private ::  ta_divide_dataset_1d, at_divide_dataset_1d

  private :: ts_power_dataset_1d, tt_power_dataset_1d, st_power_dataset_1d
  private :: setup_binary_op_dataset_1d,setup_unary_op_dataset_1d,finish_op_dataset_1d
  private :: x_hist

  !! Interface for subroutine to add two dataset_1d objects, a dataset_1d to an array of dataset_1d's or vice versa,
  !!a dataset_1d to a 1d array or vice versa or a dataset_1d to a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFplus_dataset_1d(wres,w1,w2,status) Type/Type 
  !!n call IXFplus_dataset_1d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFplus_dataset_1d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFplus_dataset_1d(wres,w1,arg2,status) Type/Array 
  !!n call IXFplus_dataset_1d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_1d object
  !! -w#	    -> input dataset_1d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	 
  interface IXFplus_dataset_1d
     module procedure ta_plus_dataset_1d, at_plus_dataset_1d
     module procedure ts_plus_dataset_1d, tt_plus_dataset_1d, st_plus_dataset_1d
  end interface
  interface IXFplus
     module procedure ta_plus_dataset_1d, at_plus_dataset_1d
     module procedure ts_plus_dataset_1d, tt_plus_dataset_1d, st_plus_dataset_1d
  end interface

  !! Interface for subroutine to subtract two dataset_1d objects, 
  !! a dataset_1d from a 1d array or vice versa or a dataset_1d from a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFminus_dataset_1d(wres,w1,w2,status) Type/Type 
  !!n call IXFminus_dataset_1d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFminus_dataset_1d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFminus_dataset_1d(wres,w1,arg2,status) Type/Array 
  !!n call IXFminus_dataset_1d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_1d object
  !! -w#	    -> input dataset_1d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	
  interface IXFminus_dataset_1d
     module procedure ta_minus_dataset_1d, at_minus_dataset_1d
     module procedure ts_minus_dataset_1d, tt_minus_dataset_1d, st_minus_dataset_1d
  end interface
  interface IXFminus
     module procedure ta_minus_dataset_1d, at_minus_dataset_1d
     module procedure ts_minus_dataset_1d, tt_minus_dataset_1d, st_minus_dataset_1d
  end interface

  !! Interface for subroutine to multiply two dataset_1d objects,
  !! a dataset_1d with a 1d array or vice versa or a dataset_1d with a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFtimes_dataset_1d(wres,w1,w2,status) Type/Type 
  !!n call IXFtimes_dataset_1d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFtimes_dataset_1d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFtimes_dataset_1d(wres,w1,arg2,status) Type/Array 
  !!n call IXFtimes_dataset_1d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_1d object
  !! -w#	    -> input dataset_1d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	
  interface IXFtimes_dataset_1d
     module procedure ta_times_dataset_1d, at_times_dataset_1d
     module procedure ts_times_dataset_1d, tt_times_dataset_1d, st_times_dataset_1d
  end interface
  interface IXFtimes
     module procedure ta_times_dataset_1d, at_times_dataset_1d
     module procedure ts_times_dataset_1d, tt_times_dataset_1d, st_times_dataset_1d
  end interface


  !! Interface for subroutine to divide two dataset_1d objects, 
  !! a dataset_1d by a 1d array or vice versa or a dataset_1d by a scalar or vice versa.   
  !!n naming convention:  tt = type,type, ta= type,array , at=array,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFdivide_dataset_1d(wres,w1,w2,status) Type/Type 
  !!n call IXFdivide_dataset_1d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFdivide_dataset_1d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFdivide_dataset_1d(wres,w1,arg2,status) Type/Array 
  !!n call IXFdivide_dataset_1d(wres,arg1,w2,status) Array/Type 
  !!
  !! -wres	-> output dataset_1d object
  !! -w#	    -> input dataset_1d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object
  interface IXFdivide_dataset_1d
     module procedure ta_divide_dataset_1d, at_divide_dataset_1d
     module procedure ts_divide_dataset_1d, tt_divide_dataset_1d, st_divide_dataset_1d
  end interface
  interface IXFdivide
     module procedure ta_divide_dataset_1d, at_divide_dataset_1d
     module procedure ts_divide_dataset_1d, tt_divide_dataset_1d, st_divide_dataset_1d
  end interface

  !! Interface for subroutine to raise one dataset_1d object to the power of another, or a dataset_1d to a scalar or vice versa.   
  !!n naming convention:  tt = type,type , ts= type,scalar , st=scalar,type
  !!n Subroutines overloaded for these different cases 
  !!
  !!Calling convention:
  !!n call IXFpower_dataset_1d(wres,w1,w2,status) Type/Type 
  !!n call IXFpower_dataset_1d(wres,w1,arg2,status) Type/Scalar 
  !!n call IXFpower_dataset_1d(wres,arg1,w2,status) Scalar/Type 
  !!n call IXFpower_dataset_1d(wres,w1,arg2,status) Type/Array 
  !!n call IXFpower_dataset_1d(wres,arg1,w2,status) Array/Type   
  !!
  !! -wres	-> output dataset_1d object
  !! -w#	    -> input dataset_1d object
  !! -arg#	-> input scalar
  !! -status  -> IXTstatus object	
  interface IXFpower_dataset_1d
     module procedure ta_power_dataset_1d, at_power_dataset_1d
     module procedure ts_power_dataset_1d, tt_power_dataset_1d, st_power_dataset_1d
  end interface
  interface IXFpower
     module procedure ta_power_dataset_1d, at_power_dataset_1d
     module procedure ts_power_dataset_1d, tt_power_dataset_1d, st_power_dataset_1d
  end interface

  interface IXFexp
     module procedure IXFexp_dataset_1d
  end interface

  interface IXFlog
     module procedure IXFlog_dataset_1d
  end interface

  interface IXFlog10
     module procedure IXFlog10_dataset_1d
  end interface


  interface IXFsin
     module procedure IXFsin_dataset_1d
  end interface

  interface IXFcos
     module procedure IXFcos_dataset_1d
  end interface

  interface IXFtan
     module procedure IXFtan_dataset_1d
  end interface

  interface IXFsinh
     module procedure IXFsinh_dataset_1d
  end interface

  interface IXFcosh
     module procedure IXFcosh_dataset_1d
  end interface

  interface IXFtanh
     module procedure IXFtanh_dataset_1d
  end interface

  interface IXFunspike
     module procedure IXFunspike_dataset_1d
  end interface

  interface operator(+)
     module procedure IXFdataset_1dPlusAAop !, IXFdataset_1dPlusAS, IXFdataset_1dPlusSA
  end interface


contains

#define IXD_DESCRIPTION	"IXTdataset_1d class"
#define IXD_TYPE dataset_1d
#define IXD_SQTYPE 'dataset_1d'
#include "class_base.f90"


  !----------------------------------------------------------------------------------------------------------------------- 
  !!  subroutine which returns the pointers of "array" elements of a dataset_1d object. These pointers
  !!  may then be used in another subroutine. It only returns pointers defined by their keyword.
  subroutine IXFget_ptr_dataset_1d(w1,x,signal,error)
    real(dp),pointer,optional::signal(:) !!output: signal array pointer
    real(dp),pointer,optional::error(:) !!output: error array pointer
    real(dp),pointer,optional::x(:) !!output: x-array pointer
    type(IXTdataset_1d),intent(in)::w1 !!input dataset_1d 

    if (present(signal))signal=>w1%signal
    if (present(error))error=>w1%error
    if (present(x))x=>w1%x

  end subroutine IXFget_ptr_dataset_1d



  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFdestroy_dataset_1d(w1d,status)
    type(IXTdataset_1d)::w1d
    type(IXTstatus)::status

    call IXFdestroy(w1d%base,status)
    call IXFdealloc(w1d%x,status)
    call IXFdealloc(w1d%signal,status)
    call IXFdealloc(w1d%error,status)
    call IXFdestroy(w1d%x_axis,status)
    call IXFdestroy(w1d%s_axis,status)
    deallocate(w1d%title)
    !    w1d%title='title'
    !    w1d%s_label='s-label' 
    !    w1d%x_label='x-label'
    !    w1d%x_axis='x-axis'	
    w1d%x_distribution=.false.

    call IXFclear_valid(w1d)

  end subroutine IXFdestroy_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which returns elements of a dataset_1d object. For "array" elements it assumes that the
  !! lengths of supplied arrays are the correct size to copy the object arrays into. If this is not the 
  !! case then an error is generated, for this circumstance the IXFget_alloc_dataset_1d subroutine must
  !! then be used.
  subroutine IXFget_dataset_1d(dataset_1d,status,title,signal,error,s_axis,x,x_axis,x_distribution,wout)
    implicit none
    type(IXTdataset_1d),intent(in)::dataset_1d !!input: dataset_1d used to fill output
    type(IXTdataset_1d),optional,intent(out):: wout !!input:: object passed if a copy of a dataset is required
    character(len=*),optional,intent(out) :: title(:) !!input: title passed
    real(dp),optional,intent(out) :: signal(:)!!input: signal ARRAY passed
    real(dp),optional,intent(out):: error(:) !!input: error ARRAY passed
    type(IXTaxis),optional,intent(out) :: s_axis !!!!input: s_axis passed
    real(dp), optional,intent(out) :: x(:) !!input: x ARRAY passed
    type(IXTaxis),optional,intent(out) :: x_axis	 !!input: x_axis passed
    logical,optional,intent(out) :: x_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status

    if(present(wout))call IXFcopy(dataset_1d,wout,status)


    call IXFget_real_array(dataset_1d%x,status,x)
    call IXFget_real_array(dataset_1d%signal,status,signal)
    call IXFget_real_array(dataset_1d%error,status,error)

    if (status == IXCseverity_error) return

    if (present(title))title=dataset_1d%title
    if (present(s_axis))call IXFcopy(dataset_1d%s_axis,s_axis,status)
    if (present(x_axis))call IXFcopy(dataset_1d%x_axis,x_axis,status)
    if (present(x_distribution))x_distribution=dataset_1d%x_distribution

  end subroutine IXFget_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !!  subroutine which returns "array" elements of a dataset_1d object into allocatable arrays.
  !! If the array passed has been allocated, it checks the array length is correct, otherwise
  !! it deallocates the passed array and reallocates it to an appropriate length.
  subroutine IXFget_alloc_dataset_1d(w1,status,title,signal,error,s_axis,x,x_axis,x_distribution,wout)
    type(IXTdataset_1d),intent(in)::w1 !!input: dataset_1d used to fill output
    type(IXTdataset_1d),optional,intent(out):: wout !!input:: object passed if a copy of a dataset is required
    character(len=*),optional,allocatable :: title(:) !!input: title passed
    real(dp),allocatable,optional::signal(:) !!signal array pointer
    real(dp),allocatable,optional::error(:) !!error array pointer
    real(dp),allocatable,optional::x(:) !!x-array pointer
    type(IXTaxis),optional,intent(out) :: s_axis !!!!input: s_axis passed
    type(IXTaxis),optional,intent(out) :: x_axis	 !!input: x_axis passed
    logical,optional,intent(out) :: x_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status

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
    call IXFget_dataset_1d(w1,status,title,signal,error,s_axis,x,x_axis,x_distribution,wout)

  end subroutine IXFget_alloc_dataset_1d


  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which when given the appropriate characteristics of a dataset_1d object 
  !! (lengths x and signal array, histogram and distribution data flags) will assign memory for its elements,
  !! the object may then be filled with data in another function/subroutine
  subroutine IXFmake_dataset_1d(dataset_1d,nx,dist,hist,status)
    implicit none
    integer(i4b),intent(in)::nx !! input: length of x array of dataset_1d object to be created
    type(IXTdataset_1d),intent(inout)::dataset_1d  !! dataset_1d object to be created to be created
    logical,intent(in)::hist !! x_histogram flag of dataset_1d object to be created
    logical,intent(in)::dist !! x_distribtion of dataset_1d object to be created
    type(IXTstatus),intent(inout)::status !! error status object
    integer(i4b)::ns !  length of signal array of dataset_1d object to be created

    if(hist)then
       ns=nx-1
    else
       ns=nx
    endif

    call IXFrealloc(dataset_1d%x,nx,.false.,status)
    call IXFrealloc(dataset_1d%signal,ns,.false.,status)
    call IXFrealloc(dataset_1d%error,ns,.false.,status)

    dataset_1d%x_distribution=dist

    call IXFmark_valid(dataset_1d)

  end subroutine IXFmake_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_dataset_1d(dataset_1d,title,signal,error,s_axis,x,x_axis,x_distribution,status)
    implicit none
    type(IXTdataset_1d),intent(out)::dataset_1d  !! the dataset_1d object to be created
    character(len=*),intent(in) :: title(:) !!input: title passed
    real(dp),intent(in) :: signal(:)!!input: signal ARRAY passed
    real(dp),intent(in):: error(:) !!input: error ARRAY passed
    type(IXTaxis),intent(in) :: s_axis !!!!input: s_label passed
    real(dp),intent(in) :: x(:) !!input: x ARRAY passed
    type(IXTaxis),intent(in) :: x_axis	 !!input: x_axis passed
    logical,intent(in) :: x_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status !! error status object

    ! nested objects should be tested for initialisation, this shows they have been created properly

    if( IXFvalid(s_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFcreate_dataset_1d)')
    endif

    if( IXFvalid(x_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFcreate_dataset_1d)')
    endif
    if(status == IXCseverity_error)return


    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(dataset_1d)

    call IXFset_dataset_1d(dataset_1d,status,title,signal,error,s_axis,x,x_axis,x_distribution)

    ! check called in the set routine

  end subroutine IXFcreate_dataset_1d

  !! IXFcreate_xye_dataset_1d, will create an IXTdataset_1d object using an x,signal,error array as input, 
  !! the rest of the values will be default 
  subroutine IXFcreatexye_dataset_1d(dataset_1d,x,signal,error,status)
    implicit none
    type(IXTdataset_1d),intent(inout)::dataset_1d
    real(dp),intent(in)::x(:),signal(:),error(:) !! arrays to populate the object with
    type(IXTstatus),intent(inout)::status !!error status object
    type(IXTaxis):: s_axis,x_axis !! default axis labels
    logical :: x_dist
    character(long_len),allocatable::nullcaption(:)
    character(len=long_len) ::  title(1)	!! default title
    title(1)='title'
    x_dist=.false.
    allocate(nullcaption(1))
    nullcaption=' '
    call IXFcreate_axis(s_axis,nullcaption,IXCnullunits,IXCnullcode,status)
    call IXFcreate_axis(x_axis,nullcaption,IXCnullunits,IXCnullcode,status)
    deallocate(nullcaption)
    call IXFcreate_dataset_1d(dataset_1d,title,signal,error,s_axis,x,x_axis,x_dist,status)

  end subroutine IXFcreatexye_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_dataset_1d(dataset_1d,status,title,signal,error,s_axis,x,&
       x_axis,x_distribution,ref)
    implicit none
    type(IXTdataset_1d),intent(inout)::dataset_1d  !! the dataset_1d object to be created
    type(IXTdataset_1d),intent(in),optional::ref !! a reference dataset_1d object
    character(len=*),optional,intent(in) :: title(:) !!input: title passed
    real(dp),optional,intent(in) :: signal(:)!!input: signal ARRAY passed
    real(dp),optional,intent(in):: error(:) !!input: error ARRAY passed
    type(IXTaxis),optional,intent(in) :: s_axis !!!!input: s_label passed
    real(dp), optional,intent(in) :: x(:) !!input: x ARRAY passed
    type(IXTaxis),optional,intent(in) :: x_axis	 !!input: x_axis passed
    logical,optional,intent(in) :: x_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status !! error status object

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_dataset_1d)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(dataset_1d)
    else    
       if(IXFvalid(dataset_1d) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_dataset_1d)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref)) call IXFset_dataset_1d(dataset_1d,status,ref%title,ref%signal, &
         ref%error,ref%s_axis,ref%x,ref%x_axis,ref%x_distribution)

    if (present(title))then
       call IXFreallocFortran(dataset_1d%title,size(title),.false.,status)       
       dataset_1d%title=title       
    endif

    if (present(s_axis))call IXFcopy(s_axis,dataset_1d%s_axis,status)
    if (present(x_axis))call IXFcopy(x_axis,dataset_1d%x_axis,status)
    if (present(x_distribution))dataset_1d%x_distribution=x_distribution

    call IXFset_real_array(dataset_1d%x,status,x)
    call IXFset_real_array(dataset_1d%signal,status,signal)
    call IXFset_real_array(dataset_1d%error,status,error)

    call IXFcheck_dataset_1d(dataset_1d,status)

  end subroutine IXFset_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_dataset_1d(op, field, arg, status)
    implicit none
    type(IXTdataset_1d),intent(inout) :: arg
    type(IXToperation),intent(inout) :: op
    type(IXTstatus),intent(inout) :: status
    character(len=*),intent(in) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdataset_1d', field, status,arg%base,cont_op)
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
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! check routine for dataset_1d object: verifies the array lengths are compatible with eachother 
  !! and also agree with the histogram flag assigned
  subroutine IXFcheck_dataset_1d(w1, status)
    implicit none
    type(IXTdataset_1d),intent(in) :: w1 !!input dataset_1d object to be checked
    type(IXTStatus),intent(inout) :: status !! error status object
    integer :: xflag , sflag

    if( .not. associated(w1%x))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_1d%x must be associated with an array (IXFcheck_dataset_1d)')
    endif
    if( .not. associated(w1%signal))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_1d%signal must be associated with an array (IXFcheck_dataset_1d)')
    endif
    if( .not. associated(w1%error))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_1d%error must be associated with an array (IXFcheck_dataset_1d)')
    endif

    if( .not. allocated(w1%title))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_1d%title array must be allocated (IXFcheck_dataset_1d)')
    endif

    call IXFcheck(w1%x_axis,status)
    call IXFcheck(w1%s_axis,status)

    if(status == IXCseverity_error)return


    xflag=size(w1%x)
    sflag=size(w1%signal)

    !check for empty array
    if(size(w1%signal) < 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'must have at least one element in signal array (IXFcheck_dataset_1d)')
    endif

    ! check signal and error are congruent
    if (size(w1%signal) /= size(w1%error) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'signal and error data incompatible  (IXFcheck_dataset_1d)')
    endif

    ! check it is either point or histogram data
    if(xflag /= sflag)then
       if(xflag /= sflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined (IXFcheck_dataset_1d)')
       endif
    endif


  end subroutine IXFcheck_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to integrate a dataset_1d object between two limits. The output is a
  !! IXTdatum object which has a value and an error. The routine will integrate histogram and point data.
  subroutine IXFintegrate_dataset_1d(ires, w1, xmin_in, xmax_in, status)
    use IXMdatum
    use IXMintegrate
    implicit none

    type(IXTdatum),intent(out) :: ires !! output: IXTdatum
    type(IXTstatus),intent(inout) :: status !! error status object
    real(dp),intent(in) :: xmax_in,xmin_in !! integration limit
    type(IXTdataset_1d),intent(in) :: w1 !!input: dataset_1d object to be integrated over
    logical::xhist

    xhist=x_hist(w1)

    if(xhist)then
       call IXFintegrate_1d_hist(ires%val,ires%err,w1%x,w1%signal,w1%error,w1%x_distribution, xmin_in, xmax_in,status)
    else
       call IXFintegrate_1d_points(ires%val,ires%err,w1%x,w1%signal,w1%error, xmin_in, xmax_in,status)
    endif

  end subroutine IXFintegrate_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to rebunch a dataset_1d object. Regroups data by adding together nbunch successive bins.
  subroutine IXFrebunch_dataset_1d(wres,w1,nbunch,status)
    use IXMrebunch
    implicit none
    type(IXTdataset_1d),intent(in)::w1 !! input: dataset_1d object to be rebunched
    type(IXTdataset_1d),intent(out)::wres !!output: rebunched dataset-1d
    integer(i4b),intent(in)::nbunch !! input: number of bins to be grouped together
    type(IXTstatus),intent(inout) :: status !! eroor status object
    logical::xhist

    xhist=x_hist(w1)
    call IXFallocFortran(wres%title,size(w1%title),status)
    wres%title=w1%title
    call IXFcopy(w1%s_axis,wres%s_axis,status)

    wres%x_distribution=w1%x_distribution
    call IXFcopy(w1%x_axis,wres%x_axis,status)


    if(xhist)then
       call IXFrebunchHist(w1%x,w1%signal,w1%error,wres%x,wres%signal,wres%error,w1%x_distribution,nbunch,status)
    else
       call IXFrebunchPoints(w1%x,w1%signal,w1%error,wres%x,wres%signal,wres%error,nbunch,status)
    endif

    if (status == IXCseverity_error) return
    call IXFmark_valid(wres)

    !has it written back a valid dataset_1d ?
    call IXFcheck_dataset_1d(wres, status)

  end subroutine IXFrebunch_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which will rebin a dataset_1d object provided it is histogram data.
  !! It will accept a reference dataset_1d object or a descriptor array to calculate the new bin boundaries
  subroutine IXFrebin_dataset_1d(wres,status,w2,Xdesc,Xref)
    use IXMrebin
    implicit none
    type (IXTdataset_1d),intent(out):: wres !! output: rebinned dataset_1d object
    type (IXTdataset_1d),intent(in):: w2 !! input: dataset_1d to be rebinned
    type (IXTdataset_1d),intent(in),optional::Xref !!optional input - reference dataset  
    real(dp),intent(in),optional::Xdesc(:) !! optional input - descriptor array    
    type (IXTstatus),intent(inout)::status
    integer(i4b)::nx_out !new length of x-array
    logical::xhist

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
       call IXFalloc(wres%signal, nx_out-1 ,status)
       call IXFalloc(wres%error,  nx_out-1 ,status)
       ! 
       ! now fill the wres%x with data (it is called using x_out, but it is just a list of numbers no real reference to "x")
       call IXFrebin_1d_hist_get_arr(Xdesc,x_in=w2%x,x_out=wres%x,status=status)
       ! perform rebinning with new output x_arrax
       call IXFrebin_1d_hist( w2%x ,w2%signal,w2%error,wres%x ,wres%signal ,wres%error ,w2%x_distribution ,status)
    endif

    if(present(Xref))then
       call IXFalloc(wres%x,size(Xref%x),status)
       call IXFalloc(wres%signal,  size(Xref%x)-1   ,status)
       call IXFalloc(wres%error,   size(Xref%x)-1   ,status)
       ! create the output x array with the reference dataset_2d x array -> Xref%x
       wres%x=Xref%x
       call IXFrebin_1d_hist( w2%x ,w2%signal,w2%error,wres%x ,wres%signal ,wres%error ,w2%x_distribution ,status)
    endif
    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w2%title),status)
    wres%title=w2%title
    call IXFcopy(w2%s_axis,wres%s_axis,status)
    wres%x_distribution=w2%x_distribution
    call IXFcopy(w2%x_axis,wres%x_axis,status)
    wres%x_axis=w2%x_axis
    call IXFmark_valid(wres)
    call IXFcheck_dataset_1d(wres, status)

  end subroutine IXFrebin_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to regroup a dataset_1d object with bin boundaries to ensure that bins have minimum width 
  !! determined by the parameter dx, but ensuring the bin boundaries are always coincedent with original
  !! bin boundaries. dx is defined in the input array param[xlo,dx,xhi], where xlo and xhi are the minimum
  !! and maximum array bin boundaries.
  subroutine IXFregroup_dataset_1d(wres,w1,param,status)
    use IXMregroup
    implicit none
    type(IXTdataset_1d),intent(out)::wres !!output: dataset_1d object
    type(IXTdataset_1d),intent(in)::w1 !!input: dataset_1d to be regrouped
    type(IXTstatus),intent(inout) :: status !! error status object
    real(dp),intent(in) :: param(3) !!input: param array defines regrouping parameters -> (xlo,dx,xhi)
    integer(i4b) :: ny_out !no. of output xbins
    logical::xhist

    xhist=x_hist(w1)
    if(.not.(xhist))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'regrouping of data only permitted on histogram data ') 
    endif
    ! call subroutine to find the no.of bins of the output y array
    call IXFregroup_1d_hist(param(1),param(2),param(3),&
         w1%x_distribution,w1%x,nout=ny_out,status=status)
    if (status == IXCseverity_error) return
    ! assign memory space to wres
    call IXFalloc(wres%x,ny_out+1,status)
    call IXFalloc(wres%signal,ny_out,status)
    call IXFalloc(wres%error,ny_out,status)

    if (status == IXCseverity_error) return

    call IXFregroup_1d_hist(param(1),param(2),param(3),w1%x_distribution,&
         w1%x,w1%signal,w1%error,wres%x,wres%signal,wres%error,status=status)
    if (status == IXCseverity_error) return
    call IXFallocFortran(wres%title,size(w1%title),status)
    wres%title=w1%title
    call IXFcopy(w1%s_axis,wres%s_axis,status)
    wres%x_distribution=w1%x_distribution
    call IXFcopy(w1%x_axis,wres%x_axis,status)
    call IXFmark_valid(wres)
    call IXFcheck_dataset_1d(wres, status)

  end subroutine IXFregroup_dataset_1d


  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine to shift the x-array of a dataset_1d object.
  subroutine IXFshift_dataset_1d(wres,w1,status,shift)
    use IXMshift
    implicit none
    type(IXTdataset_1d), intent(in) :: w1 !! input: dataset_1d to be operated
    type(IXTdataset_1d),intent(out) :: wres !! output: dataset_1d
    real(dp),intent(in)::shift !! amount to shift x-array 
    type(IXTstatus),intent(inout) :: status !! error status object

    !allocate memory for output dataset_1d
    call IXFalloc(wres%x,size(w1%x),status)
    call IXFalloc(wres%signal,size(w1%signal),status)
    call IXFalloc(wres%error,size(w1%error),status)
    call IXFshift(w1%x,wres%x,shift)

    wres%signal=w1%signal
    wres%error=w1%error
    call IXFallocFortran(wres%title,size(w1%title),status)
    wres%title=w1%title
    call IXFcopy(w1%s_axis,wres%s_axis,status)

    wres%x_distribution=w1%x_distribution
    call IXFcopy(w1%x_axis,wres%x_axis,status)
    call IXFmark_valid(wres)
    call IXFcheck_dataset_1d(wres, status)

  end subroutine IXFshift_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_binary_op_dataset_1d(wres, w1, w2, status)
    implicit none
    type(IXTdataset_1d),intent(in) :: w1, w2
    type(IXTdataset_1d),intent(out):: wres
    type(IXTstatus),intent(inout) :: status
    logical:: xhist_w1,xhist_w2

    xhist_w1=x_hist(w1)
    xhist_w2=x_hist(w2)

    !checks in data compatibility 
    !check x-axis

    ! need a units comparison    
    if(IXFcompare_units(w1%x_axis,w2%x_axis) .neqv. .true.) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x_axis incompatible for desired operation') 
    endif
    ! check s-label compatibility
    if(IXFcompare_units(w1%s_axis,w2%s_axis) .neqv. .true.) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 's_label incompatible for desired operation') 
    endif
    ! check histogram compatibility
    if( xhist_w1 .neqv. xhist_w2 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'histogram flags incompatible') 
    endif
    ! check distribution compatibilty
    if( w1%x_distribution .neqv. w2%x_distribution ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'distribution flags incompatible') 
    endif
    ! since now must be both same TYPE, only have to check number of elements
    if (size(w1%x) /= size(w2%x)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'no. of array elements incompatible') 
    endif
    if (status == IXCseverity_error) return
    ! if length test passed then make final checks on x-array values 

    !check all elements of x_array are the same
    !OPENGENIE currently checks identically the same 
    !a tolerance can easily be put in be it in percent or 1e-6
    if (sum(abs(w1%x - w2%x))/=0 ) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array elements incompatible') 
    endif
    if (status == IXCseverity_error) return

    ! we will allocate one array with standard fortran, the other with
    ! MATLAB memory. When we write the object back to matlab we will get an informational
    ! message telling us that some additional MATLAB memory had to be created for
    ! the fortran memory
    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%signal, size(w1%signal),.false., status)
    call IXFrealloc(wres%error, size(w1%error),.false., status)

  end subroutine setup_binary_op_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setup_unary_op_dataset_1d(wres,w1,status)
    type(IXTdataset_1d),intent(in) :: w1
    type(IXTdataset_1d),intent(out):: wres
    type(IXTstatus),intent(inout) :: status
    ! nochecks need to be made -> w1 SHOULD have been properly created

    ! we will allocate one array with standard fortran, the other with
    ! MATLAB memory. When we write the object back to matlab we will get an informational
    ! message telling us that some additional MATLAB memory had to be created for
    ! the fortran memory
    call IXFrealloc(wres%x, size(w1%x),.false., status)
    call IXFrealloc(wres%signal, size(w1%signal),.false., status)
    call IXFrealloc(wres%error, size(w1%error),.false., status)

  end subroutine setup_unary_op_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine finish_op_dataset_1d(wres, w1, status)
    implicit none
    type(IXTdataset_1d),intent(in) :: w1
    type(IXTdataset_1d),intent(inout)::  wres
    type(IXTstatus),intent(inout) :: status

    wres%x = w1%x
    !all foll variables are necessarily the same 
    call IXFcopy(w1%s_axis,wres%s_axis,status)
    call IXFcopy(w1%x_axis,wres%x_axis,status)
    wres%x_distribution=w1%x_distribution
    call IXFallocFortran(wres%title,size(w1%title),status)
    ! could be changed to show combination
    wres%title=w1%title
    wres%base=w1%base
    call IXFcheck_dataset_1d(wres,status)

  end subroutine finish_op_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  !! example of a possible function for a functional addition of two dataset
  function IXFdataset_1dPlusAAop(arg1, arg2) result(wres)
    implicit none
    type(IXTdataset_1d), intent(in) :: arg1, arg2
    type(IXTdataset_1d) :: wres
    type(IXTstatus) :: status
    !    call IXFPlusdataset_1d(wres, arg1, arg2, status)
    call IXFreport_status(status)
  end function IXFdataset_1dPlusAAop

  !! this will create an x_label and an s_label from an IXTdataset_1d object
  subroutine IXFmake_label_dataset_1d(d1d,x_label,s_label,status)
    implicit none
    type(IXTdataset_1d),intent(in)::d1d
    character(len=long_len),allocatable::x_label(:),s_label(:)
    type(IXTstatus)::status  
    call IXFmake_label_axis(d1d%x_axis,d1d%s_axis,d1d%x_distribution,x_label,s_label,status)
  end subroutine IXFmake_label_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        plus_dataset_1d
#define IXD_TYPE        dataset_1d
#define IXD_OPERATION   Plus
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        minus_dataset_1d
#define IXD_TYPE        dataset_1d
#define IXD_OPERATION   Minus
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        times_dataset_1d
#define IXD_TYPE        dataset_1d
#define IXD_OPERATION   Times
#include "binary_ops.f90"

  !----------------------------------------------------------------------------------------------------------------------- 
#define IXD_NAME        divide_dataset_1d
#define IXD_TYPE        dataset_1d
#define IXD_OPERATION   Divide
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME        power_dataset_1d
#define IXD_TYPE        dataset_1d
#define IXD_OPERATION   Power
#include "binary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	log_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Log
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	log10_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Log10
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	exp_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Exp
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	sin_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Sin
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	cos_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Cos
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	tan_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Tan
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	sinh_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Sinh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	cosh_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Cosh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define IXD_NAME	tanh_dataset_1d
#define IXD_TYPE	dataset_1d
#define IXD_OPERATION   Tanh
#include "unary_ops.f90"

  !!array/type binary operations on a dataset_2d
#define IXD_OPERATION   Plus
#define IXD_TYPE dataset_1d
#define IXD_DIMS :
#include "binary_ops.f90"

#define IXD_OPERATION   Minus
#define IXD_TYPE dataset_1d
#define IXD_DIMS :
#include "binary_ops.f90"

#define IXD_OPERATION   Times
#define IXD_TYPE dataset_1d
#define IXD_DIMS :
#include "binary_ops.f90"

#define IXD_OPERATION   Divide
#define IXD_TYPE dataset_1d
#define IXD_DIMS :
#include "binary_ops.f90"

#define IXD_OPERATION   Power
#define IXD_TYPE dataset_1d
#define IXD_DIMS :
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv1_dataset_1d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_1d),intent(in) :: w1
    type(IXTdataset_1d),intent(out):: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx

    call IXFalloc(wres%x,size(w1%x),status)
    call IXFalloc(wres%signal,size(w1%signal),status)
    call IXFalloc(wres%error,size(w1%error),status)

    if (status == IXCseverity_error) return

    if(x_hist(w1))then !determine bincentres of histogram data
       nx=size(w1%x)
       allocate(bincentres(nx-1))    
       bincentres=(w1%x(2:nx)+w1%x(1:nx-1))/2.0d0
       call IXFderiv_1_1d (bincentres,w1%signal,w1%error,wres%signal,wres%error,status)
       call finish_op_dataset_1d(wres, w1, status)
       deallocate(bincentres)
    else !point data so use xarray
       call IXFderiv_1_1d (w1%x,w1%signal,w1%error,wres%signal,wres%error,status)
       call finish_op_dataset_1d(wres, w1, status)
    endif
    call IXFmark_valid(wres)
    call IXFcheck_dataset_1d(wres,status)

  end subroutine IXFderiv1_dataset_1d


  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFderiv2_dataset_1d(wres,w1,status)
    use IXMderivative
    implicit none
    type (IXTdataset_1d),intent(in) :: w1
    type(IXTdataset_1d),intent(out):: wres
    type(IXTstatus),intent(inout) :: status
    real(dp),allocatable::bincentres(:)
    integer(i4b)::nx


    call IXFalloc(wres%x,size(w1%x),status)
    call IXFalloc(wres%signal,size(w1%signal),status)
    call IXFalloc(wres%error,size(w1%error),status)

    if (status == IXCseverity_error) return

    if(x_hist(w1))then !determine bincentres
       nx=size(w1%x)
       allocate(bincentres(nx-1))    
       bincentres=(w1%x(2:nx)+w1%x(1:nx-1))/2.0d0
       call IXFderiv_2_1d (bincentres,w1%signal,w1%error,wres%signal,wres%error,status)
       call finish_op_dataset_1d(wres, w1, status)
       deallocate(bincentres)
    else !point data so use xarray
       call IXFderiv_2_1d (w1%x,w1%signal,w1%error,wres%signal,wres%error,status)
       call finish_op_dataset_1d(wres, w1, status)
    endif
    call IXFmark_valid(wres)
    call IXFcheck_dataset_1d(wres,status)

  end subroutine IXFderiv2_dataset_1d


  !-----------------------------------------------------------------------------------------------------------------------
  function x_hist(w1d)result(xhist)
    type (IXTdataset_1d),intent(in)::w1d
    logical:: xhist

    if(size(w1d%x) == size(w1d%signal)+1)then
       xhist=.true.
    else
       xhist=.false.
    endif

  end function x_hist

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFcatarray_dataset_1d(array1d,dataset1d,status)
    implicit none
    type(IXTdataset_1d),intent(in)::array1d(:)
    type(IXTdataset_1d),intent(out)::dataset1d
    type(IXTstatus),intent(inout)::status
    integer(i4b)::nd,len_arr,xlen,astart,aend
    integer(i4b),allocatable::lengths(:)

    len_arr=size(array1d)
    xlen=0

    allocate(lengths(len_arr))

    do nd=1,len_arr
       if(x_hist(array1d(nd)))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'catenation of dataset_1d arrays only possible with point data') 
       endif
       if (status == IXCseverity_error) return

       lengths(nd)=size(array1d(nd)%x)
    enddo

    xlen=sum(lengths)

    call IXFalloc(dataset1d%x,xlen,status)   
    call IXFalloc(dataset1d%signal,xlen,status)
    call IXFalloc(dataset1d%error,xlen,status)


    do nd=1,len_arr
       astart=sum(lengths(1:nd-1))+1
       aend=sum(lengths(1:nd))
       dataset1d%x(astart:aend)=array1d(nd)%x
       dataset1d%signal(astart:aend)=array1d(nd)%signal
       dataset1d%error(astart:aend)=array1d(nd)%error
    enddo

    !fill up the other dataset_1d elements with nominal array_1d element
    call IXFcopy(array1d(1)%s_axis,dataset1d%s_axis,status)
    call IXFcopy(array1d(1)%x_axis,dataset1d%x_axis,status)
    ! take title information from first element in arrayd1d
    call IXFallocFortran(dataset1d%title,size(array1d(1)%title),status)
    dataset1d%title=array1d(1)%title

    dataset1d%x_distribution=array1d(1)%x_distribution
    call IXFmark_valid(dataset1d)
    call IXFcheck_dataset_1d(dataset1d,status)

  end subroutine IXFcatarray_dataset_1d

  !-----------------------------------------------------------------------------------------------------------------------
  subroutine setbase(d1d,name,status)
    implicit none
    type(IXTdataset_1d)::d1d
    type(IXTstatus)::status
    character(len=*)::name

    call IXFset_base(d1d%base,status,entry_name=name)

  end subroutine setbase
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFunits_dataset_1d(d1d,status,emode,efixed,L1,L2,theta,delay,axis_out)
    implicit none
    type(IXTdataset_1d),intent(inout)::d1d
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::emode
    real(dp),intent(in)::efixed,L1,L2,theta,delay
    type(IXTaxis),intent(in)::axis_out
    character(len=5)::units_O,units_I
    integer(i4b)::qopt_in,qopt_out,sgn_in,sgn_out,ilo,ihi,len_o
    real(dp)::shift_in,shift_out,ctot,gtot
    real(dp),allocatable::x_out(:),s_out(:),e_out(:)

    !histogram only for the moment
    if (x_hist(d1d) .neqv. .true.)then     
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'units change only permitted on histogram data ') 
    endif
    ! check somewhere we are dealing with standard units..... possibly done already in IXFunits_get_len_arr
    
    ! if units the same then don't worry

    if (IXFcompare_units(axis_out,d1d%x_axis) .eqv. .true.)return


    call IXFget_axis(axis_out,status,code=units_O)
    call IXFget_axis(d1d%x_axis,status,code=units_I)

    call IXFunits_get_len_arr (units_I, d1d%x, units_O, emode, delay, L1,L2, theta, efixed, &
         ilo, ihi, ctot, gtot, sgn_in, shift_in, qopt_in, sgn_out, shift_out, qopt_out,status)
    if(status == IXCseverity_error)return
    len_o=ihi-ilo+1

    allocate(x_out(len_o),s_out(len_o-1),e_out(len_o-1))

    call IXFunits_convert (d1d%x, d1d%signal, d1d%error, emode, theta, efixed, d1d%x_distribution, &
         ilo, ihi, ctot, gtot, sgn_in, shift_in, qopt_in, sgn_out, shift_out, qopt_out, x_out, s_out, e_out,status)
    if(status == IXCseverity_error)return
    call IXFset_dataset_1d(d1d,status,x=x_out,signal=s_out,error=e_out,x_axis=axis_out)

    deallocate(x_out,s_out,e_out)

  end subroutine IXFunits_dataset_1d
  !! IXFunspike_dataset_1d, will unspike data from the IXTdataset_1d signal array 
  subroutine IXFunspike_dataset_1d(d1dout,d1d,ymin,ymax,fac,sfac,status)
    use IXMunspike
    implicit none
    type(IXTdataset_1d),intent(out)::d1dout
    type(IXTdataset_1d),intent(in)::d1d
    real(dp),intent(in),optional::fac,sfac,ymin,ymax
    type(IXTstatus),intent(inout)::status
    real(dp),allocatable::x_pt(:)
    integer(i4b)::j

    call setup_unary_op_dataset_1d(d1dout,d1d,status)    
    if(x_hist(d1d))then
       j=size(d1d%signal)
       allocate(x_pt(j))      
       x_pt=(d1d%x(1:j)+d1d%x(2:j+1))/2
       call IXFunspike_1d(status,x_pt,d1d%signal,d1d%error,&
            d1dout%signal,d1dout%error,fac=fac,sfac=sfac)
    else
       call IXFunspike_1d(status,d1d%x,d1d%signal,d1d%error,&
            d1dout%signal,d1dout%error,fac=fac,sfac=sfac)
    endif
    call finish_op_dataset_1d (d1dout,d1d,status)
  end subroutine IXFunspike_dataset_1d

end module IXMdataset_1d
!-----------------------------------------------------------------------------------------------------------------------













