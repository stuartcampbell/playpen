!------------------------------
! MODULE: IXMpeaks
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 645 $ ($Date: 2006-04-06 07:39:28 -0400 (Thu, 06 Apr 2006) $)
!!
!! FORTRAN definition of IXMpeaks class
! peaks module
module IXMpeaks
  use IXMtype_definitions
  use IXMdatum_array
  use IXMmoments
  implicit none
  public :: IXTpeaks

  type IXTpeaks
     private
     type(IXTbase):: base	
     integer(i4b),pointer :: monitor_no(:)=>NULL()		!! No of monitor. 
     type(IXTdatum_array):: integral				!! Monitor integral and error
     real(dp),pointer:: irange_low(:)=>NULL()		!! Integration range lower limit
     real(dp),pointer:: irange_high(:)=>NULL()    !! Integration range higher limit
     ! length of array is equal to number of monitors defined, and same as length of moments array
     character(len=short_len):: integral_units='units'	!! Units in which the integration was performed
     type(IXTmoments):: moments		!! Various derived quantities for peaks (see separate derived data type)
     character(len=short_len):: moments_units='units'	!! Units in which the moments are expressed				
  end type IXTpeaks

#define IXD_TYPE peaks
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTpeaks class"
#define IXD_TYPE peaks
#define IXD_SQTYPE 'peaks'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_peaks(op, field, arg, status)
    implicit none
    type(IXTpeaks) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTpeaks', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'monitor_no', arg%monitor_no, status)
    call IXFoperation_run(op,'integral',arg%integral,status)
    call IXFoperation_run_ptr(op,'irange_low', arg%irange_low, status)
    call IXFoperation_run_ptr(op,'irange_high', arg%irange_high, status)
    call IXFoperation_run(op,'integral_units', arg%integral_units, status)
    call IXFoperation_run(op,'moments',arg%moments,status)
    call IXFoperation_run(op,'moments_units',arg%moments_units,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_peaks(peaks,status,monitor_no,integral, &
       irange_low,irange_high,integral_units,moments,moments_units,base,ref)
    implicit none
    type(IXTpeaks),intent(inout)::peaks
    type(IXTpeaks),optional,intent(in)::ref
    type(IXTbase),optional,intent(in) :: base  !! input: base class
    integer(i4b),optional,intent(in) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),optional,intent(in):: integral				!! Monitor integral and error
    real(dp),optional,intent(in):: irange_low(:)					!! Integration range low limit
    real(dp),optional,intent(in):: irange_high(:)					!! Integration range high limit
    character(len=*),optional,intent(in):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),optional,intent(in):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),optional,intent(in):: moments_units	!! Units in which the moments are expressed				
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFinitialised(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_peaks)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_init(peaks)
    else    
       if(IXFinitialised(peaks) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_peaks)')
       endif
       if(status == IXCseverity_error)return
    endif


    if (present(ref))call IXFset_peaks(peaks,status,&
         ref%monitor_no,ref%integral,ref%irange_low,ref%irange_high,&
         ref%integral_units,ref%moments,ref%moments_units,ref%base)

    if (present(base))call IXFset_base(peaks%base,status,ref=base)
    if (present(monitor_no))then
       call IXFrealloc(peaks%monitor_no,size(monitor_no),.false.,status)
       peaks%monitor_no=monitor_no
    endif

    if (present(integral))call IXFcopy(integral,peaks%integral,status)
    if (present(irange_low))then
       call IXFrealloc(peaks%irange_low,size(irange_low),.false.,status)
       peaks%irange_low=irange_low
    endif

    if (present(irange_high))then
       call IXFrealloc(peaks%irange_high,size(irange_high),.false.,status)
       peaks%irange_high=irange_high
    endif

    if (present(integral_units))peaks%integral_units=integral_units

    if (present(moments)) call IXFcopy(moments,peaks%moments,status)   

    if (present(moments_units))peaks%moments_units=moments_units

    call IXFcheck_peaks(peaks,status)

  end subroutine IXFset_peaks
  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_peaks(peaks,status,base,monitor_no,integral,&
       irange_low,irange_high,integral_units,moments,moments_units,wout)
    implicit none
    type(IXTpeaks),intent(inout)::peaks
    type(IXTpeaks),optional,intent(out)::wout
    type(IXTbase),optional,intent(out) :: base
    integer(i4b),optional,intent(out) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),optional,intent(out):: integral				!! Monitor integral and error
    real(dp),optional,intent(out):: irange_low(:)					!! Integration range low limit
    real(dp),optional,intent(out):: irange_high(:)					!! Integration range high limit
    character(len=*),optional,intent(out):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),optional,intent(out):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),optional,intent(out):: moments_units	!! Units in which the moments are expressed				
    type(IXTstatus)::status
    if (present(wout))call IXFset_peaks(wout,status,ref=peaks)

    if (present(base))call IXFset_base(base,status,ref=peaks%base)
    if(present(monitor_no))then 
       if(size(monitor_no) == size(peaks%monitor_no))then
          monitor_no=peaks%monitor_no
       else
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct size to receive monitor_no array (IXFget_peaks)')
       endif
    endif

    if (present(integral))call IXFcopy(peaks%integral,integral,status)

    if(present(irange_low))then 
       if(size(irange_low) == size(peaks%irange_low))then
          irange_low=peaks%irange_low
       else
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct size to receive irange_low array (IXFget_peaks)')
       endif
    endif

    if(present(irange_high))then 
       if(size(irange_high) == size(peaks%irange_high))then
          irange_high=peaks%irange_high
       else
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct size to receive irange_high array (IXFget_peaks)')
       endif
    endif

    if (present(integral_units))integral_units=peaks%integral_units

    if(present(moments)) call IXFcopy(peaks%moments,moments,status)

    if (present(moments_units))moments_units=peaks%moments_units

  end subroutine IXFget_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_peaks(peaks,status)
    implicit none  
    type(IXTpeaks)::peaks
    type(IXTstatus)::status

    call IXFcheck_base(peaks%base,status)
    call IXFcheck(peaks%integral,status)
    call IXFcheck(peaks%moments,status)

  end subroutine IXFcheck_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_peaks(peaks,status)
    implicit none
    type(IXTpeaks)::peaks
    type(IXTstatus)::status
    call IXFdestroy(peaks%base,status)

    if(IXFinitialised(peaks%integral))call IXFdestroy(peaks%integral,status)
    call IXFdealloc(peaks%monitor_no,status)
    call IXFdealloc(peaks%irange_low,status)
    call IXFdealloc(peaks%irange_high,status)
    if(IXFinitialised(peaks%moments)) call IXFdestroy(peaks%moments,status)

    call IXFclear_init(peaks)
  end subroutine IXFdestroy_peaks


  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_peaks(peaks,monitor_no,integral,irange_low,&
       irange_high,integral_units,moments,moments_units,status,base)  
    implicit none
    type(IXTpeaks)::peaks
    type(IXTstatus)::status
    type(IXTbase),intent(in),optional :: base  !! input: base class
    integer(i4b),intent(in) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),intent(in):: integral				!! Monitor integral and error
    real(dp),intent(in):: irange_low(:)					!! Integration range low limit
    real(dp),intent(in):: irange_high(:)					!! Integration range high limit
    character(len=*),intent(in):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),intent(in):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),intent(in):: moments_units	!! Units in which the moments are expressed				

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFinitialised(integral) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_peaks)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFinitialised(moments) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTmoments failure, all nested objects MUST be initialised (IXFcreate_peaks)')
    endif

    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_init(peaks)

    ! for the optional present base class then the full set command is called
    if(present(base))then
       ! set is called with all the components of the object
       call IXFset_peaks(peaks,status,monitor_no,integral,irange_low,irange_high,integral_units,moments,moments_units,base)
    else
       call IXFset_peaks(peaks,status,monitor_no,integral,irange_low,irange_high,integral_units,moments,moments_units)
    endif

  end subroutine IXFcreate_peaks


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_peaks(peaks,monitor_no,irange_low,irange_high)
    real(dp),pointer,optional::irange_low(:) !!output: irange_low array pointer
    real(dp),pointer,optional::irange_high(:) !!output: irange_high array pointer
    integer(i4b),pointer,optional::monitor_no(:)
    type(IXTpeaks),intent(in)::peaks !!input moderator 

    if (present(monitor_no))monitor_no=>peaks%monitor_no
    if (present(irange_low))irange_low=>peaks%irange_low
    if (present(irange_high))irange_high=>peaks%irange_high

  end subroutine IXFget_ptr_peaks
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_peaks(peaks,monitor_no,irange_high,irange_low)
    implicit none
    type(IXTpeaks),intent(in) :: peaks
    real(dp),optional,allocatable::irange_high(:)
    real(dp),optional,allocatable::irange_low(:)
    integer(i4b),optional,allocatable::monitor_no(:)

    if (present(monitor_no))then
       if (allocated(monitor_no))then
          if (size(monitor_no) /=  size(peaks%monitor_no) )then
             deallocate(monitor_no)
             allocate(monitor_no(size(peaks%monitor_no)))
          endif
       else
          allocate(monitor_no(size(peaks%monitor_no)))
       endif
       monitor_no=peaks%monitor_no   
    endif

    if (present(irange_high))then
       if (allocated(irange_high))then
          if (size(irange_high) /=  size(peaks%irange_high) )then
             deallocate(irange_high)
             allocate(irange_high(size(peaks%irange_high)))
          endif
       else
          allocate(irange_high(size(peaks%irange_high)))
       endif
       irange_high=peaks%irange_high   
    endif

    if (present(irange_low))then
       if (allocated(irange_low))then
          if (size(irange_low) /=  size(peaks%irange_low) )then
             deallocate(irange_low)
             allocate(irange_low(size(peaks%irange_low)))
          endif
       else
          allocate(irange_low(size(peaks%irange_low)))
       endif
       irange_low=peaks%irange_low   
    endif

  end subroutine IXFget_alloc_peaks

end module IXMpeaks
