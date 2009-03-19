!------------------------------
! MODULE: IXMsource
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMsource class
module IXMsource
  use IXMbase
  implicit none
  public :: IXTsource
  type IXTsource
     private
     type(IXTbase):: base
     character(len=short_len):: facility_name='isis'	!! Source name e.g. ISIS
     real(dp):: frequency=0.0_dp					!! Source frequency (Hz)		
  end type IXTsource

  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC infrastructure
#define IXD_TYPE source
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTsource class"
#define IXD_TYPE source
#define IXD_SQTYPE 'source'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.
  subroutine IXFdestroy_source(arg, status)
    implicit none
    type(IXTsource) :: arg
    type(IXTstatus) :: status

    call IXFdestroy(arg%base,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(arg)

  end subroutine IXFdestroy_source

  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments for the Fermi chopper class
  subroutine IXFcheck_source(source, status)
    type(IXTsource) :: source
    type(IXTstatus) :: status
    call IXFcheck(source%base,status)
    ! Do not do any checks at present - just a dummy routine	
  end subroutine IXFcheck_source

  !----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide the operation IXFxxxxxOperationRun; it loops through
  ! all members of the class doing the supplied operation. Needed by 
  ! Fortran - e.g. used in IXFxxxxxDisplay (ther dafault display function) as well as
  ! Matlab & any other binding language (e.g. Python)

  recursive subroutine IXFoperation_run_source(op, field, arg, status)
    implicit none
    type(IXTsource) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTsource', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! The order of the fields below must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'facility_name', arg%facility_name, status)
    call IXFoperation_run(op,'frequency', arg%frequency, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_source



  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_source(source, facility_name,frequency,status)
    implicit none
    type(IXTsource),intent(out) :: source
    character(len=*),intent(in):: facility_name
    real(dp),intent(in):: frequency
    type(IXTstatus),intent(inout):: status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(source)

    call IXFset_source(source, status,facility_name, frequency)

  end subroutine IXFcreate_source

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_source(source, status,facility_name,frequency, ref)
    implicit none
    type(IXTsource),intent(inout) :: source
    type(IXTsource),optional,intent(in) :: ref
    character(len=*),optional, intent(in):: facility_name
    real(dp),optional, intent(in):: frequency
    type(IXTstatus),intent(inout) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_source)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(source)
    else    
       if(IXFvalid(source) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_source)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_source(source, status, ref%facility_name, ref%frequency)
    if (present(facility_name))source%facility_name=facility_name
    if (present(frequency))source%frequency=frequency

    call IXFcheck_source(source, status)

  end subroutine IXFset_source

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_source(source, status,facility_name, frequency, wout)
    implicit none
    type(IXTsource),intent(inout) :: source
    type(IXTsource),optional,intent(out) :: wout
    character(len=*),optional, intent(out):: facility_name
    real(dp),optional, intent(out):: frequency
    type(IXTstatus),intent(inout) :: status

    if (present(wout))call IXFcopy(source,wout, status)
    if (present(facility_name))facility_name=source%facility_name
    if (present(frequency))frequency=source%frequency

  end subroutine IXFget_source


end module IXMsource
