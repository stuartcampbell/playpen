!------------------------------
! MODULE: IXMmoderator
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMmoderator class
! moderator module
module IXMmoderator
  use IXMbase
  use IXMtype_definitions
  implicit none
  public :: IXTmoderator      
  type IXTmoderator
     private
     type(IXTbase)::base
     character(len=long_len):: name='name'			!! Name of the moderator
     real(dp):: distance=0.0_dp						!! Distance of moderator face from sample (m)
     real(dp):: width=0.0_dp							!! Moderator width (m)
     real(dp):: height=0.0_dp							!! Moderator height (m)
     real(dp):: thickness=0.0_dp						!! Moderator thickness (m)
     real(dp):: angle=0.0_dp							!! Angle of normal to incident beam (rad): +ve if normal is anticlockwise from incident beam
     real(dp):: temperature=0.0_dp					!! Temperature of moderator (k)
     character(len=short_len):: pulse_model='model'	!! Moderator pulse shape model
     real(dp), pointer :: pulse_pars(:)=>NULL()		!! Moderator pulse-shape parameters (interpretation depends on pulse_model)

  end type IXTmoderator
#define IXD_TYPE	moderator
#include "class_header.f90"

contains

  !----------------------------------------------------------------------------------------------------------------------
  ! Generate standard display subroutine: SUBROUTINE IXFmoderatorDisplay(moderator,status).
  ! This can be used in any Fortran application that uses this module. The subroutine
  ! will also be used by the binding language (Matlab, Python or whatever) to display
  ! the contents of an instance of the class.
#define IXD_DESCRIPTION	"IXTmoderator class"
#define IXD_TYPE moderator
#define IXD_SQTYPE 'moderator'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_moderator(arg, status)
    implicit none
    type(IXTmoderator) :: arg
    type(IXTstatus) :: status

    call IXFdealloc(arg%pulse_pars,status)
    call IXFdestroy(arg%base,status)

    call IXFclear_valid(arg)

  end subroutine IXFdestroy_moderator

  !----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide the operation IXFxxxxxOperationRun; it loops through
  ! all members of the class doing the supplied operation. Needed by 
  ! Fortran - e.g. used in IXFxxxxxDisplay (ther dafault display function) as well as
  ! Matlab & any other binding language (e.g. Python)

  recursive subroutine IXFoperation_run_moderator(op, field, arg, status)
    implicit none
    type(IXTmoderator) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTmoderator', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! The order of the fields below must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'distance', arg%distance, status)
    call IXFoperation_run(op,'width', arg%width, status)
    call IXFoperation_run(op,'height', arg%height, status)
    call IXFoperation_run(op,'thickness', arg%thickness, status)
    call IXFoperation_run(op,'angle', arg%angle, status)
    call IXFoperation_run(op,'temperature', arg%temperature, status)
    call IXFoperation_run(op,'pulse_model', arg%pulse_model, status)
    call IXFoperation_run_ptr(op,'pulse_pars', arg%pulse_pars, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_moderator(moderator, name, distance, width, &
       height, thickness, angle, temperature, &
       pulse_model,pulse_pars, status)
    implicit none
    type(IXTmoderator),intent(out) :: moderator
    character(len=*),intent(in):: name
    real(dp),intent(in) :: distance, width, height, thickness, angle, temperature
    character(len=*),intent(in):: pulse_model
    real(dp),intent(in) :: pulse_pars(:)
    type(IXTstatus),intent(inout) :: status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(moderator)

    call IXFset_moderator(moderator,status, name, distance, &
            width, height, thickness, angle, temperature, &
            pulse_model,pulse_pars)

  end subroutine IXFcreate_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_moderator(moderator,status, name, distance, width, height, thickness, angle, temperature, &
       pulse_model,pulse_pars, ref)
    implicit none
    type(IXTmoderator),intent(inout)::moderator
    type(IXTmoderator),optional,intent(in)::ref
    character(len=*),intent(in),optional:: name			!! Name of the slit package (e.g. 'sloppy')
    real(dp),intent(in),optional:: distance						!! distance from sample (m) (-ve if upstream of sample)
    real(dp),intent(in),optional:: width						!! width of rotation (hz)
    real(dp),intent(in),optional:: height							!! height of chopper rotation (s) = 1/width
    real(dp),intent(in),optional:: thickness							!! thickness of chopper body (m)
    real(dp),intent(in),optional:: angle						!! thickness of angle of slits (m)
    real(dp),intent(in),optional:: temperature						!! Slit width (m)  (fermi)
    character(len=*),intent(in),optional:: pulse_model
    real(dp),intent(in),optional :: pulse_pars(:)        
    type(IXTstatus),intent(inout)::status !! error status object

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_moderator)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(moderator)
    else    
       if(IXFvalid(moderator) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_moderator)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_moderator(moderator,status, ref%name, ref%distance, ref%width, ref%height, &
         ref%thickness, ref%angle, ref%temperature, ref%pulse_model, ref%pulse_pars)

    if (present(name))moderator%name = name
    if (present(distance))moderator%distance = distance
    if (present(width))moderator%width = width
    if (present(height))moderator%height = height
    if (present(thickness))moderator%thickness = thickness
    if (present(angle))moderator%angle = angle
    if (present(temperature))moderator%temperature = temperature
    if (present(pulse_model))moderator%pulse_model=pulse_model
    call IXFset_real_array(moderator%pulse_pars,status,pulse_pars)
    
    call IXFcheck_moderator(moderator,status)

  end subroutine IXFset_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_moderator(moderator,status,name, distance, width, height, thickness, angle, temperature, &
       & pulse_model,pulse_pars, wout)
    implicit none
    type(IXTmoderator),intent(in)::moderator
    type(IXTmoderator),optional,intent(out)::wout
    character(len=*),intent(out),optional:: name			!! Name of the slit package (e.g. 'sloppy')
    real(dp),intent(out),optional:: distance						!! distance from sample (m) (-ve if upstream of sample)
    real(dp),intent(out),optional:: width						!! width of rotation (hz)
    real(dp),intent(out),optional:: height							!! height of chopper rotation (s) = 1/width
    real(dp),intent(out),optional:: thickness							!! thickness of chopper body (m)
    real(dp),intent(out),optional:: angle						!! thickness of angle of slits (m)
    real(dp),intent(out),optional:: temperature						!! Slit width (m)  (fermi)
    character(len=*),intent(out),optional:: pulse_model
    real(dp),intent(out),optional :: pulse_pars(:)        
    type(IXTstatus),intent(inout)::status !! error status object

    if (present(wout))call IXFcopy(moderator,wout,status)
    
    if (present(name))name=moderator%name
    if (present(distance))distance=moderator%distance
    if (present(width))width=moderator%width
    if (present(height))height=moderator%height
    if (present(thickness))thickness=moderator%thickness
    if (present(angle))angle=moderator%angle 
    if (present(temperature))temperature=moderator%temperature
    if (present(pulse_model))pulse_model=moderator%pulse_model

    call IXFget_real_array(moderator%pulse_pars,status,pulse_pars)

  end subroutine IXFget_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_moderator(moderator,pars_ptr)
    real(dp),pointer,optional::pars_ptr(:) !!output: pulse_pars array pointer
    type(IXTmoderator),intent(in)::moderator !!input moderator 

    if(present(pars_ptr))pars_ptr=>moderator%pulse_pars

  end subroutine IXFget_ptr_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_moderator(moderator,status,name, distance, width, height, thickness, angle, temperature, &
       & pulse_model,pulse_pars, wout)
    real(dp),allocatable::pulse_pars(:) !!pulse_pars array pointer
    type(IXTmoderator),intent(in)::moderator !!input dataset_1d 
    type(IXTmoderator),optional,intent(out)::wout
    character(len=*),intent(out),optional:: name			!! Name of the slit package (e.g. 'sloppy')
    real(dp),intent(out),optional:: distance						!! distance from sample (m) (-ve if upstream of sample)
    real(dp),intent(out),optional:: width						!! width of rotation (hz)
    real(dp),intent(out),optional:: height							!! height of chopper rotation (s) = 1/width
    real(dp),intent(out),optional:: thickness							!! thickness of chopper body (m)
    real(dp),intent(out),optional:: angle						!! thickness of angle of slits (m)
    real(dp),intent(out),optional:: temperature						!! Slit width (m)  (fermi)
    character(len=*),intent(out),optional:: pulse_model
    type(IXTstatus),intent(inout)::status !! error status object

    if (allocated(pulse_pars))then
       call IXFreallocdimsFortran(pulse_pars,shape(moderator%pulse_pars),.false.,status)
    endif

    call IXFget_moderator(moderator,status,name, distance, width, height, thickness, angle, temperature, &
       & pulse_model,pulse_pars, wout)
  end subroutine IXFget_alloc_moderator

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_moderator(moderator, status)
    type(IXTmoderator) :: moderator
    type(IXTstatus) :: status
    ! Do not do any checks at present - just a dummy routine	
    call IXFcheck_base(moderator%base,status)
  end subroutine IXFcheck_moderator

end module IXMmoderator

