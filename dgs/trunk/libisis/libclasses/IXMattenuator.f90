!------------------------------
! MODULE: IXMattenuator
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMattenuator class
! attenuator module
module IXMattenuator
  use IXMtype_definitions
  use IXMbase
  use IXMdataset_1d
  implicit none
  public :: IXTattenuator
  type IXTattenuator
     private
     type(IXTbase):: base
     character(len=long_len):: name='name'		!! Name of the attenuator
     real(dp):: distance=0.0_dp					!! Distance of centre from sample (m) (-ve if upstream of sample)
     character(len=long_len):: material='material'	!! Type (e.g. polythene)
     real(dp):: thickness=0.0_dp				!! Thickness (m)
     type(IXTdataset_1d):: attenuation	!! Attenuation factor as a function of energy (meV)	
  end type IXTattenuator

  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE attenuator
#include "class_header.f90"

contains

  !----------------------------------------------------------------------------------------------------------------------
  ! Generate standard display subroutine: SUBROUTINE IXFattenuatorDisplay(attenuator,status).
  ! This can be used in any Fortran application that uses this module. The subroutine
  ! will also be used by the binding language (Matlab, Python or whatever) to display
  ! the contents of an instance of the class.
#define IXD_DESCRIPTION	"IXTattenuator class"
#define IXD_TYPE attenuator
#define IXD_SQTYPE 'attenuator'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_attenuator(arg, status)
    implicit none
    type(IXTattenuator) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
    if(IXFvalid(arg%attenuation))call IXFdestroy(arg%attenuation,status)

    call IXFclear_valid(arg)

  end subroutine IXFdestroy_attenuator


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_attenuator(attenuator, status)
    type(IXTattenuator) :: attenuator
    type(IXTstatus) :: status

    call IXFcheck_base(attenuator%base,status)
    call IXFcheck(attenuator%attenuation,status)

  end subroutine IXFcheck_attenuator

  !----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide the operation IXFxxxxxOperationRun; it loops through
  ! all members of the class doing the supplied operation. Needed by 
  ! Fortran - e.g. used in IXFxxxxxDisplay (ther dafault display function) as well as
  ! Matlab & any other binding language (e.g. Python)

  recursive subroutine IXFoperation_run_attenuator(op, field, arg, status)
    implicit none
    type(IXTattenuator) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTattenuator', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! The order of the fields below must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'distance', arg%distance, status)
    call IXFoperation_run(op,'material', arg%material, status)
    call IXFoperation_run(op,'thickness', arg%thickness, status)
    call IXFoperation_run(op,'attenuation',arg%attenuation, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_attenuator

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised.

  subroutine IXFcreate_attenuator(attenuator,name,distance,material,thickness,attenuation,status)
    implicit none
    type(IXTattenuator),intent(out) :: attenuator
    character(len=*),intent(in):: name
    real(dp),intent(in):: distance
    character(len=*),intent(in):: material
    real(dp),intent(in):: thickness
    type(IXTdataset_1d),intent(in)::attenuation
    type(IXTstatus) ,intent(inout):: status

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(attenuator%attenuation) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdataset_1d (attenuation) failure, all nested objects MUST be initialised (IXFcreate_attenuator)')
    endif
    if(status == IXCseverity_error)return
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(attenuator)

    call IXFset_attenuator(attenuator,status,name,distance,material,thickness,attenuation)

  end subroutine IXFcreate_attenuator

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_attenuator(attenuator, status,name,distance,material,thickness,attenuation,ref)
    implicit none
    type(IXTattenuator),intent(inout) :: attenuator
    type(IXTattenuator),optional,intent(in) :: ref
    character(len=*),optional, intent(in):: name
    real(dp),optional, intent(in):: distance
    character(len=*),optional, intent(in):: material
    real(dp),optional, intent(in):: thickness
    type(IXTdataset_1d),optional,intent(in)::attenuation
    type(IXTstatus),intent(inout) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_attenuator)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(attenuator)
    else    
       if(IXFvalid(attenuator) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_attenuator)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_attenuator(attenuator,status,ref%name,ref%distance,ref%material, &
         ref%thickness,ref%attenuation)         
    if (present(name))attenuator%name=name
    if (present(distance))attenuator%distance=distance
    if (present(material))attenuator%material=material
    if (present(thickness))attenuator%thickness=thickness
    ! create a copy from supplied object to new subobject
    if (present(attenuation))call IXFcopy(attenuation,attenuator%attenuation,status)

    call IXFcheck_attenuator(attenuator, status)

  end subroutine IXFset_attenuator

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_attenuator(attenuator, status,name, distance,material,thickness,attenuation,wout)
    implicit none
    type(IXTattenuator),intent(inout) :: attenuator
    type(IXTattenuator),optional,intent(out) :: wout
    character(len=*),optional, intent(out):: name
    real(dp),optional, intent(out):: distance
    character(len=*),optional, intent(out):: material
    real(dp),optional, intent(out):: thickness
    type(IXTdataset_1d),optional,intent(out)::attenuation
    type(IXTstatus),intent(inout) :: status

    if (present(wout))call IXFcopy(attenuator,wout, status)                        
    if (present(name))name=attenuator%name
    if (present(distance))distance=attenuator%distance
    if (present(material))material=attenuator%material
    if (present(thickness))thickness=attenuator%thickness
    ! create a copy from existing subobject into supplied object
    if (present(attenuation))call IXFcopy(attenuator%attenuation,attenuation,status)

  end subroutine IXFget_attenuator

end module IXMattenuator

