!------------------------------
! MODULE: IXMchopper_instrument
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1219 $ ($Date: 2007-09-10 12:57:23 -0400 (Mon, 10 Sep 2007) $)
!!
!! FORTRAN definition of IXMchopper_instrument class
module IXMchopper_instrument
  use IXMbase
  use IXMfermi_chopper
  implicit none	
  public :: IXTchopper_instrument	
  type IXTchopper_instrument
     private
     type(IXTbase):: base
     type(IXTfermi_chopper)::monochromator
  end type IXTchopper_instrument

#define IXD_TYPE chopper_instrument
#include "class_header.f90"
  private::get_emode

  interface IXFget_ptr
     module procedure get_ptr
  end interface
  
  interface IXFget_emode
     module procedure get_emode
  end interface
  !! global chopper_instrument object
  type(IXTchopper_instrument),private,save,target::chop_inst !! instrument detector object


contains

#define IXD_DESCRIPTION	"IXTchopper_instrument class"
#define IXD_TYPE chopper_instrument
#define IXD_SQTYPE 'chopper_instrument'
#include "class_base.f90"
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to the global diffraction instruemnt object declared in the IXMinstrument module.
  subroutine get_ptr(ci_ptr)
    implicit none
    type(IXTchopper_instrument),pointer::ci_ptr
       ci_ptr=>chop_inst
  end subroutine get_ptr  
!-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_emode will return   
  subroutine get_emode(c_inst,status,emode,efixed)
    implicit none
    type (IXTchopper_instrument):: c_inst
    integer(i4b)::emode
    real(dp),optional::efixed
    type(IXTstatus)::status
    emode=1
    if(present(efixed))call IXFget_fermi_chopper(c_inst%monochromator,status,energy=efixed)

  end subroutine get_emode


  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_chopper_instrument(op, field, arg, status)
    implicit none
    type(IXTchopper_instrument) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTchopper_instrument', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'monochromator', arg%monochromator, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_chopper_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_chopper_instrument(chopper_instrument,monochromator, status)
    implicit none
    type(IXTchopper_instrument) :: chopper_instrument
    type(IXTstatus) :: status
    type(IXTfermi_chopper),intent(in)::monochromator

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(monochromator) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTfermi_chopper failure, all nested objects MUST be initialised (IXFcreate_)')
    endif

    if(status == IXCseverity_error)return
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(chopper_instrument)
    call IXFset_chopper_instrument(chopper_instrument,status,monochromator)
  end subroutine IXFcreate_chopper_instrument


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_chopper_instrument(arg, status)
    implicit none
    type(IXTchopper_instrument) :: arg
    type(IXTstatus) :: status

    call IXFcheck_base(arg%base,status)
    call IXFcheck(arg%monochromator,status)

  end subroutine IXFcheck_chopper_instrument


  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_chopper_instrument(chopper_instrument,status)
    implicit none
    type(IXTchopper_instrument) :: chopper_instrument
    type(IXTstatus) :: status

    call IXFdestroy(chopper_instrument%base,status)
    if(IXFvalid(chopper_instrument%monochromator)) call IXFdestroy(chopper_instrument%monochromator,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(chopper_instrument)

  end subroutine IXFdestroy_chopper_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_chopper_instrument(chopper_instrument,status,monochromator,ref)
    implicit none
    type(IXTchopper_instrument),intent(inout) :: chopper_instrument
    type(IXTchopper_instrument),intent(in),optional:: ref
    type(IXTfermi_chopper),optional::monochromator
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_chopper_instrument)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(chopper_instrument)
    else    
       if(IXFvalid(chopper_instrument) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_chopper_instrument)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_chopper_instrument(chopper_instrument,status,ref%monochromator)
    if(present(monochromator))call IXFcopy(monochromator,chopper_instrument%monochromator,status)

    call IXFcheck_chopper_instrument(chopper_instrument,status)

  end subroutine IXFset_chopper_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_chopper_instrument(chopper_instrument, status,monochromator,wout)
    implicit none
    type(IXTchopper_instrument),intent(in) :: chopper_instrument
    type(IXTchopper_instrument),optional,intent(out)::wout
    type(IXTstatus) :: status
    type(IXTfermi_chopper),optional,intent(out)::monochromator

    if (present(wout))call IXFcopy(chopper_instrument,wout,status)
    if (present(monochromator))call IXFcopy(chopper_instrument%monochromator,monochromator,status)

  end subroutine IXFget_chopper_instrument


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_chopper_instrument(chopper_instrument,monochromator)
    implicit none
    type(IXTchopper_instrument),target :: chopper_instrument !! it is a target since a pointer will point to a part of it
    type(IXTfermi_chopper),pointer,optional:: monochromator

    if(present(monochromator))monochromator=>chopper_instrument%monochromator
    !may need to put a check in if it doesn't return a valid pointer

  end subroutine IXFget_ptr_chopper_instrument

  subroutine IXFpopulate_chopper_instrument(ci,dso,status)
    use IXMdata_source
    implicit none
    type(IXTchopper_instrument)::ci
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    character(len=long_len)::path,obj_name
    logical::found
    call IXFmark_valid(ci)
  ! this may then call a function on the dso to populate the fermi_chopper from a file.xml or file.nxs  
    call IXFfindpath_data_source(dso,'fermi_chopper',path,obj_name,found,status)
    if(found)then
      if(trim(adjustl(obj_name)) /= IXCundef_char)then
        call IXFpopulate_file_fermi_chopper(ci%monochromator,path,status,obj_name)
      else
        call IXFpopulate_file_fermi_chopper(ci%monochromator,path,status)
      endif
    else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
             IXCerr_invparam, ' fermi_chopper must be specified in data_source object(IXFpopulate_chopper_instrument)')
    endif   
  end subroutine IXFpopulate_chopper_instrument
end module IXMchopper_instrument
