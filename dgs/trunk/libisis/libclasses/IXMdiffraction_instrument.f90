!------------------------------
! MODULE: IXMdiffraction_instrument
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of  IXMdiffraction_instrument object 

module IXMdiffraction_instrument
  use IXMtype_definitions
  use IXMbase
  implicit none
  public :: IXTdiffraction_instrument
  type IXTdiffraction_instrument
     private
     type(IXTbase)::base
  end type IXTdiffraction_instrument

#define IXD_TYPE diffraction_instrument
#include "class_header.f90"

  private::get_emode

  interface IXFget_ptr
     module procedure get_ptr
  end interface

  
  interface IXFget_emode
     module procedure get_emode
  end interface
  !! global diff_instrument object
  type(IXTdiffraction_instrument),private,save,target::diff_inst !! instrument detector object

contains

#define IXD_DESCRIPTION	"IXTdiffraction_instrument class"
#define IXD_TYPE diffraction_instrument
#define IXD_SQTYPE 'diffraction_instrument'
#include "class_base.f90"
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to the global diffraction instruemnt object declared in the IXMinstrument module.
  subroutine get_ptr(di_ptr)
    implicit none
    type(IXTdiffraction_instrument),pointer::di_ptr
     di_ptr=>diff_inst
  end subroutine get_ptr
  
  subroutine get_emode(d_inst,status,emode,efixed)
    implicit none
    type (IXTdiffraction_instrument):: d_inst
    integer(i4b)::emode
    real(dp),optional::efixed
    type(IXTstatus)::status
    if(present(efixed))efixed=IXCundef_dp
    emode=0
  end subroutine get_emode
    
  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.
  subroutine IXFdestroy_diffraction_instrument(arg, status)
    implicit none
    type(IXTdiffraction_instrument) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_diffraction_instrument

  recursive subroutine IXFoperation_run_diffraction_instrument(op, field, arg, status)
    implicit none
    type(IXTdiffraction_instrument) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdiffraction_instrument', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declarion order in matlab as it is
    ! used when parsing arguments passed in class creation with varargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_diffraction_instrument

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_diffraction_instrument(diffraction_instrument,status,wout)
    type(IXTdiffraction_instrument),intent(in)::diffraction_instrument
    type(IXTdiffraction_instrument),optional,intent(out)::wout
    type(IXTstatus),intent(inout)::status

    if(present(wout))call IXFcopy(diffraction_instrument,wout,status)

  end subroutine IXFget_diffraction_instrument
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_diffraction_instrument(diffraction_instrument,status)
    type(IXTdiffraction_instrument),intent(out)::diffraction_instrument
    type(IXTstatus),intent(inout)::status

    call IXFmark_valid(diffraction_instrument)

  end subroutine IXFcreate_diffraction_instrument
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_diffraction_instrument(diffraction_instrument,status,ref)
    type(IXTdiffraction_instrument),intent(inout)::diffraction_instrument
    type(IXTdiffraction_instrument),intent(in),optional::ref
    type(IXTstatus),intent(inout)::status

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_diffraction_instrument)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(diffraction_instrument)
    else    
       if(IXFvalid(diffraction_instrument) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_diffraction_instrument)')
       endif
       if(status == IXCseverity_error)return
    endif

!  there is nothing in the instrument to set
    !if(present(ref))call IXFset_diffraction_instrument(diffraction_instrument,status)
    call IXFcheck_diffraction_instrument(diffraction_instrument,status)

  end subroutine IXFset_diffraction_instrument

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_diffraction_instrument(di, status)
    implicit none
    type(IXTdiffraction_instrument) :: di
    type(IXTStatus) :: status
  end subroutine IXFcheck_diffraction_instrument

  subroutine IXFpopulate_diffraction_instrument(di,dso,status)
    use IXMdata_source
    implicit none
    type(IXTdiffraction_instrument)::di
    type(IXTdata_source)::dso
    type(IXTstatus)::status
  
    call IXFmark_valid(di)
  ! this may then call a function on the dso to populate the fermi_diffraction from a file.xml
  
  end subroutine
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_diffraction_instrument(diffraction_instrument)
    implicit none
    type(IXTdiffraction_instrument) :: diffraction_instrument
  end subroutine IXFget_ptr_diffraction_instrument

end module IXMdiffraction_instrument

