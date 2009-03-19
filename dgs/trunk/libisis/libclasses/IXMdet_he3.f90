!------------------------------
! MODULE: IXMdet_he3
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMdet_he3 class 
module IXMdet_he3
  use IXMbase
  implicit none
  public :: IXTdet_he3
  type IXTdet_he3
     private
     type(IXTbase):: base
     integer(i4b), pointer  :: checksum(:)=>NULL()		!! to compare different det_he3 object values
     real(dp), pointer  :: gas_pressure(:)=>NULL()    !! 3He gas pressure in atmospheres
     real(dp), pointer  :: wall_thickness(:)=>NULL()  !! tube wall thickness (m)

  end type IXTdet_he3

#define IXD_TYPE det_he3
#include "class_header.f90"

contains  

#define IXD_DESCRIPTION	"IXTdet_he3 class"
#define IXD_TYPE det_he3
#define IXD_SQTYPE 'det_he3'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_det_he3(op, field, arg, status)
    implicit none
    type(IXTdet_he3) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdet_he3', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_ptr(op,'checksum', arg%checksum, status)
    call IXFoperation_run_ptr(op,'gas_pressure',arg%gas_pressure,status)
    call IXFoperation_run_ptr(op,'wall_thickness', arg%wall_thickness, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_det_he3

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_det_he3(arg,status)
    implicit none
    type(IXTdet_he3)::arg
    type(IXTstatus)::status

    call IXFcheck_base(arg%base,status)

  end subroutine IXFcheck_det_he3

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_det_he3(det,status)
    implicit none
    type(IXTdet_he3)::det
    type(IXTstatus)::status
    call IXFdestroy(det%base,status)
    call IXFdealloc(det%checksum,status)
    call IXFdealloc(det%gas_pressure,status)
    call IXFdealloc(det%wall_thickness,status)
    call IXFclear_valid(det)
  end subroutine IXFdestroy_det_he3


  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_det_he3(arg,checksum,gas_pressure,wall_thickness,status)
    implicit none
    type(IXTdet_he3)::arg
    integer(i4b),intent(in)::checksum(:)
    real(dp),intent(in)::gas_pressure(:),wall_thickness(:)
    type(IXTstatus)::status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(arg)
    call IXFset_det_he3(arg,status,checksum,gas_pressure,wall_thickness)

  end subroutine IXFcreate_det_he3

  subroutine IXFcreate_reference_det_he3(arg,pressure,thickness,fill,status)
    implicit none
    type(IXTdet_he3)::arg    
    real(dp),intent(in)::thickness(:),pressure(:)
    logical,intent(in)::fill(:)
    integer(i4b)::ntype
    type(IXTstatus)::status

    ntype=count(fill)
    call IXFrealloc(arg%checksum,ntype,.false.,status) !value in checksum is set to zero
    call IXFrealloc(arg%gas_pressure,ntype,.false.,status)
    call IXFrealloc(arg%wall_thickness,ntype,.false.,status)
    arg%gas_pressure=pack(pressure,fill)
    arg%wall_thickness=pack(thickness,fill)
    
    call IXFmark_valid(arg)
  end subroutine IXFcreate_reference_det_he3
  
  subroutine IXFmake_det_he3(arg,len,keep,status)
    implicit none
    type(IXTdet_he3)::arg
    integer(i4b),intent(in)::len
    type(IXTstatus)::status
    logical,intent(in)::keep

    call IXFrealloc(arg%checksum,len,keep,status) !value in checksum is set to zero
    call IXFrealloc(arg%gas_pressure,len,keep,status)
    call IXFrealloc(arg%wall_thickness,len,keep,status)
    if(keep)call IXFmark_valid(arg)
  end subroutine IXFmake_det_he3
  
  subroutine IXFcopy_index_from_det_he3(ref_he3,index_ref,index,det_he3,status)
    implicit none
    type(IXTdet_he3)::ref_he3,det_he3
    integer(i4b),intent(in)::index_ref,index
    type(IXTstatus)::status
    
    if(index_ref > size(ref_he3%gas_pressure))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'ref_he3 array lengths are too short for supplied index (IXFcopy_index_from_det_he3)')              
        return
    endif   

    if(index > size(det_he3%gas_pressure))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'det_he3 array lengths are too short for supplied index (IXFcopy_index_from_det_he3)')              
        return
    endif   

    
    det_he3%checksum(index)=ref_he3%checksum(index_ref)
    det_he3%gas_pressure(index)=ref_he3%gas_pressure(index_ref)
    det_he3%wall_thickness(index)=ref_he3%wall_thickness(index_ref)
  end subroutine  IXFcopy_index_from_det_he3
    
  
  subroutine IXFpopulate_reference_det_he3(det_he3_new,ref_det_he3,index_in,status)
    implicit none
    type(IXTdet_he3),intent(in)::ref_det_he3
    type(IXTdet_he3),intent(out)::det_he3_new
    integer(i4b),intent(in)::index_in(:)
    integer(i4b)::len
    type(IXTstatus)::status
    
    len=size(index_in)    
    call IXFalloc(det_he3_new%checksum,len,status)
    call IXFalloc(det_he3_new%gas_pressure,len,status)
    call IXFalloc(det_he3_new%wall_thickness,len,status)

    det_he3_new%checksum=ref_det_he3%checksum(index_in)
    det_he3_new%gas_pressure=ref_det_he3%gas_pressure(index_in)
    det_he3_new%wall_thickness=ref_det_he3%wall_thickness(index_in)
    
    call IXFmark_valid(det_he3_new)
    
  end subroutine IXFpopulate_reference_det_he3

  subroutine IXFchecksamepars_det_he3(det_he3_full,index_in,pars_true)
    implicit none    
    type(IXTdet_he3),intent(in)::det_he3_full
    integer(i4b),intent(in)::index_in(:)
    logical,intent(out)::pars_true    
    
    pars_true=.true.    
    !if(maxval(det_he3_full%checksum(index_in)) /= minval(det_he3_full%checksum(index_in)))pars_true=.false.
    if(maxval(det_he3_full%gas_pressure(index_in)) /= minval(det_he3_full%gas_pressure(index_in)))pars_true=.false.    
    if(maxval(det_he3_full%wall_thickness(index_in)) /= minval(det_he3_full%wall_thickness(index_in)))pars_true=.false.    
   
  end subroutine IXFchecksamepars_det_he3   
  
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_det_he3(det_he3,status,checksum,gas_pressure,wall_thickness,ref)! not
    implicit none
    type(IXTdet_he3),intent(inout)::det_he3
    type(IXTdet_he3),optional,intent(in)::ref
    integer(i4b),optional,intent(in) :: checksum(:)	!! to compare different det_he3 object values
    real(dp),optional,intent(in):: gas_pressure(:)              !! electronic delay time (microseconds)
    real(dp),optional,intent(in):: wall_thickness(:)          !! det_he3 dead time (microseconds)
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_det_he3)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(det_he3)
    else    
       if(IXFvalid(det_he3) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_det_he3)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))then
       call IXFset_det_he3(det_he3,status,ref%checksum,ref%gas_pressure,ref%wall_thickness)
    endif

    call IXFset_integer_array(det_he3%checksum,status,checksum)    

    call IXFset_real_array(det_he3%gas_pressure,status,gas_pressure)    
    call IXFset_real_array(det_he3%wall_thickness,status,wall_thickness)    

  end subroutine IXFset_det_he3
!-----------------------------------------------------------------------------------------------------------------------
!! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
!! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
!! contents of a whole object to a new one.
  subroutine IXFget_det_he3(det_he3,status,checksum,gas_pressure,wall_thickness,wout)! not
    implicit none
    type(IXTdet_he3),intent(inout)::det_he3
    type(IXTdet_he3),optional,intent(out)::wout
    integer(i4b),optional,intent(out) :: checksum(:)	!! to compare different det_he3 object values
    real(dp),optional,intent(out):: gas_pressure(:)              !! electronic delay time (microseconds)
    real(dp),optional,intent(out):: wall_thickness(:)          !! det_he3 dead time (microseconds)
    type(IXTstatus)::status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(wout))then
      call IXFcopy(det_he3,wout,status)
    endif

    call IXFget_integer_array(det_he3%checksum,status,checksum)    

    call IXFget_real_array(det_he3%gas_pressure,status,gas_pressure)    
    call IXFget_real_array(det_he3%wall_thickness,status,wall_thickness)    


   end subroutine IXFget_det_he3
end module IXMdet_he3












