!------------------------------
! MODULE: IXMdet_solid
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMdet_solid class
module IXMdet_solid
  use IXMbase
  implicit none
  public :: IXTdet_solid
  type IXTdet_solid
     private
     type(IXTbase):: base
     integer(i4b), pointer  :: checksum(:)=>NULL()		!! to compare different det_solid object values
     real(dp), pointer  :: macro_xs(:)=>NULL()    !! Macroscopic Cross-section
  end type IXTdet_solid

#define IXD_TYPE det_solid
#include "class_header.f90"

contains  

#define IXD_DESCRIPTION	"IXTdet_solid class"
#define IXD_TYPE det_solid
#define IXD_SQTYPE 'det_solid'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_det_solid(op, field, arg, status)
    implicit none
    type(IXTdet_solid) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdet_solid', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_ptr(op,'checksum', arg%checksum, status)
    call IXFoperation_run_ptr(op,'macro_xs',arg%macro_xs,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_det_solid

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_det_solid(arg,status)
    implicit none
    type(IXTdet_solid)::arg
    type(IXTstatus)::status

    call IXFcheck_base(arg%base,status)

  end subroutine IXFcheck_det_solid

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_det_solid(det,status)
    implicit none
    type(IXTdet_solid)::det
    type(IXTstatus)::status
    call IXFdestroy(det%base,status)
    call IXFdealloc(det%checksum,status)
    call IXFdealloc(det%macro_xs,status)

    call IXFclear_valid(det)
  end subroutine IXFdestroy_det_solid

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_det_solid(arg,checksum,macro_xs,status)
    implicit none
    type(IXTdet_solid)::arg
    integer(i4b),intent(in)::checksum(:)
    real(dp),intent(in)::macro_xs(:)
    type(IXTstatus)::status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(arg)

    call IXFset_det_solid(arg,status,checksum,macro_xs)
    !check is called in the set routine    

  end subroutine IXFcreate_det_solid

  subroutine IXFcreate_reference_det_solid(arg,macro_xs,fill,status)
    implicit none
    type(IXTdet_solid)::arg    
    real(dp),intent(in)::macro_xs(:)
    logical,intent(in)::fill(:)
    integer(i4b)::ntype
    type(IXTstatus)::status

    ntype=count(fill)
    call IXFrealloc(arg%checksum,ntype,.false.,status) !value in checksum is set to zero
    call IXFrealloc(arg%macro_xs,ntype,.false.,status)
    arg%macro_xs=pack(macro_xs,fill)    
    call IXFmark_valid(arg)
  end subroutine IXFcreate_reference_det_solid

  subroutine IXFmake_det_solid(arg,len,keep,status)
    implicit none
    type(IXTdet_solid)::arg
    integer(i4b),intent(in)::len
    type(IXTstatus)::status
    logical,intent(in)::keep

    call IXFrealloc(arg%checksum,len,keep,status) !value in checksum is set to zero
    call IXFrealloc(arg%macro_xs,len,keep,status)
    if(keep)call IXFmark_valid(arg)
  end subroutine IXFmake_det_solid
  
  subroutine IXFcopy_index_from_det_solid(ref_solid,index_ref,index,det_solid,status)
    implicit none
    type(IXTdet_solid)::ref_solid,det_solid
    integer(i4b),intent(in)::index_ref,index
    type(IXTstatus)::status
    
     if(index_ref > size(ref_solid%macro_xs))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'ref_solid array lengths are too short for supplied index (IXFcopy_index_from_det_solid)')              
        return
    endif   

    if(index > size(det_solid%macro_xs))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'det_solid array lengths are too short for supplied index (IXFcopy_index_from_det_solid)')              
        return
    endif     
    det_solid%checksum(index)=ref_solid%checksum(index_ref)
    det_solid%macro_xs(index)=ref_solid%macro_xs(index_ref)
    
  end subroutine  IXFcopy_index_from_det_solid
    
   subroutine IXFpopulate_reference_det_solid(det_solid_new,ref_det_solid,index_in,status)
    implicit none
    type(IXTdet_solid),intent(in)::ref_det_solid
    type(IXTdet_solid),intent(out)::det_solid_new
    integer(i4b),intent(in)::index_in(:)
    integer(i4b)::len
    type(IXTstatus)::status
    
    len=size(index_in)    
    call IXFalloc(det_solid_new%checksum,len,status)
    call IXFalloc(det_solid_new%macro_xs,len,status)

    det_solid_new%checksum=ref_det_solid%checksum(index_in)
    det_solid_new%macro_xs=ref_det_solid%macro_xs(index_in)    
    
    call IXFmark_valid(det_solid_new)
    
  end subroutine IXFpopulate_reference_det_solid

   subroutine IXFchecksamepars_det_solid(det_solid_full,index_in,pars_true)
    implicit none    
    type(IXTdet_solid),intent(in)::det_solid_full
    integer(i4b),intent(in)::index_in(:)
    logical,intent(out)::pars_true    
    
    pars_true=.true.    
    !if(maxval(det_solid_full%checksum(index_in)) /= minval(det_solid_full%checksum(index_in)))pars_true=.false.
    if(maxval(det_solid_full%macro_xs(index_in)) /= minval(det_solid_full%macro_xs(index_in)))pars_true=.false.    
    
  end subroutine IXFchecksamepars_det_solid  
  

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_det_solid(det_solid,status,checksum,macro_xs,ref)
    implicit none
    type(IXTdet_solid),intent(inout)::det_solid
    type(IXTdet_solid),optional,intent(in)::ref
    integer(i4b),optional,intent(in) :: checksum(:)	!! to compare different det_solid object values
    real(dp),optional,intent(in):: macro_xs(:)              !! electronic delay time (microseconds)
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_det_solid)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(det_solid)
    else    
       if(IXFvalid(det_solid) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_det_solid)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))then
       call IXFset_det_solid(det_solid,status,ref%checksum,ref%macro_xs)
    endif
    
    call IXFset_integer_array(det_solid%checksum,status,checksum)    
    call IXFset_real_array(det_solid%macro_xs,status,macro_xs)    

  end subroutine IXFset_det_solid
!-----------------------------------------------------------------------------------------------------------------------
!! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
!! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
!! contents of a whole object to a new one.
  subroutine IXFget_det_solid(det_solid,status,checksum,macro_xs,wout)
    implicit none
    type(IXTdet_solid),intent(inout)::det_solid
    type(IXTdet_solid),optional,intent(out)::wout
    integer(i4b),optional,intent(out) :: checksum(:)	!! to compare different det_solid object values
    real(dp),optional,intent(out):: macro_xs(:)              !! macroscopic cross-section
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(wout))then
      call IXFcopy(det_solid,wout,status)
    endif
    
    call IXFget_integer_array(det_solid%checksum,status,checksum)    
    call IXFget_real_array(det_solid%macro_xs,status,macro_xs)    

   end subroutine IXFget_det_solid
end module IXMdet_solid
