!------------------------------
! MODULE: IXMbridge
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1386 $ ($Date: 2008-05-20 04:26:17 -0400 (Tue, 20 May 2008) $)
!!
!! FORTRAN definition of IXMbridge class
module IXMbridge
  use IXMbase
  use IXMdata_source
  use IXMws_bridge
  use IXMsw_bridge
  implicit none	
  public :: IXTbridge	
  type IXTbridge
     private
     type(IXTbase):: base
     type(IXTws_bridge)::ws_bridge
     type(IXTsw_bridge)::sw_bridge
  end type IXTbridge
 

#define IXD_TYPE bridge
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTbridge class"
#define IXD_TYPE bridge
#define IXD_SQTYPE 'bridge'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_bridge(op, field, arg, status)
    implicit none
    type(IXTbridge) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTbridge', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'ws_bridge', arg%ws_bridge, status)
    call IXFoperation_run(op,'sw_bridge',arg%sw_bridge,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_bridge(bridge,ws_bridge,sw_bridge, status)
    implicit none
    type(IXTbridge) :: bridge
    type(IXTstatus) :: status
    type(IXTws_bridge),intent(in)::ws_bridge
    type(IXTsw_bridge),intent(in)::sw_bridge

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(ws_bridge) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTws_bridge failure, all nested objects MUST be initialised (IXFcreate_)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(sw_bridge) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTsw_bridge failure, all nested objects MUST be initialised (IXFcreate_bridge)')
    endif
    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(bridge)

    call IXFset_bridge(bridge,status,ws_bridge,sw_bridge)
  end subroutine IXFcreate_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_bridge(arg, status)
    implicit none
    type(IXTbridge) :: arg
    type(IXTstatus) :: status

    call IXFcheck_base(arg%base,status)
    call IXFcheck(arg%ws_bridge,status)
    call IXFcheck(arg%sw_bridge,status)

  end subroutine IXFcheck_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_bridge(bridge,status)
    implicit none
    type(IXTbridge) :: bridge
    type(IXTstatus) :: status

    call IXFdestroy(bridge%base,status)
    if(IXFvalid(bridge%ws_bridge)) call IXFdestroy(bridge%ws_bridge,status)
    if(IXFvalid(bridge%sw_bridge)) call IXFdestroy(bridge%sw_bridge,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(bridge)

  end subroutine IXFdestroy_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_bridge(bridge,status,ws_bridge,sw_bridge,ref)
    implicit none
    type(IXTbridge),intent(inout) :: bridge
    type(IXTbridge),intent(in),optional:: ref
    type(IXTws_bridge),optional::ws_bridge
    type(IXTsw_bridge),optional::sw_bridge
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_bridge)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(bridge)
    else    
       if(IXFvalid(bridge) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_bridge)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_bridge(bridge,status,ref%ws_bridge,ref%sw_bridge)
    if(present(ws_bridge))call IXFcopy(ws_bridge,bridge%ws_bridge,status)
    if(present(sw_bridge))call IXFcopy(sw_bridge,bridge%sw_bridge,status)
    call IXFcheck_bridge(bridge,status)

  end subroutine IXFset_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_bridge(bridge, status,ws_bridge,sw_bridge,wout)
    implicit none
    type(IXTbridge),intent(in) :: bridge
    type(IXTbridge),optional,intent(out)::wout
    type(IXTstatus) :: status
    type(IXTws_bridge),optional,intent(out)::ws_bridge
    type(IXTsw_bridge),optional,intent(out)::sw_bridge

    if (present(wout))call IXFcopy(bridge,wout,status)
    if (present(ws_bridge))call IXFcopy(bridge%ws_bridge,ws_bridge,status)
    if (present(sw_bridge))call IXFcopy(bridge%sw_bridge,sw_bridge,status)
  end subroutine IXFget_bridge


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_bridge(bridge,ws_bridge,sw_bridge)
    implicit none
    type(IXTbridge),target :: bridge !! it is a target since a pointer will point to a part of it
    type(IXTws_bridge),pointer,optional:: ws_bridge
    type(IXTsw_bridge),pointer,optional:: sw_bridge

    if(present(ws_bridge))ws_bridge=>bridge%ws_bridge
    if(present(sw_bridge))sw_bridge=>bridge%sw_bridge

    !may need to put a check in if it doesn't return a valid pointer

  end subroutine IXFget_ptr_bridge
  !-----------------------------------------------------------------------------------------------------------------------
  pure logical function IXFcompare_bridge(bridge1,bridge2)result(ident)
    implicit none
    type(IXTbridge),intent(in)::bridge1,bridge2
    ident=.false.
    !provided they have been properly populated, it is only necessary to check
    ! the ws-> spectra mapping are the same
    ident=IXFcompare_ws_bridge(bridge1%ws_bridge,bridge2%ws_bridge)
  end function IXFcompare_bridge
  !-----------------------------------------------------------------------------------------------------------------------
  !! populates an IXTbridge structure given a map file and a mask file
  subroutine IXFpopulate_bridge(bridge,map,mask,status)
    use IXMraw_file
    use IXMmap
    use IXMdetector
    use IXMmask
    implicit none
    type(IXTbridge)::bridge
    type(IXTmap),intent(in)::map
    type(IXTmask),intent(in)::mask
    type(IXTstatus)::status
    integer(i4b),pointer::work_no(:),total_spec(:),spec_ind(:),spec_no(:)
    integer(i4b),allocatable::wk_spec(:),bad_spectra_flag(:)
    integer(i4b),pointer::badspectra(:)
    integer(i4b)::i
      
    !get elements from map/mask to poulate ws_bridge object   
    call IXFget_ptr_mask(mask,badspectra)      
!determine wk_spec array from populated IXTmap
    call IXFwkspec_map(map,wk_spec)             
    call IXFget_ptr_map(map, work_no,total_spec,spec_ind,spec_no)             

    ! if mask file was not found this will make the default not to mask any spectra
    ! and also prepare the array for filling with masked spectra
    allocate(bad_spectra_flag(size(spec_no)))
    bad_spectra_flag=0
  ! if mask file empty then don't need to search trhough all spectra 
    if (sum(badspectra)>0)then    !spectra will now be masked and badspectra pointer is now used
      do i=1,size(badspectra)
         where (spec_no == badspectra(i) ) bad_spectra_flag=1
      enddo
    endif

    call IXFcreate_ws_bridge(bridge%ws_bridge,work_no,total_spec,spec_ind,bad_spectra_flag,spec_no,status)

    deallocate(bad_spectra_flag)
    ! if the mask file was not found then badspectra array hould be deallocated
    call IXFpopulate_sw_bridge(bridge%sw_bridge,wk_spec,spec_no,status)
    deallocate(wk_spec)
    work_no=>NULL()
    total_spec=>NULL()
    spec_ind=>NULL()
    spec_no=>NULL()
    !IXTbridge is now fully populated, nested objects have been filled in situ, so can be initialised
    call IXFmark_valid_bridge(bridge)
  end subroutine IXFpopulate_bridge

end module IXMbridge
