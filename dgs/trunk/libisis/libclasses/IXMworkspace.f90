!------------------------------
! MODULE: IXMworkspace
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1417 $ ($Date: 2008-07-04 13:04:09 -0400 (Fri, 04 Jul 2008) $)
!!
!! FORTRAN definition of IXMworkspace class
! workspace module
module IXMworkspace
  use IXMdetector
  use IXMbase
  use IXMeffdet_index
  implicit none
  public :: IXTworkspace
  type IXTworkspace
     private
     type(IXTbase):: base
     !! Workspace information
     !! Workspace numbers are contained in work_no(1:nwork_tot).
     integer(i4b), pointer :: work_no(:)=>NULL()
     !! Index of Good effective detector parameters for the workspace in the derived type
     !! IXTdetarray with name DET_EFF. ('eff' stands for 'effective').
     type (IXTeffdet_index) :: effdet_index
     !! Detector information
     !! eff_det(i) contains the effective detector parameters for a single detector corresponding to 
     !! the ith workspace. Elements run over eff_det(1:nwork_tot)    
     !XXX type (IXTdetector),pointer :: eff_det=>NULL()
     type (IXTdetector) :: eff_det
  end type IXTworkspace

#define IXD_TYPE workspace
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTworkspace class"
#define IXD_TYPE workspace
#define IXD_SQTYPE 'workspace'
#include "class_base.f90"


  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation

  recursive subroutine IXFoperation_run_workspace(op, field, arg, status)
    implicit none
    type(IXTworkspace) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTworkspace', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'work_no',arg%work_no,status)
    call IXFoperation_run(op,'effdet_index',arg%effdet_index,status)
    !XXX    call IXFoperation_run_ptr(op,'eff_det',arg%eff_det,status)
    call IXFoperation_run(op,'eff_det',arg%eff_det,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_workspace

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 
  subroutine IXFcreate_workspace(ws,work_no,effdet_index,status)
    implicit none
    type(IXTworkspace),intent(out)::ws
    integer(i4b),intent(in)::work_no(:)
    type(IXTeffdet_index),intent(in)::effdet_index
    !    type(IXTdetector),intent(in)::effdet_target
    type(IXTstatus)::status

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(effdet_index) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTeffdet_index failure, all nested objects MUST be initialised (IXFcreate_workspace)')
    endif

    if(status == IXCseverity_error)return

    ! make a pointer to the chosen target detector
    ! call IXFmake_ptr_detector(effdet_target,ws%eff_det)

    ! we now make our workspace point at the detector we want to fill
    ! the ref_count variable is also incremented in this routine
    !XXX call IXFget_ptr(e_ptr=ws%eff_det)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(ws)

    ! makes a special call to the set routine without the detector pointer
    ! since we have set it with the call above and do not want to over increment the reference counter

    call IXFset_workspace(ws,status,work_no,effdet_index)

  end subroutine IXFcreate_workspace

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_workspace(ws,status)
    implicit none
    type(IXTworkspace)::ws
    type(IXTstatus)::status
    !    integer(i4b)::ref !! variable which is checked by instrument class to see if it can destroy detector instance fully

    call IXFdestroy(ws%base,status)  
    call IXFdealloc(ws%work_no,status)

    if(IXFvalid(ws%effdet_index))call IXFdestroy(ws%effdet_index,status)
    !*******************************************************
    ! this will decrement the counter in ws%eff_det and destroy it if it is unused elsewhere   
    call IXFdestroy(ws%eff_det,status)

    ! check value of ref_count of detector
    !call IXFget_detector(ws%eff_det,status,ref_count=ref)
    ! if ref_count is zero destroy it totally
    !if (ref .eq. 0)deallocate(ws%eff_det)
    !always remove pointer from THIS instrument 
    !XXX ws%eff_det=>NULL()



  end subroutine IXFdestroy_workspace


  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_workspace(ws,status,work_no,effdet_index,eff_det,ref)
    implicit none
    type(IXTworkspace)::ws
    type(IXTworkspace),optional,intent(in)::ref
    integer(i4b),optional,intent(in)::work_no(:)
    type(IXTeffdet_index),optional,intent(in)::effdet_index
    type(IXTdetector),optional,target::eff_det
    type(IXTstatus)::status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_workspace)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(ws)
    else    
       if(IXFvalid(ws) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_workspace)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_workspace(ws,status,ref%work_no,ref%effdet_index,ref%eff_det)  
    call IXFset_integer_array(ws%work_no,status,work_no)

    if(present(effdet_index))then
       call IXFcopy(effdet_index,ws%effdet_index,status)
    endif

    if(present(eff_det))then
       call IXFcopy(eff_det,ws%eff_det,status)
    endif

!!$    ! need to think more about this section of code, not needed for the moment
!!$    ! 
!!$    ! this should be called if setting eff_det to be new or from a copy
!!$    ! is not called by create routine
!!$    ! if (present(eff_det))then
!!$    ! if the set command is called to change a eff_det table that the workspace points to
!!$
!!$    ! a) check if pointer of ws is associated
!!$    !   if (associated(ws%eff_det))then
!!$    ! b) decrement the eff_det being changed
!!$    !       call IXFdecref_detector(ws%eff_det)
!!$    ! initialize the pointer of the wsrument
!!$    !         ws%eff_det=>NULL()
!!$    ! c) increment will be made below
!!$    !     endif
!!$
!!$    ! and then create new pointer (either copy or a change)
!!$    !    ws%eff_det=>eff_det
!!$    ! and increment the reference (possibly to the new eff_det)
!!$    !    call IXFincref_detector(ws%eff_det)
!!$    !  endif

    call IXFcheck_workspace(ws,status)

  end subroutine IXFset_workspace

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_workspace(ws,status)
    implicit none
    type(IXTworkspace)::ws
    type(IXTstatus)::status
    call IXFcheck_base(ws%base,status)
    call IXFcheck(ws%effdet_index,status)
    call IXFcheck(ws%eff_det,status)
  end subroutine IXFcheck_workspace


  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_workspace(ws,status,work_no,effdet_index,eff_det,wout)
    implicit none
    type(IXTworkspace),intent(in)::ws
    type(IXTworkspace),optional,intent(out)::wout
    integer(i4b),optional,intent(out)::work_no(:)
    type(IXTeffdet_index),optional,intent(out)::effdet_index
    type(IXTdetector),optional,intent(out)::eff_det
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(ws,wout,status)  

    call IXFget_integer_array(ws%work_no,status,work_no)
    if (present(effdet_index))call IXFcopy(ws%effdet_index,effdet_index,status)

    if(present(eff_det))then
       call IXFcopy(ws%eff_det,eff_det,status)
    endif

  end subroutine IXFget_workspace

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_workspace(ws, work_no,eff_det,effdet_index)
    implicit none
    type(IXTworkspace),intent(in),target :: ws
    integer(i4b),pointer,optional::work_no(:)
    type(IXTdetector),pointer,optional::eff_det
    type(IXTeffdet_index),pointer,optional::effdet_index

    if(present(work_no))work_no=>ws%work_no
    if(present(eff_det))eff_det=>ws%eff_det
    if(present(effdet_index))effdet_index=>ws%effdet_index
  end subroutine IXFget_ptr_workspace


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.
  subroutine IXFget_alloc_workspace(ws,status,work_no,effdet_index,eff_det,wout)
    implicit none
    type(IXTworkspace),intent(in)::ws
    type(IXTworkspace),optional,intent(out)::wout
    integer(i4b),optional,allocatable::work_no(:)
    type(IXTeffdet_index),optional,intent(out)::effdet_index
    type(IXTdetector),optional,intent(out)::eff_det
    type(IXTstatus)::status

    if (allocated(work_no))then
       call IXFreallocdimsFortran(work_no,shape(ws%work_no),.false.,status)    
    endif

    call IXFget_workspace(ws,status,work_no,effdet_index,eff_det,wout)    

  end subroutine IXFget_alloc_workspace

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFpopulate will populate an object 
  subroutine IXFpopulate_workspace(workspace,wsbrg_ptr,spe_ptr,det_ptr,status)
    use IXMws_bridge
    use IXMdetector
    use IXMspectra
    use IXMws_bridge
    implicit none
    type(IXTws_bridge),pointer::wsbrg_ptr
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    type(IXTworkspace)::workspace
    type(IXTstatus)::status
    integer(i4b),pointer::work_no(:)

    !get the work_no array pointer from ws_bridge object
    call IXFget_ptr_ws_bridge(wsbrg_ptr,work_no)
    !copy the contents to the work_no array to workspace%work_no
    ! we now make our workspace point at the effective detector we want to fill
    !  the reference counter is incremented in this subroutine call    

    ! also populates effective detector on the fly
    call IXFpopulate_effdet_index(workspace%effdet_index,workspace%eff_det,wsbrg_ptr,spe_ptr,det_ptr,status)
    !special call to private mark_init routine
    if (status == IXCseverity_error)return
    call IXFmark_valid(workspace)
    ! set command can now be called
    call IXFset_workspace(workspace,status,work_no=work_no)

  end subroutine IXFpopulate_workspace

end module IXMworkspace
