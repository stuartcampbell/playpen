!------------------------------
! MODULE: IXMws_bridge
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1339 $ ($Date: 2008-04-08 11:55:33 -0400 (Tue, 08 Apr 2008) $)
!!
!! FORTRAN definition of IXMws_bridge class
! This is the workspace to spectra bridge
module IXMws_bridge
  use IXMbase
  implicit none	
  public :: IXTws_bridge	
  type IXTws_bridge
     private
     !! workspace->spectra elements
     type(IXTbase):: base
     integer(i4b),pointer::work_no(:)=>NULL() !! unique identifier
     integer(i4b),pointer::total_spec(:)=>NULL()
     integer(i4b),pointer::spec_ind(:)=>NULL()
     integer(i4b),pointer::bad_spectra_flag(:)=>NULL()
     integer(i4b),pointer::spec_no(:)=>NULL() 
  end type IXTws_bridge

#define IXD_TYPE ws_bridge
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTws_bridge class"
#define IXD_TYPE ws_bridge
#define IXD_SQTYPE 'ws_bridge'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_ws_bridge(op, field, arg, status)
    implicit none
    type(IXTws_bridge) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTws_bridge', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'work_no', arg%work_no, status)
    call IXFoperation_run_ptr(op,'total_spec',arg%total_spec,status)
    call IXFoperation_run_ptr(op,'spec_ind', arg%spec_ind, status)
    call IXFoperation_run_ptr(op,'bad_spectra_flag', arg%bad_spectra_flag, status)
    call IXFoperation_run_ptr(op,'spec_no',arg%spec_no,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_ws_bridge


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_ws_bridge(arg, status)
    implicit none
    type(IXTws_bridge) :: arg
    type(IXTstatus) :: status
    call IXFcheck_base(arg%base,status)
    if(.not.(size(arg%work_no).eq.size(arg%total_spec)).and.(size(arg%total_spec).eq.size(arg%spec_ind)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'work_no, total_spec and spec_ind array lengths incompatible  (IXFcheck_ws_bridge)')
    endif

    if(.not. (sum(arg%total_spec) .eq. size(arg%spec_no)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, ' total_spec and spec_no arrays incompatible  (IXFcheck_ws_bridge)')
    endif    
  end subroutine IXFcheck_ws_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_ws_bridge(ws_bridge,work_no,total_spec,spec_ind,bad_spectra_flag,spec_no,status)
    type(IXTws_bridge) :: ws_bridge
    type(IXTstatus) :: status
    integer(i4b),intent(in)::work_no(:),spec_ind(:),total_spec(:),bad_spectra_flag(:),spec_no(:)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(ws_bridge)

    call IXFset_ws_bridge(ws_bridge,status,work_no,total_spec,spec_ind,bad_spectra_flag,spec_no)

  end subroutine IXFcreate_ws_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_ws_bridge(ws_bridge,status)
    implicit none
    type(IXTws_bridge) :: ws_bridge
    type(IXTstatus) :: status

    call IXFdestroy(ws_bridge%base,status)
    call IXFdealloc(ws_bridge%work_no,status)
    call IXFdealloc(ws_bridge%total_spec,status)
    call IXFdealloc(ws_bridge%spec_ind,status)
    call IXFdealloc(ws_bridge%bad_spectra_flag,status)
    call IXFdealloc(ws_bridge%spec_no,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines

    call IXFclear_valid(ws_bridge)

  end subroutine IXFdestroy_ws_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_ws_bridge(ws_bridge,status,work_no,total_spec,spec_ind,bad_spectra_flag,spec_no,ref)
    implicit none
    type(IXTws_bridge),intent(inout) :: ws_bridge
    type(IXTws_bridge),intent(in),optional:: ref
    integer(i4b),optional,intent(in)::work_no(:)
    integer(i4b),optional,intent(in)::total_spec(:)
    integer(i4b),optional,intent(in)::spec_ind(:)
    integer(i4b),optional,intent(in)::bad_spectra_flag(:)
    integer(i4b),optional,intent(in)::spec_no(:)
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_ws_bridge)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(ws_bridge)
    else    
       if(IXFvalid(ws_bridge) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_ws_bridge)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_ws_bridge(ws_bridge,status,&
         ref%work_no,ref%total_spec,ref%spec_ind,&
         ref%bad_spectra_flag,ref%spec_no)

    call IXFset_integer_array(ws_bridge%work_no,status,work_no)    
    call IXFset_integer_array(ws_bridge%total_spec,status,total_spec)    
    call IXFset_integer_array(ws_bridge%spec_ind,status,spec_ind)  
    call IXFset_integer_array(ws_bridge%bad_spectra_flag,status,bad_spectra_flag)          
    call IXFset_integer_array(ws_bridge%spec_no,status,spec_no)    

    call IXFcheck_ws_bridge(ws_bridge,status)

  end subroutine IXFset_ws_bridge


  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_ws_bridge(ws_bridge, status,work_no,total_spec,spec_ind,bad_spectra_flag, &
       spec_no,wout)
    implicit none
    type(IXTws_bridge),intent(in) :: ws_bridge
    type(IXTws_bridge),optional,intent(out)::wout
    type(IXTstatus) :: status
    integer(i4b),optional,intent(out)::work_no(:)
    integer(i4b),optional,intent(out)::total_spec(:)
    integer(i4b),optional,intent(out)::spec_ind(:)
    integer(i4b),optional,intent(out)::bad_spectra_flag(:)
    integer(i4b),optional,intent(out)::spec_no(:)

    if (present(wout))call IXFcopy(ws_bridge,wout,status)

    call IXFget_integer_array(ws_bridge%work_no,status,work_no)    
    call IXFget_integer_array(ws_bridge%total_spec,status,total_spec)    
    call IXFget_integer_array(ws_bridge%spec_ind,status,spec_ind)  
    call IXFget_integer_array(ws_bridge%bad_spectra_flag,status,bad_spectra_flag)          
    call IXFget_integer_array(ws_bridge%spec_no,status,spec_no)    

  end subroutine IXFget_ws_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_ws_bridge(ws_bridge, work_no,total_spec,spec_ind,bad_spectra_flag,spec_no)
    implicit none
    type(IXTws_bridge) :: ws_bridge
    integer(i4b),optional,pointer::work_no(:),total_spec(:),spec_ind(:),bad_spectra_flag(:),spec_no(:)

    if (present(work_no))work_no=>ws_bridge%work_no
    if (present(total_spec))total_spec=>ws_bridge%total_spec
    if (present(spec_ind))spec_ind=>ws_bridge%spec_ind
    if (present(bad_spectra_flag))bad_spectra_flag=>ws_bridge%bad_spectra_flag
    if (present(spec_no))spec_no=>ws_bridge%spec_no

  end subroutine IXFget_ptr_ws_bridge


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_ws_bridge(ws_bridge,status, work_no,total_spec,spec_ind,bad_spectra_flag,spec_no,wout)
    implicit none            
    type(IXTws_bridge),intent(in) :: ws_bridge
    type(IXTws_bridge),intent(out) :: wout
    type(IXTstatus)::status
    integer(i4b),optional,allocatable::work_no(:)
    integer(i4b),optional,allocatable::total_spec(:)
    integer(i4b),optional,allocatable::spec_ind(:)
    integer(i4b),optional,allocatable::bad_spectra_flag(:)
    integer(i4b),optional,allocatable::spec_no(:)

    if (present(work_no))then
       call IXFreallocdimsFortran(work_no,shape(ws_bridge%work_no),.false.,status)
    endif
    if (present(total_spec))then
      call IXFreallocdimsFortran(total_spec,shape(ws_bridge%total_spec),.false.,status)
    endif
    if (present(spec_ind))then
      call IXFreallocdimsFortran(spec_ind,shape(ws_bridge%spec_ind),.false.,status)
    endif

    if (present(bad_spectra_flag))then
      call IXFreallocdimsFortran(bad_spectra_flag,shape(ws_bridge%bad_spectra_flag),.false.,status)    
    endif
    if (present(spec_no))then
      call IXFreallocdimsFortran(spec_no,shape(ws_bridge%spec_no),.false.,status)
    endif

    call IXFget_ws_bridge(ws_bridge,status, work_no,total_spec,spec_ind,bad_spectra_flag,spec_no,wout)

  end subroutine IXFget_alloc_ws_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXF getspecstotal subroutine to get list of all spectra in a particular workspace (masked and unmasked)
  subroutine IXFgetspecstotal_ws_bridge(ws,wk_index,specs_out)
    implicit none
    type(IXTws_bridge) :: ws
    integer(i4b),allocatable::specs_out(:)
    integer(i4b),intent(in)::wk_index
    integer(i4b)::i1,i2

    if(allocated(specs_out))deallocate(specs_out)
    !determine range of spec_no indices
    i1=ws%spec_ind(wk_index)
    i2=ws%spec_ind(wk_index)+ws%total_spec(wk_index)-1

    !masked spectra irrelevant use total no of spectra  
    allocate (specs_out(ws%total_spec(wk_index)))
    !need to reference section of spec_no array using spec_ind for each spectrum 
    specs_out = ws%spec_no(i1:i2)

  end subroutine IXFgetspecstotal_ws_bridge
  !-----------------------------------------------------------------------------------------------------------------------
  !!IXF getspecsgood subroutine to get list of unmasked spectra in a particular workspace 
  subroutine IXFgetspecsgood_ws_bridge(ws,wk_index,specs_out,ind_out)
    implicit none
    type(IXTws_bridge) :: ws
    integer(i4b),allocatable,optional::specs_out(:),ind_out(:)
    integer(i4b),intent(in)::wk_index !! index of workspace
    integer(i4b)::i1,i2,i


    !determine range of spec_no indices
    i1=ws%spec_ind(wk_index)
    i2=ws%spec_ind(wk_index)+ws%total_spec(wk_index)-1

    !use bad spectra mask to allocate size of array
    if(present(specs_out))then
       if(allocated(specs_out))deallocate(specs_out)
       allocate (specs_out(count(ws%bad_spectra_flag(i1:i2)==0)))
       specs_out=pack(ws%spec_no(i1:i2),ws%bad_spectra_flag(i1:i2)==0)
    endif

    if(present(ind_out))then
       if(allocated(ind_out))deallocate(ind_out)
       allocate (ind_out(count(ws%bad_spectra_flag(i1:i2)==0)))
       !apply mask to spec_no array 
       ind_out=pack((/ (i,i=i1,i2) /),ws%bad_spectra_flag(i1:i2)==0)
    endif

  end subroutine IXFgetspecsgood_ws_bridge
  !-----------------------------------------------------------------------------------------------------------------------  
  !!IXFcheck_subsid_ws_bridge will determine if the existing mapping (old_ws) contains enough singly occupied workspaces and
  !! associated spectra which can be remapped to make a new mapping defined by new_ws. if possible the appropriate lookup 
  !! array is returned, if not a deallocated array is returned
    subroutine IXFcheck_subsid_ws_bridge(old_ws,new_ws,lookup)
    implicit none
    type(IXTws_bridge),intent(in)::old_ws,new_ws
    integer(i4b),allocatable::o_mask(:),n_mask(:),lookup(:)
    integer:: omax,nmax,i,o_len,n_len,new_a,new_b,j
    
    omax=maxval(old_ws%spec_no)
    nmax=maxval(new_ws%spec_no)
    
    if(omax  < nmax)return    
    
    o_len=size(old_ws%work_no)
    n_len=size(new_ws%work_no)
    
    allocate(o_mask(omax),n_mask(omax),lookup(omax))
    o_mask=0
    n_mask=0
    lookup=0
    
    !step through each workspace    
    do i=1,max(n_len,o_len)
    ! fill the o_mask with existing appropriate spectra from each workspace
      if(i<=o_len)then
        if(old_ws%total_spec(i)==1)then
        o_mask(old_ws%spec_no(old_ws%spec_ind(i)))=1
        lookup(old_ws%spec_no(old_ws%spec_ind(i)))=i
        endif
      endif
      !fill the n_mask with required spectra range for each workspace
      if(i<=n_len)then
        new_a=new_ws%spec_ind(i) !index of first spectrum in wkspace
        new_b=new_ws%spec_ind(i)+new_ws%total_spec(i)-1 !index of last spectrum in wkspace
        forall(j=new_a:new_b)n_mask(new_ws%spec_no(j))=1
      endif
    enddo

! test for presence of each spectra in n_mask in o_mask
    do i=1,nmax
      if((n_mask(i)==1) .and. (o_mask(i)/=1))then
        deallocate(lookup)
        return
      endif
    enddo
    deallocate(o_mask,n_mask)
    !all tests passed so return lookup array
    return
    
  end subroutine IXFcheck_subsid_ws_bridge
      !-----------------------------------------------------------------------------------------------------------------------

  pure logical function IXFcompare_ws_bridge(wsb1,wsb2)result(ident)
    implicit none
    type(IXTws_bridge),intent(in)::wsb1,wsb2

    ident=.true.
    !work_no is not a meaningful number so does not have to be the same
    !others are more important
    if(size(wsb1%total_spec) /= size(wsb2%total_spec))ident=.false.
    if(size(wsb1%spec_ind) /= size(wsb2%spec_ind))ident=.false.
    if(size(wsb1%bad_spectra_flag) /= size(wsb2%bad_spectra_flag))ident=.false.
    if(size(wsb1%spec_no) /= size(wsb2%spec_no))ident=.false.
    if(.not. ident)return    
    if (sum(abs(wsb1%total_spec - wsb2%total_spec))/=0  ) ident=.false.        
    if (sum(abs(wsb1%spec_ind - wsb2%spec_ind))/=0) ident=.false.
!    we are essentially checking the mapping in the object, so therefore it does not matter if some are bad or not
!    if (sum(abs(wsb1%bad_spectra_flag - wsb2%bad_spectra_flag))/=0) ident=.false.
    if (sum(abs(wsb1%spec_no - wsb2%spec_no))/=0) ident=.false.
    
  end function

   subroutine IXFtubes_to_spectra_ws_bridge(wsbrg,tubes,spectra,status)
    use IXMmask
    implicit none
    type(IXTws_bridge),intent(in)::wsbrg
    type(IXTmask),intent(in)::tubes
    type(IXTmask),intent(out)::spectra
    type(IXTstatus)::status    
    integer(i4b)::i,position,sp_start,sp_end,wk
    integer(i4b),pointer::temp(:),mask_array(:)
    
    call IXFalloc(temp,size(wsbrg%spec_no),status)
    call IXFget_ptr_mask(tubes,mask_array=mask_array)
    temp=0    
    position=0
    do i=1,size(mask_array)
      wk=mask_array(i)
      if(wsbrg%work_no(wk)==wk)then        
        sp_start=wsbrg%spec_ind(wk)
        sp_end=sp_start + wsbrg%total_spec(wk) - 1
        temp(position+1:position+ wsbrg%total_spec(wk) )= wsbrg%spec_no(sp_start:sp_end)
        position=position+wsbrg%total_spec(wk)
      else
return        
      endif
    enddo
    call IXFrealloc(temp,position,.true.,status)
    call IXFcreate_mask(spectra,temp,status)
  end subroutine

end module IXMws_bridge
