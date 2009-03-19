!------------------------------
! MODULE: IXMeffdet_index
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMeffdet_index class

module IXMeffdet_index
  use IXMtype_definitions
  use IXMspectra
  use IXMdetector
  use IXMws_bridge

  public :: IXTeffdet_index
  type IXTeffdet_index
     private
     type(IXTbase):: base
     integer(i4b), pointer :: good_index(:)=>NULL()
     integer(i4b), pointer :: total_index(:)=>NULL()
  end type IXTeffdet_index

#define IXD_TYPE effdet_index
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTeffdet_index class"
#define IXD_TYPE effdet_index
#define IXD_SQTYPE 'effdet_index'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation

  recursive subroutine IXFoperation_run_effdet_index(op, field, arg, status)
    implicit none
    type(IXTeffdet_index) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTeffdet_index', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_ptr(op,'good_index', arg%good_index, status)
    call IXFoperation_run_ptr(op,'total_index',arg%total_index,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_effdet_index(effdet_index,good_index,total_index,status)
    implicit none
    type(IXTeffdet_index) :: effdet_index
    type(IXTstatus) :: status
    integer(i4b),intent(in)::good_index(:),total_index(:)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(effdet_index)

    call IXFset_effdet_index(effdet_index,status,good_index,total_index)

  end subroutine IXFcreate_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_effdet_index(arg, status)
    implicit none
    type(IXTeffdet_index) :: arg
    type(IXTstatus) :: status
    call IXFcheck_base(arg%base,status)
    if(size(arg%total_index) /= size(arg%good_index))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'good_index /total_index array lengths incompatible (IXFcheck_effdet_index)')
    endif    
  end subroutine IXFcheck_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_effdet_index(effdet_index,status)
    implicit none
    type(IXTeffdet_index) :: effdet_index
    type(IXTstatus) :: status

    call IXFdestroy(effdet_index%base,status)
    call IXFdealloc(effdet_index%good_index,status)
    call IXFdealloc(effdet_index%total_index,status)
    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(effdet_index)

  end subroutine IXFdestroy_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_effdet_index(effdet_index,status,good_index,total_index,ref)
    implicit none
    type(IXTeffdet_index),intent(inout) :: effdet_index
    type(IXTeffdet_index),intent(in),optional:: ref
    integer(i4b),optional,intent(in)::good_index(:)
    integer(i4b),optional,intent(in)::total_index(:)
    type(IXTstatus) :: status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_effdet_index)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(effdet_index)
    else    
       if(IXFvalid(effdet_index) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_effdet_index)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_effdet_index(effdet_index,status,ref%good_index,ref%total_index)

    call IXFset_integer_array(effdet_index%good_index,status,good_index)    
    call IXFset_integer_array(effdet_index%total_index,status,total_index)    
    call IXFcheck_effdet_index(effdet_index,status)

  end subroutine IXFset_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_effdet_index(effdet_index, status,good_index,total_index,wout)
    implicit none
    type(IXTeffdet_index),intent(in) :: effdet_index
    type(IXTeffdet_index),optional,intent(out)::wout
    type(IXTstatus) :: status
    integer(i4b),optional,intent(out)::good_index(:)
    integer(i4b),optional,intent(out)::total_index(:)

    if (present(wout))call IXFcopy(effdet_index,wout,status)
    call IXFget_integer_array(effdet_index%good_index,status,good_index)    
    call IXFget_integer_array(effdet_index%total_index,status,total_index)    

  end subroutine IXFget_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_effdet_index(effdet_index, good_index,total_index)
    implicit none
    type(IXTeffdet_index) :: effdet_index
    integer(i4b),optional,pointer::good_index(:),total_index(:)

    if (present(good_index))good_index=>effdet_index%good_index
    if (present(total_index))total_index=>effdet_index%total_index

  end subroutine IXFget_ptr_effdet_index

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_effdet_index(effdet_index, status,good_index,total_index,wout)
    implicit none
    type(IXTeffdet_index),intent(in) :: effdet_index
    type(IXTeffdet_index),intent(out) :: wout
    integer(i4b),optional,allocatable::good_index(:)
    integer(i4b),optional,allocatable::total_index(:)
    type(IXTstatus)::status

    if (present(good_index))then
       call IXFreallocdimsFortran(good_index,shape(effdet_index%good_index),.false.,status)    
    endif
    if (present(total_index))then
       call IXFreallocdimsFortran(total_index,shape(effdet_index%total_index),.false.,status)        
    endif    
    call IXFget_effdet_index(effdet_index, status,good_index,total_index,wout)
    
  end subroutine IXFget_alloc_effdet_index

 
         !*********************************************

  subroutine IXFpopulate_effdet_index(effdet_index,eff_det,wsbrg_ptr,spe_ptr,det_full,status)
    implicit none
    type(IXTeffdet_index)::effdet_index
    type(IXTstatus)::status
    type(IXTdetector)::eff_det !!effective detector will be populated
    type(IXTdetector),intent(in)::det_full
    type(IXTspectra),intent(in)::spe_ptr
    type(IXTws_bridge),pointer::wsbrg_ptr
    integer(i4b)::i,j,check,nwork,Leff,posn_index,solid_counter,he3_counter
    integer(i4b),pointer::total_spec(:),bad_spectra_flag(:),spec_ind(:)
integer(i4b),allocatable::speclist(:),detlist(:),w_type(:)
real(dp),allocatable::w_dimension(:,:)
    ! this is a type to put in effdet object, which is filled from the det_full object
    type(IXTdet_he3)::avg_he3
    type(IXTgeometry)::avg_geometry
    !! in the effective detector table there is no duplication of entries
    !! if all the spectra in a workspace are good the indeces are the same
    !! if at least one spectrum is bad then the index for the good workspace 
    !! comes before the total workspace
    !! if all the spectra are masked then the index is 0 in the good index

    call IXFget_ptr_ws_bridge(wsbrg_ptr,total_spec=total_spec,spec_ind=spec_ind,bad_spectra_flag=bad_spectra_flag)

    nwork=size(total_spec) !number of workspaces
    call IXFalloc(effdet_index%good_index,nwork,status)
    call IXFalloc(effdet_index%total_index,nwork,status)

    effdet_index%good_index=0
    effdet_index%total_index=0

    !fill up good_index and total_index arrays
    i=1 ! workspace index
    j=1 ! index in effective detector array
    do while ( i <= nwork )
       !are any of the spectra in workspace bad
       check=sum(bad_spectra_flag(spec_ind(i): spec_ind(i)+total_spec(i)-1))
       if (check == 0)then
          ! GOOD = TOTAL
          effdet_index%good_index(i)=j
          effdet_index%total_index(i)=j
          j=j+1
       else if (check == total_spec(i))then
          ! fully masked TOTAL SPECTRA only
          effdet_index%good_index(i)=0
          effdet_index%total_index(i)=j
          j=j+1
       else
          ! GOOD and TOTAL
          ! i is workspace index
          effdet_index%good_index(i)=j
          effdet_index%total_index(i)=j+1  
          !*********************************************
          j=j+2
          !*********************************************
       endif
       i=i+1
    enddo

    ! calculate length of arrays in effective detector is the last index in the total_index array
    Leff=effdet_index%total_index(nwork)
    ! allocate memory to arrays in effective detector.. all except IXTdet-solid/IXTdet_he3/IXTgeometry_IXTshape
solid_counter=1
he3_counter=1    
    
call IXFsetup_effective_detector(eff_det,det_full,Leff,status)
!ntypes is the total number of shape types present in detfull, and this
! is mirrored in eff_det accordingly
! size of w_dimensions is for a box
allocate(w_type(Leff),w_dimension(3,Leff))
w_type=0
w_dimension=0.0  
    
 if (status == IXCseverity_error) return

posn_index=1
    do i=1,nwork
       if(effdet_index%good_index(i) == effdet_index%total_index(i) .or. effdet_index%good_index(i)==0)then          
          call IXFgetspecstotal_ws_bridge(wsbrg_ptr,i,speclist)
          !fill up detector list from spectra list
          call IXFgetdets_spectra(spe_ptr,speclist,detlist,status)
          if (status == IXCseverity_error) return
!GOOD = TOTAL or TOTAL only

! combine position

call IXFaverageparts_detector(detlist,posn_index,det_full,eff_det,solid_counter,he3_counter,w_type,w_dimension,status)
posn_index=posn_index+1
       else
!GOOD /= TOTAL       
          ! no check necessary since .or. check used
          call IXFgetspecsgood_ws_bridge(wsbrg_ptr,i,speclist)
          !fill up detector list from spectra list
          call IXFgetdets_spectra(spe_ptr,speclist,detlist,status)
          if (status == IXCseverity_error) return
! GOOD LIST
call IXFaverageparts_detector(detlist,posn_index,det_full,eff_det,solid_counter,he3_counter,w_type,w_dimension,status)
posn_index=posn_index+1
          call IXFgetspecstotal_ws_bridge(wsbrg_ptr,i,speclist)
          !fill up detector list from spectra list
          call IXFgetdets_spectra(spe_ptr,speclist,detlist,status)
          if (status == IXCseverity_error) return
!TOTAL LIST
call IXFaverageparts_detector(detlist,posn_index,det_full,eff_det,solid_counter,he3_counter,w_type,w_dimension,status)
posn_index=posn_index+1
       endif
    enddo
    
! has been incrementing as it used so therefore remove last increment
he3_counter=he3_counter-1
solid_counter=solid_counter-1

call IXFfinish_effective_detector(eff_det,solid_counter,he3_counter,w_type,w_dimension,status)
if (status == IXCseverity_error) return
deallocate(w_type,w_dimension)
    call IXFmark_valid(effdet_index)

  end subroutine IXFpopulate_effdet_index
  

  
  
end module IXMeffdet_index
