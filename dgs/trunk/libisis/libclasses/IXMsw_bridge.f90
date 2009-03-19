!------------------------------
! MODULE: IXMsw_bridge
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMsw_bridge class
module IXMsw_bridge
  use IXMtype_definitions
  use IXMbase
  implicit none	
  public :: IXTsw_bridge
  type IXTsw_bridge
     private
     type(IXTbase):: base
     integer(i4b), pointer :: spec_no(:)=>NULL()
     integer(i4b), pointer :: total_work(:)=>NULL()
     integer(i4b),pointer:: work_ind(:)=>NULL()
     integer(i4b),pointer :: work_no(:)=>NULL()
  end type IXTsw_bridge

#define IXD_TYPE sw_bridge
#include "class_header.f90"


contains

#define IXD_DESCRIPTION	"IXTsw_bridge class"
#define IXD_TYPE sw_bridge
#define IXD_SQTYPE 'sw_bridge'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_sw_bridge(op, field, arg, status)
    implicit none
    type(IXTsw_bridge) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTsw_bridge', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'spec_no', arg%spec_no, status)
    call IXFoperation_run_ptr(op,'total_work',arg%total_work,status)
    call IXFoperation_run_ptr(op,'work_ind', arg%work_ind, status)
    call IXFoperation_run_ptr(op,'work_no', arg%work_no, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 
  subroutine IXFcreate_sw_bridge(sw_bridge,spec_no,total_work,work_ind,work_no, status)
    implicit none
    type(IXTsw_bridge) :: sw_bridge
    type(IXTstatus) :: status
    integer(i4b),intent(in)::spec_no(:),work_ind(:),total_work(:),work_no(:)

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(sw_bridge)

    call IXFset_sw_bridge(sw_bridge,status,spec_no,total_work,work_ind,work_no)
  end subroutine IXFcreate_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_sw_bridge(arg, status)
    implicit none
    type(IXTsw_bridge) :: arg
    type(IXTstatus) :: status
    call IXFcheck_base(arg%base,status)
    if(.not.(size(arg%spec_no).eq.size(arg%total_work)).and.(size(arg%total_work).eq.size(arg%work_ind)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'spec_no, total_work and work_ind array lengths incompatible  (IXFcheck_ws_bridge)')
    endif

    if(.not. (sum(arg%total_work) .eq. size(arg%work_no)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'total_work and work_no arrays incompatible  (IXFcheck_ws_bridge)')
    endif 
  end subroutine IXFcheck_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_sw_bridge(sw_bridge,status)
    implicit none
    type(IXTsw_bridge) :: sw_bridge
    type(IXTstatus) :: status

    call IXFdestroy(sw_bridge%base,status)
    call IXFdealloc(sw_bridge%spec_no,status)
    call IXFdealloc(sw_bridge%total_work,status)
    call IXFdealloc(sw_bridge%work_ind,status)
    call IXFdealloc(sw_bridge%work_no,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(sw_bridge)

  end subroutine IXFdestroy_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.
  recursive subroutine IXFset_sw_bridge(sw_bridge,status,spec_no,total_work,work_ind,work_no,ref)
    implicit none
    type(IXTsw_bridge),intent(inout) :: sw_bridge
    type(IXTsw_bridge),intent(in),optional:: ref
    integer(i4b),optional,intent(in)::spec_no(:)
    integer(i4b),optional,intent(in)::total_work(:)
    integer(i4b),optional,intent(in)::work_ind(:)
    integer(i4b),optional,intent(in)::work_no(:)
    type(IXTstatus) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(sw_bridge)
    else    
       if(IXFvalid(sw_bridge) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_)')
       endif
       if(status == IXCseverity_error)return
    endif

    if(present(ref))call IXFset_sw_bridge(sw_bridge,status,ref%spec_no,ref%total_work,ref%work_ind,ref%work_no)

    call IXFset_integer_array(sw_bridge%spec_no,status,spec_no)
    call IXFset_integer_array(sw_bridge%total_work,status,total_work)
    call IXFset_integer_array(sw_bridge%work_ind,status,work_ind)
    call IXFset_integer_array(sw_bridge%work_no,status,work_no)

    call IXFcheck_sw_bridge(sw_bridge,status)


  end subroutine IXFset_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_sw_bridge(sw_bridge, status,spec_no,total_work,work_ind,work_no,wout)
    implicit none
    type(IXTsw_bridge),intent(in) :: sw_bridge
    type(IXTsw_bridge),optional,intent(out)::wout
    type(IXTstatus) :: status
    integer(i4b),optional,intent(out)::spec_no(:)
    integer(i4b),optional,intent(out)::total_work(:)
    integer(i4b),optional,intent(out)::work_ind(:)
    integer(i4b),optional,intent(out)::work_no(:)

    if (present(wout))call IXFcopy(sw_bridge,wout,status)
    call IXFget_integer_array(sw_bridge%spec_no,status,spec_no)
    call IXFget_integer_array(sw_bridge%total_work,status,total_work)
    call IXFget_integer_array(sw_bridge%work_ind,status,work_ind)
    call IXFget_integer_array(sw_bridge%work_no,status,work_no)

  end subroutine IXFget_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_sw_bridge(sw_bridge, spec_no,total_work,work_ind,work_no)
    implicit none
    type(IXTsw_bridge) :: sw_bridge
    integer(i4b),optional,pointer::spec_no(:),total_work(:),work_ind(:),work_no(:)

    if (present(spec_no))spec_no=>sw_bridge%spec_no
    if (present(total_work))total_work=>sw_bridge%total_work
    if (present(work_ind))work_ind=>sw_bridge%work_ind
    if (present(work_no))work_no=>sw_bridge%work_no

  end subroutine IXFget_ptr_sw_bridge

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_sw_bridge(sw_bridge,status, spec_no,total_work,work_ind,work_no,wout)
    implicit none
    type(IXTsw_bridge),intent(in) :: sw_bridge
    type(IXTsw_bridge),optional,intent(out) :: wout
    integer(i4b),optional,allocatable::spec_no(:)
    integer(i4b),optional,allocatable::total_work(:)
    integer(i4b),optional,allocatable::work_ind(:)
    integer(i4b),optional,allocatable::work_no(:)
    type(IXTstatus)::status

    if (present(spec_no))then
       call IXFreallocdimsFortran(spec_no,shape(sw_bridge%spec_no),.false.,status)    
    endif

    if (present(total_work))then
       call IXFreallocdimsFortran(total_work,shape(sw_bridge%total_work),.false.,status)    
    endif

    if (present(work_ind))then
       call IXFreallocdimsFortran(work_ind,shape(sw_bridge%work_ind),.false.,status)    
    endif

    if (present(work_no))then
       call IXFreallocdimsFortran(work_no,shape(sw_bridge%work_no),.false.,status)    
    endif
    call IXFget_sw_bridge(sw_bridge,status, spec_no,total_work,work_ind,work_no,wout)
  end subroutine IXFget_alloc_sw_bridge



  ! populates a sw_bridge from a corresponding ws_bridge
  subroutine IXFpopulate_sw_bridge(sw_bridge,wk_spec,specs,status)
    use IXMsort
    implicit none
    type(IXTsw_bridge)::sw_bridge
    type(IXTstatus)::status
    integer(i4b)::wk_spec(:),specs(:),ispec,i,j,count
    integer(i4b),allocatable::ranks(:),work_no(:),s_sorted(:),work_ind(:),total_work(:),spec_no(:)

    allocate(ranks(size(specs)),work_no(size(specs)),s_sorted(size(specs)))

    call IXFrank(specs,ranks)

    ! sort the wks and specs array respectively to create work_no array and s_sorted array
    s_sorted=specs(ranks)
    work_no=wk_spec(ranks)

    !find total # of independent spectra (ispec) from sorted list
    ispec=1
    do i=2,size(specs)
       if (s_sorted(i) /= s_sorted(i-1))then
          ispec=ispec+1 
       endif
    enddo
    ! allocate arrays according to ispec
    allocate (total_work(ispec),spec_no(ispec),work_ind(ispec))

    !set initial values in arrays, first array index will always be 1
    work_ind(1)=1
    total_work=0

    j=1
    i=1
    count=1
    !step thru sorted spectra to determine how many workspaces each spectrum 
    !contributes to and increment index array
    do while (i <= size(specs) .and. j <= ispec)
       if (s_sorted(i) == count) then
          total_work(j)=total_work(j)+1
          spec_no(j)=s_sorted(i)
          i=i+1
       else
          count=count+1
          if (total_work(j)/=0)then
             work_ind(j+1)=total_work(j)+work_ind(j)  
             j=j+1
          endif
       endif
    enddo

    !should be a create call and use default object base type
    !call IXFset_sw_bridge(sw_bridge,status,spec_no,total_work,work_ind,work_no,base)
    call IXFcreate_sw_bridge(sw_bridge,spec_no,total_work,work_ind,work_no,status)
    !not necessary since check called in create routine
    !call IXFcheck(sw_bridge,status)

    deallocate(ranks,work_no,s_sorted,work_ind,total_work,spec_no)  

  end subroutine IXFpopulate_sw_bridge


end module IXMsw_bridge
