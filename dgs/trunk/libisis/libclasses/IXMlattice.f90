!------------------------------
! MODULE: IXMlattice
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMlattice class
module IXMlattice
  use IXMbase

  public :: IXTlattice
  type IXTlattice
     private
     type(IXTbase):: base
     real(dp)::a=0.0_dp
     real(dp)::b=0.0_dp
     real(dp)::c=0.0_dp
     real(dp)::alpha=0.0_dp
     real(dp)::beta=0.0_dp
     real(dp)::gamma=0.0_dp
     character(len=long_len):: space_group='space_group'
  end type IXTlattice


#define IXD_TYPE lattice
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTlattice class"
#define IXD_TYPE lattice
#define IXD_SQTYPE 'lattice'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled

  subroutine IXFcheck_lattice(lattice,status)
    implicit none
    type(IXTlattice)::lattice
    type(IXTstatus)::status
    call IXFcheck_base(lattice%base,status)
  end subroutine IXFcheck_lattice

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation

  recursive subroutine IXFoperation_run_lattice(op, field, arg, status)
    implicit none
    type(IXTlattice) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTlattice', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'a', arg%a, status)
    call IXFoperation_run(op,'b',arg%b,status)
    call IXFoperation_run(op,'c', arg%c, status)
    call IXFoperation_run(op,'alpha', arg%alpha, status)
    call IXFoperation_run(op,'beta',arg%beta,status)
    call IXFoperation_run(op,'gamma',arg%gamma,status)
    call IXFoperation_run(op,'space_group',arg%space_group,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_lattice

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_lattice(lattice,status,a,b,c,alpha,beta,gamma,space_group,ref)
    implicit none
    type(IXTlattice),intent(inout)::lattice
    type(IXTlattice),optional,intent(in)::ref
    real(dp),optional,intent(in)::a,b,c,alpha,beta,gamma
    character(len=*),optional,intent(in)::space_group
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_lattice)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(lattice)
    else    
       if(IXFvalid(lattice) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_lattice)')
       endif
       if(status == IXCseverity_error)return
    endif


    if (present(ref))call IXFset_lattice(lattice,status,ref%a,ref%b,ref%c,ref%alpha,ref%beta,ref%gamma,ref%space_group)

    if (present(a))lattice%a=a
    if (present(b))lattice%b=b
    if (present(c))lattice%c=c
    if (present(alpha))lattice%alpha=alpha
    if (present(beta))lattice%beta=beta
    if (present(gamma))lattice%gamma=gamma
    if (present(space_group))lattice%space_group=space_group

    call IXFcheck_lattice(lattice,status)
  end subroutine IXFset_lattice

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_lattice(lattice,status,a,b,c,alpha,beta,gamma,space_group,wout)
    implicit none
    type(IXTlattice),intent(inout)::lattice
    type(IXTlattice),optional,intent(out)::wout
    real(dp),optional,intent(out)::a,b,c,alpha,beta,gamma
    character(len=*),optional,intent(out)::space_group
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(lattice,wout,status)
    if (present(a))a=lattice%a
    if (present(b))b=lattice%b
    if (present(c))c=lattice%c
    if (present(alpha))alpha=lattice%alpha
    if (present(beta))beta=lattice%beta
    if (present(gamma))gamma=lattice%gamma
    if (present(space_group))space_group=lattice%space_group

  end subroutine IXFget_lattice

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_lattice(lattice,status)
    implicit none  
    type(IXTlattice)::lattice
    type(IXTstatus)::status
    call IXFdestroy(lattice%base,status)
    call IXFclear_valid(lattice)

  end subroutine IXFdestroy_lattice

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_lattice(lattice,a,b,c,alpha,beta,gamma,space_group,status)
    implicit none
    type(IXTlattice),intent(out)::lattice
    type(IXTstatus)::status
    real(dp),intent(in)::a,b,c,alpha,beta,gamma
    character(len=*),intent(in)::space_group

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(lattice)

    call IXFset_lattice(lattice,status,a,b,c,alpha,beta,gamma,space_group)

  end subroutine IXFcreate_lattice


end module IXMlattice
