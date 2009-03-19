!------------------------------
! MODULE: IXMuser
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! FORTRAN definition of IXMuser class
! user module
module IXMuser
  use IXMbase
  implicit none	
  public :: IXTuser	
  type IXTuser
     private
     type(IXTbase):: base
     character(len=long_len):: name='name'				!! Name of user
     character(len=long_len) :: affiliation='affiliation'		!! name of home institution
     character(len=long_len):: address='address'                        !! address of user
     character(len=long_len) :: telephone='telephone'                   !! telephone number of user
     character(len=long_len) :: fax='fax'                               !! fax number of user
     character(len=long_len) :: email='email'                           !! e-mail address of user
  end type IXTuser

#define IXD_TYPE user
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTuser class"
#define IXD_TYPE user
#define IXD_SQTYPE 'user'
#include "class_base.f90"

!-----------------------------------------------------------------------------------------------------------------------
! All classes must provide this operation; it loops through
! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_user(op, field, arg, status)
    implicit none
    type(IXTuser) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTuser', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'affiliation',arg%affiliation,status)
    call IXFoperation_run(op,'address', arg%address, status)
    call IXFoperation_run(op,'telephone', arg%telephone, status)
    call IXFoperation_run(op,'fax',arg%fax,status)
    call IXFoperation_run(op,'email',arg%email,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_user
  
!-----------------------------------------------------------------------------------------------------------------------
!!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
!! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
!! declaration will be used, it is therefore the last argument. If an element of the object is another class then
!! it MUST be initialised. 

  subroutine IXFcreate_user(user,name,affiliation,address,telephone,fax,email,status)
    implicit none
    type(IXTuser),intent(out)::user
    type(IXTstatus)::status
    character(len=*),intent(in)::name,affiliation,address,telephone,fax,email
    
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(user)
    
     ! for the optional present base class then the full set command is called
    call IXFset_user(user,status,name,affiliation,address,telephone,fax,email)

  end subroutine IXFcreate_user

!-----------------------------------------------------------------------------------------------------------------------
!!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
!! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
!! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
!! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
!! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
!!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_user(user,status,name,affiliation,address,telephone,fax,email,ref)
    implicit none
    type(IXTuser),intent(inout)::user
    type(IXTuser),optional,intent(in)::ref
    character(len=*),optional,intent(in)::name,affiliation,address,telephone,fax,email
    type(IXTstatus)::status

   ! check that either the reference object is initialised
   ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_user)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(user)
    else    
       if(IXFvalid(user) .neqv. .true.) then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_user)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_user(user,status,ref%name,ref%affiliation,ref%address,ref%telephone,ref%fax,ref%email)
    if (present(name))user%name=name
    if (present(affiliation))user%affiliation=affiliation
    if (present(address))user%address=address
    if (present(telephone))user%telephone=telephone
    if (present(fax))user%fax=fax
    if (present(email))user%email=email
    
    call IXFcheck_user(user,status)

  end subroutine IXFset_user

  subroutine IXFget_user(user,status,name,affiliation,address,telephone,fax,email,wout)
    implicit none
    type(IXTuser),intent(inout)::user
    type(IXTuser),optional::wout
    character(len=*),optional,intent(out)::name,affiliation,address,telephone,fax,email
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(user,wout,status)
    if (present(name))name=user%name
    if (present(affiliation))affiliation=user%affiliation
    if (present(address))address=user%address
    if (present(telephone))telephone=user%telephone
    if (present(fax))fax=user%fax
    if (present(email))email=user%email

    ! there is no point in doing a check since all the entries are just characters

  end subroutine IXFget_user

!-----------------------------------------------------------------------------------------------------------------------
!! IXFcheck will make internal consistency checks in the object, such as array length checking to make
!! sure the object is properly filled.

  subroutine IXFcheck_user(user,status)
    implicit none  
    type(IXTuser)::user
    type(IXTstatus)::status
    call IXFcheck_base(user%base,status)
  end subroutine IXFcheck_user

!-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_user(user,status)
    implicit none  
    type(IXTuser)::user
    type(IXTstatus)::status
    call IXFdestroy(user%base,status)
  end subroutine IXFdestroy_user


end module IXMuser
