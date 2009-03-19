!------------------------------
! MODULE: IXMgroup
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 818 $ ($Date: 2006-09-20 05:39:06 -0400 (Wed, 20 Sep 2006) $)
!!
!! group ids start at 1 and are +ve integers
!!
module IXMgroup
  use IXMbase
  implicit none
  public :: IXTgroup
  integer, parameter, public :: IXCinvalid_id_group = -1
  integer, parameter, public :: IXCno_parent_group = 0
  type IXTgroup
     type(IXTbase) :: base
     character(len=long_len) :: name = ' '      !! group name
     integer :: id = IXCinvalid_id_group        !! unique id for the group
     integer :: parent = IXCinvalid_id_group    !! id of parent group
  end type IXTgroup

#define IXD_TYPE group
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTgroup class"
#define IXD_TYPE group
#define IXD_SQTYPE 'group'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_group(op, field, arg, status)
    implicit none
    type(IXTgroup) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTgroup', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'id', arg%id, status)
    call IXFoperation_run(op,'parent', arg%parent, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_group

  recursive subroutine IXFset_group(opt,status,name,ref)
    implicit none
    type(IXTgroup),intent(inout)::opt
    type(IXTgroup),optional,intent(in)::ref
    type(IXTstatus)::status
    character(len=*),optional :: name

    if (present(ref))call IXFset_group(opt,status,ref%name)

    if (present(name))opt%name=name

  end subroutine IXFset_group

  subroutine IXFget_group(opt,status,name,wout)
    implicit none
    type(IXTgroup),intent(inout)::opt
    type(IXTgroup),optional::wout
    character(len=*),optional :: name
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(opt,wout,status)
    if (present(name))name=opt%name

  end subroutine IXFget_group

  subroutine IXFcheck_group(opt,status)
    implicit none  
    type(IXTgroup)::opt
    type(IXTstatus)::status
  end subroutine IXFcheck_group

  subroutine IXFdestroy_group(opt,status)
    implicit none  
    type(IXTgroup)::opt
    type(IXTstatus)::status
  end subroutine IXFdestroy_group

  subroutine IXFcreate_group(opt,name,status)
    implicit none
    type(IXTgroup),intent(out)::opt
    type(IXTstatus)::status
    character(len=*),optional :: name

    call IXFset_group(opt,status,name)
  end subroutine IXFcreate_group

end module IXMgroup
