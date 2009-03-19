!------------------------------
! MODULE: IXMgroups
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 818 $ ($Date: 2006-09-20 05:39:06 -0400 (Wed, 20 Sep 2006) $)
!!
!! Routines for manipulating groups. A group has a name and an id;  
!! groups are arranged in a hierarchy so a group may also have a parent group
!! to which it belongs. The functions available are:
!!> 
!! IXFname_groups          Retun the name of group given its ID
!! IXFid_groups            Return ID of group given its name 
!! IXFparent_groups        Return parent name of group given either its name or id 
!! IXFparent_id_groups     Return parent ID of group given either its name or id 
!! IXFadd_groups           Add a new group, specifying its name and parent
!! IXFremove_groups        Remove a group, specifying either its name or id
!! IXFis_member_groups     Test for group membership given either names or ids
!! IXFparent_list_groups   Return all parent groups IDs for a given group i.e.
!!                         all other groups that we also belong to  
!! IXFmember_list_groups   Return all children groups for a given group i.e.
!!                         all groups that are members of us 
!! IXFprint_groups         Print out a group ID list returned from 
!!                         IXFparent_list_groups or IXFmember_list_groups
!!< 
module IXMgroups
  use IXMgroup
  implicit none
!  private	
  public :: IXTgroups	
  type IXTgroups
    private
        type(IXTbase) :: base
	    type(IXTgroup) :: list(100)
        integer(i4b) :: n = 0 !! size of group list
  end type IXTgroups

  public :: IXFparent_id_groups, IXFparent_groups, IXFadd_groups, IXFid_groups, &
            IXFmember_list_groups, IXFparent_list_groups, IXFprint_groups, &
            IXFis_member_groups, IXFremove_groups, IXFname_groups  
  
  interface IXFparent_id_groups
    module procedure parent_id_byname, parent_id_byid
  end interface
  
  interface IXFparent_groups
    module procedure parent_byname, parent_byid
  end interface

  interface IXFis_member_groups
    module procedure member_byname, member_byid
  end interface

  interface IXFremove_groups
    module procedure remove_byname, remove_byid
  end interface

  interface IXFparent_list_groups
    module procedure parent_list_byname, parent_list_byid
  end interface

  interface IXFmember_list_groups
    module procedure member_list_byname, member_list_byid
  end interface

#define IXD_TYPE groups
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTgroups class"
#define IXD_TYPE groups
#define IXD_SQTYPE 'groups'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_groups(op, field, arg, status)
    implicit none
    type(IXTgroups) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTgroups', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'list', arg%list, status)
    call IXFoperation_run(op,'n', arg%n, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_groups
  
  recursive subroutine IXFset_groups(opt,status,names,ref)
    implicit none
    type(IXTgroups),intent(inout)::opt
    type(IXTgroups),optional,intent(in)::ref
    type(IXTstatus)::status
    character(len=*),optional :: names(:)
    
!    if (present(ref))call IXFset_groups(opt,status,ref%names)
    
  end subroutine IXFset_groups

  subroutine IXFget_groups(opt,status,names,wout)
    implicit none
    type(IXTgroups),intent(inout)::opt
    type(IXTgroups),optional::wout
    character(len=*),optional :: names(:)
    type(IXTstatus)::status
    
    if (present(wout))call IXFcopy(opt,wout,status)

  end subroutine IXFget_groups

  subroutine IXFcheck_groups(opt,status)
    implicit none  
    type(IXTgroups)::opt
    type(IXTstatus)::status
  end subroutine IXFcheck_groups
  
  subroutine IXFdestroy_groups(opt,status)
    implicit none  
    type(IXTgroups)::opt
    type(IXTstatus)::status
  end subroutine IXFdestroy_groups

  subroutine IXFcreate_groups(opt,names,status)
    implicit none
    type(IXTgroups),intent(out)::opt
    type(IXTstatus)::status
    character(len=*),optional :: names(:)

    call IXFset_groups(opt,status,names)
  end subroutine IXFcreate_groups  

!! add a new group to the group list
  subroutine IXFadd_groups(groups,name,parent,status)
    implicit none
    type(IXTgroups) :: groups !! group list
    character(len=*) :: name !! name of new group to add
    character(len=*) :: parent !! name of parent group
    type(IXTbase) :: base
    type(IXTstatus)::status
    integer n, id, pid
    if (parent /= ' ') then
       pid = IXFid_groups(groups,parent)
    else
       pid = IXCno_parent_group ! no parent
    endif
    if (pid .lt. 0) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'parent group does not exist')
        return
    endif
    id = IXFid_groups(groups,name)
    if (id .lt. 0) then ! group does not exist
        groups%n = groups%n + 1
        groups%list(groups%n) = IXTgroup(base,name,-id,pid)
    else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'group already exists')
    endif
  end subroutine

!! returns group name or ' ' if not found
  pure function IXFname_groups(groups,id) result(name)
    implicit none
    integer idx, max_id, i
    type(IXTgroups) , intent(in) :: groups
    integer, intent(in) :: id
    character(len=long_len)  :: name
    name = ' '
    i = index_byid(groups, id)
    if (i > 0) name = groups%list(i)%name
  end function

!! returns list of parent group ID's for a given id
!! first element of list is the group itself 
  subroutine parent_list_byid(groups,id,id_list,n,status)
    implicit none
    integer idx, max_id, i, j, pid
    type(IXTgroups) , intent(in) :: groups
    type(IXTstatus)::status
    integer, intent(in) :: id !! id to check
    integer, intent(out) :: id_list(:) 
    integer, intent(out) :: n !! number of groups returned
!    j = 0
    j = 1
    id_list(j) = id
    pid = IXFparent_id_groups(groups,id)
    do while (pid > 0 .and. j < size(id_list))
        j = j + 1
        id_list(j) = pid
        pid = IXFparent_id_groups(groups,pid)
    enddo
    if (pid /= IXCno_parent_group) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'list overflow')
    endif
    n = j
  end subroutine

  subroutine parent_list_byname(groups,name,id_list,n,status)
    implicit none
    integer idx, max_id, i, j, pid
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: name
    integer , intent(out) :: id_list(:) 
    integer, intent(out) :: n !! number of groups returned
    type(IXTstatus)::status
    integer :: id 
    id = IXFid_groups(groups,name)
    call parent_list_byid(groups,id,id_list,n,status)
  end subroutine
  
!! returns list of parent group ID's for a given id
!! first element of list is the group itself 
  subroutine member_list_byid(groups,id,id_list,n,status)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    type(IXTstatus)::status
    integer, intent(in) :: id !! id to check
    integer, intent(out) :: id_list(:) 
    integer, intent(out) :: n !! number of groups returned
    integer i
    n = 0
    do i=1,groups%n
!       if (groups%list(i)%id /= id) .and.
       if ( IXFis_member_groups(groups,groups%list(i)%id, id) .and. (n < size(id_list)) ) then
           n = n + 1
           id_list(n) = groups%list(i)%id
       endif
    enddo
  end subroutine

  subroutine member_list_byname(groups,name,id_list,n,status)
    implicit none
    integer idx, max_id, i, j, pid
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: name
    integer , intent(out) :: id_list(:) 
    integer, intent(out) :: n !! number of groups returned
    type(IXTstatus)::status
    integer :: id !! id to check
    id = IXFid_groups(groups,name)
    call member_list_byid(groups,id,id_list,n,status)
  end subroutine


  subroutine remove_byid(groups,id,status)
    implicit none
    type(IXTgroups) , intent(inout) :: groups
    type(IXTstatus)::status
    integer, intent(in) :: id
    integer i
    i = index_byid(groups, id)
    if (i > 0) then
        groups%list(i)%id = IXCinvalid_id_group
        groups%list(i)%parent = IXCinvalid_id_group
        groups%list(i)%name = ' '
    else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'error removing group')
    endif
  end subroutine
  
  subroutine remove_byname(groups,name,status)
    implicit none
    type(IXTgroups) , intent(inout) :: groups
    character(len=*), intent(in) :: name
    type(IXTstatus)::status
    integer id
    id = IXFid_groups(groups, name)
    call remove_byid(groups, id, status)
  end subroutine

  subroutine IXFprint_groups(groups,id_list,n,status)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    integer, intent(in) :: n, id_list(n)
    type(IXTstatus)::status
    integer i
    do i=1,n
       call IXFwrite_line(IXFname_groups(groups,id_list(i)), status)
    enddo
  end subroutine
  
!! return group index or IXCinvalid_id_group on error
  pure function index_byname(groups,name) result(idx)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    character(len=*) , intent(in) :: name
    integer idx, i
    do i=1,groups%n
       if (groups%list(i)%name == name) then
          idx = i
          return
       endif
    enddo
    idx = IXCinvalid_id_group
  end function

!! return group index or IXCinvalid_id_group on error
  pure function index_byid(groups,id) result(idx)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    integer , intent(in) :: id
    integer idx, i
    do i=1,groups%n
       if (groups%list(i)%id == id) then
          idx = i
          return
       endif
    enddo
    idx = IXCinvalid_id_group
  end function

!! returns group id 
!! +ve = found an id
!! -ve = not found, but (-val) is an empty id to use for a new group
  pure function IXFid_groups(groups,name) result(id)
    implicit none
    integer max_id, i
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: name
    integer :: id
    max_id = 0
    do i=1,groups%n
       max_id = max(max_id, groups%list(i)%id, groups%list(i)%parent)
       if (groups%list(i)%name == name) then
          id = groups%list(i)%id
          return
       endif
    enddo
    id = -(max_id + 1) 
  end function

!! returns group parent id 
!! returns IXCinvalid_id_group if invalid group
!! returns IXCno_parent_group  if no parent

  pure function parent_id_byname(groups,name) result(pid)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: name
    integer :: id, pid
    integer i
    pid = IXCinvalid_id_group
    i = index_byname(groups,name)
    if (i > 0) pid = groups%list(i)%parent
  end function

!! returns group parent id 
!! returns IXCinvalid_id_group if invalid group
!! returns IXCno_parent_group  if no parent
  pure function parent_id_byid(groups,id) result(pid)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    integer , intent(in) :: id
    integer :: pid
    integer i
    pid = IXCinvalid_id_group
    i = index_byid(groups,id)
    if (i > 0) pid = groups%list(i)%parent
  end function parent_id_byid

!! returns group parent name 
!! returns IXCinvalid_id_group if invalid group
!! returns IXCno_parent_group  if no parent

  pure function parent_byname(groups,name) result(pname)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: name
    character(len=long_len) :: pname
    integer i, pid
    pname = ' '
    i = index_byname(groups,name)
    if (i > 0) pname = IXFname_groups(groups, groups%list(i)%parent)
  end function

!! returns group parent name 
!! returns IXCinvalid_id_group if invalid group
!! returns IXCno_parent_group  if no parent
  pure function parent_byid(groups,id) result(pname)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    integer , intent(in) :: id
    character(len=long_len) :: pname
    integer :: pid
    integer i
    pname = ' '
    i = index_byid(groups,id)
    if (i > 0) pname = IXFname_groups(groups, groups%list(i)%parent)
  end function parent_byid

  pure function member_byid(groups, this_id, test_id) result(member)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    integer, intent(in) :: this_id
    integer, intent(in) :: test_id
    logical :: member
    integer :: pid
    member = .false.
    pid = this_id
    do while(pid /= IXCno_parent_group)
        if (pid == test_id) then
            member = .true.
            return
        endif
        pid = IXFparent_id_groups(groups, pid)
    enddo
  end function member_byid

  pure function member_byname(groups, this_name, test_name) result(member)
    implicit none
    type(IXTgroups) , intent(in) :: groups
    character(len=*), intent(in) :: this_name
    character(len=*), intent(in) :: test_name
    logical :: member
    integer :: this_id, test_id
    this_id = IXFid_groups(groups, this_name)
    test_id = IXFid_groups(groups, test_name)
    member = member_byid(groups, this_id, test_id)
  end function member_byname

end module IXMgroups
