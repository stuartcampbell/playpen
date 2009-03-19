!------------------------------
! MODULE: IXMoptions
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1311 $ ($Date: 2008-03-07 04:22:27 -0500 (Fri, 07 Mar 2008) $)
!!
!! FORTRAN definition of IXMoptions object 
! options module
module IXMoptions
  use IXMbase
  use IXMaxis
  implicit none	
  public :: IXToptions	
  type IXToptions
    private
        type(IXTbase) :: base
	    logical :: bgrd	    
	    logical :: m_axis
	    logical :: m_rebin
	    logical :: d_int	    
	    logical :: d_axis
	    logical :: d_rebin
	    logical :: ei
  end type IXToptions

#define IXD_TYPE options
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXToptions class"
#define IXD_TYPE options
#define IXD_SQTYPE 'options'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_options(op, field, arg, status)
    implicit none
    type(IXToptions) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXToptions', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'bgrd', arg%bgrd, status)
    call IXFoperation_run(op,'m_axis', arg%m_axis, status)
    call IXFoperation_run(op,'m_rebin', arg%m_rebin, status)
    call IXFoperation_run(op,'d_int', arg%d_int, status)
    call IXFoperation_run(op,'d_axis', arg%d_axis, status)
    call IXFoperation_run(op,'d_rebin', arg%d_rebin, status)
    call IXFoperation_run(op,'ei', arg%ei, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_options
  
  recursive subroutine IXFset_options(opt,status,bgrd,m_axis,m_rebin,d_int,d_axis,d_rebin,ei,ref)
    implicit none
    type(IXToptions),intent(inout)::opt
    type(IXToptions),optional,intent(in)::ref
    logical,optional,intent(in)::bgrd,m_axis,m_rebin,d_axis,d_rebin,ei,d_int
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_options)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(opt)
    else    
       if(IXFvalid(opt) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_options)')
       endif
       if(status == IXCseverity_error)return
    endif
        
    if (present(ref))call IXFset_options(opt,status,ref%bgrd,ref%m_axis,ref%m_rebin,ref%d_axis,ref%d_rebin)
    
    if (present(bgrd))opt%bgrd=bgrd
    if (present(m_axis))opt%m_axis=m_axis
    if (present(m_rebin))opt%m_rebin=m_rebin
    if (present(d_int))opt%d_int=d_int
    if (present(d_axis))opt%d_axis=d_axis
    if (present(d_rebin))opt%d_rebin=d_rebin
    if (present(ei))opt%ei=ei

    call IXFcheck_options(opt,status)

  end subroutine IXFset_options

  subroutine IXFget_options(opt,status,bgrd,m_axis,m_rebin,d_int,d_axis,d_rebin,ei,wout)
    implicit none
    type(IXToptions),intent(inout)::opt
    type(IXToptions),intent(out),optional::wout
    logical,intent(out),optional::bgrd,m_axis,m_rebin,d_axis,d_rebin,ei,d_int
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(opt,wout,status)
    if (present(bgrd))bgrd=opt%bgrd
    if (present(m_axis))m_axis=opt%m_axis
    if (present(m_rebin))m_rebin=opt%m_rebin
    if (present(d_int))d_int=opt%d_int    
    if (present(d_axis))d_axis=opt%d_axis
    if (present(d_rebin))d_rebin=opt%d_rebin
    if (present(ei))ei=opt%ei

  end subroutine IXFget_options

  subroutine IXFcheck_options(opt,status)
    implicit none  
    type(IXToptions)::opt
    type(IXTstatus)::status
  end subroutine IXFcheck_options
  
  subroutine IXFdestroy_options(opt,status)
    implicit none  
    type(IXToptions)::opt
    type(IXTstatus)::status
    
    call IXFclear_valid(opt)
  end subroutine IXFdestroy_options

  subroutine IXFcreate_options(opt,bgrd,m_axis,m_rebin,d_int,d_axis,d_rebin,ei,status)
    implicit none
    type(IXToptions),intent(out)::opt
    type(IXTstatus)::status
    logical,intent(in)::bgrd,m_rebin,m_axis,d_int,d_rebin,d_axis,ei

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(opt)
    call IXFset_options(opt,status,bgrd,m_axis,m_rebin,d_int,d_axis,d_rebin,ei)
   
  end subroutine IXFcreate_options  
  
  logical function IXFpresent(opt,bgrd,m_axis,m_rebin,d_int,d_axis,d_rebin,ei)result(res)
    use IXMaxis
    implicit none
    type(IXToptions),optional,intent(in)::opt
    type (IXTaxis),optional,intent(in)::m_axis,d_axis
    real(dp),optional,intent(in)::m_rebin(:),d_rebin(:),bgrd(2),ei(2),d_int(2)
    type(IXTstatus)::status
    integer(i4b)::i
   
    res=.false.
    i=0
    if(present(bgrd))i=i+1
    if(present(m_axis))i=i+1
    if(present(m_rebin))i=i+1
    if(present(d_int))i=i+1
    if(present(d_axis))i=i+1
    if(present(d_rebin))i=i+1
    if(present(ei))i=i+1
    
    if(i >1)then
      call IXFwrite_line('IXFpresent called with too many arguments',status)
      return
    endif
        
    if(present(opt))then  
      if(present(bgrd)) res = opt%bgrd  
      if(present(m_axis))res =  opt%m_axis
      if(present(m_rebin))res =  opt%m_rebin
      if(present(d_int))res =  opt%d_int      
      if(present(d_axis))res =  opt%d_axis
      if(present(d_rebin))res =  opt%d_rebin
      if(present(ei))res =  opt%ei
    else
      if(present(bgrd)) res = .true. 
      if(present(m_axis)) res=.true. 
      if(present(m_rebin)) res=.true.
      if(present(d_int)) res=.true.
      if(present(d_axis)) res=.true.
      if(present(d_rebin)) res=.true.
      if(present(ei)) res=.true.
    endif
        
  end function
  
end module IXMoptions
