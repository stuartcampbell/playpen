!------------------------------
! MODULE: IXMaxis
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1299 $ ($Date: 2008-01-16 09:41:22 -0500 (Wed, 16 Jan 2008) $)
!!
!! FORTRAN definition of IXMaxis object 
! options module
module IXMaxis
  use IXMbase
  use IXMneutron_units
  implicit none	
  public :: IXTaxis	
  type IXTaxis
     private
     type(IXTbase) :: base
     character(long_len),allocatable :: caption(:)
     character(long_len) :: units   !! physical units e.g. eV
     character(len=5) :: code='    '   !! what this represents e.g. a d spacing or energy transfer
  end type IXTaxis

#define IXD_TYPE axis
#include "class_header.f90"
  public::IXFmake_label_axis
  private:: make_label_1d_axis,make_label_2d_axis
  interface IXFmake_label_axis
    module procedure make_label_1d_axis,make_label_2d_axis
  end interface

contains

#define IXD_DESCRIPTION	"IXTaxis class"
#define IXD_TYPE axis
#define IXD_SQTYPE 'axis'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_axis(op, field, arg, status)
    implicit none
    type(IXTaxis) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTaxis', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_alloc(op,'caption', arg%caption, status)
    call IXFoperation_run(op,'units', arg%units, status)
    call IXFoperation_run(op,'code', arg%code, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_axis

  pure function IXFcompare_units(a1,a2)result(equal)
    implicit none
    type(IXTaxis),intent(in)::a1,a2
    logical::equal

    ! this will change if/when we make them integers
    if((trim(a1%code) == trim(a2%code)) .and. (trim(a1%units) == trim(a2%units)))then
       equal=.true.
    else 
       equal = .false.
    endif
  end function IXFcompare_units

  subroutine IXFcreate_axis(arg,caption,units,code,status)
    implicit none
    type(IXTaxis),intent(out)::arg
    type(IXTstatus)::status
    character(len=*),allocatable,intent(in)::caption(:)
    character(len=*),intent(in) :: code  
    character(len=*),intent(in) :: units 

    call IXFmark_valid(arg)
    call IXFset_axis(arg,status,caption,units,code)
  end subroutine IXFcreate_axis


!! this will create an IXTaxis object given a standard units code, 
!! and fill the caption and units with standard values, and fail otherwise
  subroutine IXFcreate_code_axis(arg,code,status)
    implicit none
    type(IXTaxis),intent(out)::arg
    type(IXTstatus)::status
    character(len=*),intent(in) :: code  
    character(len=long_len) :: units
    character(long_len),allocatable::caption(:)
    logical::found
    integer(i4b)::i
    found=.false.
    
    call IXFmark_valid(arg)    
    i=1
    do while ((found .eqv. .false.) .and.  (i<=list_len) )
        if ( trim(code) == trim(code_list(i))   )then
          units=units_list(i)
          allocate(caption(1))
          caption(1)=cap_list(i)
          found=.true.
        endif
        i=i+1
    end do
! now check for special units codes
! spectrum number, workspace number and Counts    
    if (.not. found) then
       if ( trim(code) == trim(IXCspecnoC))then          
         allocate(caption(1))
         caption(1)='Spectrum Number'
         units=IXCnullunits
         found=.true.
       endif
     endif
    
    if (.not. found) then
       if ( trim(code) == trim(IXCworknoC))then          
         allocate(caption(1))
         caption(1)='Workspace Number'
         units=IXCnullunits
         found=.true.
       endif
     endif
     if (.not. found) then
       if ( trim(code) == trim(IXCcountsC))then          
         allocate(caption(1))
         caption(1)='Counts'
         units=IXCnullunits
         found=.true.
       endif
     endif
     if (.not. found) then
       if ( trim(code) == trim(IXCnullcode))then          
         allocate(caption(1))
         caption(1)=' '
         units=IXCnullunits
         found=.true.
       endif
     endif
     
             
    if(found .eqv. .true.) then
       call IXFset_axis(arg,status,caption,units,code)
    else
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
           IXCerr_invparam,'Invalid units code supplied(IXFcreate_code_axis)')
      return
    endif
    deallocate (caption)
  end subroutine IXFcreate_code_axis
  
      
  !! this will create an IXTaxis object given a standard units code, and fail otherwise
  ! assumes 'code' is a standard unit, units is user input, caption is standard
  subroutine IXFcreate_code_units_axis(arg,code,units,status)
    implicit none
    type(IXTaxis),intent(out)::arg
    type(IXTstatus)::status
    character(len=*),intent(in) :: code  
    character(len=*),intent(in) :: units
    character(long_len),allocatable::caption(:) 
    logical::found
    integer(i4b)::i
    found=.false.
    
    call IXFmark_valid(arg)    
    i=1
    do while ((found .eqv. .false.) .and.  (i<=list_len) )
        if ( trim(code) == trim(code_list(i))   )then          
          found=.true.
          allocate(caption(1))
          caption(1)=cap_list(i)
        endif
        i=i+1
    end do     
    
     if(found .eqv. .true.) then
    ! allow a different units string if code is valid
       call IXFset_axis(arg,status,caption,units,code)
     else    
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
           IXCerr_invparam,'Invalid units code supplied, must have $ prefix(IXFcreate_code_units_axis)')
       call IXFclear_valid(arg)
       return
     endif         
     deallocate (caption)
  end subroutine IXFcreate_code_units_axis


  subroutine IXFcreate_caption_axis(arg,caption,status)
    implicit none
    type(IXTaxis),intent(out)::arg
    type(IXTstatus)::status
    character(len=*),allocatable,intent(in)::caption(:)    
    call IXFmark_valid(arg)
    call IXFset_axis(arg,status,caption,IXCnullunits,IXCnullcode)
  end subroutine IXFcreate_caption_axis

  subroutine IXFcreate_caption_units_axis(arg,caption,units,status)
    implicit none
    type(IXTaxis),intent(out)::arg
    type(IXTstatus)::status
    character(len=*),allocatable,intent(in)::caption(:)
    character(len=*),intent(in)::units    
    call IXFmark_valid(arg)
    call IXFset_axis(arg,status,caption,units,IXCnullcode)
  end subroutine IXFcreate_caption_units_axis


!! this will create an x_label and an s_label from IXTaxis (x & y) and x_distribution flag
!! will work for all axis types
  subroutine make_label_1d_axis(x_axis,s_axis,x_dist,x_label,s_label,status)
    implicit none
    type(IXTaxis),intent(in)::x_axis,s_axis
    type(IXTstatus)::status    
    character(len=long_len),allocatable :: x_label(:),s_label(:)
    character(len=long_len)::s_lab
    logical,intent(in)::x_dist
    integer(i4b)::x_len,s_len
    s_lab=' '
    x_len=size(x_axis%caption)
    s_len=size(s_axis%caption)
    
    call IXFreallocfortran(x_label,x_len,.false.,status)
    call IXFreallocfortran(s_label,s_len,.false.,status)
    
    x_label=x_axis%caption
    s_label=s_axis%caption
    if(len_trim(x_axis%units) .gt. 0)x_label(x_len)=trim(x_label(x_len))//' ('//trim(x_axis%units)//')'
        
    s_lab=trim(s_axis%units)
    if(x_dist)s_lab=trim(s_lab)//' / '//trim(x_axis%units)       
    if(len_trim(s_axis%units) .gt. 0) s_lab='('//trim(s_lab)//')'    
    s_label(s_len)=trim(s_label(s_len))//' '//trim(s_lab)
    
  end subroutine make_label_1d_axis

  subroutine make_label_2d_axis(x_axis,y_axis,s_axis,x_dist,y_dist,x_label,y_label,s_label,status)
    implicit none
    type(IXTaxis),intent(in)::x_axis,s_axis,y_axis
    type(IXTstatus)::status    
    character(len=long_len),allocatable :: x_label(:),s_label(:),y_label(:)
    character(len=long_len)::s_lab
    logical,intent(in)::x_dist,y_dist
    integer(i4b)::x_len,s_len,y_len
    
    s_lab=' '
    x_len=size(x_axis%caption)
    s_len=size(s_axis%caption)
    y_len=size(y_axis%caption)
    
    call IXFreallocfortran(x_label,x_len,.false.,status)
    call IXFreallocfortran(y_label,y_len,.false.,status)
    call IXFreallocfortran(s_label,s_len,.false.,status)
    
    x_label=x_axis%caption
    s_label=s_axis%caption
    y_label=y_axis%caption
    if(len_trim(x_axis%units) .gt. 0)x_label(x_len)=trim(x_label(x_len))//' ('//trim(x_axis%units)//')'
    if(len_trim(y_axis%units) .gt. 0)y_label(y_len)=trim(y_label(x_len))//' ('//trim(y_axis%units)//')'
        
    s_lab=s_axis%units
    if(x_dist)s_lab=trim(s_lab)//' / '//trim(x_axis%units)       
    if(y_dist)s_lab=trim(s_lab)//' / '//trim(y_axis%units)
    
    if(len_trim(s_axis%units) .gt. 0) s_lab='('//trim(s_lab)//')'    
    s_label(s_len)=trim(s_label(s_len))//' '//trim(s_lab)
  end subroutine make_label_2d_axis
  
  
  recursive subroutine IXFset_axis(arg,status,caption,units,code,ref)
    implicit none
    type(IXTaxis),intent(inout)::arg
    type(IXTaxis),optional,intent(in)::ref
    character(len=*),optional,allocatable,intent(in)::caption(:)
    character(len=*),optional,intent(in) :: code  
    character(len=*),optional,intent(in) :: units
    type(IXTstatus)::status
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_axis)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(arg)
    else    
       if(IXFvalid(arg) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_axis)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_axis(arg,status,ref%caption,ref%units,ref%code)

    if (present(caption))then   
       call IXFreallocFortran(arg%caption,size(caption),.false.,status)
       arg%caption=caption 
    endif
    if (present(units))arg%units=units
    if (present(code))arg%code=code
    call IXFcheck(arg,status)

  end subroutine IXFset_axis

  subroutine IXFget_axis(arg,status,caption,units,code,wout)
    implicit none
    type(IXTaxis),intent(in)::arg
    type(IXTaxis),optional,intent(out)::wout
    character(len=*),optional,allocatable::caption(:)
    character(len=*),optional,intent(out) :: code  
    character(len=*),optional,intent(out) :: units 
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(arg,wout,status)
    if (present(caption))then
      call IXFreallocdimsfortran(caption,(/ size(arg%caption) /),.false.,status)
      caption=arg%caption
    endif    
    if (present(code))code=arg%code
    if (present(units))units=arg%units

  end subroutine IXFget_axis

  subroutine IXFcheck_axis(axis,status)
    implicit none  
    type(IXTaxis)::axis
    type(IXTstatus)::status
  end subroutine IXFcheck_axis

  subroutine IXFdestroy_axis(axis,status)
    implicit none  
    type(IXTaxis)::axis
    type(IXTstatus)::status

    call IXFdestroy(axis%base,status)
    call IXFdeallocfortran(axis%caption,status)
    call IXFclear_valid(axis)
  end subroutine IXFdestroy_axis
  
  pure logical function IXFisunitcode(axis,code)result(isunit)
    implicit none
    type(IXTaxis),intent(in)::axis
    character(len=4),intent(in)::code
    isunit=.false.
    if(axis%code == code)isunit=.true.    
  end function IXFisunitcode
end module IXMaxis
