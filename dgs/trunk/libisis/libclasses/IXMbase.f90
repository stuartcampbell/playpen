!------------------------------
! MODULE: IXMbase
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!
!! FORTRAN definition of IXMbase class
module IXMbase
  use IXMoperation
  use IXMregrid
  integer(i4b), parameter, private :: IXCobject_initialised = 15738456
  type IXTbase
     private
     character(len=name_len) :: entry_name = 'unknown'		!! Name of entry in NeXus file
     !     integer(i4b)::ref_count                                !! reference counter so that can be generally pointed to and in/decremented
     integer(i4b) :: initialised = IXCobject_initialised
     logical :: valid = .false.
  end type IXTbase

  character(len=short_len),parameter::IXCdetmask='detmaskfile',IXCmonmask='monmaskfile',IXCmaskfile='genericmaskfile'
  character(len=short_len),parameter::IXCdetmap='detmapfile',IXCmonmap='monmapfile',IXCmapfile='genericmapfile'
  character(len=short_len),parameter::IXCrawfile='rawfile',IXCrawfile_mult='rawfile_mult',IXCfermi_chopper='chopper',IXCsource='source'
  character(len=short_len),parameter::IXCchop_inst='chop_inst',IXCdiff_inst='diff_inst',IXCmoderator='moderator'
  character(len=short_len),parameter::IXCprogname='progname',IXCcommand_line='command',IXCdet_ref='full_reference_detector'
!not used as yet
  character(len=short_len),parameter::IXCindi_inst='indi_inst'
  integer(i4b)::IXCdiffraction=0, IXCdirect=1, IXCindirect=2

#define IXD_TYPE base
#define IXD_SQTYPE 'base'
#define IXD_NO_BASE	1
#include "class_header.f90"

  private::getr_1d,getr_2d,geti_1d,geti_2d,geti_3d,getr_3d
  private::setr_1d,seti_1d ,setr_2d,seti_2d,seti_3d,setr_3d
  private:: operationStart_base
  
  interface IXFoperationStart
    module procedure operationStart_base
  end interface

  interface IXFget_real_array
    module procedure getr_1d,getr_2d,getr_3d
  end interface

  interface IXFget_integer_array
    module procedure geti_1d,geti_2d,geti_3d
  end interface
  
  interface IXFset_real_array
    module procedure setr_1d ,setr_2d,setr_3d
  end interface

  interface IXFset_integer_array
    module procedure seti_1d  ,seti_2d, seti_3d
  end interface
contains

#define IXD_TYPE base
#define IXD_SQTYPE 'base'
#define IXD_NO_BASE	1
#include "class_base.f90"

  subroutine operationStart_base(op, name, field, status,base,cont_op)
    implicit none
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    character(len=*) :: name
    type (IXTbase),intent(in)::base
    logical,intent(inout)::cont_op
    cont_op=.true.
    
    if (associated(op%matlabwrite)) then
      if (base%valid .EQV. .false.)then
        cont_op=.false.
        return
      endif
    endif
    
    call IXFoperationStart(op, name, field, status)
    
  end subroutine operationStart_base


  subroutine IXFcheck_base(arg, status)
    implicit none
    type(IXTbase) :: arg
    type(IXTstatus) :: status
!    if (IXFinitialised_base(arg) .eqv. .false.) then
!    endif
  end subroutine IXFcheck_base

  pure function IXFinitialised_base(arg) result(init)
    implicit none
    type(IXTbase), intent(in) :: arg
    logical :: init
    if (arg%initialised == IXCobject_initialised) then
        init = .true.
    else
        init = .false.
    endif
  end function IXFinitialised_base

!  subroutine IXFmark_init_base(arg)
!    implicit none
!    type(IXTbase) :: arg
!    arg%initialised=.true.
!  end subroutine IXFmark_init_base

!  subroutine IXFclear_init_base(arg)
!    implicit none
!    type(IXTbase) :: arg
!    arg%initialised=.false.
!  end subroutine IXFclear_init_base

  pure function IXFvalid_base(arg) result(valid)
    implicit none
    type(IXTbase), intent(in) :: arg
    logical :: valid
    valid = arg%valid
  end function IXFvalid_base

  subroutine IXFmark_valid_base(arg)
    implicit none
    type(IXTbase) :: arg
    arg%valid=.true.
  end subroutine IXFmark_valid_base

  subroutine IXFclear_valid_base(arg)
    implicit none
    type(IXTbase) :: arg
    arg%valid=.false.
  end subroutine IXFclear_valid_base

  subroutine IXFdestroy_base(arg, status)
    implicit none
    type(IXTbase) :: arg
    type(IXTstatus) :: status
    arg%valid=.false.
  end subroutine IXFdestroy_base

  !!generic create routine which does nothing but helps compilation
  subroutine IXFcreate_base(arg, status)
    implicit none
    type(IXTbase) :: arg
    type(IXTstatus) :: status
  end subroutine IXFcreate_base

  recursive subroutine IXFoperation_run_base(op, field, arg, status)
    implicit none
    type(IXTbase) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTbase', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing argemnts passed in class creation with vargin
    call IXFoperation_run(op, 'entry_name', arg%entry_name, status)
    call IXFoperation_run(op, 'initialised', arg%initialised, status)
    call IXFoperation_run(op, 'valid', arg%valid, status)
    if (associated(op%init)) then
        arg%initialised = IXCobject_initialised
        arg%valid = .false.
        arg%entry_name = 'unknown'
    endif 
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_base

  recursive subroutine IXFset_base(var, status, entry_name, initialised,valid,ref)
    implicit none
    type(IXTbase) :: var
    type(IXTstatus) :: status
    character(len=*), optional,intent(in) :: entry_name
    integer(i4b), optional,intent(in) :: initialised 
    logical,optional,intent(in) :: valid   
    type(IXTbase),intent(in), optional :: ref

    if (present(ref)) call IXFset_base(var,status,ref%entry_name,ref%initialised,ref%valid)
    if (present(entry_name)) var%entry_name = entry_name
    if (present(initialised))var%initialised=initialised
    if (present(valid))var%valid=valid
    call IXFcheck(var, status)
  end subroutine IXFset_base


  subroutine IXFget_base(var, status,entry_name, initialised,valid,wout)
    implicit none
    type(IXTbase) :: var
    type(IXTstatus) :: status
    character(len=*), optional,intent(out) :: entry_name
    type(IXTbase), optional,intent(out) :: wout
    integer(i4b), optional,intent(out) :: initialised 
    logical,optional,intent(out) :: valid
    
    if (present(wout))call IXFset_base(wout,status,ref=var)
    if (present(entry_name)) entry_name = var%entry_name
    if (present(initialised))initialised=var%initialised
    if (present(valid))valid=var%valid    
    
  end subroutine IXFget_base

! some base operations which are not class specific, but need to be seen by all classes
    
  subroutine geti_1d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    integer(i4b),intent(in)::arr_in(:)
    integer(i4b),optional,intent(out)::arr_out(:)
    if(present(arr_out))then 
      if(size(arr_in) == size(arr_out))then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct length to receivearray (IXFget_integer)')
        arr_out=IXCundef_i4b
      endif
    endif
  end subroutine geti_1d

  subroutine getr_1d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    real(dp),intent(in)::arr_in(:)
    real(dp),optional,intent(out)::arr_out(:)
    if(present(arr_out))then
      if(size(arr_in) == size(arr_out))then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'supplied array must be correct length to receivearray (IXFget_real)')
        arr_out=IXCundef_dp
      endif
    endif
  end subroutine getr_1d

  subroutine geti_2d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    integer(i4b),intent(in)::arr_in(:,:)
    integer(i4b),optional,intent(out)::arr_out(:,:)
    if(present(arr_out))then
      if(sum(abs(shape(arr_in) - shape(arr_out))) == 0)then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                 IXCerr_outofmem, 'supplied array must be correct shape to receivearray (IXFget_integer)')
        arr_out=IXCundef_i4b
      endif
    endif
  end subroutine geti_2d

  subroutine getr_2d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    real(dp),intent(in)::arr_in(:,:)
    real(dp),optional,intent(out)::arr_out(:,:)  
    if(present(arr_out))then      
      if(sum(abs(shape(arr_in) - shape(arr_out))) == 0)then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                 IXCerr_outofmem, 'supplied array must be correct shape to receivearray (IXFget_real)')
        arr_out=IXCundef_dp
      endif
    endif
  end subroutine getr_2d
  subroutine geti_3d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    integer(i4b),intent(in)::arr_in(:,:,:)
    integer(i4b),optional,intent(out)::arr_out(:,:,:)  
    if(present(arr_out))then      
      if(sum(abs(shape(arr_in) - shape(arr_out))) == 0)then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                 IXCerr_outofmem, 'supplied array must be correct shape to receivearray (IXFget_real)')
        arr_out=IXCundef_i4b
      endif
    endif
  end subroutine geti_3d  
  subroutine getr_3d(arr_in,status,arr_out)
    implicit none
    type(IXTstatus)::status
    real(dp),intent(in)::arr_in(:,:,:)
    real(dp),optional,intent(out)::arr_out(:,:,:)  
    if(present(arr_out))then      
      if(sum(abs(shape(arr_in) - shape(arr_out))) == 0)then
        arr_out=arr_in
      else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                 IXCerr_outofmem, 'supplied array must be correct shape to receivearray (IXFget_real)')
        arr_out=IXCundef_dp
      endif
    endif
  end subroutine getr_3d
  
  subroutine seti_1d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    integer(i4b),pointer::arr_out(:)
    integer(i4b),optional::arr_in(:)
    if(present(arr_in))then 
      call IXFrealloc(arr_out,size(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine seti_1d

  subroutine setr_1d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    real(dp),pointer::arr_out(:)
    real(dp),optional::arr_in(:)
    if(present(arr_in))then 
      call IXFrealloc(arr_out,size(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine setr_1d
  
  subroutine seti_2d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    integer(i4b),pointer::arr_out(:,:)
    integer(i4b),optional::arr_in(:,:)
    if(present(arr_in))then 
      call IXFreallocdims(arr_out,shape(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine seti_2d

  subroutine setr_2d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    real(dp),pointer::arr_out(:,:)
    real(dp),optional::arr_in(:,:)
    if(present(arr_in))then 
      call IXFreallocdims(arr_out,shape(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine setr_2d
  subroutine seti_3d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    integer(i4b),pointer::arr_out(:,:,:)
    integer(i4b),optional::arr_in(:,:,:)
    if(present(arr_in))then 
      call IXFreallocdims(arr_out,shape(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine seti_3d  
  subroutine setr_3d(arr_out,status,arr_in)
    implicit none
    type(IXTstatus)::status
    real(dp),pointer::arr_out(:,:,:)
    real(dp),optional::arr_in(:,:,:)
    if(present(arr_in))then 
      call IXFreallocdims(arr_out,shape(arr_in),.false.,status)
      arr_out=arr_in
    endif
  end subroutine setr_3d
end module IXMbase

!subroutine IXFWriteMsg(line,status)
!use IXMstatus
!implicit none
!character(len=*) :: line
!type(IXFstatus) status
!call mexPrintf(line)
!call IXFstatus_clear(status)
!end subroutine

!  integer, parameter :: IXCSourceMatlabCode = 1, IXCSourceASCIICode = 2, IXCSourceNeXusCode = 3
! type IXIO
!    integer :: code			 !! e.g. IXSourceMatlabCode
!	  integer :: direction		!! 0 = input, 1 = output
!!	  integer(cpointer_t), pointer :: cp2 !! Matlab prhs
!	  integer :: i				!! fileid, unit number 
!	  type(NXhandle) :: nxhandle
! end type IXIO