!------------------------------
! MODULE: IXMinput_source
!------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1145 $ ($Date: 2007-05-30 08:57:56 -0400 (Wed, 30 May 2007) $)
!!
!! FORTRAN definition of IXMinput_source object 
! options module
module IXMinput_source
  use IXMraw_file
  use IXMdataset_1d
  use IXMdataset_2d
  implicit none	
  public :: IXTinput_source	
  integer, parameter :: IXCtype_unknown = 0
  integer, parameter :: IXCtype_isisraw = 1
  integer, parameter :: IXCtype_nexus = 2
  type IXTinput_source
    private
            type(IXTbase) :: base
	    integer :: n  = 0 !! number of sources
	    character(len=long_len) :: sources(100) !! file names
	    integer :: file_type(100) = IXCtype_unknown
  end type IXTinput_source

#if 0
  interface IXFget
     module procedure get_real, get_real1, get_real2, get_int, &
          get_int1, get_int2, get_dp, get_dp1, get_dp2, &
          get_char ! , get_d1d, get_d2d, get_d1d_array, get_d2d_array
  end interface
#endif

  interface IXFsize
     module procedure size_i, size_i_array
  end interface

#define IXD_TYPE input_source
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTinput_source class"
#define IXD_TYPE input_source
#define IXD_SQTYPE 'input_source'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_input_source(op, field, arg, status)
    implicit none
    type(IXTinput_source) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTinput_source', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'n', arg%n, status)
!    call IXFoperation_run(op, 'sources', arg%sources, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_input_source
  
  recursive subroutine IXFset_input_source(opt,status,ref)
    implicit none
    type(IXTinput_source),intent(inout)::opt
    type(IXTinput_source),optional,intent(in)::ref
    type(IXTstatus)::status
    
    if (present(ref))call IXFset_input_source(opt,status)
    
  end subroutine IXFset_input_source

  subroutine IXFget_input_source(opt,status,wout)
    implicit none
    type(IXTinput_source),intent(inout)::opt
    type(IXTinput_source),optional::wout
    type(IXTstatus)::status
    
    if (present(wout))call IXFset_input_source(wout,status,ref=opt)

  end subroutine IXFget_input_source

  subroutine IXFcheck_input_source(opt,status)
    implicit none  
    type(IXTinput_source)::opt
    type(IXTstatus)::status
  end subroutine IXFcheck_input_source
  
  subroutine IXFdestroy_input_source(opt,status)
    implicit none  
    type(IXTinput_source)::opt
    type(IXTstatus)::status
  end subroutine IXFdestroy_input_source

  subroutine IXFcreate_input_source(opt,status)
    implicit none
    type(IXTinput_source),intent(out)::opt
    type(IXTstatus)::status

    call IXFset_input_source(opt,status)
  end subroutine IXFcreate_input_source  

  subroutine size_i_array(handle, item_name, item_size, status)
    implicit none
    type(IXTinput_source) :: handle
    type(IXTstatus), target :: status
    type(IXTraw_file) :: rf
    integer :: lt, item_size(:)
    character(len=*) :: item_name
    call IXFopen_raw(handle%sources(1), rf, status)
    call IXFsize_raw(rf, item_name, item_size, status)
  end subroutine size_i_array

  subroutine size_i(handle, item_name, item_size, status)
    implicit none
    type(IXTinput_source) :: handle
    type(IXTstatus), target :: status
    integer item_size
    character(len=*) :: item_name
    type(IXTraw_file) :: rf
    call IXFopen_raw(handle%sources(1), rf, status)
    call IXFsize_raw(rf, item_name, item_size, status)
  end subroutine size_i

#if 0

  subroutine IXFadd(opt,name,status)
    implicit none
    type(IXTinput_source)::opt
    character(len=*) :: name
    type(IXTstatus)::status
    opt%n = opt%n + 1
    opt%sources(opt%n) = name
    opt%file_type(opt%n) = get_type(name)
  end subroutine

  function get_type(file_name) result(the_type)
    implicit none
    integer the_type
    character(len=*) :: file_name
    the_type = IXCtype_isisraw
  end function

#define IXD_NAME		real
#define IXD_TYPE		real*4
#include "input_source_routines.f90"

#define IXD_NAME		real1
#define IXD_TYPE		real*4
#define IXD_DIMS		:
#include "input_source_routines.f90"

#define IXD_NAME		real2
#define IXD_TYPE		real*4
#define IXD_DIMS		:,:
#include "input_source_routines.f90"

#define IXD_NAME		int
#define IXD_TYPE		integer(i4b)
#include "input_source_routines.f90"

#define IXD_NAME		int1
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:
#include "input_source_routines.f90"

#define IXD_NAME		int2
#define IXD_TYPE		integer(i4b)
#define IXD_DIMS		:,:
#include "input_source_routines.f90"

#define IXD_NAME		dp
#define IXD_TYPE		real(dp)
#include "input_source_routines.f90"

#define IXD_NAME		dp1
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:
#include "input_source_routines.f90"

#define IXD_NAME		dp2
#define IXD_TYPE		real(dp)
#define IXD_DIMS		:,:
#include "input_source_routines.f90"

#define IXD_NAME		char
#define IXD_TYPE		character(len=*)
#include "input_source_routines.f90"

#if 0
#define IXD_NAME		d1d
#define IXD_TYPE		type(IXTdataset_1d)
#include "input_source_routines.f90"

#define IXD_NAME		d1d_array
#define IXD_TYPE		type(IXTdataset_1d)
#define IXD_DIMS		:
#include "input_source_routines.f90"

#define IXD_NAME		d2d
#define IXD_TYPE		type(IXTdataset_2d)
#include "input_source_routines.f90"

#define IXD_NAME		d2d_array
#define IXD_TYPE		type(IXTdataset_2d)
#define IXD_DIMS		:
#include "input_source_routines.f90"
#endif

#endif

end module IXMinput_source

