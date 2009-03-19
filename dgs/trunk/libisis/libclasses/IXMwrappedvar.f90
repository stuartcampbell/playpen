!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMwrappedvar
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 638 $ ($Date: 2006-04-02 18:18:55 -0400 (Sun, 02 Apr 2006) $)
!!
!! This module implements the IXFoperationRun interface and its sub interfaces
!! such as IXFoperationRunPrint to prints out arrays etc.
!! Note that to matlab x(5) is usually the row vector x(1,5) as all matlab arrays are at least two
!! dimensional - we print it out as x[5]
!!
module IXMwrapped_var
  use IXMmemory
  implicit none
  ! default all hidden
  ! private
  ! public types
  public :: IXTwrapped_var, IXTwrapped_object
  ! public interfaces
  public :: IXFwrap_var, IXFunwrap_var, IXFunwrap_varAlloc, IXFunwrap_varPtr, IXFwrap_type, IXFwrap
  ! public variables   
  integer, parameter :: IXCvartype_unknown = 0, IXCvartype_i = 1, IXCvartype_i1 = 2, IXCvartype_i2 = 3, IXCvartype_i3 = 4, &
       IXCvartype_i4 = 5, IXCvartype_dp = 6, IXCvartype_dp1 = 7, IXCvartype_dp2 = 8, IXCvartype_dp3 = 9, &
       IXCvartype_dp4 = 10, IXCvartype_char = 11, IXCvartype_logical = 12, IXCvartype_object = 13, IXCvartype_char1 = 14
  type base_object
     real(dp) :: d
  end type base_object
  type IXTwrapped_object
     private
     type(base_object), pointer :: ptr =>NULL()
  end type IXTwrapped_object
  type IXTwrapped_var
     private
     integer :: vartype = IXCvartype_unknown	!! one of IXCvartype_*
     ! derived types
     type(IXTwrapped_object) :: object ! IXCvartype_object
     ! basic types
     real(dp) :: dp
     real(dp), pointer :: dp1(:) => NULL()
     real(dp), pointer :: dp2(:,:) => NULL()
     real(dp), pointer :: dp3(:,:,:) => NULL()
     real(dp), pointer :: dp4(:,:,:,:) => NULL()
     integer(i4b) :: i
     integer(i4b), pointer :: i1(:) => NULL()
     integer(i4b), pointer :: i2(:,:) => NULL()
     integer(i4b), pointer :: i3(:,:,:) => NULL()
     integer(i4b), pointer :: i4(:,:,:,:) => NULL()
     character(len=long_len) :: char = ' '
     character(len=long_len), pointer :: char1(:) => NULL()
     logical :: logval
  end type IXTwrapped_var

  interface IXFwrap_var
     module procedure wrap_i, wrap_i1, wrap_i2, wrap_i3, wrap_i4, &
          wrap_dp, wrap_dp1, wrap_dp2, wrap_dp3, wrap_dp4, &
          wrap_char, wrap_char1, wrap_logval, wrap_object
  end interface

  interface IXFwrap
     module procedure f_wrap_i, f_wrap_i1, f_wrap_i2, f_wrap_i3, f_wrap_i4, &
          f_wrap_dp, f_wrap_dp1, f_wrap_dp2, f_wrap_dp3, f_wrap_dp4, &
          f_wrap_char, f_wrap_char1, f_wrap_logval, f_wrap_object
  end interface

  interface IXFunwrap_var
     module procedure unwrap_i, unwrap_i1, unwrap_i2, unwrap_i3, unwrap_i4, &
          unwrap_dp, unwrap_dp1, unwrap_dp2, unwrap_dp3, unwrap_dp4, &
          unwrap_char, unwrap_char1, unwrap_logval, unwrap_object
  end interface

  interface IXFunwrap_varAlloc
     module procedure unwrap_alloc_i1, unwrap_alloc_i2, unwrap_alloc_i3, unwrap_alloc_i4, &
          unwrap_alloc_dp1, unwrap_alloc_dp2, unwrap_alloc_dp3, unwrap_alloc_dp4, unwrap_alloc_char1
  end interface

  interface IXFunwrap_varPtr
     module procedure unwrap_ptr_i1, unwrap_ptr_i2, unwrap_ptr_i3, unwrap_ptr_i4, &
          unwrap_ptr_dp1, unwrap_ptr_dp2, unwrap_ptr_dp3, unwrap_ptr_dp4, unwrap_ptr_char1
  end interface

contains
  function IXFwrap_type(wrapped_var) result(wrap_type)
    implicit none
    integer(i4b) :: wrap_type
    type(IXTwrapped_var), intent(in) :: wrapped_var
    wrap_type = wrapped_var%vartype
  end function IXFwrap_type

  subroutine wrap_i(var, wrapped_var, status)
    implicit none
    integer(i4b), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    wrapped_var%vartype = IXCvartype_i
    wrapped_var%i = var
  end subroutine wrap_i

  function f_wrap_i(var) result(wrapped_var)
    implicit none
    integer(i4b), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    wrapped_var%vartype = IXCvartype_i
    wrapped_var%i = var
  end function f_wrap_i

  subroutine wrap_dp(var, wrapped_var, status)
    implicit none
    real(dp), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    wrapped_var%vartype = IXCvartype_dp
    wrapped_var%dp = var
  end subroutine wrap_dp

  function f_wrap_dp(var) result(wrapped_var)
    implicit none
    real(dp), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    wrapped_var%vartype = IXCvartype_dp
    wrapped_var%dp = var
  end function f_wrap_dp

  subroutine wrap_char(var, wrapped_var, status)
    implicit none
    character(len=*), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    wrapped_var%vartype = IXCvartype_char
    wrapped_var%char = var
  end subroutine wrap_char

  function f_wrap_char(var) result(wrapped_var)
    implicit none
    character(len=*), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    wrapped_var%vartype = IXCvartype_char
    wrapped_var%char = var
  end function f_wrap_char

  subroutine wrap_logval(var, wrapped_var, status)
    implicit none
    logical, intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    wrapped_var%vartype = IXCvartype_logical
    wrapped_var%logval = var
  end subroutine wrap_logval

  function f_wrap_logval(var) result(wrapped_var)
    implicit none
    logical, intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    wrapped_var%vartype = IXCvartype_logical
    wrapped_var%logval = var
  end function f_wrap_logval

  subroutine wrap_object(var, wrapped_var, status)
    implicit none
    type(IXTwrapped_object), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    wrapped_var%vartype = IXCvartype_object
    wrapped_var%object = var
  end subroutine wrap_object

  function f_wrap_object(var) result(wrapped_var)
    implicit none
    type(IXTwrapped_object), intent(in) :: var
    type(IXTwrapped_var) :: wrapped_var
    wrapped_var%vartype = IXCvartype_object
    wrapped_var%object = var
  end function f_wrap_object

  subroutine unwrap_i(wrapped_var, var, status)
    implicit none
    integer(i4b) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    if (wrapped_var%vartype == IXCvartype_i) then
       var = wrapped_var%i
    else
       call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
            IXCerr_invparam, 'unwrap integer from non-integer')
    endif
  end subroutine unwrap_i

  subroutine unwrap_dp(wrapped_var, var, status)
    implicit none
    real(dp) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    if (wrapped_var%vartype == IXCvartype_dp) then
       var = wrapped_var%dp
    else
       call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
            IXCerr_invparam, 'unwrap real from non-real')
    endif
  end subroutine unwrap_dp

  subroutine unwrap_char(wrapped_var, var, status)
    implicit none
    character(len=*) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    if (wrapped_var%vartype == IXCvartype_char) then
       var = wrapped_var%char
    else
       call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
            IXCerr_invparam, 'unwrap char from non-char')
    endif
  end subroutine unwrap_char

  subroutine unwrap_logval(wrapped_var, var, status)
    implicit none
    logical :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    if (wrapped_var%vartype == IXCvartype_logical) then
       var = wrapped_var%logval
    else
       call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
            IXCerr_invparam, 'unwrap logical from non-logical')
    endif
  end subroutine unwrap_logval

  subroutine unwrap_object(wrapped_var, var, status)
    implicit none
    type(IXTwrapped_object) :: var
    type(IXTwrapped_var) :: wrapped_var
    type(IXTstatus) :: status
    if (wrapped_var%vartype == IXCvartype_object) then
       var = wrapped_var%object
    else
       call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
            IXCerr_invparam, 'unwrap object from non object')
    endif
  end subroutine unwrap_object

#define IXD_NAME	char1
#define IXD_DIMS	:
#define IXD_TYPE	character(len=*)
#include "wrappedvar_routines.f90"

#define IXD_NAME	dp1
#define IXD_DIMS	:
#define IXD_TYPE	real(dp)
#include "wrappedvar_routines.f90"

#define IXD_NAME	dp2
#define IXD_DIMS	:,:
#define IXD_TYPE	real(dp)
#include "wrappedvar_routines.f90"

#define IXD_NAME	dp3
#define IXD_DIMS	:,:,:
#define IXD_TYPE	real(dp)
#include "wrappedvar_routines.f90"

#define IXD_NAME	dp4
#define IXD_DIMS	:,:,:,:
#define IXD_TYPE	real(dp)
#include "wrappedvar_routines.f90"

#define IXD_NAME	i1
#define IXD_DIMS	:
#define IXD_TYPE	integer(i4b)
#include "wrappedvar_routines.f90"

#define IXD_NAME	i2
#define IXD_DIMS	:,:
#define IXD_TYPE	integer(i4b)
#include "wrappedvar_routines.f90"

#define IXD_NAME	i3
#define IXD_DIMS	:,:,:
#define IXD_TYPE	integer(i4b)
#include "wrappedvar_routines.f90"

#define IXD_NAME	i4
#define IXD_DIMS	:,:,:,:
#define IXD_TYPE	integer(i4b)
#include "wrappedvar_routines.f90"

end module IXMwrapped_var
