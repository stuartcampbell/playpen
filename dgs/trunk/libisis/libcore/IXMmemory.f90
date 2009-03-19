! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-------------------------------------------------------------------------------
! MODULE: IXMmemory
!-------------------------------------------------------------------------------
!! A set of routines to provide memory management as well as 
!! mapping and copying of memory between FORTRAN and an external program
!!
!! With no bindings these may still be used to provide a memory tracking and 
!! debugging mechanism
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 759 $ ($Date: 2006-07-21 09:22:23 -0400 (Fri, 21 Jul 2006) $)
    
module IXMmemory
  use IXMtype_definitions
  use IXMstatus
  implicit none
  
  type IXTmemory_info
    logical :: fortran_alloc = .false.
    integer(cpointer_t) :: external_ptr = 0
  end type

  integer, parameter :: IXCmemory_buckets = 9973 !! number of memory buckets (using a prime number is best)

  ! binding overrideable routines
  integer(cpointer_t) :: IXBexternalMakeResult,IXBallocArrayDescriptor,IXBgetArraydata
  external IXBexternalMakeResult,IXBallocArrayDescriptor,IXBgetArraydata

#define IXD_NAME  dp1
#define IXD_FTYPE_FIXED real(dp)
#define IXD_FTYPE real(dp)
#define IXD_DIMS  :
#include "IXMmemory_interface.f90"

#define IXD_NAME  dp2
#define IXD_FTYPE_FIXED real(dp)
#define IXD_FTYPE real(dp)
#define IXD_DIMS  :,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  dp3
#define IXD_FTYPE_FIXED real(dp)
#define IXD_FTYPE real(dp)
#define IXD_DIMS  :,:,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  dp4
#define IXD_FTYPE_FIXED real(dp)
#define IXD_FTYPE real(dp)
#define IXD_DIMS  :,:,:,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  i1
#define IXD_FTYPE_FIXED integer(i4b)
#define IXD_FTYPE integer(i4b)
#define IXD_DIMS  :
#include "IXMmemory_interface.f90"

#define IXD_NAME  i2
#define IXD_FTYPE_FIXED integer(i4b)
#define IXD_FTYPE integer(i4b)
#define IXD_DIMS  :,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  i3
#define IXD_FTYPE_FIXED integer(i4b)
#define IXD_FTYPE integer(i4b)
#define IXD_DIMS  :,:,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  i4
#define IXD_FTYPE_FIXED integer(i4b)
#define IXD_FTYPE integer(i4b)
#define IXD_DIMS  :,:,:,:
#include "IXMmemory_interface.f90"

#define IXD_NAME  c1
#define IXD_FTYPE_FIXED character(len=long_len)
#define IXD_FTYPE character(len=*)
#define IXD_DIMS  :
#include "IXMmemory_interface.f90"

contains

#define IXD_NAME  dp1
#define IXD_FTYPE real(dp)
#define IXD_FTYPE_TEMP real(dp)
#define IXD_MTYPE 'double'
#define IXD_DIMS  :
#define IXD_STACK stackdp1
#define IXD_NDIMS 1
#define IXD_NULL  IXCundef_dp
#include "IXMmemory_routines.f90"

#define IXD_NAME  dp2
#define IXD_FTYPE real(dp)
#define IXD_FTYPE_TEMP real(dp)
#define IXD_MTYPE 'double'
#define IXD_DIMS  :,:
#define IXD_STACK stackdp2
#define IXD_NDIMS 2
#define IXD_NULL  IXCundef_dp
#include "IXMmemory_routines.f90"

#define IXD_NAME  dp3
#define IXD_FTYPE real(dp)
#define IXD_FTYPE_TEMP real(dp)
#define IXD_MTYPE 'double'
#define IXD_DIMS  :,:,:
#define IXD_STACK stackdp3
#define IXD_NDIMS 3
#define IXD_NULL  IXCundef_dp
#include "IXMmemory_routines.f90"

#define IXD_NAME  dp4
#define IXD_FTYPE real(dp)
#define IXD_FTYPE_TEMP real(dp)
#define IXD_MTYPE 'double'
#define IXD_DIMS  :,:,:,:
#define IXD_STACK stackdp4
#define IXD_NDIMS 4
#define IXD_NULL  IXCundef_dp
#include "IXMmemory_routines.f90"

#define IXD_NAME  i1
#define IXD_FTYPE integer(i4b)
#define IXD_FTYPE_TEMP integer(i4b)
#define IXD_MTYPE 'int32'
#define IXD_DIMS  :
#define IXD_STACK stacki1
#define IXD_NDIMS 1
#define IXD_NULL  IXCundef_i4b
#include "IXMmemory_routines.f90"

#define IXD_NAME  i2
#define IXD_FTYPE integer(i4b)
#define IXD_FTYPE_TEMP integer(i4b)
#define IXD_MTYPE 'int32'
#define IXD_DIMS  :,:
#define IXD_STACK stacki2
#define IXD_NDIMS 2
#define IXD_NULL  IXCundef_i4b
#include "IXMmemory_routines.f90"

#define IXD_NAME  i3
#define IXD_FTYPE integer(i4b)
#define IXD_FTYPE_TEMP integer(i4b)
#define IXD_MTYPE 'int32'
#define IXD_DIMS  :,:,:
#define IXD_STACK stacki3
#define IXD_NDIMS 3
#define IXD_NULL  IXCundef_i4b
#include "IXMmemory_routines.f90"

#define IXD_NAME  i4
#define IXD_FTYPE integer(i4b)
#define IXD_FTYPE_TEMP integer(i4b)
#define IXD_MTYPE 'int32'
#define IXD_DIMS  :,:,:,:
#define IXD_STACK stacki4
#define IXD_NDIMS 4
#define IXD_NULL  IXCundef_i4b
#include "IXMmemory_routines.f90"

#define IXD_NAME  c1
#define IXD_FTYPE character(len=*)
#define IXD_FTYPE_TEMP character(len=len(array))
#define IXD_MTYPE 'char'
#define IXD_DIMS  :
#define IXD_STACK stackc1
#define IXD_NDIMS 1
#define IXD_NULL  'NaN'
#include "IXMmemory_routines.f90"

  !! Initialise the stacks used by push_memory and find_memory 
  subroutine IXFmemory_init(size)
    implicit none
	integer :: size
  end subroutine  

  !! Cleanup the stacks used by push_memory and find_memory 
  subroutine IXFmemory_cleanup(status)
    implicit none
	type(IXTstatus) :: status
    call cleanup_dp1(status)
    call cleanup_dp2(status)
    call cleanup_dp3(status)
    call cleanup_dp4(status)
    call cleanup_i1(status)
    call cleanup_i2(status)
    call cleanup_i3(status)
    call cleanup_i4(status)
    call cleanup_c1(status)
  end subroutine  
   
  !! Associate a FORTRAN INTEGER*4 variable with a pointer
  subroutine associate_integer4(ptr, value)
    use IXMtype_definitions
    implicit none
    external c_associate_integer4
    integer(cpointer_t), intent(in) :: ptr  ! address of value
    integer(i4b), intent(out) :: value
    call c_associate_integer4(ptr, value)
  end subroutine

  !! Associate a FORTRAN REAL*8 variable with a pointer
  subroutine associate_real8(ptr, value)
    use IXMtype_definitions
	implicit none
    external c_associate_real8
    integer(cpointer_t), intent(in) :: ptr
    real(dp), intent(out) :: value
    call c_associate_real8(ptr, value)
  end subroutine

end module IXMmemory
