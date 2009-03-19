!! Basic data type definitions for floats and integers
!! Taken from Numerical Recipes in Fortran 90, and added to these definitions
!! @author Toby Perring, ISIS
!! @version $Revision: 601 $ ($Date: 2006-01-19 13:57:17 -0500 (Thu, 19 Jan 2006) $)
module IXMtype_definitions
    implicit none

	integer, parameter :: i4b = selected_int_kind(9) !! 4 byte integer
	integer, parameter :: i2b = selected_int_kind(4) !! 2 byte integer
	integer, parameter :: i1b = selected_int_kind(2) !! 1 byte integer
	integer, parameter :: sp = kind(1.0)	!! single precision float
	integer, parameter :: dp = kind(1.0d0)	!! double precision float
	integer, parameter :: spc = kind((1.0,1.0)) !! single precision complex
	integer, parameter :: dpc = kind((1.0d0,1.0d0)) !! double precision complex
	integer, parameter :: lgt = kind(.true.) !! logical true

#include "ixctypes.f90"

	real(sp), parameter :: pi_sp=3.141592653589793238462643383279502884197_sp       !! Single Precision PI
	real(sp), parameter :: pio2_sp=1.57079632679489661923132169163975144209858_sp   !! Single Precision PI/2
	real(sp), parameter :: twopi_sp=6.283185307179586476925286766559005768394_sp    !! Single Precision 2*PI
    real(sp), parameter :: fourpi_sp=12.56637061435917295385057353311801153679_sp   !! Single Precision 4*PI
	real(sp), parameter :: sqrt2_sp=1.41421356237309504880168872420969807856967_sp  !! Single Precision SQRT(2)
	real(sp), parameter :: euler_sp=0.5772156649015328606065120900824024310422_sp   !! Single precision Euler constant
	real(sp), parameter :: deg_to_rad_sp=0.01745329251994329576923690768488612713443_sp !! Single precision degrees to radians
	real(sp), parameter :: rad_to_deg_sp=57.29577951308232087679815481410517033241_sp   !! Single precision radians to degrees

	real(dp), parameter :: pi_dp=3.141592653589793238462643383279502884197_dp       !! Double Precision PI
	real(dp), parameter :: pio2_dp=1.57079632679489661923132169163975144209858_dp   !! Single Precision PI/2
	real(dp), parameter :: twopi_dp=6.283185307179586476925286766559005768394_dp    !! Single Precision 2*PI
    real(dp), parameter :: fourpi_dp=12.56637061435917295385057353311801153679_dp   !! Double Precision 4*PI
	real(dp), parameter :: sqrt2_dp=1.41421356237309504880168872420969807856967_dp  !! Double Precision SQRT(2)
	real(dp), parameter :: euler_dp=0.5772156649015328606065120900824024310422_dp   !! Single precision Euler constant
	real(dp), parameter :: deg_to_rad_dp=0.01745329251994329576923690768488612713443_dp !! Single precision degrees to radians
	real(dp), parameter :: rad_to_deg_dp=57.29577951308232087679815481410517033241_dp   !! Single precision radians to degrees

	real(sp), parameter :: null_sp = -1.0e30	!! Single precision undefined (unless we use IEEE NaN)
	real(dp), parameter :: null_dp = -1.0d30	!! Double precision undefined (unless we use IEEE NaN)

	real(sp), parameter :: epsilon_sp = epsilon(1.0_sp)	!! Single precision smallest number compared to unity
	real(dp), parameter :: epsilon_dp = epsilon(1.0_dp)	!! Double precision smallest number compared to unity
  integer(i4b), parameter ::  name_len = 256 !! length of a name string
  integer(i4b), parameter ::  long_len = 256 !! length of a "long string"
  integer(i4b), parameter :: short_len = 32  !! length of an abbreviated name string

  real(dp), parameter :: IXCundef_dp = 0.0_dp / 0.0_dp
  real(sp), parameter :: IXCundef_sp = 0.0_sp / 0.0_sp
  integer(i4b), parameter :: IXCundef_i4b = 0
  logical(i4b), parameter :: IXCundef_logical = .false.
  character(len=*), parameter :: IXCundef_char = 'undefined'

end module IXMtype_definitions
