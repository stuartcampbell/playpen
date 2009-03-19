!------------------------------
! MODULE: IXMpointer_to_array
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 51 $ ($Date: 2004-06-10 04:22:13 -0400 (Thu, 10 Jun 2004) $)
!!
!! FORTRAN definition of IXMpointer_to_array object 

module IXMpointer_to_array
	use IXMtype_definitions
	implicit none
	public :: IXTpointer_to_array

!----------------------------
! TYPE: pointer_to_array
!----------------------------
!! Pointer to a 1D array. Will be used to enable an array of different length arrays to be constructed
!! using the construction e.g. type(pointer_to_array) :: arr(5), which is an array length 5, each element
!! containing a pointer to an array; or type (pointer_to_array), pointer :: arr(:), which can be used
!! to allow dynamic allocation of the length of an array, each of whose elements is a pointer to an array.
	type IXTpointer_to_array
		real(dp), pointer :: x(:)
	end type

end module IXMpointer_to_array
