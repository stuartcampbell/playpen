!----------------------------------------------------------------------------------------------------------------------
! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!----------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmaths_utils
!----------------------------------------------------------------------------------------------------------------------
!! A set of general utility routines.
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1413 $ ($Date: 2008-07-03 12:20:40 -0400 (Thu, 03 Jul 2008) $)
!----------------------------------------------------------------------------------------------------------------------

module IXMmaths_utils
	use IXMtype_definitions
	implicit none

interface IXFfind_if_same_value
  module procedure find_if_same_value_r,find_if_same_value_i
end interface

interface IXFfind_val_and_indexmask
  module procedure find_val_and_indexmask_r,find_val_and_indexmask_i
end interface


private:: staggered_position_private,find_if_same_value_i,find_if_same_value_r,find_val_and_indexmask_r,find_val_and_indexmask_i

contains

!----------------------------------------------------------------------------------------------------------------------
!! Initialise a two-dimensional matrix as the unit matrix. If the matrix is not square, the diagonal from the
!! top-left is filled, and the 
	subroutine IXFunit_matrix (mat)
		real(dp), intent(out) :: mat(:,:)
		integer(i4b) :: i

		mat = 0.0_dp
		forall (i=1:min(size(mat,1),size(mat,2))) mat(i,i)=1.0_dp

	end subroutine

	subroutine IXFrotation_matrix_x (angle, rotmat)
	real(dp),intent(in):: angle
	real(dp),intent(out):: rotmat(3,3)
!
!  Fills up a 3 X 3 matrix corresponding to rotation by ANGLE radians about the x axis.
!
!  If vector U is rotated by angle ANGLE (in radians) about the z-axis to vector V, then the components
!  of the vectors in the orthonormal basis are related by:
!
!         V(i) = Sum_over_j [ ROTMAT(i,j) * U(j) ]
!
	rotmat(1,1) =  1.0d0
	rotmat(2,1) =  0.0d0
	rotmat(3,1) =  0.0d0
	rotmat(1,2) =  0.0d0
	rotmat(2,2) =  cos(angle)
	rotmat(3,2) =  sin(angle)
	rotmat(1,3) =  0.0d0
	rotmat(2,3) = -rotmat(3,2)
	rotmat(3,3) =  rotmat(2,2)

	end	subroutine IXFrotation_matrix_x 

	subroutine IXFrotation_matrix_y (angle, rotmat)
	real(dp),intent(in):: angle
	real(dp),intent(out):: rotmat(3,3)
!
!  Fills up a 3 X 3 matrix corresponding to rotation by ANGLE radians about the yaxis.
!
!  If vector U is rotated by angle ANGLE (in radians) about the z-axis to vector V, then the components
!  of the vectors in the orthonormal basis are related by:
!
!         V(i) = Sum_over_j [ ROTMAT(i,j) * U(j) ]
!
	rotmat(1,1) =  cos(angle)
	rotmat(2,1) =  0.0d0
	rotmat(3,1) = -sin(angle)
	rotmat(1,2) =  0.0d0
	rotmat(2,2) =  1.0d0
	rotmat(3,2) =  0.0d0
	rotmat(1,3) = -rotmat(3,1)
	rotmat(2,3) =  0.0d0
	rotmat(3,3) =  rotmat(1,1)

	end subroutine IXFrotation_matrix_y

	subroutine IXFrotation_matrix_z (angle, rotmat)
	real(dp),intent(in):: angle
	real(dp),intent(out):: rotmat(3,3)
!
!  Fills up a 3 X 3 matrix corresponding to rotation by ANGLE radians about the z axis.
!
!  If vector U is rotated by angle ANGLE (in radians) about the z-axis to vector V, then the components
!  of the vectors in the orthonormal basis are related by:
!
!         V(i) = Sum_over_j [ ROTMAT(i,j) * U(j) ]
!
	rotmat(1,1) =  cos(angle)
	rotmat(2,1) =  sin(angle)
	rotmat(3,1) =  0.0d0
	rotmat(1,2) = -rotmat(2,1)
	rotmat(2,2) =  rotmat(1,1)
	rotmat(3,2) =  0.0d0
	rotmat(1,3) =  0.0d0
	rotmat(2,3) =  0.0d0
	rotmat(3,3) =  1.0d0

	end subroutine IXFrotation_matrix_z
!----------------------------------------------------------------------------------------------------------------------
! Staggered array operations
! ---------------------------
! Suppose that two arrays ind_dim1(i) and ind_dim2(i) (i=1,2,3...N) give the row and the column of a set of elements
! in a staggered array where column k has ndim1(k) elements. 
! 
! This routine optionally return one or both of:
!  (1) The position in the staggered array, counting in the standard Fortran (i.e. column-major) sense.
!
!  (2) The inverse mapping array, inv_position. That is, inv_position(1) gives the value of i corresponding to the
!      smallest element index in the staggered array, counting in the standard Fortran (i.e. column-major) sense,
!      inv_position(2) gives the value of i corresponding to the next smallest element index ... inv_position(N)
!      has the largest element index.
! 
! The routine assumes the input indices are valid i.e. 1 <= ind_dim2(i) <= size(ndim1) and 
! 1 <= ind_dim1(i) <= ndim1(ind_dim2(i)), and that ndim1(k)>=1.
!

	subroutine IXFstaggered_position (ind_dim1, ndim1, ind_dim2, position, inv_position)
	    use IXMsort
		integer(i4b), intent(in) :: ind_dim1(:), ndim1(:), ind_dim2(:)
		integer(i4b), intent(out), optional :: position(:), inv_position(:)
		integer(i4b), allocatable :: work(:)

        ! Get index into the staggered array, and its inverse
        if (present(position)) then
            call staggered_position_private (ind_dim1, ndim1, ind_dim2, position)
            if (present(inv_position)) call IXFrank(position,inv_position)
        elseif (present(inv_position)) then
            allocate(work(size(ind_dim1)))
            call staggered_position_private (ind_dim1, ndim1, ind_dim2, work)
            call IXFrank(work,inv_position)
            deallocate(work)
        endif
            
	end subroutine IXFstaggered_position
	
	
    subroutine staggered_position_private (ind_dim1, ndim1, ind_dim2, position)
		integer(i4b), intent(in) :: ind_dim1(:), ndim1(:), ind_dim2(:)
		integer(i4b), intent(out) :: position(:)
		integer(i4b) :: i, cumsum(size(ndim1)), ndim2
		
		! Get cumulative array of previous elements of ndim:
		cumsum(1)=0
		ndim2=size(ndim1)
		if (ndim2>1) then
    		do i=2,ndim2
    		    cumsum(i)=cumsum(i-1)+ndim1(i-1)
    		end do
        endif

        ! Get index into the staggered array
        do i=1,size(position)    ! assume that the output array has been pre-allocated to the correct length
            position(i)=cumsum(ind_dim2(i))+ind_dim1(i)
        end do

	end subroutine staggered_position_private
	
subroutine IXFfind_max_and_indexmask(array_in,list,max)

implicit none

real(dp),intent(in)::array_in(:)

logical::list(:)

real(dp),intent(out)::max


list=.false.

max=maxval(array_in)
where(array_in == max)list=.true.


end subroutine IXFfind_max_and_indexmask



subroutine IXFfind_min_and_indexmask(array_in,list,min)


implicit none

real(dp),intent(in)::array_in(:)

logical::list(:)

real(dp),intent(out)::min


list=.false.

min=minval(array_in)

where(array_in == min)list=.true.


end subroutine IXFfind_min_and_indexmask

subroutine find_val_and_indexmask_r(array_in,val,list)

implicit none

real(dp),intent(in)::array_in(:),val

logical::list(:)

list=.false.

where(array_in == val)list=.true.

end subroutine find_val_and_indexmask_r

subroutine find_val_and_indexmask_i(array_in,val,list)

implicit none

integer(i4b),intent(in)::array_in(:),val

logical::list(:)

list=.false.

where(array_in == val)list=.true.

end subroutine find_val_and_indexmask_i


subroutine find_if_same_value_r(array_in,same,value)
implicit none
real(dp),intent(in)::array_in(:)
real(dp),intent(out),optional::value
logical,intent(out)::same
same=.false.
if(maxval(array_in) == minval(array_in))then
  same=.true.
if(present(value))  value=array_in(1)
else
if(present(value))  value=IXCundef_dp
endif
end subroutine find_if_same_value_r

subroutine find_if_same_value_i(array_in,same,value)
implicit none
integer(i4b),intent(in)::array_in(:)
integer(i4b),intent(out),optional::value
logical,intent(out)::same
same=.false.
if(maxval(array_in) == minval(array_in))then
  same=.true.
if(present(value))  value=array_in(1)
else
if(present(value))  value=IXCundef_i4b
endif
end subroutine find_if_same_value_i

end module
