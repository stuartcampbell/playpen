!----------------------------------------------------------------------------------------------------------------------
! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!----------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmaths_basis
!----------------------------------------------------------------------------------------------------------------------
!! A set of routines to provide utility routines for manipulating vectors and transformations in 3D space.
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1396 $ ($Date: 2008-06-05 04:58:24 -0400 (Thu, 05 Jun 2008) $)
!----------------------------------------------------------------------------------------------------------------------

module IXMmaths_basis
	use IXMtype_definitions
	use IXMmaths_utils
	implicit none

!	public:: IXFdot, IXFcross, IXFnorm
	interface IXFnorm
		module procedure IXFnorm
	end interface

	interface IXFdot
		module procedure IXFdot
	end interface

	interface IXFcos_angle
		module procedure IXFcos_angle
	end interface

	interface IXFcross
		module procedure IXFcross
	end interface

contains
	subroutine IXFtrans_spherical_polars (phi, theta, tmat)
!
!  Given a vector V with components x1,x2,x3 in reference frame S, this routine
! returns the matrix TMAT such that
!	y(i) = TMAT(i,j)*x(j)
! are the coordinates of the same vector in the reference frame S' which is related
! to S by rotation PHI about z followed by THETA about the resulting y axis.
!
	real(dp),intent(in):: phi, theta
	real(dp),intent(out)::tmat(3,3)
	real(dp):: cp, sp, ct, st

	cp = cos(phi)
	sp = sin(phi)
	ct = cos(theta)
	st = sin(theta)
	tmat(1,1) =  ct*cp
	tmat(1,2) =  ct*sp
	tmat(1,3) = -st
	tmat(2,1) = -sp
	tmat(2,2) =  cp
	tmat(2,3) =  0.0d0
	tmat(3,1) =  st*cp
	tmat(3,2) =  st*sp
	tmat(3,3) =  ct

	end subroutine IXFtrans_spherical_polars
!----------------------------------------------------------------------------------------------------------------------
!! Calculate the length/norm of a three-vector in a robust fashion.

	function IXFnorm (v) result(vmod)
		real(dp), intent(IN) :: v(3)
		real(dp) vmod
		integer(i4b) i
!	find index of largest element:
		i = 1
		if (abs(v(i)) .lt. abs(v(2))) i = 2
		if (abs(v(i)) .lt. abs(v(3))) i = 3
!	find the modulus in a robust way:
		if (v(i) .ne. 0.0_dp) then
			if (i .eq. 1) then
				vmod = abs(v(1))*sqrt(1.0_dp+(v(2)/v(1))**2+(v(3)/v(1))**2)
			else if (i .eq. 2) then
				vmod = abs(v(2))*sqrt(1.0_dp+(v(1)/v(2))**2+(v(3)/v(2))**2)
			else if (i .eq. 3) then
				vmod = abs(v(3))*sqrt(1.0_dp+(v(1)/v(3))**2+(v(2)/v(3))**2)
			endif
		else
			vmod = 0.0_dp
		endif

		return
	end function IXFnorm

      subroutine IXFvnorm (vin, vout, vlen)
!C
!C  Calculates the length of a (3D) vector VIN and returns the normal
!C vector VOUT and the length of the original vector.
!C
      real(dp):: vin(3), vout(3), vlen     

      vlen = IXFnorm (vin)
      if (vlen .eq. 0.0d0) then
         vout(1) = 0.0d0
         vout(2) = 0.0d0
         vout(3) = 0.0d0
      else
         vout(1) = vin(1) / vlen
         vout(2) = vin(2) / vlen
         vout(3) = vin(3) / vlen
      endif

      return
      end subroutine IXFvnorm

!----------------------------------------------------------------------------------------------------------------------
!!  Calculates the dot product of two three-vectors in a robust fashion.

	function IXFdot(v1, v2) result(dot)
		real(dp), intent(IN) :: v1(3), v2(3)
		real(dp) dot
		real(dp) vmod1, vmod2

		vmod1 = IXFnorm(v1)
		vmod2 = IXFnorm(v2)
		if (vmod1 .eq. 0.0_dp .or. vmod2 .eq. 0.0_dp) then
			dot = 0.0_dp
		else
			dot = (vmod1*vmod2)*( (v1(1)/vmod1)*(v2(1)/vmod2) &
					&  + (v1(2)/vmod1)*(v2(2)/vmod2) + (v1(3)/vmod1)*(v2(3)/vmod2) )
		endif

		return
	end function IXFdot

!----------------------------------------------------------------------------------------------------------------------
!! Calculates the cosine of the angle between two three-vectors in a robust
!! fashion.
!! If one of the vectors has zero length, then IXFcos is returned as 1.0_dp
!! (corresponds to angle being zero).

	function IXFcos_angle(v1, v2) result(res)
		real(dp), intent(IN) :: v1(3), v2(3)
		real(dp) res
		real(dp) vmod1, vmod2

		vmod1 = IXFnorm(v1)
		vmod2 = IXFnorm(v2)
		if (vmod1 .eq. 0.0_dp .or. vmod2 .eq. 0.0_dp) then
			res = 0.0_dp
		else
			res = (vmod1*vmod2)*( (v1(1)/vmod1)*(v2(1)/vmod2) &
					&  + (v1(2)/vmod1)*(v2(2)/vmod2) + (v1(3)/vmod1)*(v2(3)/vmod2) )
		endif

		return
	end function IXFcos_angle

!----------------------------------------------------------------------------------------------------------------------
!  Calculate the cross product of two three-vectors.

	function IXFcross(v1, v2) result(vres)
		real(dp), intent(IN) :: v1(3), v2(3)
		real(dp):: vres(3)
		real(dp):: vmod1, vmod2

		vmod1 = IXFnorm(v1)
		vmod2 = IXFnorm(v2)
		if (vmod1 .eq. 0.0_dp .or. vmod2 .eq. 0.0_dp) then
	        vres = 0.0_dp
		else
			vres(1) = (vmod1*vmod2) * ( (v1(2)/vmod1)*(v2(3)/vmod2) - &
                                      & (v1(3)/vmod1)*(v2(2)/vmod2) )
			vres(2) = (vmod1*vmod2) * ( (v1(3)/vmod1)*(v2(1)/vmod2) - &
                                      & (v1(1)/vmod1)*(v2(3)/vmod2) )
			vres(3) = (vmod1*vmod2) * ( (v1(1)/vmod1)*(v2(2)/vmod2) - &
                                      & (v1(2)/vmod1)*(v2(1)/vmod2) )
		endif

		return
	end function IXFcross

!----------------------------------------------------------------------------------------------------------------------
!!  Function to obtain rotation matrix from rotation vector.
!!
!!  Consider two orthonormal frames S and S', where S' is related to S by rotation about a unit vector
!!  (n(1),n(2),n(3)) in S by angle THETA (in a right-hand sense).
!!
!!  This defines a 3-vector (THETA(1), THETA(2), THETA(3)) where THETA(i) = THETA*n(i) (radians).
!!
!!  This routine returns the transformation matrix R(i,j) that relates the components of a vector v expressed
!!  in the two coordinate frames:
!!
!!        v'(i) = R(i,j) v(j)
!!
!!  See also IXFrotmat_to_rotvec

	function IXFrotvec_to_rotmat (theta) result(r)
!	input and output variables:
		real(dp), intent(IN) :: theta(3)
		real(dp) :: r(3,3)
!	local variables nad parameters:
        real(dp), parameter :: small_number = 10.0_dp*epsilon_dp
		real(dp) :: n1, n2, n3, c, s, theta0, a

		theta0 = IXFnorm(theta)
		if (theta0 < small_number) then
			call IXFunit_matrix(r)	! return identity matrix if small rotation
		else
			n1 = theta(1) / theta0
			n2 = theta(2) / theta0
			n3 = theta(3) / theta0
			c = cos(theta0)
			s = sin(theta0)
			a = 1.0_dp - c
			r(1,1) = a*n1*n1 + c
			r(2,1) = a*n1*n2 - s*n3
			r(3,1) = a*n1*n3 + s*n2
			r(1,2) = a*n2*n1 + s*n3
			r(2,2) = a*n2*n2 + c
			r(3,2) = a*n2*n3 - s*n1
			r(1,3) = a*n3*n1 - s*n2
			r(2,3) = a*n3*n2 + s*n1
			r(3,3) = a*n3*n3 + c
		endif

		return
	end function

!----------------------------------------------------------------------------------------------------------------------
!!  Function to obtain rotation vector from rotation matrix.
!!
!!  Suppose the components of a vector in two orthonormal bases S and S' are related by matrix R:
!!
!!        v'(i) = R(i,j) v(j)
!!
!!  S' can be related to S by rotation about a unit vector (n(1),n(2),n(3)) in S by angle THETA
!! (in a right-hand sense).
!!
!!  This routine returns the vector (THETA(1), THETA(2), THETA(3)), where THETA(i) = THETA*n(1) (radians)
!!
!!  It is assumed that the matrix properly corresponds to a rotation, that is, the subroutine does
!! NOT check that R is an orthogonal matrix.
!!
!!  See also IXFrotvec_to_rotmat.

	function IXFrotmat_to_rotvec(r) result(theta)
!	input and output variables
		real(dp), intent(IN) :: r(3,3)
		real(dp) :: theta(3)

!	local variables and parameters
        real(dp), parameter :: small_number = 10.0_dp*epsilon_dp
		real(dp) :: s(3,3), costh, theta0, sgn, theta_len
		integer(i4b) :: icol

!	Get symmetric part of rotation matrix:
		s = 0.5_dp*(r+transpose(r))

!	Get theta (0 =< theta =< pi):
! [If had algorithm that reliably calculates determinant of matrix (Numerical Recipes routine does not could cope with Det[s]=0):
!	costh = min(1.0d0,sqrt(abs(determinant(s)))) ;  if ((s(1,1)+s(2,2)+s(3,3)) < 1.0d0) costh = -costh ]

		costh = 0.5_dp*((s(1,1)+s(2,2)+s(3,3)) - 1.0_dp)
		if (abs(costh) > 1.0_dp) costh = sign(1.0_dp,costh)
		theta0 = acos(costh)

		s(1,1) = s(1,1) - costh
		s(2,2) = s(2,2) - costh
		s(3,3) = s(3,3) - costh

!	Find the index and sign of the largest of n(1), n(2), n(3):
		icol = 1
		if (r(2,3) .ge. r(3,2)) then
			sgn = 1.0_dp
		else
			sgn =-1.0_dp
		endif

		if (s(2,2) .gt. s(icol,icol)) then
			icol = 2
			if (r(3,1) .ge. r(1,3)) then
				sgn = 1.0_dp
			else
				sgn =-1.0_dp
			endif
		endif

		if (s(3,3) .gt. s(icol,icol)) then
			icol = 3
			if (r(1,2) .ge. r(2,1)) then
				sgn = 1.0_dp
			else
				sgn =-1.0_dp
			endif
		endif

!	Get the rotation vector and its magnitude:
		theta = s(:,icol)
		theta_len = IXFnorm(theta)
		if (theta_len < small_number .OR. max(s(1,1),s(2,2),s(3,3)) < small_number) then
			theta = 0.0_dp
		else
			theta = (sgn*theta0/theta_len)*theta
		endif

		return
		end function

!----------------------------------------------------------------------------------------------------------------------
!! Function to check if a 3-by-3 matrix is orthogonal.
!! The defining propery is that the fact that the transpose of the rotation matrix relating orthonormal bases
!! is the inverse of the rotation matrix.
!!
!! Syntax:
!!
!! ans = IXFrotmat_orthogonal (matrix)
!!

	function IXFrotmat_orthogonal (r) result(ans)
!	i/o arguments:
		real(dp), intent(IN) :: r(3,3)
		logical :: ans
!	internal variables and parameters:
        real(dp), parameter :: small_number = 10.0_dp*epsilon_dp
		real(dp) :: mat(3,3), umat(3,3)
		logical :: first_time=.TRUE.
		save first_time, umat

		if (first_time) then
			call IXFunit_matrix (umat)
			first_time = .FALSE.
		endif
		mat = matmul(r,transpose(r)) - umat
		if (abs(maxval(mat)) < small_number .AND. abs(minval(mat)) < small_number) then
			ans = .TRUE.
		else
			ans = .FALSE.
		endif

		return
		end function
!----------------------------------------------------------------------------------------------------------------------
end module
