!----------------------------------------------------------------------------------------------------------------------
! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!----------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmaths_geometry
!----------------------------------------------------------------------------------------------------------------------
! A set of general geometry routines.
!
! @author Toby Perring, ISIS
! @version $Revision: 1413 $ ($Date: 2008-07-03 12:20:40 -0400 (Thu, 03 Jul 2008) $)
!----------------------------------------------------------------------------------------------------------------------

module IXMmaths_geometry
	use IXMtype_definitions
	use IXMstatus
	use IXMio
	implicit none

contains

!----------------------------------------------------------------------------------------------------------------------
! Calculate the area and centroid of a polygon 
! The polygon lies in the x-y plane, with verticies (x(i),y(i)), i=1:n
!
! Note that the centroid in NOT given by the average of the verticies in general. The general algorithm for the
! centroid requires the area of the polygon to have been previously calculated. To avoid numerical stability
! problems in the calculation of the area, we need to perform sums with respect to a point near the centre
! of the polygon. To get a good first estimate, use the weighted sum of the verticies.
!
! Area calculation alone: http://geometryalgorithms.com/Archive/algorithm_0101/
!
! Area and centroid: in addition, http://astronomy.swin.edu.ac/~pbourke/geometry/polyarea
!
	subroutine IXFpolygon_moments (x, y, status, area, centroid)
		real(dp), intent(in) :: x(:), y(:)  !! vertex coordinates
		real(dp), intent(out), optional :: area, centroid(2)
		type(IXTstatus) :: status
! internal variables:
        real(dp), allocatable :: atemp(:)
        real(dp) xref, yref, asum
		integer(i4b) :: n
		CHARACTER(80) ::MESS

		n = size(x)
		if (size(x) /= size(y)) then
			call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
			 & 'Length of x and y arrays of vertex coordinates inconsistent in IXFpolygon_area')
        elseif (n < 3) then
            if (present(area)) area = 0.0_dp   ! point or line segment, so area = 0
            if (present(centroid)) then
                if (n==2) then
                    centroid = (/0.5_dp*(x(2)+x(1)), 0.5_dp*(y(2)+y(1))/)
                else
                    centroid = (/x(1),y(1)/)
                endif
            endif
        else
            xref = sum(x)/real(n,kind(1.0_dp))  ! not the centroid, but a good first stab
            if ((present(area)) .AND. (.NOT. present(centroid))) then
                area = 0.5_dp*( sum((x(2:n-1)-xref)*(y(3:n)-y(1:n-2))) + &
                    (x(n)-xref)*(y(1)-y(n-1)) + (x(1)-xref)*(y(2)-y(n)) )
            else if (present(centroid)) then
                allocate(atemp(n))
                xref = sum(x)/real(n,kind(1.0_dp))  ! not the centroid, but a good first stab
                yref = sum(y)/real(n,kind(1.0_dp))  ! not the centroid, but a good first stab
                atemp(1:n-1) = (x(1:n-1)-xref)*(y(2:n)-yref) - (x(2:n)-xref)*(y(1:n-1)-yref)
                atemp(n) = (x(n)-xref)*(y(1)-yref) - (x(1)-xref)*(y(n)-yref)
                asum = sum(atemp)
                centroid(1) = ( sum( ((x(1:n-1)-xref) + (x(2:n)-xref))*atemp(1:n-1) ) + ((x(n)-xref) + (x(1)-xref))*atemp(n) ) &
                                & / (3.0_dp*asum) + xref
                centroid(2) = ( sum( ((y(1:n-1)-yref) + (y(2:n)-yref))*atemp(1:n-1) ) + ((y(n)-yref) + (y(1)-yref))*atemp(n) ) &
                                & / (3.0_dp*asum) + yref
                if (present(area)) area = 0.5_dp*asum
                deallocate(atemp)
!                WRITE(MESS,'('' XREF:'',F18.8,''    YREF:'',F18.8)') XREF, YREF
!                CALL IXFwrite_line(MESS,STATUS)
!                WRITE(MESS,'('' AREA:'',F18.8,''    XCENT:'',F18.8,''    YCENT:'',F18.8)') ASUM/2.0_DP,CENTROID(1),CENTROID(2)
!                CALL IXFwrite_line(MESS,STATUS)
                endif
        endif

	end subroutine

end module
