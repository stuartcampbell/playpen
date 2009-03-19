!----------------------------------------------------------------------------------------------------------------------
! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!----------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmaths_projection
!----------------------------------------------------------------------------------------------------------------------
!! A set of routines for projecting objects
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 503 $ ($Date: 2005-10-24 14:04:40 -0400 (Mon, 24 Oct 2005) $)
!----------------------------------------------------------------------------------------------------------------------

module IXMmaths_projection
	use IXMtype_definitions
	use IXMstatus
	implicit none
! public parameters
	integer(i4b), parameter, public :: IXCspherical_polar=1,    &
	                                   IXCcylindrical_polar=2,  &
	                                   IXCpolar = 3,            &
	                                   IXCplanar = 4
contains

!----------------------------------------------------------------------------------------------------------------------
!! Given teh vertices of a polygon and the projection type and coordinate frame, return a list of points for the
!! polygon to be plotted in the 2D plot window.

	subroutine IXFproj_projection (vertices, projection, px, py, status, radius, axes)
		real(dp), intent(in) :: vertices(:,:)
		integer(i4b), intent(in) :: projection
		real(dp), intent(out) :: px(size(vertices,2)), py(size(vertices,2))
		real(dp), intent(in), optional :: radius
		integer(i4b), intent(in), optional :: axes(2)   ! 1,2,3 for x,y,z; give a pair for the plot x and y axes
		type(IXTstatus) :: status
! internal variables:
        real(dp) :: theta(size(vertices,2)), phi(size(vertices,2))
		integer(i4b) :: ix, iy

        if (projection==IXCspherical_polar) then
            px = atan2(sqrt(vertices(1,:)**2+vertices(2,:)**2),vertices(3,:))   ! x-axis is theta
            py = atan2(vertices(2,:),vertices(1,:))                             ! y-axis is phi
            
        else if (projection==IXCcylindrical_polar) then
            px = atan2(vertices(2,:),vertices(1,:))     ! x-axis is phi
            if (present(radius)) px = px*radius         ! scale by radius
            py = vertices(3,:)                          ! y-axis is z
            
        else if (projection==IXCpolar) then
            theta = atan2(sqrt(vertices(1,:)**2+vertices(2,:)**2),vertices(3,:))
            phi = atan2(vertices(2,:),vertices(1,:))
            px = -theta*cos(phi)
            py =  theta*sin(phi)

        else if (projection==IXCplanar) then
            if (present(axes)) then
                ix = modulo(axes(1)-1,3)+1
                iy = modulo(axes(2)-1,3)+1
            else
                ix = 1
                iy = 2
            endif
            px = vertices(ix,:)
            py = vertices(iy,:)
            
        else
            call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
			    & 'Unrecognised projection in IXFproj_projection')
	    endif
	    
	    return
	    end subroutine
	    
!----------------------------------------------------------------------------------------------------------------------
end module

