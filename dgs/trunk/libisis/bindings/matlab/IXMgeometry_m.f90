module IXMm_geometry

#define IXD_TYPE geometry
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE geometry
#include "bindings_base.f90"

end module IXMm_geometry

#define IXD_TYPE geometry
#include "bindings_extra.f90"


!----------------------------------------------------------------------------------------------------------------------
! Specific matlab functions
!----------------------------------------------------------------------------------------------------------------------

subroutine IXBvolume_geometry (nlhs, plhs, nrhs, prhs, status)
	use IXMm_geometry
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTgeometry) :: geometry
	real(dp) :: volume

    call IXBgetFromBinding(prhs(1),' ', 1, 0, geometry, status)
!	volume = IXFvolume (geometry)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, volume, status)

end subroutine


subroutine IXBsolid_angle_geometry (nlhs, plhs, nrhs, prhs, status)
	use IXMm_geometry
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTgeometry) :: geometry
	real(dp) :: vp(3), omega

    call IXBgetFromBinding(prhs(1),' ', 1, 0, geometry, status)
    call IXBgetFromBinding(prhs(2),' ', 1, 0, vp, status)
!	omega = IXFsolid_angle (geometry, vp)
    call IXBsendToBinding(plhs(1), ' ', 1, 0, omega, status)

end subroutine

    
subroutine IXBprojarea_vertices_geometry (nlhs, plhs, nrhs, prhs, status)
	use IXMm_geometry
	use IXMm_orientation
	use IXMm_translation
	implicit none
! i/o declarations (follow Matlab rules for type declaration i.e. don't use integer(i4b) if the matlab manaul says use integer*4)
	integer :: nlhs, nrhs
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	type(IXTstatus) :: status
! internal declarations
	type(IXTgeometry) :: geometry
	type(IXTorientation) :: or
	type(IXTtranslation) :: t
	integer(i4b) :: projection, axes(2)
	real(dp) :: radius, r_axes(2)       !*** have to perform conversion of integers
	real(dp), pointer :: px(:), py(:)
	

    call IXBgetFromBinding(prhs(1),' ', 1, 0, geometry, status)
    call IXBgetFromBinding(prhs(2),' ', 1, 0, or, status)
    call IXBgetFromBinding(prhs(3),' ', 1, 0, t, status)
    call IXBgetFromBinding(prhs(4),' ', 1, 0, projection, status)
    if (nrhs >= 5) then
        if (ixIsClass(prhs(5),'double') .ne. 0) then
            if (ixGetN(prhs(5))==1 .AND. ixGetM(prhs(5))==1) then   ! one element only
                call IXBgetFromBinding(prhs(5),' ', 1, 0, radius, status)
                if (status == IXCseverity_error) return
 !               call IXFprojarea_vertices_geometry (geometry, or, t, projection, px, py, status, radius=radius)
            else
                call IXBgetFromBinding(prhs(5),' ', 1, 0, r_axes, status)
                if (status == IXCseverity_error) return
                axes=int(r_axes)
 !               call IXFprojarea_vertices_geometry (geometry, or, t, projection, px, py, status, axes=axes)
            endif
        endif   ! just ignore any further rhs arguments
    else
	    if (status == IXCseverity_error) return    
  !      call IXFprojarea_vertices_geometry (geometry, or, t, projection, px, py, status)    
    endif
	if (status == IXCseverity_error) return  
    call IXBsendToBinding(plhs(1), ' ', 1, 0, px, status)
    call IXBsendToBinding(plhs(2), ' ', 1, 0, py, status)
    return

end subroutine    
    
    
    
    
    
    
    
    
    
    
