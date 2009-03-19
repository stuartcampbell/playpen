!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMdet_box_utils
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 778 $ ($Date: 2006-08-18 04:48:03 -0400 (Fri, 18 Aug 2006) $)
!!
!! Module containing utility routines for cuboidal detectors. No assumption is made about the nature
!! of the absorbing material in the detector; that will be done in the routines that are specific to
!! particular types of detector - what is needed here is the mean free path for absorption and the 
!! knowledge of the particular geometry of the detector.

module IXMdet_box_utils
  use IXMtype_definitions
  use IXMstatus
  implicit none
  private
  public :: effic_box

contains

!-----------------------------------------------------------------------------------------------------------------------

    subroutine effic_box (thick, sigma_macro, theta, dx, vx)
    implicit none
    real(dp), intent(IN) :: rad                 !! Total thickness of detector perpendicular to front face (m)
    real(dp), intent(IN) :: sigma_macro         !! Macroscopic absorption cross-section (m^-1)
    real(dp), intent(IN) :: theta               !! Angle of incident beam from face normal (rad)
    real(dp), intent(OUT), optional :: effic    !! Efficiency of detector (0-1)
    real(dp), intent(OUT), optional :: dx       !! Mean depth of absorption w.r.t. front face, along path defined by theta (m)
    real(dp), intent(OUT), optional :: vx       !! Variance of depth of absorption along path defined by theta (m)
    
    real(dp) rad_long, alf, effic_temp, dx_temp, xsqr_temp, vx_temp, vy_temp
    
    rad_long = rad/cos(theta)   ! ellipse halfwidth along beam flightpath
    alf = 2.0_dp*sigma_macro*rad_long

! We want to minimise unnecessary calculation of what is a fairly expensive calculation - but not have the ridiculous
! amount of coding that optimises every permutation of the five possible output arguments
! Assume that one want the zeroth, first or second moments 
    if (.not.(present(xsqr).or.present(vx).or.present(xsqr))) then
        if (present(effic).and.(.not.present(dx))) then
            call effic_cyl_internal (alf, effic)
        elseif ((.not.present(effic)).and.present(dx)) then
            call effic_cyl_internal (alf, dx_temp)
            dx = rad_long*dx_temp
        elseif (present(effic).and.present(dx)) then
            call effic_cyl_internal (alf, effic, dx_temp)
            dx = rad_long*dx_temp
        endif
    else
        call effic_cyl_internal (alf, effic_temp, dx_temp, xsqr_temp, vx_temp, vy_temp)
        if (present(effic)) effic = effic_temp
        if (present(dx))     dx = rad_long*dx_temp
        if (present(xsqr)) xsqr =(rad_long**2)*xsqr_temp
        if (present(vx))     vx =(rad_long**2)*vx_temp
        if (present(vy))     vy = rad*vy_temp
    endif
    
    end subroutine effic_cyl
    
end module IXMdet_cylinder_utils



