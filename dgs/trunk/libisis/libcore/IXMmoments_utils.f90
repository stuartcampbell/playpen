!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmoments_utils
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 1281 $ ($Date: 2007-11-19 10:10:11 -0500 (Mon, 19 Nov 2007) $)
!!
!! Module containing subroutines that return information about peaks

module IXMmoments_utils
  use IXMtype_definitions
  use IXMindex
  use IXMstatus
  use IXMintegrate
  implicit none
contains
    !! Obtains peak information
    subroutine get_moments (x, y, e, x_min, x_max, prominence, status, &
    & area, bkgd, c, c_fwhm, h, w, xbar, sig, gam1, gam2, &
    & sig_area, sig_bkgd, sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max)
!
! Calculates the area and moments of a peak
! Based on get_moments routine from VMS version of Homer (R.Osborn ~1992, T.G.Perring 19/3/95, 6/8/01)
!
! INPUT:
!   x_in(1:n)   real        Point positions (assumed to be monotonic increasing)
!   y_in(1:n)   real        Intensity at those positions
!   e_in(1:n)   real        Standard deviations
!   x_min       real        Lower x limit of range in which to search for a peak
!   x_max       real        Upper x limit of range in which to search for a peak
!   prominence  real        Factor by which peak must exceed sorrounding data to count as a peak
!                               Require: prominence > 1; set to default otherwise
!                               Default: prominence = 2
!
! OUTPUT:
!   status      IXTstatus   Status
!   area        real        Area of peak
!   bkgd        real        Background intensity
!   c           real        Position of maximum intensity
!   c_fwhm      real        Average of half-height positions
!   h           real        Height at peak maximum
!   w           real        FWHH
!   xbar        real        First moment
!   sig         real        Square root of variance
!   gam1        real        Skewness (Kendall and Stewart Vol.I pp.87-89)
!   gam2        real        Kurtosis (..................................)
!   sig_area    real        Error on area
!   sig_bkgd    real        Error on the constant background
!   sig_xbar    real        Error on xbar
!   sig_sig     real        Error on sig
!   sig_gam1    real        Error on gam1
!   sig_gam2    real        Error on gam2
!   pk_min      real        Lower limit of peak proper
!   pk_max      real        Upper limit of peak proper
!   bkgd_min    real        Lower limit from which background was estimated
!   bkgd_max    real        Upper limit from which background was estimated
!
! If all ouput parameters = 0:  no peak found, or search range outside data range.
! Otherwise, a peak was found
!
! Method:
! The routine attempts to determine the extent of a peak by numberically differentiating the peak and defining the extent
! as the points where the differential is statistically zero. The algorithm implicitly assumes that the background is 
! constant.
!  
!
    implicit none

    real(dp), parameter :: sfac_pk=2_dp         ! multiple of standard deviation for significance
    real(dp), parameter :: sfac_deriv=1_dp      ! multiple of standard deviation for significance
    real(dp), parameter :: bkgd_fac=0.5_dp      ! multiple of peak width for calculation of background on either side of peak

    real(dp), intent(IN) :: x(:), y(:), e(:), x_min, x_max, prominence
    type(IXTstatus), intent(INOUT) :: status
    real(dp), intent(OUT) :: area, bkgd, c, c_fwhm, h, w, xbar, sig, gam1, gam2, &
        & sig_area, sig_bkgd, sig_xbar, sig_sig, sig_gam1, sig_gam2, pk_min, pk_max, bkgd_min, bkgd_max

    integer(i4b) :: i, ilo, ihi, ipkarr(1), ipk, im, ip, ipk_int, im1, im2, ip1, ip2
    real(dp) :: ypk, epk, rat, rat_err, deriv, error, dtp, dtm, pk_width, &
             &  bkgd_m, bkgd_err_m, bkgd_p, bkgd_err_p, bkgd_range, hby2, xm_hh, xp_hh, dummy
    real(dp), allocatable :: xint(:), yint(:), eint(:)
    
! *** check prominence>1, xmin<xmax

! Initialise return values:
    area = 0.0_dp
    bkgd = 0.0_dp
    w = 0.0_dp
    c = 0.0_dp
    c_fwhm = 0.0_dp
    xbar = 0.0_dp
    sig = 0.0_dp
    gam1 = 0.0_dp
    gam2 = 0.0_dp
    sig_area = 0.0_dp
    sig_bkgd = 0.0_dp
    sig_xbar = 0.0_dp
    sig_sig = 0.0_dp
    sig_gam1 = 0.0_dp
    sig_gam2 = 0.0_dp
    pk_min = 0.0_dp
    pk_max = 0.0_dp
    bkgd_min = 0.0_dp
    bkgd_max = 0.0_dp
    
! Determine peak position within the peak search range
    ilo = IXFlower_index(x,x_min)
    ihi = IXFupper_index(x,x_max)
    if (ihi < ilo) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'No data in peak search range (IXFget_moments)')
       return
    endif
    ipkarr = maxloc(y(ilo:ihi))    ! position of peak
    ipk = ipkarr(1)+ilo-1
    ypk = y(ipk)
    epk = e(ipk)

! Find data range that satisfies prominence criterion: im < ipk < ip will be nearest points that satisfy this
    im = 0
    if (ipk>1) then
        do i = ipk-1, 1, -1
            rat = y(i)/ypk
            rat_err = sqrt(e(i)**2_i4b + (rat*epk)**2_i4b) / ypk
            if (rat .lt. (1.0_dp/prominence - sfac_pk*rat_err)) then
                im = i
                goto 100
            endif
        end do
    endif
100 continue

    ip = size(x)+1
    if (ipk<size(x)) then
        do i = ipk+1, size(x)
            rat = y(i)/ypk
            rat_err = sqrt(e(i)**2_i4b + (rat*epk)**2_i4b) / ypk
            if (rat .lt. (1.0_dp/prominence - sfac_pk*rat_err)) then
                ip = i
                goto 200
            endif
        end do
    endif
200 continue

    if (ip<=size(x) .and. im>=1) then  ! peak in data
        c = x(ipk)
    else
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'No peak found in data that satisfies prominence criterion (IXFget_moments)')
        return
    endif

! We now have a peak, so can start filling output arguments
! Determine extent of peak using derivatives
! At this point 1 =< im < ipk < ip =< size(x)
! After this section, new values will be given to im, ip that still satisfy these inequalities.
!
! The algorithm for negative side skipped if im=1; positive side skipped if ip=size(x); 
! if fails derivative criterion -> ip=size(x) (+ve)  im=1 (-ve)
! In either case, we deem that the peak has a tail(s) that extend outside the range of x

    if (ip < size(x)) then
        deriv = -1000.0_dp
        error = 0.0_dp
        do while (ip < size(x) .and. deriv < -sfac_deriv*error)
            dtp = x(ip+1) - x(ip)
            dtm = x(ip) - x(ip-1)
            deriv = 0.5_dp*( ((y(ip+1) - y(ip)) / dtp) + ((y(ip) - y(ip-1)) / dtm) )
            error = 0.5_dp*sqrt( ((e(ip+1)**2 + e(ip)**2) / dtp**2) + ((e(ip)**2 + e(ip-1)**2) / dtm**2) &
     &                                      - 2.0*(e(ip)**2 / (dtp*dtm)) )
            ip = ip + 1
        end do
        ip = ip - 1
        if (deriv .lt. -error) ip = size(x) ! derivative criterion not met
    endif

    if (im > 1) then
        deriv = 1000.0_dp
        error = 0.0_dp
        do while (im > 1 .and. deriv > sfac_deriv*error)
            dtp = x(im+1) - x(im)
            dtm = x(im) - x(im-1)
            deriv = 0.5_dp*(((y(im+1) - y(im)) / dtp) + ((y(im) - y(im-1)) / dtm))
            error = 0.5_dp*sqrt( ((e(im+1)**2 + e(im)**2) / dtp**2) + ((e(im)**2 + e(im-1)**2) / dtm**2) &
     &                                      - 2.0*(e(im)**2 / (dtp*dtm)) )
            im = im - 1
        end do
        im = im + 1
        if (deriv .gt. error) im = 1        ! derivative criterion not met
    endif
    pk_min = x(im)
    pk_max = x(ip)
    pk_width = x(ip) - x(im)
        
! Determine background from either side of peak.
! At this point, im and ip define the extreme points of the peak
! Assume flat background
    bkgd = 0.0_dp
    bkgd_range = 0.0_dp
    bkgd_min = max(x(1), pk_min - bkgd_fac*pk_width)
    bkgd_max = min(x(size(x)), pk_max + bkgd_fac*pk_width)
    if (im>1) then
        call IXFintegrate_1d_points (bkgd_m, bkgd_err_m, x, y, e, bkgd_min, pk_min, status)
        bkgd = bkgd + bkgd_m
        bkgd_range = bkgd_range + (pk_min-bkgd_min)
    endif

    if (ip<size(x)) then
        call IXFintegrate_1d_points (bkgd_p, bkgd_err_p, x, y, e, pk_max, bkgd_max, status)
        bkgd = bkgd + bkgd_p
        bkgd_range = bkgd_range + (bkgd_max-pk_max)
    endif
    
    if (im>1 .or. ip<size(x)) then  ! background from at least one side
        bkgd = bkgd / bkgd_range
    endif

! Perform moment analysis on the peak after subtracting the background
!   Fill arrays with peak only:
    allocate (xint(ip-im+1), yint(ip-im+1), eint(ip-im+1))
    xint = x(im:ip)
    yint = y(im:ip) - bkgd
    eint = e(im:ip)

!   FWHH:
    ipk_int = ipk-im+1  ! peak position in internal array
    hby2 = 0.5_dp*yint(ipk_int)
    if (yint(size(yint))<hby2) then
        do i=ipk_int,size(yint)
            if (yint(i)<hby2) then
                ip1 = i-1   ! after this point the intensity starts to go below half-height
                goto 901
            endif
        end do
901     do i=size(yint),ipk_int,-1
            if (yint(i)>hby2) then
                ip2 = i+1   ! point closest to peak after which the intensity is always below half height
                goto 902
            endif
        end do
902     xp_hh = xint(ip2) + (xint(ip1)-xint(ip2))*((hby2-yint(ip2))/(yint(ip1)-yint(ip2)))
    else
        xp_hh = xint(size(yint))
    endif

    if (yint(1)<hby2) then
        do i=ipk_int,1,-1
            if (yint(i)<hby2) then
                im1 = i+1   ! after this point the intensity starts to go below half-height
                goto 911
            endif
        end do
911     do i=1,ipk_int
            if (yint(i)>hby2) then
                im2 = i-1   ! point closest to peak after which the intensity is always below half height
                goto 912
            endif
        end do
912     xm_hh = xint(im2) + (xint(im1)-xint(im2))*((hby2-yint(im2))/(yint(im1)-yint(im2)))
    else
        xm_hh = xint(1)
    endif
    c_fwhm = 0.5*(xp_hh + xm_hh)
    h = ypk - bkgd
    w = xp_hh - xm_hh

! area:
    yint = y(im:ip) - bkgd
    call IXFintegrate_1d_points (area, dummy, xint, yint, eint, pk_min, pk_max, status)
! first moment:
    yint = (y(im:ip)-bkgd)*x(im:ip)
    call IXFintegrate_1d_points (xbar, dummy, xint, yint, eint, pk_min, pk_max, status)
    xbar = xbar / area
! second moment:
    yint = (y(im:ip)-bkgd)*((x(im:ip)-xbar)**2)
    call IXFintegrate_1d_points (sig, dummy, xint, yint, eint, pk_min, pk_max, status)
    sig = sqrt(abs(sig) / area)
! third moment:
    yint = (y(im:ip)-bkgd)*((x(im:ip)-xbar)**3)
    call IXFintegrate_1d_points (gam1, dummy, xint, yint, eint, pk_min, pk_max, status)
    gam1 = gam1 / area
! fourth moment:
    yint = (y(im:ip)-bkgd)*((x(im:ip)-xbar)**4)
    call IXFintegrate_1d_points (gam2, dummy, xint, yint, eint, pk_min, pk_max, status)
    gam2 = gam2 / area
    
    deallocate(xint, yint, eint)
    
    return
    end subroutine get_moments
    
end module IXMmoments_utils
