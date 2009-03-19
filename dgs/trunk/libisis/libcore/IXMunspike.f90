!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMunspike
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 1240 $ ($Date: 2007-09-25 11:30:45 -0400 (Tue, 25 Sep 2007) $)
!!
!! FORTRAN removal of spikes from array of data points

module IXMunspike
  use IXMtype_definitions
  use IXMstatus
  implicit none
contains
    !! Remove spikes from a 1D point data set.
    subroutine IXFunspike_1d (status, x_in, y_in, e_in, y_out, e_out, ymin, ymax, fac, sfac, nbad)
!-----------------------------------------------------------------------------------------------------------------------------------
! T.G.Perring July 2001     2002-08-15      First formal release
!                           2006-04-20      Modified for LIBISIS
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Remove spikes from a 1D point data set.
!
! INPUT: [Optional arguments are marked with * below]
!   x_in(1:n)   real        point positions
!   y_in(1:n)   real        Intensity
!   e_in(1:n)   real        Standard deviation
! * ymin        real        Minimum acceptable value (i.e. underflow)
! * ymax        real        Maximum acceptable value (i.e. overflow)
! * fac         real        Intensity deviation factor
! * sfac        real        Standard deviation discrimination factor
!
! OUTPUT: [Optional arguments are marked with * below]
!   status      IXTstatus   Status
!   y_out(1:n)  real        Unspiked intensity
!   e_out(1:n)  real        Unspiked standard deviations
! * nbad        int         Number of data points removed by the algorithm
!
! Removes spikes and overflows from a spectrum, where a spike is a single point that
! is markedly higher or lower than its immediate neighbours. The signal is replaced by the
! linear interpolation between the nearest unremoved neighbouring channels. Before this
! filtering is perfomed, those points which lie outside the range YMIN to YMAX are
! removed (if one or both of these limits are given). 
!
! Define a spike as a point that: 
!  (1) is smaller or larger than both its neighbours, and differs by more
!     than SFAC standard deviations from each of those neighbours
! AND AT THE SAME TIME
!  (2) if it value has the same sign as both its neighbours, it differs by more than a factor FAC
!     in absolute magnitude from both its neighbours.
!
! The algorithm assumes that spikes are isolated.
!
! The second condition is to avoid the summit point of a high statistics peak being treated as
! a spike. It is not entirely robust - if the yscale is offset by 90% of the peak so that only the
! top three points are above the x-axis, the 1st and 3rd only just above, then condition (2)
! will be fooled and the central point considered a spike. However, for the purposes for which
! this routine is intended (raw counts or first-line analysis) this circumstance will not
! usually arise.
!
!
!
!-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    type(IXTstatus),intent(inout) :: status !! error status flag
    real(dp), intent(in) :: x_in(:)         !! input x array
    real(dp), intent(in) :: y_in(:)         !! input intensity array
    real(dp), intent(in) :: e_in(:)         !! input error array
    real(dp), intent(in), optional :: ymin  !! underflow bound
    real(dp), intent(in), optional :: ymax  !! overflow bound
    real(dp), intent(in), optional :: fac   !! spike intensity factor criterion
    real(dp), intent(in), optional :: sfac  !! spike error deviation criterion
    real(dp), intent(out) :: y_out(:)       !! unspiked intensity array
    real(dp), intent(out) :: e_out(:)       !! unspiked error array
    integer(i4b), intent(out), optional :: nbad !! number of points removed by algorithm

    integer(i4b) n, n_keep, n_keep_new, i, j, k, jp, jn
    integer(i4b) ind0(size(x_in))
    real(dp) fac_in, sfac_in, dx_lo, dx_hi
    logical keep(size(x_in)), keep_fac, keep_sfac

    integer(i4b), allocatable :: ind(:)

! Remove all underflows and overflows, and fill packed array of indicies of unmasked signal:
    n = size(x_in)
    keep = .TRUE.
    if (present(ymin))then
      if (.not. isnan(ymin))then
        where (y_in < ymin) keep = .FALSE.
      endif
    endif
    if (present(ymax)) then
      if(.not. isnan(ymax))then
        where (y_in > ymax) keep = .FALSE.
      endif
    endif
    n_keep = count(keep)
    if (n_keep == 0) then   ! all points have been removed
        y_out = 0.0_dp
        e_out = 0.0_dp
        if (present(nbad)) nbad = n
        return
    else
        ind0 = (/(i,i=1,n)/)
        allocate(ind(n_keep))
        ind = pack (ind0, keep)
    endif
    ! At this point, we may have just one point left
    
! Now identify spikes, and refill packed array of indicies of unmasked signal:
! This block will be skipped if either 1 or 2 points left
    if ((n_keep > 2) .and. (present(fac) .or. present(sfac))) then
        if (present(fac))   fac_in = max(abs(fac),1.0_dp)       !ensure positive, and at least unity
        if (present(sfac)) sfac_in = abs(sfac)
        do i = 2, n_keep-1
            jp = ind(i-1)
            j  = ind(i)
            jn = ind(i+1)
            if (  (y_in(j) > max(y_in(jp),y_in(jn))) .or. (y_in(j) < min(y_in(jp),y_in(jn))) ) then
                keep_sfac = .FALSE.
                if ( present(sfac) ) &
     &              keep_sfac = (abs(y_in(j)-y_in(jp)) < sfac_in*sqrt(e_in(j)**2+e_in(jp)**2))  &
     &                      .or.(abs(y_in(j)-y_in(jn)) < sfac_in*sqrt(e_in(j)**2+e_in(jn)**2))
                keep_fac = .FALSE.
                if ( present(fac) ) &
     &              keep_fac = .not. ( (min(y_in(jp),y_in(j),y_in(jn))>0.0_dp .or. max(y_in(jp),y_in(j),y_in(jn))<0.0_dp) &
     &                      .and. (abs(y_in(j))>max(abs(y_in(jp)),abs(y_in(jn)))*fac_in &
     &                       .or.  abs(y_in(j))<min(abs(y_in(jp)),abs(y_in(jn)))/fac_in) )
                keep(j) = keep_sfac .or. keep_fac
            endif
        end do
        n_keep_new = count(keep)
        if (n_keep_new == 0) then   ! all points have been removed
            y_out = 0.0_dp
            e_out = 0.0_dp
            if (present(nbad)) nbad = n
            return
        elseif (n_keep_new /= n_keep) then  ! further points were removed
            n_keep = n_keep_new
            deallocate (ind)
            allocate(ind(n_keep))
            ind = pack (ind0, keep)
        endif
    endif

! Fill output arrays (we have at least one retained value if reached this point):
    y_out = y_in    ! we assume that only a few points are spiked, so fill entire array
    e_out = e_in
    if (present(nbad)) nbad = n - n_keep

    if (ind(1) /= 1) then
        y_out(1:ind(1)-1) = y_in(ind(1))
        e_out(1:ind(1)-1) = e_in(ind(1))
    endif

    if (n_keep > 1) then
        j = ind(1)
        do i = 2, n_keep
            jp = j
            j = ind(i)
            if (j /= jp+1) then ! some points masked between the retained points
                do k = jp+1, j-1
                    dx_lo = x_in(k) - x_in(jp)
                    dx_hi = x_in(j) - x_in(k)
                    y_out(k) = (dx_hi*y_in(jp) + dx_lo*y_in(j)) / (x_in(j)-x_in(jp))
                    e_out(k) = sqrt((dx_hi*e_in(jp))**2 + (dx_lo*y_in(j))**2) / (x_in(j)-x_in(jp))
                end do
            endif
        end do
    endif

    if (ind(n_keep) /= n) then
        y_out(ind(n_keep)+1:n) = y_in(ind(n_keep))
        e_out(ind(n_keep)+1:n) = e_in(ind(n_keep))
    endif

    return
    end subroutine IXFunspike_1d

end module IXMunspike
