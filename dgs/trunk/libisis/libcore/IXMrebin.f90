module IXMrebin
  use IXMtype_definitions
  use IXMstatus
  use IXMindex

contains

  subroutine IXFrebin_1d_hist( xin, s_in_int, e_in_int, xout, s_out, eout,xdist,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Rebins histogram data.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First formal release	
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT (mandatory):
    !	xin(:)	real		Input histogram bin boundaries (run from 1 to MX+1)
    !	yin(:)	real		Input signal (runs from 1 to MX)
    !	ein(:)	real		Input error bars on signal (runs from 1 to MX)
    !	xout(:)	real		Output histogram bin boundaries (run from 1 to NX+1)
    !	xdist	logical		xdistribution flag
    !
    ! OUTPUT (mandatory):
    !	status			Error flag 
    !	yout(:)	real		Output signal (runs from 1 to NX)
    !	eout(:)	real		Output error bars on signal (runs from 1 to NX)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: xin(:),xout(:)
    real(dp), intent(in),target:: s_in_int(:), e_in_int(:)
    real(dp), intent(out) ::  s_out(:), eout(:)
    type(IXTstatus) :: status
    logical,intent(in) :: xdist
    integer(i4b) :: mx, nx, iin, iout,i
    real(dp),pointer::s_in(:),e_in(:)

    !nx :: no. of input signal bins
    !mx :: no. of output signal bins 

    ! Perform checks on input parameters:
    ! ---------------------------------------
    mx = size(s_in_int)
    if ((mx < 1) .or. (size(xin) /= mx+1) .or. (size(e_in_int) /= mx)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of input arrays (IXFrebin_1d_hist)')
    endif

    nx = size(s_out)
    if ((nx < 1) .or. (size(xout) /= nx+1) .or. (size(eout) /= nx)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of output arrays (IXFrebin_1d_hist)')

    endif

    if(status == IXCseverity_error) return

    ! Get integration ranges:
    ! --------------------------
    s_out = 0.0_dp
    eout = 0.0_dp

    iin = max(1, IXFupper_index(xin, xout(1)))
    iout= max(1, IXFupper_index(xout, xin(1)))
    if ((iin == mx+1) .or. (iout == nx+1)) return	! guarantees that there is an overlap between XIN and XOUT

    if(xdist)then
      s_in=>s_in_int
      e_in=>e_in_int
    else
      allocate(s_in(size(s_in_int)))
      allocate(e_in(size(e_in_int)))
      do i=1,size(s_in_int)
        s_in(i)=s_in_int(i)/(xin(i+1)-xin(i))
        e_in(i)=e_in_int(i)/(xin(i+1)-xin(i))
      enddo
    endif
      
    

    if (xdist) then
       ! if distribution data
10     s_out(iout) = s_out(iout) + (min(xout(iout+1),xin(iin+1)) - max(xout(iout),xin(iin))) * s_in(iin)
       eout(iout) = eout(iout) + ((min(xout(iout+1),xin(iin+1)) - max(xout(iout),xin(iin))) * e_in(iin))**2
       if (xout(iout+1) >= xin(iin+1)) then
          if (iin < mx) then
             iin = iin + 1
             goto 10
          endif
          s_out(iout) = s_out(iout) / (xout(iout+1)-xout(iout))		! end of input array reached
          eout(iout) = sqrt(eout(iout)) / (xout(iout+1)-xout(iout))
       else
          s_out(iout) = s_out(iout) / (xout(iout+1)-xout(iout))
          eout(iout) = sqrt(eout(iout)) / (xout(iout+1)-xout(iout))
          if (iout < nx) then
             iout = iout + 1
             goto 10
          endif
       endif
    else
       ! if not distribution data no multiplying or dividing of binwidths - simple stepping through arrays

20     s_out(iout) = s_out(iout) + (min(xout(iout+1),xin(iin+1)) - max(xout(iout),xin(iin))) * s_in(iin)
       eout(iout) = eout(iout) + ((min(xout(iout+1),xin(iin+1)) - max(xout(iout),xin(iin))) * e_in(iin))**2
       if (xout(iout+1) >= xin(iin+1)) then
          if (iin < mx) then
             iin = iin + 1
             goto 20
          endif
          !yout(iout) = s_out(iout) / (xout(iout+1)-xout(iout))		! end of input array reached
          eout(iout) = sqrt(eout(iout)) 
       else
          !yout(iout) = s_out(iout) / (xout(iout+1)-xout(iout))
          eout(iout) = sqrt(eout(iout)) 
          if (iout < nx) then
             iout = iout + 1
             goto 20
          endif
       endif
    deallocate(s_in)
    deallocate(e_in)
    endif

    return

  end subroutine IXFrebin_1d_hist
  !********************************************************************************************************
  !********************************************************************************************************
  !********************************************************************************************************
  subroutine IXFrebinX_2d_hist( x_in, s_in_int, e_in_int, x_out, s_out, e_out,xdist,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Rebins histogram data.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First formal release	
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT (mandatory):
    !	x_in(:)	real		Input histogram bin boundaries (run from 1 to MX+1)
    !	yin(:)	real		Input signal (runs from 1 to MX)
    !	e_in(:)	real		Input error bars on signal (runs from 1 to MX)
    !	x_out(:)	real		Output histogram bin boundaries (run from 1 to NX+1)
    !	xdist	logical		xdistribution flag
    !
    ! OUTPUT (mandatory):
    !	status			Error flag 
    !	yout(:)	real		Output signal (runs from 1 to NX)
    !	e_out(:)	real		Output error bars on signal (runs from 1 to NX)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: x_in(:),x_out(:)
    real(dp),intent(in),target:: s_in_int(:,:), e_in_int(:,:)
    real(dp), intent(out) :: s_out(:,:), e_out(:,:)
    type(IXTstatus) :: status
    logical,intent(in) :: xdist
    integer(i4b) :: mx, nx, iin, iout,i
    real(dp),pointer::s_in(:,:),e_in(:,:)

    !mx :: no. of input signal bins along X dimension
    !nx :: no. of output signal bins along X dimension 

    ! Perform checks on input parameters:
    ! ---------------------------------------
    mx = size(s_in_int,1)
    if ((mx < 1) .or. (size(x_in) /= mx+1) .or. (size(e_in_int,1) /= mx)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of input arrays (IXFrebin_2d_hist)')
    endif

    nx = size(s_out,1)
    if ((nx < 1) .or. (size(x_out) /= nx+1) .or. (size(e_out,1) /= nx)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of output arrays (IXFrebin_2d_hist)')

    endif

    if(status == IXCseverity_error) return

    ! Get integration ranges:
    ! --------------------------
    s_out = 0.0_dp
    e_out = 0.0_dp

    iin = max(1, IXFupper_index(x_in, x_out(1)))
    iout= max(1, IXFupper_index(x_out, x_in(1)))
    if ((iin == mx+1) .or. (iout == nx+1)) return	! guarantees that there is an overlap between x_in and x_out

    if(xdist)then
      s_in=>s_in_int
      e_in=>e_in_int
    else
      allocate(s_in(size(s_in_int,1),size(s_in_int,2)))
      allocate(e_in(size(s_in_int,1),size(s_in_int,2)))
      do i=1,size(s_in_int,2)
        s_in(:,i)=s_in_int(:,i)/(x_in(2:size(x_in))-x_in(1:size(x_in)-1))
        e_in(:,i)=e_in_int(:,i)/(x_in(2:size(x_in))-x_in(1:size(x_in)-1))
      enddo
    endif
      

    if (xdist) then
       ! if distribution data

       ! will be faster to do in a loop 
       do i=1,size(s_out,2)

10        s_out(iout,i) = s_out(iout,i) + (min(x_out(iout+1),x_in(iin+1)) - max(x_out(iout),x_in(iin))) * s_in(iin,i)
          e_out(iout,i) = e_out(iout,i) + ((min(x_out(iout+1),x_in(iin+1)) - max(x_out(iout),x_in(iin))) * e_in(iin,i))**2
          if (x_out(iout+1) >= x_in(iin+1)) then
             if (iin < mx) then
                iin = iin + 1
                goto 10
             endif
             s_out(iout,i) = s_out(iout,i) / (x_out(iout+1)-x_out(iout))		! end of input array reached
             e_out(iout,i) = sqrt(e_out(iout,i)) / (x_out(iout+1)-x_out(iout))
          else
             s_out(iout,i) = s_out(iout,i) / (x_out(iout+1)-x_out(iout))
             e_out(iout,i) = sqrt(e_out(iout,i)) / (x_out(iout+1)-x_out(iout))
             if (iout < nx) then
                iout = iout + 1
                goto 10
             endif
          endif

          !need to reset values of iin and iout as we have been changing them during the rebinning process
          iin = max(1, IXFupper_index(x_in, x_out(1)))
          iout= max(1, IXFupper_index(x_out, x_in(1)))

       enddo

    else
       ! if not distribution data no multiplying or dividing of binwidths - simple stepping through arrays

20     s_out(iout,:) = s_out(iout,:) + (min(x_out(iout+1),x_in(iin+1)) - max(x_out(iout),x_in(iin))) * s_in(iin,:)
       e_out(iout,:) = e_out(iout,:) + ((min(x_out(iout+1),x_in(iin+1)) - max(x_out(iout),x_in(iin))) * e_in(iin,:))**2

       if (x_out(iout+1) >= x_in(iin+1)) then
          if (iin < mx) then
             iin = iin + 1
             goto 20
          endif
          e_out(iout,:)=sqrt(e_out(iout,:))
       else
          e_out(iout,:)=sqrt(e_out(iout,:))
          if (iout < nx) then
             iout = iout + 1
             goto 20
          endif
       endif

    endif

    return

  end subroutine IXFrebinX_2d_hist



  !********************************************************************************************************
  !********************************************************************************************************
  !********************************************************************************************************
  subroutine IXFrebinY_2d_hist( yin, s_in_int, e_in_int, yout, s_out, e_out,ydist,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Rebins histogram data.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First formal release	
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT (mandatory):
    !	x_in(:)	real		Input histogram bin boundaries (run from 1 to MX+1)
    !	yin(:)	real		Input signal (runs from 1 to MX)
    !	e_in(:)	real		Input error bars on signal (runs from 1 to MX)
    !	x_out(:)	real		Output histogram bin boundaries (run from 1 to NX+1)
    !	xdist	logical		xdistribution flag
    !
    ! OUTPUT (mandatory):
    !	status			Error flag 
    !	yout(:)	real		Output signal (runs from 1 to NX)
    !	e_out(:)	real		Output error bars on signal (runs from 1 to NX)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: yin(:),yout(:)
    real(dp),target,intent(in):: s_in_int(:,:), e_in_int(:,:)
    real(dp), intent(out) :: s_out(:,:), e_out(:,:)
    type(IXTstatus) :: status
    logical,intent(in) :: ydist
    integer(i4b) :: my, ny, iin, iout,i
    real(dp),pointer:: s_in(:,:),e_in(:,:)

    !mx :: no. of input signal bins along Y dimension
    !nx :: no. of output signal bins along Y dimension 

    ! Perform checks on input parameters:
    ! ---------------------------------------
    my = size(s_in_int,2)
    if ((my < 1) .or. (size(yin) /= my+1) .or. (size(e_in_int,2) /= my)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of input arrays (IXFrebin_2d_hist)')
    endif

    ny = size(s_out,2)
    if ((ny < 1) .or. (size(yout) /= ny+1) .or. (size(e_out,2) /= ny)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of output arrays (IXFrebin_2d_hist)')

    endif

    if(status == IXCseverity_error) return

    ! Get integration ranges:
    ! --------------------------
    s_out = 0.0_dp
    e_out = 0.0_dp

    iin = max(1, IXFupper_index(yin, yout(1)))
    iout= max(1, IXFupper_index(yout, yin(1)))
    if ((iin == my+1) .or. (iout == ny+1)) return	! guarantees that there is an overlap between x_in and x_out

    if(ydist)then
      s_in=>s_in_int
      e_in=>e_in_int
    else
      allocate(s_in(size(s_in_int,1),size(s_in_int,2)))
      allocate(e_in(size(s_in_int,1),size(s_in_int,2)))
      do i=1,size(s_in_int,1)
        s_in(i,:)=s_in_int(i,:)/(yin(2:size(yin))-yin(1:size(yin)-1))
        e_in(i,:)=e_in_int(i,:)/(yin(2:size(yin))-yin(1:size(yin)-1))
      enddo
    endif



    if (ydist) then
       ! if distribution data

       ! will be faster to do in a loop 
       do i=1,size(s_out,1)

10        s_out(i,iout) = s_out(i,iout) + (min(yout(iout+1),yin(iin+1)) - max(yout(iout),yin(iin))) * s_in(i,iin)
          e_out(i,iout) = e_out(i,iout) + ((min(yout(iout+1),yin(iin+1)) - max(yout(iout),yin(iin))) * e_in(i,iin))**2
          if (yout(iout+1) >= yin(iin+1)) then
             if (iin < my) then
                iin = iin + 1
                goto 10
             endif
             s_out(i,iout) = s_out(i,iout) / (yout(iout+1)-yout(iout))		! end of input array reached
             e_out(i,iout) = sqrt(e_out(i,iout)) / (yout(iout+1)-yout(iout))
          else
             s_out(i,iout) = s_out(i,iout) / (yout(iout+1)-yout(iout))
             e_out(i,iout) = sqrt(e_out(i,iout)) / (yout(iout+1)-yout(iout))
             if (iout < ny) then
                iout = iout + 1
                goto 10
             endif
          endif

          !need to reset values of iin and iout as we have been changing them during the rebinning process
          iin = max(1, IXFupper_index(yin, yout(1)))
          iout= max(1, IXFupper_index(yout, yin(1)))

       enddo

    else
       ! if not distribution data no multiplying or dividing of binwidths - simple stepping through arrays

20     s_out(:,iout) = s_out(:,iout) + (min(yout(iout+1),yin(iin+1)) - max(yout(iout),yin(iin))) * s_in(:,iin)
       e_out(:,iout) = e_out(:,iout) + ((min(yout(iout+1),yin(iin+1)) - max(yout(iout),yin(iin))) * e_in(:,iin))**2

       if (yout(iout+1) >= yin(iin+1)) then
          if (iin < my) then
             iin = iin + 1
             goto 20
          endif
          e_out(:,iout)=sqrt(e_out(:,iout))
       else
          e_out(:,iout)=sqrt(e_out(:,iout))
          if (iout < ny) then
             iout = iout + 1
             goto 20
          endif
       endif

    endif

    return

  end subroutine IXFrebinY_2d_hist

  !********************************************************************************************************
  !********************************************************************************************************
  !********************************************************************************************************
  subroutine IXFrebin_1d_hist_get_arr (xbounds, x_in, n_out, x_out,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Obtains the new bin boundaries for rebinning histogram data using the onformation in the array XBOUNDS.
    ! XBOUNDS is an array of boundaries and intervals. Linear or logarithmic rebinning can be accommodated
    ! by conventionally specifying the rebin interval as positive or negative respectively:
    !
    !   e.g. XBOUNDS = (2000,10,3000)  rebins from 2000 to 3000 in bins of 10
    !
    !   e.g. XBOUNDS = (5,-0.01,3000)  rebins from 5 to 3000 with logarithmically spaced bins i.e. with
    !                                 width equal to 0.01 the previous bin boundary 
    !
    !  The conventions can be mixed on one line:
    !
    !   e.g. XBOUNDS = (5,-0.01,1000,20,4000,50,20000)
    !
    ! If the original bins in an interval are required, then set the interval to zero:
    !
    !   e.g. XBOUNDS = (5,50,1000,0,4000,50,20000)
    !      -----------------------^
    !
    ! Where the original bins are required for at least one interval, those boundaries, XIN, are required
    !
    ! The output bin boundaries are optional if it is desired only to get the number or new boundaries n_out. This
    ! is useful for allocating arrays of just the right length, and then the routine can be called again (e.g.
    ! when interfacing to MATLAB).
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-06-21		First release
    !						2002-08-15		Make IERR the first argument
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    ! ------
    !	xbounds(:)	real	Histogram bin boundaries descriptor:
    !						(x_1, del_1, x_2,del_2 ... x_n-1, del_n-1, x_n)
    !						Bin from x_1 to x_2 in units of del_1 etc.
    !						del > 0: linear bins
    !						del < 0: logarithmic binning
    !						del = 0: Use bins from input array
    !						[If only two elements, then interpreted as lower and upper bounds, with DEL=0]
    ! * x_in(:)		real	Input x-array - only needed if DEL=0 for one of the rebin ranges
    ! ** if point data del=0 not possible**
    ! OUTPUT:
    ! --------
    !	status		integer Error flag
    ! * n_out		integer	No. of bin boundaries in the rebin array x_out
    !						Note: do NOT rely on being an error flag; always use IERR for this
    ! * x_out(:)	real	Bin boundaries for rebin array.
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: xbounds(:)
    real(dp), intent(in), optional :: x_in(:)
    type(IXTstatus) :: status
    integer(i4b), intent(out), optional :: n_out
    real(dp), intent(out), optional :: x_out(:)

    real(dp), parameter :: small=1.0e-10_dp

    integer(i4b) :: mx, m_in, m_out, i, j, n, ntot, imin, imax
    real(dp) :: xlo, del, xhi, logdel
    logical :: first_time
    character(len=132) :: mess

    ! Perform checks on input parameters:
    ! ---------------------------------------
    mx = size(xbounds)
    if ((mx < 2) .or. (mx > 2 .and. mod(mx,2) /= 1)) then
       mess = 'ERROR: Check size of xbounds array (rebin_1d_hist_get_xarr)'
       goto 99
    endif
    if (present(x_in)) m_in = size(x_in)
    if (present(x_out)) m_out = size(x_out)

    ! Get integration ranges:
    ! --------------------------
    ntot = 1	! total number of bin boundaries in output array (accumulates during algorithm)
    first_time = .TRUE.
    do i = 1, mx/2
       if (mx /= 2) then
          xlo = xbounds(2*i-1)
          del = xbounds(2*i)
          xhi = xbounds(2*i+1)
       else
          xlo = xbounds(1)
          del = 0.0_dp
          xhi = xbounds(2)
       endif

       if (xhi <= xlo) then
          mess = 'ERROR: Check boundaries monotonically increasing (rebin_1d_hist_get_xarr)'
          goto 99
       endif

       if (del > 0.0_dp) then
          n = int((xhi-xlo)/del - small)
          if (xlo+real(n,dp)*del < xhi) n=n+1	! n = no. bin boundaries in addition to XLO (i.e. includes XHI)
          if (present(x_out)) then
             if (ntot+n > m_out) then
                mess = 'ERROR: Output bin boundary array too small (rebin_1d_hist_get_xarr)'
                goto 99
             endif
             x_out(ntot) = xlo
             if (n > 1) then
                do j = 1, n-1
                   x_out(j+ntot) = xlo + real(j,dp)*del
                end do
             endif
          endif
          ntot = ntot + n
       else if (del < 0.0_dp) then
          if (xlo <= 0.0_dp) then
             mess = 'ERROR: Logarithmic bins starting with XLO <= 0 forbidden (rebin_1d_hist_get_xarr)'
             goto 99
          endif
          logdel = log(1.0_dp-del)
          n = int(log(xhi/xlo)/logdel - small)
          if (xlo*exp(real(n,dp)*logdel) < xhi) n=n+1
          if (present(x_out)) then
             if (ntot+n > m_out) then
                mess = 'ERROR: Output bin boundary array too small (rebin_1d_hist_get_xarr)'
                goto 99
             endif
             x_out(ntot) = xlo
             if (n > 1) then
                do j = 1, n-1
                   x_out(j+ntot) = xlo*exp(real(j,dp)*logdel)
                end do
             endif
          endif
          ntot = ntot + n
       else
          !  Check that input array is present and monotonically increasing:
          if (first_time) then
             if (.not. present(x_in)) then
                mess = 'ERROR: No input x array provided to supply bin boundaries (rebin_1d_hist_get_xarr)'
                goto 99
             endif
             first_time = .FALSE.
             if (m_in > 1) then
                if (minval(x_in(2:m_in)-x_in(1:m_in-1)) <= 0.0_dp) then
                   mess = 'ERROR: Input x array is not strictly monotonic increasing'
                   goto 99
                endif
             endif
          endif
          !	Get lower and upper indicies of input array of bin boundaries such that xlo < x_in(imin) < x_in(imax) < xhi:
          imin = IXFlower_index(x_in, xlo)
          imax = IXFupper_index(x_in, xhi)
          if (imin <= m_in .and. imax >= 1) then
             if (x_in(imin)==xlo) imin = imin + 1
             if (x_in(imax)==xhi) imax = imax - 1
             n = imax - imin + 2	! n is the number of extra bin boundaries that will be added (including XHI)
          else
             n = 1
          endif
          if (present(x_out)) then
             if (ntot+n > m_out) then
                mess = 'ERROR: Output bin boundary array too small (rebin_1d_hist_get_xarr)'
                goto 99
             endif
             x_out(ntot) = xlo
             if (n > 1) x_out(ntot+1:ntot+n-1) = x_in(imin:imax)	! ntot+n => ntot+n-1 TGP (2003-12-05)
          endif
          ntot = ntot + n
       endif
    end do
    if (present(n_out)) n_out = ntot
    if (present(x_out)) then
       x_out(ntot) = xhi
       if (ntot < m_out) x_out(ntot+1:m_out) = 0.0_dp
    endif

    return


    !-------------------------------------------------------------------------------------------
99  call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, mess) 

    if (present(n_out)) n_out = 0_i4b	! do not set X_OUT=0.0_dp to avoid initialising a possibly long array

    if (status == IXCseverity_error) return

  end subroutine IXFrebin_1d_hist_get_arr


  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  ! BULLSHIT routine which does not work properly and is also meaningless


  subroutine IXFrebin_points(px_in, xin, s_in, ein, xout, s_out, eout,status)

    implicit none

    real(dp), intent(in) :: xin(:), s_in(:), ein(:),px_in(:)
    real(dp), intent(out) ::xout(:) , s_out(:), eout(:)
    type(IXTstatus) :: status
    real(dp)::xtot
    integer(i4b) :: mx, nx, iin, iout, npix,inused

    !nx :: no. of input signal bins
    !mx :: no. of output signal bins 

    ! Perform checks on input parameters:
    ! ---------------------------------------
    mx = size(s_in)
    if ((mx < 1) .or. (size(xin) /= mx) .or. (size(ein) /= mx)) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'Check sizes of input arrays (IXFrebin_1d_hist)')
    endif

    nx = size(s_out)
    !    if ((nx < 1) .or. (size(xout) /= nx) .or. (size(eout) /= nx)) then
    !
    !       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    !            IXCerr_outofmem, 'Check sizes of output arrays (IXFrebin_1d_hist)')
    !
    !    endif

    if(status == IXCseverity_error) return


    ! Get integration ranges:
    ! --------------------------
    s_out = 0.0_dp
    eout = 0.0_dp
    xout = 0.0_dp

    iin = max(1, IXFupper_index(xin, px_in(1))) +1 !from debugger

    iout= max(1, IXFupper_index(px_in, xin(1)))  !OK with point data

    if ((iin == mx+1) .or. (iout == nx+1)) return	! guarantees that there is an overlap between XIN and XOUT

    ! if not distribution data no multiplying or dividing of binwidths - simple stepping through arrays
    npix=0
    xtot=0.0_dp
    inused=0

20  if (px_in(iout+1) > xin(iin)) then
       s_out(iout) = s_out(iout) +  s_in(iin)
       eout(iout) = eout(iout) +  ein(iin)**2
       xtot=xtot+xin(iin)
       npix=npix+1
       inused=1
    endif
    if((inused==0) .and. (iin == mx) )return
    if (px_in(iout+1) >= xin(iin+1)) then
       if (iin < mx) then
          iin = iin + 1
          inused=0
          goto 20
       endif

       s_out(iout)=s_out(iout)/dble(npix)
       eout(iout)=sqrt(eout(iout))/dble(npix)


    else
       if(s_out(iout)==0)then
          if(npix==0)then
             xout(iout)=0.0_dp
          else
             xout(iout)=xtot/dble(npix)
          endif
          eout(iout)=0.0_dp
       else
          xout(iout)=xtot/dble(npix)
          eout(iout)=sqrt(eout(iout))/dble(npix)
          s_out(iout)=s_out(iout)/dble(npix)
       endif
       npix=0
       xtot=0.0_dp

       if (iout < nx) then
          iout = iout + 1
          if(inused == 1)then
             iin=iin+1 
             inused=0
          endif
          goto 20
       endif
    endif

  end subroutine IXFrebin_points




end module IXMrebin

