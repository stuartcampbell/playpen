module IXMregroup
  use IXMtype_definitions
  use IXMindex
  use IXMstatus

contains

  subroutine IXFregroup_1d_hist ( xmin, delta, xmax,xdist, xin, yin, ein, xout, yout, eout, nout,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Regroups histogram data into bins with minimum bin size determined by DELTA.
    !
    ! If DELTA +ve: then the bins are linear i.e. xout(i+1) >= xout(i) + delta
    ! If DELTA -ve:	then the bins are logarithmic i.e. xout(i+1) >= xout(i)*(1+delta)
    !
    ! The value of xout(i+1) is chosen to be the smallest xin(j) that satisfies the RHS of the
    ! equations above. Each of the new bin bondaries therefore always conincides with an input
    ! bin boundary. This ensures that the data in output bins is uncorrelated with the
    ! data in its neighbours. There has to be at least one input histogram bin entirely
    ! contained within the range XMIN to XMAX i.e.
    !
    !          xmin =< xout(1) < xout(nout+1) =< xmax
    ! 
    ! SYNTAX:
    ! -------
    !  The subroutine can be used to determine the number of bins needed for the output without
    ! any input or output data. This is useful if one wants to allocate storage of the correct
    ! length beforehand. The valid useages are:
    !
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, xin, nout=nout)
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, xin, yin, ein, xout, yout, eout)
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, xin, yin, ein, xout, yout, eout, nout)
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First release
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    ! -------
    !	delta	real		Determines output bin size:
    !                        DELTA +ve:  xout(i+1) >= xout(i) + delta
    !                        DELTA -ve:	 xout(i+1) >= xout(i)*(1+delta)
    !	xin(:)	real		Input histogram bin boundaries (run from 1 to NIN+1)
    ! * yin(:)	real		Input signal (runs from 1 to NIN)
    ! * ein(:)	real		Input error bars on signal (runs from 1 to NIN)
    !   xdist	logical		distribtuon data flag
    !
    !
    ! OUTPUT:
    ! -------
    !	status	IXTstatus	Error flag
    ! * xout(:)	real		Output histogram bin boundaries (run from 1 to NOUT+1)
    ! * yout(:)	real		Output signal (runs from 1 to NOUT)
    ! * eout(:)	real		Output error bars on signal (runs from 1 to NOUT)
    ! * nout	integer		Number of histogram bins filled
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: xmin, delta, xmax, xin(:)
    real(dp), intent(in), optional :: yin(:), ein(:)
    real(dp), intent(out), optional :: xout(:), yout(:), eout(:)
    logical,intent(in) :: xdist
    type(IXTstatus) :: status
    integer(i4b), intent(out), optional :: nout

    integer(i4b):: nin, nout_max, i, j, imin, imax, iout
    real(dp) :: xnew
    character(len=132) :: mess

    ! Perform checks on input parameters:
    ! ---------------------------------------

    ! Check consistency of XMIN, DELTA, XMAX:
    if (xmin >= xmax) then						! check XMIN < XMAX
       mess = 'Check that XMIN < XMAX (regroup_1d_hist)'
       goto 999
    endif

    if (xmin <= 0.0_dp .and. delta < 0.0_dp) then	! cannot have logarithmic bins and XMIN negative
       mess = 'Check XMIN > 0 for logarithmic bins (regroup_1d_hist)'
       goto 999
    endif

    ! Check that have just XIN or all of XIN, YIN, EIN, and consistency of array lengths:
    nin = size(xin)-1	! no. input bins
    if (present(yin) .and. present(ein)) then
       if ((nin < 1) .or. (size(yin) /= nin) .or. (size(ein) /= nin)) then
          mess = 'Check sizes of input histogram data arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not. (present(yin) .or. present(ein))) then
       if (nin < 1) then
          mess = 'Check length of input histogram bin boundaries array (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have either XIN or XIN, YIN, XIN as input data arrays (IXFregroup_1d_hist)'
       goto 999
    endif

    ! Check that have all of XOUT, YOUT, EOUT, or NOUT, or all four:
    if (present(xout) .and. present(yout) .and. present(eout)) then
       nout_max = min(size(xout)-1,size(yout),size(eout))	! must be able to hold at least one bin
       if (nout_max < 1) then
          mess = 'Check sizes of output arrays (regroup_1d_hist)'
          goto 999
       endif
       if (.not. (present(yin) .and. present(ein))) then
          mess = 'Must have input data arrays as well as output arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not.(present(xout) .or. present(yout) .or. present(eout))) then
       if (.not. present(nout)) then
          mess = 'Must have at least one of NOUT and all output arrays XOUT, YOUT, EOUT (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have all of the output arrays XOUT, YOUT, EOUT, or none of them (IXFregroup_1d_hist)'
       goto 999
    endif


    ! Get regroup range:
    ! --------------------------
    imin = IXFlower_index (xin, xmin)
    imax = IXFupper_index (xin, xmax)
    if (imin > nin+1 .or. imax == 0 .or. imin == imax) then
       mess ='No complete input bins in range XMIN to XMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    i = imin
    iout = 0


    if (xdist) then
       !if xdistribution data
100    if (delta >= 0.0_dp) then
          xnew = xin(i) + delta
       else
          xnew = xin(i)*(1.0_dp-delta)	! delta < 0 so xnew > xin(i) (we already know xin(i) > xmin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (xin(j) >= xnew) then
             iout = iout + 1
             if (present(xout)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                xout(iout) = xin(i)
                yout(iout) = sum(yin(i:j-1)*(xin(i+1:j)-xin(i:j-1))) / (xin(j)-xin(i))
                eout(iout) = sqrt(sum((ein(i:j-1)*(xin(i+1:j)-xin(i:j-1)))**2)) / (xin(j)-xin(i))
             endif
             i = j
             goto 100
          endif
       end do

    else
       ! if NOT distribution data then no multiplying or dividing of binwidths
200    if (delta >= 0.0_dp) then
          xnew = xin(i) + delta
       else
          xnew = xin(i)*(1.0_dp-delta)	! delta < 0 so xnew > xin(i) (we already know xin(i) > xmin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (xin(j) >= xnew) then
             iout = iout + 1
             if (present(xout)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                xout(iout) = xin(i)
                yout(iout) = sum(yin(i:j-1))
                eout(iout) = sqrt(sum(ein(i:j-1)**2)) 
             endif
             i = j
             goto 200
          endif
       end do

    endif


    if (iout > 0) then
       if (present(nout)) nout = iout
       if (present(xout)) xout(iout+1) = xin(i)
       return
    else		! no complete output bins in the range XMIN to XMAX
       mess = 'No complete output bins in range XMIN to XMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    !----------------------------------------------------------------------------------
    ! Error condition: exit in controlled fashion
999 continue
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, mess) 

    if (present(nout)) nout = 0	! Do not set xout=0, yout=0, eout=0 to avoid initialising possibly large arrays

    if (status == IXCseverity_error) return

    return

  end subroutine IXFregroup_1d_hist




  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************







  subroutine IXFregroupX_2d_hist ( xmin, delta, xmax,xdist, x_in, s_in, e_in, x_out, s_out, e_out, nout,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Regroups histogram data into bins with minimum bin size determined by DELTA.
    !
    ! If DELTA +ve: then the bins are linear i.e. x_out(i+1) >= x_out(i) + delta
    ! If DELTA -ve:	then the bins are logarithmic i.e. x_out(i+1) >= x_out(i)*(1+delta)
    !
    ! The value of x_out(i+1) is chosen to be the smallest x_in(j) that satisfies the RHS of the
    ! equations above. Each of the new bin bondaries therefore always conincides with an input
    ! bin boundary. This ensures that the data in output bins is uncorrelated with the
    ! data in its neighbours. There has to be at least one input histogram bin entirely
    ! contained within the range XMIN to XMAX i.e.
    !
    !          xmin =< x_out(1) < x_out(nout+1) =< xmax
    ! 
    ! SYNTAX:
    ! -------
    !  The subroutine can be used to determine the number of bins needed for the output without
    ! any input or output data. This is useful if one wants to allocate storage of the correct
    ! length beforehand. The valid useages are:
    !
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, x_in, nout=nout)
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, x_in, s_in, e_in, x_out, s_out, e_out)
    !	call regroup_1d_hist (ierr, xmin, delta, xmax, x_in, s_in, e_in, x_out, s_out, e_out, nout)
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First release
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    ! -------
    !	delta	real		Determines output bin size:
    !                        DELTA +ve:  x_out(i+1) >= x_out(i) + delta
    !                        DELTA -ve:	 x_out(i+1) >= x_out(i)*(1+delta)
    !	x_in(:)	real		Input histogram bin boundaries (run from 1 to NIN+1)
    ! * s_in(:)	real		Input signal (runs from 1 to NIN)
    ! * e_in(:)	real		Input error bars on signal (runs from 1 to NIN)
    !   xdist	logical		distribtuon data flag
    !
    !
    ! OUTPUT:
    ! -------
    !	status	IXTstatus	Error flag
    ! * x_out(:)	real		Output histogram bin boundaries (run from 1 to NOUT+1)
    ! * s_out(:)	real		Output signal (runs from 1 to NOUT)
    ! * e_out(:)	real		Output error bars on signal (runs from 1 to NOUT)
    ! * nout	integer		Number of histogram bins filled
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: xmin, delta, xmax, x_in(:)
    real(dp), intent(in), optional :: s_in(:,:), e_in(:,:)
    real(dp), intent(out), optional :: x_out(:), s_out(:,:), e_out(:,:)
    logical,intent(in) :: xdist
    type(IXTstatus) :: status
    integer(i4b), intent(out), optional :: nout

    integer(i4b):: nin, nout_max, i, j,k, imin, imax, iout
    real(dp) :: xnew
    character(len=132) :: mess

    ! Perform checks on input parameters:
    ! ---------------------------------------

    ! Check consistency of XMIN, DELTA, XMAX:
    if (xmin >= xmax) then						! check XMIN < XMAX
       mess = 'Check that XMIN < XMAX (regroup_1d_hist)'
       goto 999
    endif

    if (xmin <= 0.0_dp .and. delta < 0.0_dp) then	! cannot have logarithmic bins and XMIN negative
       mess = 'Check XMIN > 0 for logarithmic bins (regroup_1d_hist)'
       goto 999
    endif

    ! Check that have just XIN or all of XIN, YIN, EIN, and consistency of array lengths:
    nin = size(x_in)-1	! no. input bins
    if (present(s_in) .and. present(e_in)) then
       if ((nin < 1) .or. (size(s_in,1) /= nin) .or. (size(e_in,1) /= nin)) then
          mess = 'Check sizes of input histogram data arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not. (present(s_in) .or. present(e_in))) then
       if (nin < 1) then
          mess = 'Check length of input histogram bin boundaries array (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have either XIN or XIN, YIN, XIN as input data arrays (IXFregroup_1d_hist)'
       goto 999
    endif

    ! Check that have all of XOUT, YOUT, EOUT, or NOUT, or all four:
    if (present(x_out) .and. present(s_out) .and. present(e_out)) then
       nout_max = min(size(x_out)-1,size(s_out,1),size(e_out,1))	! must be able to hold at least one bin
       if (nout_max < 1) then
          mess = 'Check sizes of output arrays (regroup_1d_hist)'
          goto 999
       endif
       if (.not. (present(s_in) .and. present(e_in))) then
          mess = 'Must have input data arrays as well as output arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not.(present(x_out) .or. present(s_out) .or. present(e_out))) then
       if (.not. present(nout)) then
          mess = 'Must have at least one of NOUT and all output arrays XOUT, YOUT, EOUT (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have all of the output arrays XOUT, YOUT, EOUT, or none of them (IXFregroup_1d_hist)'
       goto 999
    endif


    ! Get regroup range:
    ! --------------------------
    imin = IXFlower_index (x_in, xmin)
    imax = IXFupper_index (x_in, xmax)
    if (imin > nin+1 .or. imax == 0 .or. imin == imax) then
       mess ='No complete input bins in range XMIN to XMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    i = imin
    iout = 0


    if (xdist) then
       !if xdistribution data
100    if (delta >= 0.0_dp) then
          xnew = x_in(i) + delta
       else
          xnew = x_in(i)*(1.0_dp-delta)	! delta < 0 so xnew > x_in(i) (we already know x_in(i) > xmin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (x_in(j) >= xnew) then
             iout = iout + 1
             if (present(x_out)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                x_out(iout) = x_in(i)
        !***************************************
            do k=1,size(s_in,2) ! loop thru' Y dimension, so DO NOT need dimension in sum loop      
                s_out(iout,k) = sum(s_in(i:j-1,k)*(x_in(i+1:j)-x_in(i:j-1))) / (x_in(j)-x_in(i))
                e_out(iout,k) = sqrt(sum((e_in(i:j-1,k)*(x_in(i+1:j)-x_in(i:j-1)))**2)) / (x_in(j)-x_in(i))
        !***************************************        
            enddo
             endif
             i = j
             goto 100
          endif
       end do

    else
       ! if NOT distribution data then no multiplying or dividing of binwidths
200    if (delta >= 0.0_dp) then
          xnew = x_in(i) + delta
       else
          xnew = x_in(i)*(1.0_dp-delta)	! delta < 0 so xnew > x_in(i) (we already know x_in(i) > xmin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (x_in(j) >= xnew) then
             iout = iout + 1
             if (present(x_out)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                x_out(iout) = x_in(i)
                s_out(iout,:) = sum(s_in(i:j-1,:),1)
                e_out(iout,:) = sqrt(sum(e_in(i:j-1,:)**2,1)) 
             endif
             i = j
             goto 200
          endif
       end do

    endif


    if (iout > 0) then
       if (present(nout)) nout = iout
       if (present(x_out)) x_out(iout+1) = x_in(i)
       return
    else		! no complete output bins in the range XMIN to XMAX
       mess = 'No complete output bins in range XMIN to XMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    !----------------------------------------------------------------------------------
    ! Error condition: exit in controlled fashion
999 continue
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, mess) 

    if (present(nout)) nout = 0	! Do not set x_out=0, s_out=0, e_out=0 to avoid initialising possibly large arrays

    if (status == IXCseverity_error) return

    return

  end subroutine IXFregroupX_2d_hist



  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************



  subroutine IXFregroupY_2d_hist ( ymin, delta, ymax,ydist, y_in, s_in, e_in, y_out, s_out, e_out, nout,status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Regroups histogram data into bins with minimum bin size determined by DELTA.
    !
    ! If DELTA +ve: then the bins are linear i.e. y_out(i+1) >= y_out(i) + delta
    ! If DELTA -ve:	then the bins are logarithmic i.e. y_out(i+1) >= y_out(i)*(1+delta)
    !
    ! The value of y_out(i+1) is chosen to be the smallest y_in(j) that satisfies the RHS of the
    ! equations above. Each of the new bin bondaries therefore always conincides with an input
    ! bin boundary. This ensures that the data in output bins is uncorrelated with the
    ! data in its neighbours. There has to be at least one input histogram bin entirely
    ! contained within the range XMIN to XMAX i.e.
    !
    !          ymin =< y_out(1) < y_out(nout+1) =< ymax
    ! 
    ! SYNTAX:
    ! -------
    !  The subroutine can be used to determine the number of bins needed for the output without
    ! any input or output data. This is useful if one wants to allocate storage of the correct
    ! length beforehand. The valid useages are:
    !
    !	call regroup_1d_hist (ierr, ymin, delta, ymax, y_in, nout=nout)
    !	call regroup_1d_hist (ierr, ymin, delta, ymax, y_in, s_in, e_in, y_out, s_out, e_out)
    !	call regroup_1d_hist (ierr, ymin, delta, ymax, y_in, s_in, e_in, y_out, s_out, e_out, nout)
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2002-08-15		First release
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    ! -------
    !	delta	real		Determines output bin size:
    !                        DELTA +ve:  y_out(i+1) >= y_out(i) + delta
    !                        DELTA -ve:	 y_out(i+1) >= y_out(i)*(1+delta)
    !	y_in(:)	real		Input histogram bin boundaries (run from 1 to NIN+1)
    ! * s_in(:)	real		Input signal (runs from 1 to NIN)
    ! * e_in(:)	real		Input error bars on signal (runs from 1 to NIN)
    !   ydist	logical		distribtuon data flag
    !
    !
    ! OUTPUT:
    ! -------
    !	status	IXTstatus	Error flag
    ! * y_out(:)	real		Output histogram bin boundaries (run from 1 to NOUT+1)
    ! * s_out(:)	real		Output signal (runs from 1 to NOUT)
    ! * e_out(:)	real		Output error bars on signal (runs from 1 to NOUT)
    ! * nout	integer		Number of histogram bins filled
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: ymin, delta, ymax, y_in(:)
    real(dp), intent(in), optional :: s_in(:,:), e_in(:,:)
    real(dp), intent(out), optional :: y_out(:), s_out(:,:), e_out(:,:)
    logical,intent(in) :: ydist
    type(IXTstatus) :: status
    integer(i4b), intent(out), optional :: nout

    integer(i4b):: nin, nout_max, i, j,k, imin, imax, iout
    real(dp) :: ynew
    character(len=132) :: mess

    ! Perform checks on input parameters:
    ! ---------------------------------------

    ! Check consistency of YMIN, DELTA, YMAX:
    if (ymin >= ymax) then						! check YMIN < YMAX
       mess = 'Check that YMIN < YMAX (regroup_1d_hist)'
       goto 999
    endif

    if (ymin <= 0.0_dp .and. delta < 0.0_dp) then	! cannot have logarithmic bins and YMIN negative
       mess = 'Check YMIN > 0 for logarithmic bins (regroup_1d_hist)'
       goto 999
    endif

    ! Check that have just YIN or all of Y_IN, S_IN, E_IN, and consistency of array lengths:
    nin = size(y_in)-1	! no. input bins
    if (present(s_in) .and. present(e_in)) then
       if ((nin < 1) .or. (size(s_in,2) /= nin) .or. (size(e_in,2) /= nin)) then
          mess = 'Check sizes of input histogram data arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not. (present(s_in) .or. present(e_in))) then
       if (nin < 1) then
          mess = 'Check length of input histogram bin boundaries array (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have either X_IN or X_IN, S_IN, X_IN as input data arrays (IXFregroup_1d_hist)'
       goto 999
    endif

    ! Check that have all of YOUT, S_OUT, EOUT, or NOUT, or all four:
    if (present(y_out) .and. present(s_out) .and. present(e_out)) then
       nout_max = min(size(y_out)-1,size(s_out,2),size(e_out,2))	! must be able to hold at least one bin
       if (nout_max < 1) then
          mess = 'Check sizes of output arrays (regroup_1d_hist)'
          goto 999
       endif
       if (.not. (present(s_in) .and. present(e_in))) then
          mess = 'Must have input data arrays as well as output arrays (IXFregroup_1d_hist)'
          goto 999
       endif
    else if (.not.(present(y_out) .or. present(s_out) .or. present(e_out))) then
       if (.not. present(nout)) then
          mess = 'Must have at least one of NOUT and all output arrays Y_OUT, S_OUT, E_OUT (IXFregroup_1d_hist)'
          goto 999
       endif
    else
       mess = 'Must have all of the output arrays Y_OUT, S_OUT, E_OUT, or none of them (IXFregroup_1d_hist)'
       goto 999
    endif


    ! Get regroup range:
    ! --------------------------
    imin = IXFlower_index (y_in, ymin)
    imax = IXFupper_index (y_in, ymax)
    if (imin > nin+1 .or. imax == 0 .or. imin == imax) then
       mess ='No complete input bins in range YMIN to YMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    i = imin
    iout = 0


    if (ydist) then
       !if ydistribution data
100    if (delta >= 0.0_dp) then
          ynew = y_in(i) + delta
       else
          ynew = y_in(i)*(1.0_dp-delta)	! delta < 0 so ynew > y_in(i) (we already know y_in(i) > ymin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (y_in(j) >= ynew) then
             iout = iout + 1
             if (present(y_out)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                y_out(iout) = y_in(i)
        !***************************************
            do k=1,size(s_in,1) ! loop thru' X dimension       
                s_out(k,iout) = sum(s_in(k,i:j-1)*(y_in(i+1:j)-y_in(i:j-1))) / (y_in(j)-y_in(i))
                e_out(k,iout) = sqrt(sum((e_in(k,i:j-1)*(y_in(i+1:j)-y_in(i:j-1)))**2)) / (y_in(j)-y_in(i))
        !***************************************        
            enddo
             endif
             i = j
             goto 100
          endif
       end do

    else
       ! if NOT distribution data then no multiplying or dividing of binwidths
200    if (delta >= 0.0_dp) then
          ynew = y_in(i) + delta
       else
          ynew = y_in(i)*(1.0_dp-delta)	! delta < 0 so ynew > y_in(i) (we already know y_in(i) > ymin > 0)
       endif
       do j = i+1, imax	! the loop ensures that even if DELTA=0 then output bin at least one input bin wide
          if (y_in(j) >= ynew) then
             iout = iout + 1
             if (present(y_out)) then			
                if (iout > nout_max) then
                   mess = 'One or more of the output arrays too short to hold all regrouped data (IXFregroup_1d_hist)'
                   goto 999
                endif
                y_out(iout) = y_in(i)
                s_out(:,iout) = sum(s_in(:,i:j-1),2)
                e_out(:,iout) = sqrt(sum(e_in(:,i:j-1)**2,2)) 
             endif
             i = j
             goto 200
          endif
       end do

    endif


    if (iout > 0) then
       if (present(nout)) nout = iout
       if (present(y_out)) y_out(iout+1) = y_in(i)
       return
    else		! no complete output bins in the range YMIN to YMAX
       mess = 'No complete output bins in range YMIN to YMAX (IXFregroup_1d_hist)'
       goto 999
    endif

    !----------------------------------------------------------------------------------
    ! Error condition: exit in controlled fashion
999 continue
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, mess) 

    if (present(nout)) nout = 0	! Do not set y_out=0, s_out=0, e_out=0 to avoid initialising possibly large arrays

    if (status == IXCseverity_error) return

    return

  end subroutine IXFregroupY_2d_hist






end module IXMregroup

