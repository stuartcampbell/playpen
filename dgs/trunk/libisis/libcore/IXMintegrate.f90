module IXMintegrate
  use IXMtype_definitions
  use IXMstatus
  use IXMindex
contains
  !! Integration over a 1D histogram data set.
  subroutine IXFintegrate_1d_hist (val,err,x, s, e, xdist, xmin_in, xmax_in,status)
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! Integration over a 1D histogram data set.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2001-03-24		First release
    !						2002-08-15		Put IERR as first argument (new convention)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    !	x(:)	real		Histogram bin boundaries 
    !	s(:)	real		Signal
    !	e(:)	real		Error bars on signal
    !	xdist	logical		Distribution if .TRUE.; raw counts if .FALSE.
    ! * xmin	real		Lower integration limit (default: start of array)
    ! * xmax	real		Upper integration limit (default: end of array)
    !
    ! OUTPUT:
    !	status	IXTstatus		Error flag
    !	two reals are returned:
    !		val = integral
    !		err = error on integral
    !	
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: x(:)!! input x array
    real(dp), intent(in):: s(:)!! input signal array (to be integrated)
    real(dp), intent(in)::  e(:)!! input error array
    real(dp), intent(in), optional :: xmin_in, xmax_in !! integration limits
    logical,  intent(in) :: xdist !! input distribtion flag
    type(IXTstatus),intent(inout) :: status !! error status flag
    real(dp), intent(out) ::val !! output integration value
    real(dp), intent(out)::err !!output integration error

    integer(i4b) :: nx, ml, mu
    real(dp)  :: xmin, xmax

    val=0.0_dp
    err=0.0_dp	! default return values

    ! Perform checks on input parameters:
    ! ---------------------------------------
    nx = size(s)
    if (size(x) /= nx+1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and signal arrays do not correspond (integrate_1d_hist)')
    endif
    if (size(e) /= nx) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of signal and error arrays do not correspond (integrate_1d_hist)')
    endif
    if (nx < 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Must have at least one bin to perform integration (integrate_1d_hist)')
    endif

    if (present(xmin_in) .and. present(xmax_in)) then
       if (xmin_in .gt. xmax_in) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Lower integration limit greater than upper integration limit (integrate_1d_hist)')
       endif
    endif

    ! Get integration ranges:
    ! --------------------------
    if (present(xmin_in)) then
       if(isnan(xmin_in))then
          ml = 1
          xmin = x(1)
       else
          ml = IXFlower_index (x, xmin_in)	! smallest index s.t. x(ml) >= xmin
          if (ml>nx+1) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_1d_hist)')
          endif
          if (x(1) > xmin_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                  IXCerr_invparam, 'Lower integration limit less than minimum abscissa (integrate_1d_hist)')
          endif
          xmin = max(xmin_in,x(1))
       endif
    else
       ml = 1
       xmin = x(1)
    endif

    if (present(xmax_in)) then
       if(isnan(xmax_in))then
          mu = nx+1
          xmax = x(nx+1)
       else
          mu = IXFupper_index (x, xmax_in)	! largest index s.t. x(mu) =< xmax
          if (mu==0) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Upper integration limit less than minimum abscissa (integrate_1d_hist)')
          endif
          if (x(nx+1) < xmax_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                  IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_1d_hist)')
          endif
          xmax = min(xmax_in,x(nx+1))
       endif
    else
       mu = nx+1
       xmax = x(nx+1)
    endif

    if (status == IXCseverity_error) return

    ! Perform integration:
    ! ----------------------
    if (ml==mu+1) then	! xmin and xmax within the same bin
       if (xdist) then
          val = s(mu)*(xmax-xmin)
          err = e(mu)*(xmax-xmin)
       else
          val = s(mu)*(xmax-xmin)/(x(mu+1)-x(mu))
          err = e(mu)*(xmax-xmin)/(x(mu+1)-x(mu))
       endif
    else
       ! sum over complete bins in the integration range:
       if (mu > ml) then
          if (xdist) then
             val = val + sum(s(ml:mu-1)*(x(ml+1:mu)-x(ml:mu-1)))
             err = err + sum((e(ml:mu-1)*(x(ml+1:mu)-x(ml:mu-1)))**2)
          else
             val = val + sum(s(ml:mu-1))
             err = err + sum(e(ml:mu-1)**2)
          endif
       endif
       ! sum over partial bins at the ends of the integration range:
       if (ml .ge. 2) then
          if (xdist) then
             val = val + s(ml-1)*(x(ml)-xmin)
             err = err + (e(ml-1)*(x(ml)-xmin))**2
          else
             val = val + s(ml-1)*(x(ml)-xmin)/(x(ml)-x(ml-1))
             err = err + (e(ml-1)*(x(ml)-xmin)/(x(ml)-x(ml-1)))**2
          endif
       endif
       if (mu .le. nx) then
          if (xdist) then
             val = val + s(mu)*(xmax-x(mu))
             err = err + (e(mu)*(xmax-x(mu)))**2
          else
             val = val + s(mu)*(xmax-x(mu))/(x(mu+1)-x(mu))
             err = err + (e(mu)*(xmax-x(mu))/(x(mu+1)-x(mu)))**2
          endif
       endif
       err = sqrt(abs(err))
    endif

    return
  end subroutine IXFintegrate_1d_hist
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  !! integration over a 1D point data set
  subroutine IXFintegrate_1d_points (val,err,x, s, e, xmin_in, xmax_in,status)


    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! Integration over a 1D point data set.
    !
    ! The method is a simple trapezoidal rule, with the ordinates at the points being linearly interpolated between
    ! the values in the array s.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2004-01-26		First release. Based very closely on INTEGRATE_1D_HIST.
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    !	x(:)	real		Histogram bin boundaries 
    !	s(:)	real		Signal
    !	e(:)	real		Error bars on signal
    ! * xmin	real		Lower integration limit (default: start of array)
    ! * xmax	real		Upper integration limit (default: end of array)
    !
    ! OUTPUT:
    !	status	IXTstatus		Error flag
    !
    !  RESULT:
    !	two reals are returned:
    !		val = integral
    !		err = error on integral
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: x(:)!! input x array
    real(dp), intent(in):: s(:)!! input signal array (to be integrated)
    real(dp), intent(in)::  e(:)!! input error array
    real(dp), intent(in), optional :: xmin_in, xmax_in !! integration limits
    type(IXTstatus),intent(inout) :: status !! error status flag
    real(dp), intent(out) ::val !! output integration value
    real(dp), intent(out)::err !!output integration error


    integer(i4b):: nx, ml, mu, ilo, ihi
    real(dp):: xmin, xmax, x1eff, xneff, s1eff, sneff, e1eff, eneff


    val=0.0_dp
    err=0.0_dp	! default return values


    ! Perform checks on input parameters:
    ! ---------------------------------------
    nx = size(s)
    if (size(x) /= nx) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and signal arrays do not correspond (integrate_1d_points)')
    endif
    if (size(e) /= nx) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of signal and error arrays do not correspond (integrate_1d_points)')
    endif
    if (nx <= 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Must have at least two data points to perform integration (integrate_1d_points)')
    endif

    if (present(xmin_in) .and. present(xmax_in)) then
       if (xmin_in .gt. xmax_in) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Lower integration limit greater than upper integration limit (integrate_1d_points)')
       endif
    endif

    ! Get integration ranges:
    ! --------------------------
    if (present(xmin_in)) then
       if(isnan(xmin_in))then
          ml = 1
          xmin = x(1)
       else
          ml = IXFlower_index (x, xmin_in)	! smallest index s.t. x(ml) >= xmin
          if (ml>nx) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_1d_points)')
          endif
          if (x(1) > xmin_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                  IXCerr_invparam, 'Lower integration limit less than minimum abscissa (integrate_1d_points)')
          endif
          xmin = max(xmin_in,x(1))
       endif
    else
       ml = 1
       xmin = x(1)
    endif

    if (present(xmax_in)) then
       if(isnan(xmax_in))then
          mu = nx
          xmax = x(nx)
       else
          mu = IXFupper_index (x, xmax_in)	! largest index s.t. x(mu) =< xmax
          if (mu==0) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Upper integration limit less than minimum abscissa (integrate_1d_points)')
          endif
          if (x(nx) < xmax_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                  IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_1d_points)')
          endif
          xmax = min(xmax_in,x(nx))
       endif
    else
       mu = nx
       xmax = x(nx)
    endif
    if (status == IXCseverity_error) return

    ! note: At this point, 1=<ml=<nx & xmin=<x(ml); 1=<mu=<nx & x(mu)=<xmax; BUT mu > ml-1

    ! Perform integration:
    ! ----------------------

    ! Calculate integral:

    !	if (mu<ml) then
    !	special case of no data points in the integration range
    !		ilo = max(ml-1,1)	! x(1) is end point if ml=1
    !		ihi = min(mu+1,nx)	! x(mu) is end point if mu=nx
    !		val = 0.5_dp * ((xmax-xmin)/(x(ihi)-x(ilo))) * &
    !			&( s(ihi)*((xmax-x(ilo))+(xmin-x(ilo))) + s(ilo)*((x(ihi)-xmax)+(x(ihi)-xmin)) )
    !	else
    !	xmin and xmax are separated by at least one data point in x(:)
    !	  sum over complete steps in the integration range:
    !		if (mu > ml) then	! at least one complete step
    !			val = sum((s(ml+1:mu)+s(ml:mu-1))*(x(ml+1:mu)-x(ml:mu-1)))
    !		else
    !			val = 0.0_dp
    !		endif
    !	  ends of the integration range:
    !		if (ml>1) then	! x(1) is end point if ml=1
    !			x1eff = (xmin*(xmin-x(ml-1)) + x(ml-1)*(x(ml)-xmin))/(x(ml)-x(ml-1))
    !			s1eff = s(ml-1)*(x(ml)-xmin)/((x(ml)-x(ml-1)) + (xmin-x(ml-1)))
    !			val = val + (x(ml)-x1eff)*(s(ml)+s1eff)
    !		endif
    !		if (mu<nx) then	! x(mu) is end point if mu=nx
    !			xneff = (xmax*(x(mu+1)-xmax) + x(mu+1)*(xmax-x(mu)))/(x(mu+1)-x(mu))
    !			sneff = s(mu+1)*(xmax-x(mu))/((x(mu+1)-x(mu)) + (x(mu+1)-xmax))
    !			val = val + (xneff-x(mu))*(s(mu)+sneff)
    !		endif
    !		val = 0.5_dp*val
    !	endif			

    ! Calculate error on the integral:

    if (mu<ml) then
       !	special case of no data points in the integration range
       ilo = max(ml-1,1)	! x(1) is end point if ml=1
       ihi = min(mu+1,nx)	! x(mu) is end point if mu=nx
       val = 0.5_dp * ((xmax-xmin)/(x(ihi)-x(ilo))) * &
            &( s(ihi)*((xmax-x(ilo))+(xmin-x(ilo))) + s(ilo)*((x(ihi)-xmax)+(x(ihi)-xmin)) )
       err = 0.5_dp * ((xmax-xmin)/(x(ihi)-x(ilo))) * &
            & sqrt( (e(ihi)*((xmax-x(ilo))+(xmin-x(ilo))))**2 + (e(ilo)*((x(ihi)-xmax)+(x(ihi)-xmin)))**2 )
    else
       !	xmin and xmax are separated by at least one data point in x(:)

       !	Set up effective end points:
       if (ml>1) then	! x(1) is end point if ml=1
          x1eff = (xmin*(xmin-x(ml-1)) + x(ml-1)*(x(ml)-xmin))/(x(ml)-x(ml-1))
          s1eff = s(ml-1)*(x(ml)-xmin)/((x(ml)-x(ml-1)) + (xmin-x(ml-1)))
          e1eff = e(ml-1)*(x(ml)-xmin)/((x(ml)-x(ml-1)) + (xmin-x(ml-1)))
       else
          x1eff = x(ml)
          s1eff = 0.0_dp
          e1eff = 0.0_dp
       endif
       if (mu<nx) then	! x(mu) is end point if mu=nx
          xneff = (xmax*(x(mu+1)-xmax) + x(mu+1)*(xmax-x(mu)))/(x(mu+1)-x(mu))
          sneff = s(mu+1)*(xmax-x(mu))/((x(mu+1)-x(mu)) + (x(mu+1)-xmax))
          eneff = e(mu+1)*(xmax-x(mu))/((x(mu+1)-x(mu)) + (x(mu+1)-xmax))
       else
          xneff = x(nx)
          sneff = 0.0_dp
          eneff = 0.0_dp
       endif
       !	xmin to x(ml):
       val = (x(ml)-x1eff)*(s(ml)+s1eff)
       err = (e1eff*(x(ml)-x1eff))**2
       !	x(ml) to x(mu):
       if (mu==ml) then		! one data point, no complete intervals
          err = err + (e(ml)*(xneff-x1eff))**2
       elseif (mu==ml+1) then	! one complete interval
          val = val + (s(mu)+s(ml))*(x(mu)-x(ml))
          err = err + (e(ml)*(x(ml+1)-x1eff))**2 + (e(mu)*(xneff-x(mu-1)))**2
       else
          ! this is the whole trapezium summing
          val = val + sum((s(ml+1:mu)+s(ml:mu-1))*(x(ml+1:mu)-x(ml:mu-1)))
          err = err + (e(ml)*(x(ml+1)-x1eff))**2 + (e(mu)*(xneff-x(mu-1)))**2 
          err = err+ sum((e(ml+1:mu-1)*(x(ml+2:mu)-x(ml:mu-2)))**2)
       endif

       !	x(mu) to xmax:
       val = val + (xneff-x(mu))*(s(mu)+sneff)
       err = err + (eneff*(xneff-x(mu)))**2

       val = 0.5_dp*val
       err = 0.5_dp*sqrt(err)
    endif

    return
  end subroutine IXFintegrate_1d_points
  !*************************************************************************************************
  !*************************************************************************************************
  !*************************************************************************************************

  !! integration over a 2D dataset
  subroutine IXFintegrate_2d_hist (s,e,status,x,xdist,xhist,xmin_in,xmax_in, &
       val_pt,err_pt,ymin_in,ymax_in,y,ydist,yhist, &
       val_ar_in,err_ar_in,x_ar,spec_lo,spec_hi)
    use IXMarraymanips
    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    ! Integration over a 2D histogram/points data set.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2001-03-24		First release
    !						2002-08-15		Put IERR as first argument (new convention)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! [Optional arguments are marked with * below]
    !
    ! INPUT:
    !	x(:)	real		Histogram bin boundaries 
    !	s(:)	real		Signal
    !	e(:)	real		Error bars on signal
    !	xdist	logical		Distribution if .TRUE.; raw counts if .FALSE.
    ! * xmin	real		Lower integration limit (default: start of array)
    ! * xmax	real		Upper integration limit (default: end of array)
    !
    ! OUTPUT:
    !	status	IXTstatus		Error flag
    !	two reals are returned:
    !		val = integral
    !		err = error on integral
    !	
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) ::  s(:,:) !!input signal array
    real(dp),intent(in):: e(:,:) !! input error array
    type(IXTstatus),intent(inout) :: status !! error status flag
    real(dp), intent(in) :: y(:) !! input y array
    real(dp),intent(in)::x(:) !! input x array
    real(dp), intent(in), optional :: xmin_in, xmax_in !! input x integration limit
    real(dp), intent(in),optional ::ymin_in,ymax_in !! input y integration limit
    logical,  intent(in),optional :: xdist, ydist !! input array distribution flag
    logical,  intent(in),optional :: xhist, yhist !! input array histogram flag
    real(dp),optional,intent(out) ::val_pt,err_pt !! output for a full 2 dimensional integration
    real(dp),optional,intent(inout),target ::val_ar_in(:),err_ar_in(:)
    real(dp),optional,intent(out)::x_ar(:)!! output for a 1 dimensional integration to creat a dataset_1d object
    integer(i4b),optional,intent(in)::spec_lo,spec_hi !!spectrum number for intspec routine option
    !! special case of integrate routine with x histogram data and y point data
    integer(i4b) :: nx, mx_l, mx_u
    integer(i4b) :: ny, my_l, my_u
    logical :: x_flag, y_flag ,xy_flag,intspec !!flags defining which integration operation to carry out
    real(dp)  :: xmin, xmax , ymin, ymax
    real(dp),allocatable::binfacV_D1(:,:),binfacE_D1(:,:),binfacV_D2(:,:), &
         binfacE_D2(:,:),PfacV_D1(:,:),PfacE_D1(:,:),PfacV_D2(:,:),PfacE_D2(:,:)
    real(dp),allocatable::s1eff(:),sneff(:),e1eff(:),eneff(:)
    real(dp)::x1eff,xneff,y1eff,yneff
    integer(i4b)::ilo,ihi
    real(dp),pointer::val_ar(:),err_ar(:)

    ny = size(s,2)
    nx = size(s,1)

    x_flag=.false.
    y_flag=.false.
    xy_flag=.false.
    intspec=.false.
    if(  (.not. (present(val_ar_in) .and. present(err_ar_in).and. &
         present(x_ar))) .and. (.not. (present(val_pt) .and. &
         present(err_pt)))) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Invalid parameter specification: must specify result variables(integrate_2d_hist)')
    endif

    if (present(val_ar_in) .and. present(err_ar_in))then
       ! it is either an xflag operation or a yflag operation 
       val_ar=>val_ar_in
       err_ar=>err_ar_in
    else
       ! it must be an xyflag operation -> xflag operation performed first so make the output array the length of the y array
       ! or an intspec operation-> xflag operation performed first
       allocate(val_ar(ny))
       allocate(err_ar(ny))
       val_ar=0.0_dp
       err_ar=0.0_dp
    endif


    if(present(xmin_in) .and. present(xmax_in) .and. present(xdist) .and. &
         present(ymin_in) .and. present(ymax_in) .and. present(ydist)        )then
       xy_flag=.true.
       val_pt=0.0_dp
       err_pt=0.0_dp	! default return values

       ! it will go thru an xflag operation therefore if point array will  need trapesoidal arrays
       if(.not. xhist)then
          allocate(s1eff(ny))
          allocate(e1eff(ny))
          allocate(sneff(ny))
          allocate(eneff(ny))
       endif
    else if(    present(xmin_in) .and. present(xmax_in) .and. present(xdist) .and. &
         (.not.(present(ymin_in) .and. present(ymax_in) .and. present(ydist)))  .and. &
         (.not.(present(spec_lo) .and. present(spec_hi))) )then
       x_flag=.true.
       val_ar=0.0_dp
       err_ar=0.0_dp ! default return values
       x_ar=y !set output dataset_1d x array values with y array from dataset_2d (since integral along x-axis)
       if(.not. xhist)then
          allocate(s1eff(ny))
          allocate(e1eff(ny))
          allocate(sneff(ny))
          allocate(eneff(ny))
       endif
    else if(    present(ymin_in) .and. present(ymax_in) .and. present(ydist) .and. &
         (.not.(present(xmin_in) .and. present(xmax_in) .and. present(xdist)     )   ))then
       y_flag=.true.
       val_ar=0.0_dp
       err_ar=0.0_dp ! default return values
       x_ar=x !set output dataset_1d x array values with x array from dataset_2d (since integral along x-axis)
       if(.not. yhist)then
          allocate(s1eff(nx))
          allocate(e1eff(nx))
          allocate(sneff(nx))
          allocate(eneff(nx))
       endif
    else if(present(spec_lo) .and. present(spec_hi))then
       ! this will perform an x_flag operation first and then a particular intspec summing
       intspec=.true.
       val_ar=0.0_dp
       err_ar=0.0_dp
       val_pt=0.0_dp
       err_pt=0.0_dp	! default return values

       if(spec_lo < 1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'low spectrum index invalid(integrate_2d_hist)')
       endif
       if(spec_hi > ny)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'high spectrum index invalid(integrate_2d_hist)')
       endif
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Invalid parameter specification (integrate_2d_hist)')
    endif
    ! to prevent any unnecessary further action
    if (status == IXCseverity_error) return

    ! Perform checks on input parameters:
    ! ---------------------------------------
    if(x_flag .or. xy_flag .or. intspec)then

       if(size(val_ar) /= ny)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, &
               'Size of supplied output arrays incompatible with desired integration operation (integrate_2d_hist)')
       endif
       if( size(err_ar) /= size(val_ar))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Output error and value arrays not congruent (integrate_2d_hist)')
       endif
       if(xhist)then
          if (size(x) /= nx+1) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Sizes of x and signal arrays do not correspond (integrate_2d_hist)')
          endif
       else 
          if (size(x) /= nx) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Sizes of x and signal arrays do not correspond (integrate_2d_hist)')
          endif
       endif
       if (sum(abs(shape(s)-shape(e)))/=0) then

          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Sizes of signal and error arrays do not correspond (integrate_2d_hist)')
       endif
       if (nx < 1) then

          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'must have at least one bin to perform integration (integrate_2d_hist)')
       endif
       if (present(xmin_in) .and. present(xmax_in)) then
          if (xmin_in .gt. xmax_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Lower integration limit greater than upper integration limit (integrate_2d_hist)')
          endif
       endif


       ! Get integration ranges:
       ! --------------------------
       if (present(xmin_in)) then
          if(isnan(xmin_in))then
             mx_l = 1
             xmin = x(1)
          else
             mx_l = IXFlower_index (x, xmin_in)	! smallest index s.t. x(mx_l) >= xmin
             if(xhist)then !histogram data specific check
                if (mx_l>nx+1) then
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                        IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_2d_hist)')
                endif
             else ! point data specific check
                if(mx_l>nx) then
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                        IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_2d_hist)')
                endif
             endif
             if (x(1) > xmin_in) then
                call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                     IXCerr_invparam, 'Lower integration limit less than minimum abscissa (integrate_2d_hist)')
             endif
             xmin = max(xmin_in,x(1))
          endif
       else
          mx_l = 1
          xmin = x(1)
       endif
       if (present(xmax_in)) then
          if(isnan(xmax_in))then
             if(xhist)then
                mx_u = nx+1
                xmax = x(nx+1)
             else
                mx_u = nx
                xmax = x(nx)
             endif
          else
             mx_u = IXFupper_index (x, xmax_in)	! largest index s.t. x(mx_u) =< xmax
             if (mx_u==0) then
                call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                     IXCerr_invparam, 'Upper integration limit less than minimum abscissa (integrate_2d_hist)')
             endif
             if(xhist)then !histogram data specific check
                if (x(nx+1) < xmax_in) then           
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                        IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_2d_hist)')           
                endif
                xmax = min(xmax_in,x(nx+1))
             else !point data specific check
                if (x(nx) < xmax_in) then           
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                        IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_2d_hist)')           
                endif
                xmax = min(xmax_in,x(nx))
             endif
          endif
       else
          if(xhist)then
             mx_u = nx+1
             xmax = x(nx+1)
          else
             mx_u = nx
             xmax = x(nx)
          endif
       endif

    endif
    !*************************************************************************************************************************************************************************88

    if(y_flag )then

       if(size(val_ar) /= nx)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, &
               'Size of supplied output arrays incompatible with desired integration operation (integrate_2d_hist)')
       endif
       if( size(err_ar) /= size(val_ar))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Output error and value arrays not congruent (integrate_2d_hist)')
       endif

       if(yhist)then
          if (size(y) /= ny+1) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Sizes of y and signal arrays do not correspond (integrate_2d_hist)')
          endif
       else 
          if (size(y) /= ny) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Sizes of y and signal arrays do not correspond (integrate_2d_hist)')
          endif
       endif
       if (sum(abs(shape(s)-shape(e)))/=0) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'Sizes of signal and error arrays do not correspond (integrate_2d_hist)')
       endif
       if (ny < 1) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'must have at least one bin to perform integration (integrate_2d_hist)')
       endif

       if (present(ymin_in) .and. present(ymax_in)) then
          if (ymin_in .gt. ymax_in) then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                  IXCerr_invparam, 'Lower integration limit greater than upper integration limit (integrate_2d_hist)')

          endif
       endif


       ! Get integration ranges:
       ! --------------------------
       if (present(ymin_in)) then
          if(isnan(ymin_in))then
             my_l = 1
             ymin = y(1)
          else
             my_l = IXFlower_index (y, ymin_in)	! smallest index s.t. y(my_l) >= ymin
             if(yhist)then !histogram data specific check
                if (my_l>ny+1) then
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                        IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_2d_hist)')
                endif
             else ! point data specific check
                if(my_l>ny) then
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                        IXCerr_invparam, 'Lower integration limit greater than maximum abscissa (integrate_2d_hist)')
                endif
             endif

             if (y(1) > ymin_in) then
                call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                     IXCerr_invparam, 'Lower integration limit less than minimum abscissa (integrate_2d_hist)')
             endif
             ymin = max(ymin_in,y(1))
          endif
       else
          my_l = 1
          ymin = y(1)
       endif
       if (present(ymax_in)) then
          if(isnan(ymax_in))then
             if(yhist)then
                my_u = ny+1
                ymax = y(ny+1)
             else
                my_u = ny
                ymax = y(ny)
             endif
          else
             my_u = IXFupper_index (y, ymax_in)	! largest index s.t. y(my_u) =< ymax
             if (my_u==0) then
                call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                     IXCerr_invparam, 'Upper integration limit less than minimum abscissa (integrate_2d_hist)')
             endif
             if(yhist)then !histogram data specific check
                if (y(ny+1) < ymax_in) then           
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                        IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_2d_hist)')           
                endif
                ymax = min(ymax_in,y(ny+1))
             else !point data specific check
                if (y(ny) < ymax_in) then           
                   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
                        IXCerr_invparam, 'Upper integration limit greater than maximum abscissa (integrate_2d_hist)')           
                endif
                ymax = min(ymax_in,y(ny))
             endif
          endif
       else
          if(yhist)then
             my_u = ny+1
             ymax = y(ny+1)
          else
             my_u = ny
             ymax = y(ny)
          endif
       endif

    endif

    if (status == IXCseverity_error) return

    ! Perform integration: XFLAG specific
    ! ----------------------

    if(x_flag .or. xy_flag .or. intspec)then ! if its xy_flag-> do x integration first
       if(xhist)then !histogram specific integration
          if (mx_l==mx_u+1) then	! xmin and xmax within the same bin
             if (xdist) then
                val_ar = s(mx_u,:)*(xmax-xmin)
                err_ar = e(mx_u,:)*(xmax-xmin)
             else
                val_ar = s(mx_u,:)*(xmax-xmin)/(x(mx_u+1)-x(mx_u))
                err_ar = e(mx_u,:)*(xmax-xmin)/(x(mx_u+1)-x(mx_u))
             endif
          else
             ! sum over complete bins in the integration range:
             if (mx_u > mx_l) then
                if (xdist) then
                   allocate(binfacV_D1((mx_u-mx_l),ny))
                   allocate(binfacE_D1((mx_u-mx_l),ny))
                   call IXFcreatebinfac_D1(s,e,x,mx_u,mx_l,binfacV_D1,binfacE_D1)
                   val_ar = val_ar +  sum(binfacV_D1,1)
                   err_ar = err_ar + sum(binfacE_D1**2,1)
                   deallocate(binfacV_D1,binfacE_D1)
                else
                   val_ar = val_ar + sum(s(mx_l:mx_u-1,:),1)
                   err_ar = err_ar + sum(e(mx_l:mx_u-1,:)**2,1)
                endif
             endif
             ! sum over partial bins at the ends of the integration range: 
             if (mx_l .ge. 2) then
                if (xdist) then
                   val_ar = val_ar + s(mx_l-1,:)*(x(mx_l)-xmin)
                   err_ar = err_ar + (e(mx_l-1,:)*(x(mx_l)-xmin))**2
                else
                   val_ar = val_ar + s(mx_l-1,:)*(x(mx_l)-xmin)/(x(mx_l)-x(mx_l-1))
                   err_ar = err_ar + (e(mx_l-1,:)*(x(mx_l)-xmin)/(x(mx_l)-x(mx_l-1)))**2
                endif
             endif
             if (mx_u .le. nx) then
                if (xdist) then
                   val_ar = val_ar + s(mx_u,:)*(xmax-x(mx_u))
                   err_ar = err_ar + (e(mx_u,:)*(xmax-x(mx_u)))**2
                else
                   val_ar = val_ar + s(mx_u,:)*(xmax-x(mx_u))/(x(mx_u+1)-x(mx_u))
                   err_ar = err_ar + (e(mx_u,:)*(xmax-x(mx_u))/(x(mx_u+1)-x(mx_u)))**2
                endif
             endif
             err_ar = sqrt(abs(err_ar))
          endif

       else ! point specific integration

          if (mx_u<mx_l) then
             !	special case of no data points in the integration range
             ilo = max(mx_l-1,1)	! x(1) is end point if mx_l=1
             ihi = min(mx_u+1,nx)	! x(mx_u) is end point if mx_u=nx
             val_ar = 0.5_dp * ((xmax-xmin)/(x(ihi)-x(ilo))) * &
                  &( s(ihi,:)*((xmax-x(ilo))+(xmin-x(ilo))) + s(ilo,:)*((x(ihi)-xmax)+(x(ihi)-xmin)) )
             err_ar = 0.5_dp * ((xmax-xmin)/(x(ihi)-x(ilo))) * &
                  & sqrt( (e(ihi,:)*((xmax-x(ilo))+(xmin-x(ilo))))**2 + (e(ilo,:)*((x(ihi)-xmax)+(x(ihi)-xmin)))**2 )
          else
             !	xmin and xmax are separated by at least one data point in x(:)

             !	Set up effective end points:
             if (mx_l>1) then	! x(1) is end point if mx_l=1
                x1eff = (xmin*(xmin-x(mx_l-1)) + x(mx_l-1)*(x(mx_l)-xmin))/(x(mx_l)-x(mx_l-1))
                s1eff = s(mx_l-1,:)*(x(mx_l)-xmin)/((x(mx_l)-x(mx_l-1)) + (xmin-x(mx_l-1)))
                e1eff = e(mx_l-1,:)*(x(mx_l)-xmin)/((x(mx_l)-x(mx_l-1)) + (xmin-x(mx_l-1)))
             else
                x1eff = x(mx_l)
                s1eff = 0.0_dp
                e1eff = 0.0_dp
             endif
             if (mx_u<nx) then	! x(mx_u) is end point if mx_u=nx
                xneff = (xmax*(x(mx_u+1)-xmax) + x(mx_u+1)*(xmax-x(mx_u)))/(x(mx_u+1)-x(mx_u))
                sneff = s(mx_u+1,:)*(xmax-x(mx_u))/((x(mx_u+1)-x(mx_u)) + (x(mx_u+1)-xmax))
                eneff = e(mx_u+1,:)*(xmax-x(mx_u))/((x(mx_u+1)-x(mx_u)) + (x(mx_u+1)-xmax))
             else
                xneff = x(nx)
                sneff = 0.0_dp
                eneff = 0.0_dp
             endif

             !	xmin to x(mx_l):
             val_ar = (x(mx_l)-x1eff)*(s(mx_l,:)+s1eff)
             err_ar = (e1eff*(x(mx_l)-x1eff))**2

             !	x(mx_l) to x(mx_u):
             if (mx_u==mx_l) then		! one data point, no complete intervals
                err_ar = err_ar + (e(mx_l,:)*(xneff-x1eff))**2
             elseif (mx_u==mx_l+1) then	! one complete interval
                val_ar = val_ar + (s(mx_u,:)+s(mx_l,:))*(x(mx_u)-x(mx_l))
                err_ar = err_ar + (e(mx_l,:)*(x(mx_l+1)-x1eff))**2 + (e(mx_u,:)*(xneff-x(mx_u-1)))**2
             else
                ! this is the whole trapezium summing
                !         val_ar = val_ar + sum((s(mx_l+1:mx_u)+s(mx_l:mx_u-1))*(x(mx_l+1:mx_u)-x(mx_l:mx_u-1)))
                !         err_ar = err_ar + (e(mx_l,i)*(x(mx_l+1)-x1eff))**2 + (e(mx_u,:)*(xneff-x(mx_u-1)))**2 &
                !              & + sum((e(mx_l+1:mx_u-1)*(x(mx_l+2:mx_u)-x(mx_l:mx_u-2)))**2)
                allocate(PfacV_D1((mx_u-mx_l),ny))
                allocate(PfacE_D1((mx_u-mx_l-1),ny))
                call IXFcreatePfac_D1(s,e,x,mx_u,mx_l,PfacV_D1,PfacE_D1)
                val_ar = val_ar +  sum(PfacV_D1,1)
                err_ar = err_ar +  (e(mx_l,:)*(x(mx_l+1)-x1eff))**2 + (e(mx_u,:)*(xneff-x(mx_u-1)))**2 
                ! it is the same up to this point
                !                sum((e(mx_l+1:mx_u-1)*(x(mx_l+2:mx_u)-x(mx_l:mx_u-2)))**2)
                err_ar= err_ar+ sum(PfacE_D1**2,1)
                deallocate(PfacV_D1,PfacE_D1)
             endif
             !	x(mx_u) to xmax:
             val_ar = val_ar + (xneff-x(mx_u))*(s(mx_u,:)+sneff)
             err_ar = err_ar + (eneff*(xneff-x(mx_u)))**2

             val_ar = 0.5_dp*val_ar
             err_ar = 0.5_dp*sqrt(err_ar)
          endif
          deallocate(s1eff)
          deallocate(e1eff)
          deallocate(sneff)
          deallocate(eneff)
       endif
    endif

    if(y_flag)then
       if(yhist)then
          if (my_l==my_u+1) then	! ymin and ymax within the same bin
             if (ydist) then
                val_ar = s(my_u,:)*(ymax-ymin)
                err_ar = e(my_u,:)*(ymax-ymin)
             else
                val_ar = s(my_u,:)*(ymax-ymin)/(y(my_u+1)-y(my_u))
                err_ar = e(my_u,:)*(ymax-ymin)/(y(my_u+1)-y(my_u))
             endif
          else
             ! sum over complete bins in the integration range:
             if (my_u > my_l) then
                if (ydist) then           
                   allocate(binfacV_D2(nx,(my_u-my_l)))
                   allocate(binfacE_D2(nx,(my_u-my_l)))
                   call IXFcreatebinfac_D2(s,e,y,my_u,my_l,binfacV_D2,binfacE_D2)
                   val_ar = val_ar +  sum(binfacV_D2,2)
                   err_ar = err_ar + sum(binfacE_D2**2,2)
                   deallocate(binfacV_D2,binfacE_D2)
                else
                   val_ar = val_ar + sum(s(:,my_l:my_u-1),2)
                   err_ar = err_ar + sum(e(:,my_l:my_u-1)**2,2)
                endif
             endif
             ! sum over partial bins at the ends of the integration range: 
             if (my_l .ge. 2) then
                if (ydist) then
                   val_ar = val_ar + s(:,my_l-1)*(y(my_l)-ymin)
                   err_ar = err_ar + (e(:,my_l-1)*(y(my_l)-ymin))**2
                else
                   val_ar = val_ar + s(:,my_l-1)*(y(my_l)-ymin)/(y(my_l)-y(my_l-1))
                   err_ar = err_ar + (e(:,my_l-1)*(y(my_l)-ymin)/(y(my_l)-y(my_l-1)))**2
                endif
             endif
             if (my_u .le. ny) then
                if (ydist) then
                   val_ar = val_ar + s(:,my_u)*(ymax-y(my_u))
                   err_ar = err_ar + (e(:,my_u)*(ymax-y(my_u)))**2
                else
                   val_ar = val_ar + s(:,my_u)*(ymax-y(my_u))/(y(my_u+1)-y(my_u))
                   err_ar = err_ar + (e(:,my_u)*(ymax-y(my_u))/(y(my_u+1)-y(my_u)))**2
                endif
             endif
             err_ar = sqrt(abs(err_ar))
          endif

       else  !point specific integration

          if (my_u<my_l) then
             !	special case of no data points in the integration range
             ilo = max(my_l-1,1)	! y(1) is end point if my_l=1
             ihi = min(my_u+1,ny)	! y(my_u) is end point if my_u=ny
             val_ar = 0.5_dp * ((ymax-ymin)/(y(ihi)-y(ilo))) * &
                  &( s(:,ihi)*((ymax-y(ilo))+(ymin-y(ilo))) + s(:,ilo)*((y(ihi)-ymax)+(y(ihi)-ymin)) )
             err_ar = 0.5_dp * ((ymax-ymin)/(y(ihi)-y(ilo))) * &
                  & sqrt( (e(:,ihi)*((ymax-y(ilo))+(ymin-y(ilo))))**2 + (e(:,ilo)*((y(ihi)-ymax)+(y(ihi)-ymin)))**2 )
          else
             !	ymin and ymax are separated by at least one data point in y(:)

             !	Set up effective end points:
             if (my_l>1) then	! y(1) is end point if my_l=1
                y1eff = (ymin*(ymin-y(my_l-1)) + y(my_l-1)*(y(my_l)-ymin))/(y(my_l)-y(my_l-1))
                s1eff = s(:,my_l-1)*(y(my_l)-ymin)/((y(my_l)-y(my_l-1)) + (ymin-y(my_l-1)))
                e1eff = e(:,my_l-1)*(y(my_l)-ymin)/((y(my_l)-y(my_l-1)) + (ymin-y(my_l-1)))
             else
                y1eff = y(my_l)
                s1eff = 0.0_dp
                e1eff = 0.0_dp
             endif
             if (my_u<ny) then	! y(my_u) is end point if my_u=ny
                yneff = (ymax*(y(my_u+1)-ymax) + y(my_u+1)*(ymax-y(my_u)))/(y(my_u+1)-y(my_u))
                sneff = s(:,my_u+1)*(ymax-y(my_u))/((y(my_u+1)-y(my_u)) + (y(my_u+1)-ymax))
                eneff = e(:,my_u+1)*(ymax-y(my_u))/((y(my_u+1)-y(my_u)) + (y(my_u+1)-ymax))
             else
                yneff = y(ny)
                sneff = 0.0_dp
                eneff = 0.0_dp
             endif

             !	ymin to y(my_l):
             val_ar = (y(my_l)-y1eff)*(s(:,my_l)+s1eff)
             err_ar = (e1eff*(y(my_l)-y1eff))**2

             !	y(my_l) to y(my_u):
             if (my_u==my_l) then		! one data point, no complete intervals
                err_ar = err_ar + (e(:,my_l)*(yneff-y1eff))**2
             elseif (my_u==my_l+1) then	! one complete interval
                val_ar = val_ar + (s(:,my_u)+s(:,my_l))*(y(my_u)-y(my_l))
                err_ar = err_ar + (e(:,my_l)*(y(my_l+1)-y1eff))**2 + (e(:,my_u)*(yneff-y(my_u-1)))**2
             else
                ! this is the whole trapezium summing
                !         val_ar = val_ar + sum((s(my_l+1:my_u)+s(my_l:my_u-1))*(y(my_l+1:my_u)-y(my_l:my_u-1)))
                !         err_ar = err_ar + (e(my_l,i)*(y(my_l+1)-y1eff))**2 + (e(my_u,:)*(yneff-y(my_u-1)))**2 &
                !              & + sum((e(my_l+1:my_u-1)*(y(my_l+2:my_u)-y(my_l:my_u-2)))**2)
                allocate(PfacV_D2(nx,(my_u-my_l)))
                allocate(PfacE_D2(nx,(my_u-my_l-1)))
                call IXFcreatePfac_D2(s,e,y,my_u,my_l,PfacV_D2,PfacE_D2)
                val_ar = val_ar +  sum(PfacV_D2,2)
                err_ar = err_ar +  (e(:,my_l)*(y(my_l+1)-y1eff))**2 + (e(:,my_u)*(yneff-y(my_u-1)))**2 
                ! it is the same up to this point
                !                sum((e(my_l+1:my_u-1)*(y(my_l+2:my_u)-y(my_l:my_u-2)))**2)
                err_ar= err_ar+ sum(PfacE_D2**2,2)
                deallocate(PfacV_D2,PfacE_D2)
             endif
             !	y(my_u) to ymax:
             val_ar = val_ar + (yneff-y(my_u))*(s(:,my_u)+sneff)
             err_ar = err_ar + (eneff*(yneff-y(my_u)))**2

             val_ar = 0.5_dp*val_ar
             err_ar = 0.5_dp*sqrt(err_ar)
          endif
          deallocate(s1eff)
          deallocate(e1eff)
          deallocate(sneff)
          deallocate(eneff)

       endif
    endif

    !if it was xy flag do the call to the 1D integration routine according to whether it is histogram or point array

    if(xy_flag)then

       if(yhist)then
          call IXFintegrate_1d_hist(val_pt,err_pt,y, val_ar, err_ar, ydist, ymin_in, ymax_in,status)
       else
          call IXFintegrate_1d_points (val_pt,err_pt,y, val_ar, err_ar, ymin_in, ymax_in,status)
       endif
       !free the temporary variables created for the x axis integration
       deallocate(val_ar)
       deallocate(err_ar)      

    endif

    if(intspec)then

       val_pt=sum(val_ar(spec_lo:spec_hi))
       err_pt=sqrt(sum(err_ar(spec_lo:spec_hi)**2))

       deallocate(val_ar)
       deallocate(err_ar)

    endif

  end subroutine IXFintegrate_2d_hist
  !-----------------------------------------------------------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------------------------------------------------------


end module IXMintegrate
