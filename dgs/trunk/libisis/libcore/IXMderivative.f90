module IXMderivative
  use IXMtype_definitions
  use IXMstatus
contains

  subroutine IXFderiv_1_1d ( x, y, e, y1, e1,status)


    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    !	Calculate numerical first derivative.
    !	Note that the error bars for adjacent points are correlated.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2001-04-30		First release
    !						2002-08-15		Put IERR as first argument (new convention)
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT:
    !	x(:)		real		Bin centres
    !	y(:)		real		Signal
    !	e(:)		real		Error bars on signal
    !
    ! OUTPUT:
    !	y1(:)		real		First derivative of signal
    !	e1(:)		real		Error bars on derivative
    !	status		type(IXTstatus)	error code
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: x(:), y(:), e(:)
    real(dp), intent(out) :: y1(:), e1(:)
    type(IXTstatus) :: status

    integer(i4b) :: nx,i
    real(dp) :: dx(size(x)-1)

    nx = size(x)
    if (size(y) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and signal array do not correspond (IXFderiv_1_1d)')

    endif
    if (size(e) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and error array do not correspond (IXFderiv_1_1d)')

    endif
    if (size(y1) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and output signal array do not correspond (IXFderiv_1_1d)')

    endif
    if (size(e1) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and output error array do not correspond (IXFderiv_1_1d)')

    endif
    
    ! need a test to check monotonically increasing x array....
    
    do i=2,nx
      if(x(i) < x(i-1) )then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array not monotonically increasing(IXFderiv_1_1d)')
        return
      endif
    enddo
            
    
    if (status == IXCseverity_error) return
    
    dx = x(2:nx)-x(1:nx-1)

    ! Initialise the end points to zero:
    y1(1) = 0.0_dp
    e1(1) = 0.0_dp
    y1(nx) = 0.0_dp
    e1(nx) = 0.0_dp

    ! Now do all the intermediate points:
    y1(2:nx-1) = (1.0_dp/(dx(1:nx-2)+dx(2:nx-1))) * ( (y(3:nx)-y(2:nx-1))*(dx(1:nx-2)/dx(2:nx-1))	+ &
         &(y(2:nx-1)-y(1:nx-2))/(dx(1:nx-2)/dx(2:nx-1)) )
    e1(2:nx-1) = (1.0_dp/(dx(1:nx-2)+dx(2:nx-1))) * sqrt( (e(3:nx)*(dx(1:nx-2)/dx(2:nx-1)))**2 + &
         &(e(2:nx-1)*((dx(1:nx-2)/dx(2:nx-1))-1.0_dp/(dx(1:nx-2)/dx(2:nx-1))))**2 + &
         &(e(1:nx-2)/(dx(1:nx-2)/dx(2:nx-1)))**2 )

    return
  end subroutine IXFderiv_1_1d

  subroutine IXFderiv_2_1d (x, y, e, y2, e2, status)

    !-----------------------------------------------------------------------------------------------------------------------------------
    !
    !	Calculate numerical second derivative.
    !	Note that the error bars for adjacent points are correlated.
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2001-04-30		First release
    !						2002-08-15		Put IERR as first argument (new convention)
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT:
    !	x(:)		real		Bin centres
    !	y(:)		real		Signal
    !	e(:)		real		Error bars on signal
    !
    ! OUTPUT:
    !	y2(:)		real		Second derivative of signal
    !	e2(:)		real		Error bars on derivative
    !	status		type(IXTstatus)	error code
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none

    real(dp), intent(in) :: x(:), y(:), e(:)
    real(dp), intent(out) :: y2(:), e2(:)
    type(IXTstatus) :: status

    integer(i4b) :: nx,i
    real(dp) :: dx(size(x)-1)

    nx = size(x)

    if (size(y) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and signal array do not correspond (IXFderiv_2_1d)')

    endif
    if (size(e) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and error array do not correspond (IXFderiv_2_1d)')

    endif
    if (size(y2) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and output signal array do not correspond (IXFderiv_2_1d)')

    endif
    if (size(e2) /= nx) then

       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'Sizes of x and output error array do not correspond (IXFderiv_2_1d)')

    endif
    
    do i=2,nx
      if(x(i) < x(i-1) )then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'x array not monotonically increasing(IXFderiv_2_1d)')
        return
      endif
    enddo

    
    dx = x(2:nx)-x(1:nx-1)

    ! Initialise the end points to zero:
    y2(1) = 0.0_dp
    e2(1) = 0.0_dp
    y2(nx) = 0.0_dp
    e2(nx) = 0.0_dp

    ! Now do all the intermediate points:
    y2(2:nx-1) = (2.0_dp/(dx(1:nx-2)+dx(2:nx-1))) * ( (y(3:nx)-y(2:nx-1))/dx(2:nx-1) - (y(2:nx-1)-y(1:nx-2))/dx(1:nx-2) )
    e2(2:nx-1) = (2.0_dp/(dx(1:nx-2)+dx(2:nx-1))) * &
         &sqrt( (e(3:nx)/dx(2:nx-1))**2 + (e(2:nx-1)*(1.0_dp/dx(1:nx-2)+1.0_dp/dx(2:nx-1)))**2 + (e(1:nx-2)/dx(1:nx-2))**2 )

    return
  end subroutine IXFderiv_2_1d

end module IXMderivative




