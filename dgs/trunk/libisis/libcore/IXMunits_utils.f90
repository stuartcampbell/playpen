!------------------------------
! MODULE: IXMunits_utils
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1299 $ ($Date: 2008-01-16 09:41:22 -0500 (Wed, 16 Jan 2008) $)
!!
!! Contains core subroutines for the changing of units
! IXMunits_utils module
module IXMunits_utils
  use IXMneutron_units
  use IXMphysical_constants
  use IXMstatus
  implicit none
  integer(i4b), parameter :: ok=0_i4b, warn=-1_i4b, err=-100_i4b, eof=-101_i4b

  private:: units_check_parameters,units_check_codes,units_xconvert,units_composite_coefficients,units_coefficients

contains
  !-----------------------------------------------------------------------------------------------------------------------
  !!	Gets the range of indices from a (histogram) array that are valid for units conversion, together with various
  !!	parameters that are needed for units conversion
  !!
  !! It is assumed that the input x array has length at least = 2   
  subroutine IXFunits_get_len_arr (units_in, x_in, units_out, emode, delta, x1, x2, twotheta, efix, &
       ilo, ihi, ctot, gtot, sgn_in, shift_in, qopt_in, sgn_out, shift_out, qopt_out,status)
    use IXMindex
    implicit none
    type(IXTstatus)::status
    integer(i4b), intent(IN) :: emode
    character(len=*), intent(IN) :: units_in, units_out
    real(dp), intent(IN) :: x_in(:), delta, x1, x2, twotheta, efix
    integer(i4b), intent(OUT) :: ilo, ihi, sgn_in, sgn_out, qopt_in, qopt_out
    real(dp), intent (OUT) :: ctot, gtot, shift_in, shift_out

    real(dp), parameter :: small=1.0e-10_dp

    integer(i4b):: istatus_in, istatus_out, istatus, qopt_dummy
    real(dp):: a_in, b_in, g_in, c_in, a_out, b_out, g_out, c_out, xmin, xmax, xcrit
    character(len=5):: units_parsed_in, units_parsed_out, units_dummy


    ! check that the parameters EMODE ... EFIX are valid
    istatus = units_check_parameters (emode, delta, x1, x2, twotheta, efix)
    if (istatus /= OK) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'ERROR: Check the parameters: emode, delta, x1, x2, twotheta, efix (IXFunits_get_len_arr)')       
       goto 100
    endif

    istatus = units_check_codes(emode,units_in,units_out)
    if (istatus /= OK) then       
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'ERROR: Check the parameters: units in/out codes are not compatible with emode(IXFunits_get_len_arr)')
       goto 100
    endif

    ! get parameters from the incoming and out-going units
    istatus_in = units_coefficients (units_in,  emode, delta, x1, x2, twotheta, efix, &
         units_parsed_in, a_in, b_in, g_in, c_in, sgn_in, shift_in, qopt_in)
    istatus_out= units_coefficients (units_out, emode, delta, x1, x2, twotheta, efix, &
         units_parsed_out, a_out, b_out, g_out, c_out, sgn_out, shift_out, qopt_out)
    if (istatus_in/=OK .or. istatus_out/=OK) then
       !		call remark ('ERROR: Check validity of input and/or output units for units conversion')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Check validity of input and/or '//&
                &'output units for units conversion (IXFunits_get_len_arr)')       
       goto 100
    endif

    ! Get default output index range:
    ilo = 1
    ihi = size(x_in)

    ! Check if output units are same as input; then no conversion needed
    if (units_parsed_in == units_parsed_out) then
       ctot = 1.0_dp
       gtot = 1.0_dp
       ! safe status		
       return
    endif

    ! Get indices of input array that correspond to valid values for units conversion, and get conversion coefficients
    ! (The user may have shifted the array, which is why it is necessary to check every time a units conversion is made.)

    if (qopt_in == 0) then			! input units are not momentum transfer in inelastic mode
       if (sgn_in > 0) then		! no reveral in definition of generalised tm
          ilo = IXFlower_index (x_in, shift_in+small)	! so that we know x_in(ilo) > shift_in + a small amount
       else
          ihi = IXFupper_index (x_in, shift_in-small)	! so that we know x_in(ihi) < shift_in - a small amount
       endif
       if (qopt_out /= 0) then	! will have to do a conversion to k and from there to Q or Q^2
          istatus = units_coefficients ('k   ', emode, delta, x1, x2, twotheta, efix, &
               units_dummy, a_out, b_out, g_out, c_out, sgn_out, shift_out, qopt_dummy)
       endif
    else
       if (abs(qopt_in) == 2_i4b) then
          xmax = (efix/c_k_to_emev)
          xmin = xmax*(sin(twotheta))**2
       else
          xmax = sqrt(efix/c_k_to_emev)
          xmin = xmax*abs(sin(twotheta))
       endif
       ilo = IXFlower_index (x_in, xmin)					! accept x_in(ilo) = xmin
       if (qopt_in*(2_i4b*emode-3_i4b) < 0_i4b) then	! want smaller Q part of array
          ihi = IXFupper_index (x_in, xmax-small)		! accept only x_in(ihi) < xmax - a small amount
       endif
       if (qopt_out == 0) then	! will have to do a conversion from Q or Q^2 to k, and from there to output units
          istatus = units_coefficients ('k   ', emode, delta, x1, x2, twotheta, efix, &
               units_dummy, a_in, b_in, g_in, c_in, sgn_in, shift_in, qopt_dummy)
       endif
    endif

    ! Get conversion coefficients for full, or intermediate conversion to/from Q when inelastic:
    if (qopt_in ==0 .or. qopt_out ==0) then
       istatus = units_composite_coefficients (emode, delta, x1, x2, twotheta, efix, a_in, b_in, g_in, c_in, &
            a_out, b_out, g_out, c_out, ctot, gtot)
       if (istatus/=OK) then
          !			call remark ('ERROR: Check the parameters: emode, delta, x1, x2, twotheta, efix')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Check the parameters: emode, '//&
                 &'delta, x1, x2, twotheta, efix (IXFunits_get_len_arr)')                
          goto 100
       endif
    else	! conversion within the manifold of q, sq :
       ctot = 1.0_dp
       gtot = real(qopt_out)/real(qopt_in)
    endif

    ! check ILO, IHI the case of conversion to inelastic momentum transfer (must take a restricted range from data):
    if (qopt_in ==0 .and. qopt_out /= 0) then	! only need further checks if not from momentum
       xcrit = real(sgn_in)*sqrt(((efix*(cos(twotheta))**2)/(c_k_to_emev*(ctot**2)))**nint(1.0_dp/gtot)) + shift_in	! 1/gtot = integer, so OK
       if (qopt_out*(2*emode-3)*sgn_in*g_in < 0.0_dp) then		! want i >= icrit
          ilo = max (ilo, IXFlower_index (x_in, xcrit))			! accept x_in(ilo) = xcrit
       else
          ihi = min (ihi, IXFupper_index (x_in, xcrit))			! accept x_in(ihi) = xcrit
       endif
    else if (qopt_in /=0 .and. qopt_out /= 0) then	! must have same sign
       if (qopt_in*qopt_out < 0_i4b) then
          !			call remark ('ERROR: Q ranges for input and output units are mutually exclusive')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Q ranges for input and output '//&
                 &'units are mutually exclusive(IXFunits_get_len_arr)')                          
          goto 100
       endif
    endif


    ! Check that have at least one full bin:
    if (ilo >= ihi) then
       !		call remark ('ERROR: Invalid range of input and/or output units')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Invalid range of input and/or output units (IXFunits_get_len_arr)')                       
       goto 100
    endif
    ! end properly
    return

    ! Error condition:
    ! ----------------
100 continue
    ilo = 0
    ihi = 0
    ctot = 0.0_dp
    gtot = 0.0_dp
    qopt_in = 0
    qopt_out= 0

    ! error IXTstatus object has been filled at instance error
    return
  end subroutine IXFunits_get_len_arr
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFunits_convert (xin, yin, ein, emode, twotheta, efix, dist_in,&
       ilo, ihi, ctot, gtot, sgn_in, shift_in, qopt_in, sgn_out, shift_out, qopt_out, xout, yout, eout,status)
    use IXMindex
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! Performs units conversion on a single x-y-e histogram spectrum.
    ! Not all of the parameters delta, x1, x2, twotheta, efix are needed, depending on the conversion being performed e.g. efix
    ! is irrelevant for elastic scattering, or delta is not needed when converting from energy transfer to Q.
    !
    ! The routine assumes that the parameters emode...efix, ilo...qopt_out are all consistent (can be achieved by a previous
    ! call to units_get_len_arr), and that the arrays xout, yout, eout are of the correct length to hold the output.
    !
    ! units_convert = OK all fine, or ERR otherwise
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    !	T.G. Perring		2003-08-06		First formal release	
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    ! INPUT (mandatory):
    !	xin(:)	real		Input histogram bin boundaries (run from 1 to MX+1)
    !	yin(:)	real		Input signal (runs from 1 to MX)
    !	ein(:)	real		Input error bars on signal (runs from 1 to MX)
    !	emode	int			Energy mode (0=elastic, 1=direct geometry, 2=indirect geometry)
    !  twotheta	real		Scattering angle (radians)
    !	efix	real		Value of fixed energy: EI (emode=1) or EF (emode=2) (meV)
    !	ilo		int			Lower element number of XIN to be user for units conversion
    !	ihi		int			Upper element number of XIN to be user for units conversion
    !	ctot	real		Coefficient C in conversion x_out = C (x_in)**G (unless qopt_in or qopt_out /=0)
    !	gtot	real		Power G in above
    !	sgn_in	int			Sign (+1 or -1) for conversion of input units to working units
    !  shift_in	real		Offset for conversion of input units to working units
    !	qopt_in	int			Q option of input units: +/- 1 or +/- 2 of input units (EMODE=1 or 2 only)
    !	sgn_out	int			Sign (+1 or -1) for conversion of working units to output units
    ! shift_out	real		Offset for conversion of working units to output units
    !  qopt_out	int			Q option of output units: +/- 1 or +/- 2 of input units (EMODE=1 or 2 only)
    !
    ! OUTPUT (mandatory):
    !	xout(:)	real		Output histogram bin boundaries (run from 1 to MX+1)
    !	yout(:)	real		Output signal (runs from 1 to MX)
    !	eout(:)	real		Output error bars on signal (runs from 1 to MX)
    !
    !
    !-----------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(IXTstatus)::status
    integer(i4b), intent(in) :: emode, ilo, ihi, sgn_in, qopt_in, sgn_out, qopt_out
    real(dp), intent(in) :: xin(:), yin(:), ein(:), efix, twotheta, ctot, gtot, shift_in, shift_out
    real(dp), intent(out) :: xout(:), yout(:), eout(:)
    logical,intent(in)::dist_in
    integer(i4b) istatus
    real(dp) kmin, qmin, qminsqr, qx, qysqr
    real(dp), allocatable :: xtemp(:)

    ! pick up special case that the input and output units are the same:
    if (ilo==1 .and. ihi==size(xin) .and. ctot==1.0_dp .and. gtot==1.0_dp .and. sgn_in==sgn_out .and. qopt_in==qopt_out) then
       xout = xin
       yout = yin
       eout = ein
       !		IXFunits_convert = OK
       return
    endif

    ! Do x-axis coversion
    if (qopt_in /= 0) then
       if (qopt_out == 0) then
          allocate (xtemp(ihi-ilo+1))
          kmin = sqrt(efix/c_k_to_emev)*abs(cos(twotheta))
          qmin = sqrt(efix/c_k_to_emev)*abs(sin(twotheta))
          qminsqr = qmin**2
          if (abs(qopt_in) == 2_i4b) then
             if (qopt_in*(2*emode-3) < 0_i4b) then
                xtemp = kmin - sqrt(xin(ilo:ihi) - qminsqr)
             else
                xtemp = kmin + sqrt(xin(ilo:ihi) - qminsqr)
             endif
          else
             if (qopt_in*(2*emode-3) < 0_i4b) then
                xtemp = kmin - sqrt((xin(ilo:ihi) + qmin)*(xin(ilo:ihi) - qmin))
             else
                xtemp = kmin + sqrt((xin(ilo:ihi) + qmin)*(xin(ilo:ihi) - qmin))
             endif
          endif
          istatus = units_xconvert (xtemp, 1, ihi-ilo+1, ctot, gtot, sgn_in, shift_in, sgn_out, shift_out, xout)
          if (istatus /= OK) then
             !				call remark ('ERROR: Problems in x-axis conversion')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Problems in x-axis conversion(IXFunits_convert)')                                   
             goto 100
          endif
          deallocate (xtemp)
       else
          if (nint(2*gtot) == 1) then	! we assume that we have fitered out all except the two possible case gtot=+1/2 or +2
             xout = sqrt(xin(ilo:ihi))
          else
             xout = xin(ilo:ihi)**2
          endif
       endif
    else
       if (qopt_out == 0) then
          istatus = units_xconvert (xin, ilo, ihi, ctot, gtot, sgn_in, shift_in, sgn_out, shift_out, xout)
          if (istatus /= OK) then
             !				call remark ('ERROR: Problems in x-axis conversion')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'ERROR: Problems in x-axis conversion(IXFunits_convert)')                                   
             
             goto 100
          endif
       else
          istatus = units_xconvert (xin, ilo, ihi, ctot, gtot, sgn_in, shift_in, sgn_out, shift_out, xout)
          if (istatus /= OK) then
             !				call remark ('ERROR: Problems in x-axis conversion')
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, &
                &'ERROR: Problems in x-axis conversion(IXFunits_convert)')                                                
             goto 100
          endif
          qx = sqrt(efix/c_k_to_emev)*abs(cos(twotheta))
          qysqr = (efix/c_k_to_emev)*(sin(twotheta))**2
          if (abs(qopt_out) == 2_i4b) then
             xout = (qx - xout)**2 + qysqr
          else
             xout = sqrt((qx - xout)**2 + qysqr)
          endif
       endif
    endif

    ! specific to distribution!!!

    ! Now perform units conversion
    if(dist_in)then
       yout = yin(ilo:ihi-1)*abs((xin(ilo+1:ihi)-xin(ilo:ihi-1))/(xout(2:ihi-ilo+1)-xout(1:ihi-ilo)))
       eout = ein(ilo:ihi-1)*abs((xin(ilo+1:ihi)-xin(ilo:ihi-1))/(xout(2:ihi-ilo+1)-xout(1:ihi-ilo)))
    else
       yout = yin(ilo:ihi-1)
       eout = ein(ilo:ihi-1)
    endif
    if (xout(ihi-ilo+1) < xout(1)) then	! reverse the order of the arrays
       xout = xout(ihi-ilo+1:1:-1)
       yout = yout(ihi-ilo:1:-1)
       eout = eout(ihi-ilo:1:-1)
    endif
    !	IXFunits_convert = OK

    return

    ! Error condition
    ! ------------------

100 continue
    xout = 0.0_dp
    yout = 0.0_dp
    eout = 0.0_dp
    !	IXFunits_convert = ERR
    ! error IXTstatus object has been filled at instance error    

    return
  end subroutine IXFunits_convert
  !-----------------------------------------------------------------------------------------------------------------------
  integer(i4b) function units_check_parameters (emode, delta, x1, x2, twotheta, efix)
    !
    !	Check parameter values for a workspace
    !
    !	Output	= 0	no problems (except possibly EMODE=0 when the units change will involve 1/sin(theta))
    !			= 1 if problem
    !

    implicit none
    integer(i4b), intent(IN) :: emode
    real(dp), intent(IN) :: delta, x1, x2, twotheta, efix
    real(dp), parameter :: small=1.0e-10_dp

    if (emode == 0) then
       if (x1 + x2 < small) then
          units_check_parameters = 1
          return
       endif
    else if (emode == 1 .or. emode == 2) then
       if (x1 < small .or. x2 < small .or. efix < small) then
          units_check_parameters = 1
          return
       endif
    else 
       units_check_parameters = 1
       return
    endif
    units_check_parameters = 0

    return
  end function units_check_parameters
  !-----------------------------------------------------------------------------------------------------------------------
 
  integer(i4b) function units_check_codes(emode,units_in,units_out)
    !
    !	Check units codes are consistent with emode value
    !
    !	Output	= 0	no problems 
    !			= 1 if problem
    !  
    implicit none
    integer(i4b), intent(IN) :: emode
    character(len=*), intent(IN) :: units_in, units_out
    integer(i4b)::i
    logical::found_in,found_out
    found_in=.false.
    found_out=.false.
    
    select case(emode)
    case(0)
      i=1
      do while ( (i<= n_0) .and. ((found_in .eqv. .false.) .or. (found_out .eqv. .false.)))   
        if(trim(units_in) == trim(u_0(i)))found_in=.true.
        if(trim(units_out) == trim(u_0(i)))found_out=.true.    
        i=i+1  
      enddo
    case(1)
      i=1
      do while ( (i<= n_1) .and. ((found_in .eqv. .false.) .or. (found_out .eqv. .false.)))   
        if(trim(units_in) == trim(u_1(i)))found_in=.true.
        if(trim(units_out) == trim(u_1(i)))found_out=.true.    
        i=i+1  
      enddo
    case(2)
      i=1
      do while ( (i<= n_2) .and. ((found_in .eqv. .false.) .or. (found_out .eqv. .false.)))   
        if(trim(units_in) == trim(u_2(i)))found_in=.true.
        if(trim(units_out) == trim(u_2(i)))found_out=.true.    
        i=i+1  
      enddo
! there is no case default, since an invalid emode flag should have been caught already    
    end select
    
    if(found_in .and. found_out)then
      units_check_codes=0
    else
      units_check_codes=1
    endif
    
  end function units_check_codes
  !-----------------------------------------------------------------------------------------------------------------------
  integer(i4b) function units_xconvert (xin, ilo, ihi, ctot, gtot, sgn_in, shift_in, sgn_out, shift_out, xout)
    implicit none

    integer(i4b), intent(in) :: ilo, ihi, sgn_in, sgn_out
    real(dp), intent(in) :: xin(:), ctot, gtot, shift_in, shift_out
    real(dp), intent(out) :: xout(:)

    real(dp):: coeff

    units_xconvert = OK
    if (nint(2.0_dp*gtot) == 4_i4b) then
       coeff = ctot*real(sgn_out)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff*(xin(ilo:ihi)**2)
          else
             xout = coeff*(xin(ilo:ihi)**2) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff*((xin(ilo:ihi)-shift_in)**2)
          else
             xout = coeff*((xin(ilo:ihi)-shift_in)**2) + shift_out
          endif
       endif
    else if (nint(2.0_dp*gtot) == -4_i4b) then
       coeff = ctot*real(sgn_out)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff/(xin(ilo:ihi)**2)
          else
             xout = coeff/(xin(ilo:ihi)**2) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff/((xin(ilo:ihi)-shift_in)**2)
          else
             xout = coeff/((xin(ilo:ihi)-shift_in)**2) + shift_out
          endif
       endif
    else if (nint(2.0_dp*gtot) == 2_i4b) then
       coeff = ctot*real(sgn_out*sgn_in)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff*xin(ilo:ihi)
          else
             xout = coeff*xin(ilo:ihi) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff*(xin(ilo:ihi)-shift_in)
          else
             xout = coeff*(xin(ilo:ihi)-shift_in) + shift_out
          endif
       endif
    else if (nint(2.0_dp*gtot) == -2_i4b) then
       coeff = ctot*real(sgn_out*sgn_in)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff/xin(ilo:ihi)
          else
             xout = coeff/xin(ilo:ihi) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff/(xin(ilo:ihi)-shift_in)
          else
             xout = coeff/(xin(ilo:ihi)-shift_in) + shift_out
          endif
       endif
    else if (nint(2.0_dp*gtot) == 1_i4b) then
       coeff = ctot*real(sgn_out)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff*sqrt(abs(xin(ilo:ihi)))
          else
             xout = coeff*sqrt(abs(xin(ilo:ihi))) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff*sqrt(abs(xin(ilo:ihi)-shift_in))
          else
             xout = coeff*sqrt(abs(xin(ilo:ihi)-shift_in)) + shift_out
          endif
       endif
    else if (nint(2.0_dp*gtot) == -1_i4b) then
       coeff = ctot*real(sgn_out)
       if (shift_in == 0.0_dp) then
          if (shift_out == 0.0_dp) then
             xout = coeff/sqrt(abs(xin(ilo:ihi)))
          else
             xout = coeff/sqrt(abs(xin(ilo:ihi))) + shift_out
          endif
       else
          if (shift_out == 0.0_dp) then
             xout = coeff/sqrt(abs(xin(ilo:ihi)-shift_in))
          else
             xout = coeff/sqrt(abs(xin(ilo:ihi)-shift_in)) + shift_out
          endif
       endif
    else
       units_xconvert = ERR
       xout = 0.0_dp
    endif

    return
  end function units_xconvert
  !-----------------------------------------------------------------------------------------------------------------------
  integer(i4b) function units_composite_coefficients (emode, delta, x1, x2, twotheta, efix, a_in, b_in, g_in, c_in, a_out, &
       b_out, g_out, c_out, ctot, gtot)
    !
    !	Gets constant and power for units interconversion, It is assumed that the input values are valid. The only case
    !	where all input are valid, but the output might fail is if sin(theta) is needed; this can be invalid if gtot /= 0
    !
    implicit none
    integer(i4b), intent(IN) :: emode
    real(dp), intent(IN) :: delta, x1, x2, twotheta, efix, a_in, b_in, g_in, c_in, a_out, b_out, g_out, c_out
    real(dp), intent(OUT) :: ctot, gtot

    real(dp), parameter :: small=1.0e-10_dp
    integer(i4b):: a_nm, b_nm
    real(dp):: x, s

    if (emode == 0) then
       x = x1 + x2
    else if (emode == 1) then
       x = x2
    else if (emode == 2) then
       x = x1
    endif

    a_nm = nint(a_out - a_in*(g_out/g_in))	! always integer
    b_nm = nint(b_out - b_in*(g_out/g_in))	! always integer

    gtot = (g_out/g_in)						! always -2, -1, -0.5, 0.5, 1, 2
    ctot = c_out * (c_in**(-gtot))
    units_composite_coefficients = OK

    if (a_nm /= 0) then
       ctot = ctot * x**a_nm
    endif

    if (b_nm /= 0) then
       s = abs(sin(0.5_dp*twotheta))
       if (s > small) then
          ctot = ctot * s**b_nm
       else
          units_composite_coefficients = ERR
       endif
    endif

    return
  end function units_composite_coefficients
  !-----------------------------------------------------------------------------------------------------------------------
  !!	Parses units_in, returning lower case value, opt, parameters for units conversion and graphics captions
  !!	If valid, units_coefficients = OK
  !!	If not,   units_coefficients = ERR

  function units_coefficients (units, emode, delta, x1, x2, twotheta, efix, units_parsed, a, b, g, c, sgn, shift, qopt)
    use IXMtools, only: locase
    implicit none

    integer(i4b), intent(IN) :: emode
    character(*), intent(IN) :: units
    real(dp), intent(IN) :: delta, x1, x2, twotheta, efix
    integer(i4b) :: units_coefficients
    integer(i4b), intent(OUT) :: sgn, qopt
    real(dp), intent(OUT) :: a,b,g,c, shift
    character(len=5), intent(OUT) :: units_parsed

    integer(i4b):: l_test, i
    character(len=5):: test


    l_test = len_trim(adjustl(units))
    if (l_test < 1 .or. l_test > 4) goto 100	! blank or unrecognised units
    test= adjustl(units)
    call locase(test)

    if (emode == 0) then		! check elastic options
       do i = 1, n_0
          if (test == u_0(i)) then
             units_parsed = test
             a = a_0(i)
             b = b_0(i)
             g = g_0(i)
             c = c_0(i)
             qopt = qopt_0(i)
             if (test == '$t   ') then
                sgn = 1_i4b
                shift = delta
             else
                sgn = 1_i4b
                shift = 0.0_dp
             endif
             units_coefficients = OK
             return
          endif
       end do
    else if (emode == 1) then	! direct geometry
       do i = 1, n_1
          if (test == u_1(i)) then
             units_parsed = test
             a = a_1(i)
             b = b_1(i)
             g = g_1(i)
             c = c_1(i)
             qopt = qopt_1(i)
             if (test == IXCcode_t) then
                sgn = 1_i4b
                shift = delta + x1*sqrt(c_t_to_emev/efix)
             else if (test ==IXCcode_w) then
                sgn = -1_i4b
                shift = efix
             else if (test ==IXCcode_wn) then
                sgn = -1_i4b
                shift = efix*c_emev_to_ewav
             else if (test ==IXCcode_thz) then
                sgn = -1_i4b
                shift = efix*c_emev_to_ethz
             else
                sgn = 1_i4b
                shift = 0.0_dp
             endif
             units_coefficients = OK
             return
          endif
       end do
    else if (emode == 2) then	! indirect geometry
       do i = 1, n_2
          if (test == u_2(i)) then
             units_parsed = test
             a = a_2(i)
             b = b_2(i)
             g = g_2(i)
             c = c_2(i)
             qopt = qopt_2(i)
             if (test == IXCcode_t) then
                sgn = 1_i4b
                shift = delta + x2*sqrt(c_t_to_emev/efix)
             else if (test ==IXCcode_w) then
                sgn = 1_i4b
                shift = -efix
             else if (test ==IXCcode_wn) then
                sgn = 1_i4b
                shift = -efix*c_emev_to_ewav
             else if (test ==IXCcode_thz) then
                sgn = 1_i4b
                shift = -efix*c_emev_to_ethz
             else
                sgn = 1_i4b
                shift = 0.0_dp
             endif
             units_coefficients = OK
             return
          endif
       end do
    endif

100 continue
    units_parsed = '     '
    a = 0.0_dp
    b = 0.0_dp
    g = 0.0_dp
    c = 0.0_dp
    shift = 0.0_dp
    qopt = 0_i4b
    units_coefficients = ERR

    return
  end function units_coefficients

end module IXMunits_utils
