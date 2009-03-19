!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMmask_array
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 560 $ ($Date: 2006-01-11 18:56:54 +0000 (Wed, 11 Jan 2006) $)
!!
!! Fortran routine to mask the elements of an array that fail to satisfy a set of criteria about absolute magnitude,
!! relationship to median etc.
module IXMmask_array
  use IXMtype_definitions
  use IXMsort
  use IXMstatus
  integer, parameter :: IXCmask_OK=0          ! Elements OK
  integer, parameter :: IXCmask_notfinite=1   ! Elements not finite
  integer, parameter :: IXCmask_zero=2        ! Zero valued integral element
  integer, parameter :: IXCmask_tiny=3        ! Less than a very small value (e.g. -10**30 - an 'unreasonable' number
  ! for the given problem)
  integer, parameter :: IXCmask_huge=4        ! Greater than a large value (an 'unreasonably large' value for the given problem)
  integer, parameter :: IXCmask_low=5         ! Less than a lower acceptable threshold
  integer, parameter :: IXCmask_high=6        ! Greater than an upper threshold
  integer, parameter :: IXCmask_unlikely=7    ! Statistically unlikely
  integer, parameter :: IXCmask_ratio=8       ! Lies outside a threshold multiple i.e. does not satisfy
  !      1/r =< value/reference_value =< r
  integer, parameter :: IXCmask_hard=9        ! masked by hard mask array
  integer, parameter :: IXCmask_zero_total=10      ! zero total counts in sample spectrum
  integer, parameter :: IXCmask_NaN=11        ! spectrum integral is a NaN
contains
  !----------------------------------------------------------------------------------------------------------------------
  !  Compares a pair of values and masks those whose ratio does not satisfy
  !
  !           1/r =<  ratio/median_ratio  =< r 
  !
  !  The algorithm is symmetric so can swap the order of the two tables of values as input arguments but still get the
  ! same answer.
  !  The mask array may already indicate masked spectra on entry; only the unmasked values are used in the routine
  ! i.e. the results are accumulative, acting only on the unmasked values.
  !  It is assumed that the values are all greater than zero.
  !  The reason for masking is encoded in CAUSE, if present.
  !
  !	subroutine IXFmask_ratio_positive_array (value1, err1, value2, err2, mask, r, r_sig, cause)
  !
  ! Input:
  !	value1(:)	    	dp		values
  !	err1(:)			    dp		errors
  !	value2(:)	    	dp		values
  !	err2(:)			    dp		errors
  !
  ! Input/output:
  !	mask(:)			    logical	The ith value is unmasked if mask(i) = .T.; masked otherwise
  !                               Results accumulate i.e. this routine can be called several times
  !                               with different options, and the mask array contains the cumulative
  !                               result. 
  ! Input (optional):
  !   r                   dp      ratio threshold, r>=1 (if r<1 then r->1/r internally)
  !	r_sig				dp		error criterion as a multiple of error bar i.e. if the test is to fail, the (ratio/median_ratio)
  !                              must also be at least this number of error bars different to unity
  !
  ! Input/output:
  !	mask(:)			    logical	The ith value is unmasked if mask(i) = .T.; masked otherwise
  !                               Results accumulate i.e. this routine can be called several times
  !                               with different options, and the mask array contains the cumulative
  !                               result. 
  !
  ! Input/output (optional)
  !	cause(:)			i4b		Reason for masking a spectrum (integer constants)

  subroutine IXFmask_ratio_positive_array (value1, err1, value2, err2, mask, r, r_sig, cause,c_value)
    implicit none

    ! Input:
    real(dp), dimension(:), intent(in) :: value1, err1, value2, err2
    real(dp), optional, intent(in) :: r, r_sig

    ! In/out:
    logical, dimension(:), intent(INOUT) :: mask
    integer(i4b), optional, dimension(:), intent(INOUT) :: cause
    real(dp),optional,dimension(:),intent(inout)::c_value
    ! Local:
    integer(i4b) :: n_mask,size_v1
    !    integer(i4b), dimension(size(value1)) :: cause_work
    !    real(dp),dimension(size(value1))::cause_value

    integer(i4b),allocatable :: cause_work(:)
    real(dp),allocatable::cause_value(:)

    integer(i4b), dimension(:), allocatable :: ind
    real(dp) :: zfac, z_sig, ratio_median,min_stab,max_stab
    real(dp), allocatable :: z(:), zerr(:)
    real(dp), dimension(:), allocatable :: ratio

    size_v1=size(value1)
    allocate(cause_work(size_v1))
    allocate(cause_value(size_v1))
    allocate(z(size_v1))
    allocate(zerr(size_v1))
    if (present(cause)) cause_work = cause  ! copy if input argument present
    if (present(c_value)) cause_value = c_value  ! copy if input argument present    



    ! Check that discrimination parameters have sensible values:
    ! ----------------------------------------------------------------
    ! (ought to have a check on r<=0, but I'm not happy with the Fortran error handling)
    ! (also should check if r_sig <0)


    ! Ratio values (we will have already ensured that value2/value1 for unmasked elements is defined)
    ! -------------------------------------------------------------------------------------------------------
    n_mask=count(mask)
    if (n_mask==0) return	! all masked out on entry

    allocate (ratio(n_mask),ind(n_mask))
    ratio = pack (value2/value1, mask)

    ! Find median value of ratio of unmasked vanadium integrals (do NOT use IXFmedian - obvious why below):
    call IXFrank (ratio, ind)
    if (modulo(n_mask,2) == 1) then		! odd number of unmasked spectra
       ratio_median = ratio(ind(n_mask/2+1))
    else
       ratio_median = sqrt( ratio(ind(n_mask/2)) * ratio(ind(n_mask/2+1)) )	! ensures symmetric treatment of V1 and V2
    endif
    deallocate (ratio,ind)


    ! Find integrals that lie outside acceptable range:
    ! ---------------------------------------------------

    where (mask)
       z = (value2/value1)/ratio_median
       zerr = sqrt((err1/value1)**2 + (err2/value2)**2)	! only true if ratio_median appox= 1; zerr=zerr*z in fact
    elsewhere
       z = 0.0_dp
       zerr = 0.0_dp
    end where

    zfac = 0.0_dp
    z_sig= 0.0_dp
    if (present(r)) zfac = max(r,1/r)	! r always >=1
    if (present(r_sig)) z_sig = r_sig
    max_stab=1.0d0+(abs(zfac)/100.0d0) ! ie if zfac=10% then max ratio is 1.1
    min_stab=1.0d0/max_stab  ! and then min =1.0/1.1 ~=0.909

    ! this does not seem to be VMS diag testing criterion wrt zfac...
    !	where (mask .and. ( (z>max(zfac,1+z_sig*zerr)) .or. (z<min(1/zfac,1/(1+z_sig*zerr))) ))
    !		mask = .FALSE.
    !		cause_work = IXCmask_ratio
    !	end where
    ! let's first mirror VMS diag
    ! TGP can provide precise z_sig usage
    where (mask .and. ( z > max_stab .or. z<min_stab ))
       mask = .FALSE.
       cause_work = IXCmask_ratio
       cause_value = z
    end where



    if (present(cause)) cause = cause_work  ! copy if input argument present
    if (present(c_value)) c_value = cause_value  ! copy if input argument present
    deallocate(cause_value,cause_work,z,zerr)
    return
  end subroutine IXFmask_ratio_positive_array


  !----------------------------------------------------------------------------------------------------------------------
  ! Given a table of values and assosiated errors the subroutine masks which values satisfy various criteria
  ! defined by the optional input parameters.
  ! The mask array may already indicate masked spectra on entry; only the unmasked values are used in the routine
  ! i.e. the results are accumulative, acting only on the unmasked values.
  !
  ! * Removes ridiculously small and large values (<tiny and >huge)
  ! * Calculates median of remaining integrals, ignoring outliers (which are defined by out_lo, out_hi)
  ! * Masks spectra for which
  !            value(i) < v_lo*median   and  (value(i)-median) < -|v_sig|*err(i)
  !            value(i) > v_hi*median   and  (value(i)-median) >  |v_sig|*err(i)
  !
  ! The reason for masking is encoded in CAUSE, if present.
  !
  !	subroutine IXFmask_positive_array (value, err, mask, zero, tiny, huge, out_lo, out_hi, v_lo, v_hi, v_sig, cause)
  !
  ! Input:
  !	value(:)	    	dp		values
  !	err(:)			    dp		errors
  !
  ! Input/output:
  !	mask(:)			    logical	The ith value is unmasked if mask(i) = .T.; masked otherwise
  !                               Results accumulate i.e. this routine can be called several times
  !                               with different options, and the mask array contains the cumulative
  !                               result. 
  ! Input (optional):
  !   zero                log     .T. if zeros to be masked
  !   tiny                dp      lower bound of meaningful value
  !   huge                dp      upper bound of meaningful value
  !	out_lo				dp		lower bound defining outliers as fraction of median value
  !	out_hi				dp		upper bound defining outliers as fraction of median value
  !	v_lo				dp		lower acceptable bound as fraction of median value
  !	v_hi				dp		upper acceptable bound as fraction of median value
  !	v_sig				dp		error criterion as a multiple of error bar i.e. to fail the test, the magnitude of the
  !                              difference with respect to the median value must also exceed this number of error bars
  !
  !
  ! Input/output (optional)
  !	cause(:)			i4b		Reason for masking a spectrum (integer constants)

  subroutine IXFmask_positive_array (value, err, mask, zero, tiny, huge, out_lo, out_hi, v_lo, v_hi, v_sig, cause,c_value,&
       c_error,c_file)
    implicit none

    ! Input:
    real(dp), dimension(:), intent(in) :: value, err
    real(dp), optional, intent(in) :: tiny, huge, out_lo, out_hi, v_lo, v_hi, v_sig
    logical, optional, intent(in) :: zero
    ! In/out:
    logical, dimension(:), intent(INOUT) :: mask
    real(dp),dimension(:),optional,intent(inout)::c_value,c_error
    integer(i4b), optional, dimension(:), intent(INOUT) :: cause,c_file

    ! Local:
    integer(i4b) :: n_mask,size_v
    integer(i4b), allocatable :: cause_work(:)
    real(dp),allocatable :: cause_value(:),cause_error(:),cause_file(:)
    real(dp) :: median_val
    real(dp),allocatable::value_alloc(:)

    size_v=size(value)
    allocate(cause_work(size_v))
    allocate(cause_value(size_v))
    allocate(cause_error(size_v))
    allocate(cause_file(size_v))
    if (present(cause)) cause_work = cause  ! copy if input argument present    
    if (present(c_value)) cause_value = c_value  ! copy if input argument present    
    if (present(c_error)) cause_error = c_error  ! copy if input argument present    
    if (present(c_file)) cause_file = c_file  ! copy if input argument present        


    ! Mask out very large or very small integrals:
    ! --------------------------------------------
    ! Should mask out NaN and infinte elements as well. Are there widely-used intrinsics to do this ?
    ! isnan() is not strictly STANDARD fortran, but is valid in both g95 and intel fortran
    where (mask .and. isnan(value))
       mask = .FALSE.
       cause_work = IXCmask_NaN
       cause_file = -1
    end where


    if (present(zero) .and. zero) then
       where (mask .and. value == 0.0_dp)
          mask = .FALSE.
          cause_work = IXCmask_zero
          cause_file = -1
       end where
    endif

    if (present(tiny)) then
       where (mask .and. value < tiny .and. value > 0.0d0) ! otherwise 0.0 values will be masked
          mask = .FALSE.
          cause_work = IXCmask_tiny
          cause_value = value
          cause_error = err   		
          cause_file = -1 ! this is just as a marker which is changed after maskintegral process
       end where
    endif

    if (present(huge)) then
       where (mask .and. value > huge)
          mask = .FALSE.
          cause_work = IXCmask_huge
          cause_value = value
          cause_error = err    		
          cause_file = -1 ! this is just as a marker which is changed after maskintegral process    		
       end where
    endif

    if (present(v_lo) .OR. present(v_hi) .OR. present(v_sig)) then
       ! Find median value of remaining vanadium integrals:
       n_mask = count(mask)
       if (n_mask == 0) then
          if (present(cause)) cause=cause_work
          return
       endif
       ! 

       allocate(value_alloc(n_mask))
       call IXFpack(value,mask,value_alloc)
       median_val=IXFmedian(value_alloc)
       !       median_val = IXFmedian(pack(value,mask))
       deallocate(value_alloc)
       ! Remove outliers, and recalculate median
       if (present(out_lo) .or. present(out_hi)) then
          ! Remove outliers
          if (present(out_lo)) then
             where (mask .and. (value < out_lo*median_val) .and. value > 0.0d0) ! otherwise 0.0 values will be masked
                mask = .FALSE.
                cause_work = IXCmask_low
                cause_value = value/median_val
                cause_error = err/median_val    		
                cause_file = -1 ! this is just as a marker which is changed after maskintegral process
             end where
          endif
          if (present(out_hi)) then
             where (mask .and. (value > out_hi*median_val) )
                mask = .FALSE.
                cause_work = IXCmask_high
                cause_value = value/median_val
                cause_error = err /median_val   		
                cause_file = -1 ! this is just as a marker which is changed after maskintegral process
             end where
          endif
          ! Find median value of remaining vanadium integrals - a better value to work with as outliers removed:
          n_mask = count(mask)
          if (n_mask == 0) then
             if (present(cause)) cause=cause_work
             return
          endif
          allocate(value_alloc(n_mask))
          call IXFpack(value,mask,value_alloc)
          median_val=IXFmedian(value_alloc)
          deallocate(value_alloc)
          !          median_val = IXFmedian(pack(value,mask))
       endif

       ! Find integrals that lie outside acceptable range about median value:
       ! --------------------------------------------------------------------
       if (present(v_lo) .AND. present(v_sig)) then
          where (mask .and. (value < median_val*v_lo) .and. (value-median_val < -abs(v_sig)*err))
             mask = .FALSE.
             cause_work = IXCmask_low
             cause_value = value/median_val
             cause_error = err/median_val 		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       elseif (present(v_lo)) then
          where (mask .and. (value < median_val*v_lo))
             mask = .FALSE.
             cause_work = IXCmask_low
             cause_value = value/median_val
             cause_error = err/median_val   		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       elseif (present(v_sig)) then
          where (mask .and. (value-median_val < -abs(v_sig)*err))
             mask = .FALSE.
             cause_work = IXCmask_low
             cause_value = value/median_val
             cause_error = err/median_val   		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       endif
       if (present(v_hi) .AND. present(v_sig)) then
          where (mask .and. (value > median_val*v_hi) .and. (value-median_val > abs(v_sig)*err))
             mask = .FALSE.
             cause_work = IXCmask_high
             cause_value = value/median_val
             cause_error = err /median_val   		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       elseif (present(v_hi)) then
          where (mask .and. (value > median_val*v_hi))
             mask = .FALSE.
             cause_work = IXCmask_high
             cause_value = value/median_val
             cause_error = err /median_val   		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       elseif (present(v_sig)) then
          where (mask .and. (value-median_val > abs(v_sig)*err))
             mask = .FALSE.
             cause_work = IXCmask_high
             cause_value = value/median_val
             cause_error = err/median_val   		
             cause_file = -1 ! this is just as a marker which is changed after maskintegral process
          end where
       endif
    endif


    if (present(cause)) cause = cause_work  ! copy if input argument present
    if (present(c_value)) c_value = cause_value  ! copy if input argument present
    if (present(c_error)) c_error = cause_error  ! copy if input argument present
    if (present(c_file)) c_file = cause_file  ! copy if input argument present
    deallocate(cause_work,cause_value,cause_error,cause_file)

  end subroutine IXFmask_positive_array

  subroutine IXFmask_sample_array(value,error,mask,zero,v_hi,v_sig,cause,c_value,c_error)
    implicit none
    real(dp),intent(in)::value(:),error(:),v_hi,v_sig
    logical,intent(in)::zero
    logical,intent(inout)::mask(:)
    integer(i4b),intent(inout)::cause(:)
    real(dp),intent(inout)::c_value(:),c_error(:)
    logical,allocatable::t_mask(:)
    integer(i4b),allocatable::t_cause(:)
    real(dp)::t_median
    integer(i4b)::n_mask
    real(dp),allocatable::value_alloc(:)

    allocate(t_mask(size(mask)),t_cause(size(mask)))
    t_mask=mask
    t_cause=cause
    n_mask=count(mask)

    !remove zeros for temporary median calculation
    where (t_mask .and. value == 0.0_dp)
       t_mask = .FALSE.
       t_cause = IXCmask_zero	   
    end where

    allocate(value_alloc(count(t_mask)))
    call IXFpack(value,t_mask,value_alloc)
    t_median=IXFmedian(value_alloc)
    deallocate(value_alloc)

    !    t_median = IXFmedian(pack(value,t_mask))

    where (mask .and. (value > (t_median*v_hi)) .and. ((value-t_median) > (abs(v_sig)*error)))
       mask = .FALSE.
       cause = IXCmask_high
       c_value=value/t_median
       c_error=error/t_median
    end where
    !! if you were going to initiate masking for low sample spectra using sv_lo
    !! would also need to be an argument to this function
    !      where (mask .and. (value < t_median*v_lo) .and. (t_median-value > abs(v_sig)*error))
    !	    mask = .FALSE.
    !	    cause = IXCmask_low
    !	    c_value=value/t_median
    !	    c_error=error/t_median
    !      end where      

    if (zero)then
       where( .not.t_mask )
          mask=t_mask
          cause=t_cause                    
       end where
    endif
    deallocate(t_cause,t_mask)

  end subroutine IXFmask_sample_array
  !----------------------------------------------------------------------------------------------------------------------
  ! Given a table of values and assosiated weights and an optional mask array, work out statistical values
  !
  !   subroutine average_array (value, weight, mask, average, sigma, median)
  !
  ! Input:
  !	value(:)	    	dp		values
  !	weight(:)			dp		weight e.g. errors
  !
  ! Input (optional):
  !   mask(:)             log     mask array: .T. if to be included, .F. if not
  !
  ! Output (optional):
  !   average             dp      weighted average (equal weights used if none provided on input)
  !   sigma               dp      weighted sigma
  !   median              dp      median


  subroutine IXFaverage_array (value, weight, mask, average, sigma, median)
    implicit none

    ! Input:
    real(dp), dimension(:), intent(in) :: value, weight
    logical, optional, dimension(:), intent(in) :: mask
    ! Output:
    real(dp), optional, intent(out) :: average, sigma, median
    real(dp),allocatable::value_alloc(:)

    if (present(mask)) then
       if (present(average))average=sum((value*weight),mask=mask)/sum(weight,mask=mask)
       if (present(sigma))sigma=sum((value-average)*weight,mask=mask)/sum(weight,mask=mask)
       if (present(median))then
          !          median=IXFmedian(pack(value,mask))
          allocate(value_alloc(count(mask)))
          call IXFpack(value,mask,value_alloc)
          median=IXFmedian(value_alloc)
          deallocate(value_alloc)
       endif

    else
       if (present(average))average=sum((value*weight))/sum(weight)
       if (present(sigma))sigma=sum((value-average)*weight)/sum(weight)
       if (present(median))median=IXFmedian(value)        
    endif

  end subroutine IXFaverage_array
  !----------------------------------------------------------------------------------------------------------------------
  subroutine IXFpack(arr_in,mask,arr_out)
    real(dp),intent(out)::arr_out(:)
    real(dp),intent(in)::arr_in(:)
    logical,intent(in)::mask(:)
    integer(i4b)::i,j,packed_len,src_len
    type(IXTstatus)::s
    packed_len=count(mask)      
    if(size(arr_out)/=packed_len)then
       arr_out=0.0
       call IXBwrite_line('this should not happen',s)
       return
    endif
    src_len=size(arr_in)

    if(src_len == packed_len)then
       arr_out=arr_in
       return
    endif

    j=1 !from 1 to packed_len
    do i=1,src_len
       if(mask(i) .eqv. .true.)then
          arr_out(j)=arr_in(i)
          j=j+1
       endif
    enddo
  end subroutine IXFpack

end module IXMmask_array
