!------------------------
!
! Test for FORTRAN unit test codes
!
! Author: Andrew H. Chen chena@westinghouse.com
!------------------------
!
module dataset_1d_test
  use fruit
  use IXMdataset_1d
  use IXMdatum
  
  interface res_ptr
    module procedure wres_ptr,ires_val
  end interface
    
contains
  ! ----
  ! Run all the test sub routines 
  ! ----
  
  subroutine d1d_Tests
    use IXMlibcore
    implicit none
    type(IXTdataset_1d)::w1_hd,w1_h,w1_pd,w1_p !four different input pairs dataset_1d objects
    type(IXTdataset_1d)::w2_hd,w2_h,w2_pd,w2_p
    !  h=histogram p=point d=distribution and combinations thereof
    real(dp)::x_h(21),x_p(20),s1(20),e1(20),s2(20),e2(20)
    type(IXTstatus)::status
    integer(i4b)::i
    real(dp)::xmin,delta,xmax
    
    write(6,*) 'Starting dataset_1d unit test program'
    if (IXFlibrary_init() /= 0) then
      stop 'error initialising library'
    endif

    ! first we need to create test objects
    x_h=(/(i,i=1,41,2)/)
    x_p=(/(i,i=2,40,2)/)

    xmin=5.0_dp
    delta=3.0_dp
    xmax=35.0_dp
    s1=2.0_dp
    e1=1.0_dp
    s2=2.0_dp
    e2=1.0_dp

    call IXFcreatexye_dataset_1d(w1_hd,x_h,s1,e1,status)
    !default creation is xdist=false
    call IXFset_dataset_1d(w1_hd,status,x_distribution=.true.)

    call IXFcreatexye_dataset_1d(w1_pd,x_p,s1,e1,status)
    !default creation is xdist=false
    call IXFset_dataset_1d(w1_pd,status,x_distribution=.true.)

    call IXFcreatexye_dataset_1d(w1_h,x_h,s1,e1,status)
    call IXFcreatexye_dataset_1d(w1_p,x_p,s1,e1,status)

    call IXFcreatexye_dataset_1d(w2_hd,x_h,s2,e2,status)
    !default creation is xdist=false
    call IXFset_dataset_1d(w2_hd,status,x_distribution=.true.)

    call IXFcreatexye_dataset_1d(w2_pd,x_p,s2,e2,status)
    !default creation is xdist=false
    call IXFset_dataset_1d(w2_pd,status,x_distribution=.true.)

    call IXFcreatexye_dataset_1d(w2_h,x_h,s2,e2,status)
    call IXFcreatexye_dataset_1d(w2_p,x_p,s2,e2,status)
    
    ! test simple array operations
    call arrayoptests(w1_h,w2_h)
    call arrayoptests(w1_hd,w2_hd)
    call arrayoptests(w1_p,w2_p)
    call arrayoptests(w1_pd,w2_pd)
    
    !integration tests
    call integrate_test(w1_hd,xmin,xmax,60.0_dp)
    call integrate_test(w1_h,xmin,xmax,30.0_dp)
    ! trapezium sum on point dataset will be 14 with this data
    call integrate_test(w1_pd,xmin,xmax,60.0_dp)
    call integrate_test(w1_p,xmin,xmax,60.0_dp)
    !rebunch function tests
    call rebunch_test(w1_hd,2.0_dp)
    call rebunch_test(w1_pd,2.0_dp)!points distribution relatively meaningless
    call rebunch_test(w1_h,10.0_dp)
    call rebunch_test(w1_p,2.0_dp)!operation also meaningless for points only as well
 
    call rebin_test(w2_hd,(/ xmin,delta,xmax /),2.0_dp)
    call rebin_test(w2_h,(/ xmin,delta,xmax /),6.0_dp)
    call rebin_test(w2_pd,(/ xmin,delta,xmax /),fail=.true.)
    call rebin_test(w2_p,(/ xmin,delta,xmax /),fail=.true.)
    
    call regroup_test(w2_hd,(/ xmin,delta,xmax /),2.0_dp)
    call regroup_test(w2_h,(/ xmin,delta,xmax /),4.0_dp)
    call regroup_test(w2_pd,(/ xmin,delta,xmax /),fail=.true.)
    call regroup_test(w2_p,(/ xmin,delta,xmax /),fail=.true.)
    
    
  end subroutine d1d_Tests
 
  function wres_ptr(res_obj)result(result_ptr)
  implicit none
  real(dp),pointer::result_ptr(:)
  type(IXTdataset_1d)::res_obj
  call IXFget_ptr_dataset_1d(res_obj,signal=result_ptr)
  end function 

  function ires_val(res_obj)result(result_val)
  implicit none
  real(dp)::result_val
  type(IXTstatus)::status
  type(IXTdatum)::res_obj
  call IXFget_datum(res_obj,status,val=result_val)
  end function 

  
subroutine integrate_test(w_in,x_min,x_max,expect)
use IXMdataset_1d
use IXMdatum
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdatum)::ires
type(IXTstatus)::status
real(dp)::x_min,x_max,expect
call IXFintegrate_dataset_1d(ires,w_in,x_min,x_max,status)
call assertEquals(res_ptr(ires),expect,'integration test failed')
write(*,*)
call IXFclear_status(status)

end subroutine integrate_test

subroutine rebunch_test(w_in,expected)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
integer(i4b)::Xbunch
real(dp)::calc(4),expected

call IXFrebunch_dataset_1d(wres,w_in,5,status)
calc=expected
call assertEquals(res_ptr(wres),calc,4,'rebunch test failed')
write(*,*)
call IXFclear_status(status)

end subroutine rebunch_test

subroutine regroup_test(w_in,prm,expected,fail)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
integer(i4b)::Xbunch
real(dp)::prm(3),calc(7)
real(dp),optional::expected
logical,optional::fail


call IXFregroup_dataset_1d(wres,w_in,prm,status)
if(present(fail))then
  call assertTrue (.not.(status /= IXCseverity_error), 'array operations test status failed')
else
calc=expected
call assertEquals(res_ptr(wres),calc,7,'regroup test failed')
endif


write(*,*)
call IXFclear_status(status)

end subroutine regroup_test

subroutine rebin_test(w_in,prm,expected,fail)
use IXMdataset_1d
implicit none
type(IXTdataset_1d),intent(in)::w_in
type(IXTdataset_1d)::wres
type(IXTstatus)::status
real(dp)::prm(3),calc(10)
real(dp),optional::expected
logical,optional::fail

call IXFrebin_dataset_1d(wres,status,w_in,Xdesc=prm)

if(present(fail))then
  call assertTrue (.not.(status /= IXCseverity_error), 'array operations test status failed')
else
calc=expected
call assertEquals(res_ptr(wres),calc,10,'rebin test failed')
endif
write(*,*)
call IXFclear_status(status)

end subroutine rebin_test


subroutine arrayoptests(w1,w2)
implicit none
type(IXTdataset_1d),intent(in)::w1,w2
type(IXTdataset_1d)::wres
type(IXTstatus)::status
real(dp),pointer::sig(:)
real(dp)::calc(20)

call IXFplus(wres,w1,2.0_dp,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-const addition test failed')
write(*,*)
call IXFminus(wres,w1,0.75_dp,status)
calc=1.25_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-const subtraction test failed')
write(*,*)
call IXFtimes(wres,w1,3.0_dp,status)
calc=6.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-const mulitplication test failed')
write(*,*)
call IXFdivide(wres,w1,2.0_dp,status)
calc=1.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-const division test failed')
write(*,*)
call IXFpower(wres,w1,3.0_dp,status)
calc=8.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-const power test failed')
write(*,*)

call IXFplus(wres,2.0_dp,w1,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'const-d1d addition test failed')
write(*,*)
call IXFminus(wres,2.0_dp,w1,status)
calc=0.0_dp
call assertEquals(res_ptr(wres),calc,20,'const-d1d subtraction test failed')
write(*,*)
call IXFtimes(wres,4.0_dp,w1,status)
calc=8.0_dp
call assertEquals(res_ptr(wres),calc,20,'const-d1d multiplication test failed')
write(*,*)
call IXFdivide(wres,8.0_dp,w1,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'const-d1d division test failed')
write(*,*)
call IXFpower(wres,5.0_dp,w1,status)
calc=25.0_dp
call assertEquals(res_ptr(wres),calc,20,'const-d1d power test failed')
write(*,*)

call IXFplus(wres,w2,w1,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-d1d addition test failed')
write(*,*)
call IXFminus(wres,w2,w1,status)
calc=0.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-d1d subtraction test failed')
write(*,*)
call IXFtimes(wres,w2,w1,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-d1d multiplication test failed')
write(*,*)
call IXFdivide(wres,w2,w1,status)
calc=1.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-d1d division test failed')
write(*,*)
call IXFpower(wres,w2,w1,status)
calc=4.0_dp
call assertEquals(res_ptr(wres),calc,20,'d1d-d1d power test failed')
write(*,*)

call IXFexp(wres,w1,status)
call IXFlog(wres,w1,status)
call IXFsin(wres,w1,status)
call IXFcos(wres,w1,status)
call IXFtan(wres,w1,status)
call IXFsinh(wres,w1,status)
call IXFcosh(wres,w1,status)
call IXFtanh(wres,w1,status)

call assertTrue ((status /= IXCseverity_error), 'array operations test status failed')
write(*,*)
call IXFclear_status(status)

end subroutine arrayoptests

  
!  subroutine allFruitTest  
!    implicit none;
!    call initializeFruitTest;
!    
!    call markTest()
!    
!    call assertTrueResultTest;
!    call assertTrueResultTest_invalid;
!    call assertTrueResultMessageTest;
!    
!    call assertTrueTest;
!    call assertTrueMessageTest;
!    
!    call addSuccessTest;
!    call addSuccessMessageTest;
!    call addFailTest;
!    call addFailMessageTest;
!    call getTestSummaryTest;
!    call isAllSuccessfulTest;
!    call showOutputForReport;
!    
!    call showOutputTest;
!    
!    call testAssertEqualsFloat;
!  end subroutine allFruitTest
!  
!  ! ----
!  subroutine initializeFruitTest  
!    use fruit
!    implicit none;
!
!    call initializeFruit;
!    return
!  end subroutine initializeFruitTest
!  
!  subroutine markTest
!    use fruit
!    
!    write (*,*) "Should see . here:"
!    call successfulMark()
!    write (*,*) "Should see .. here:"
!    call successfulMark()
!    call successfulMark()
!
!    write (*,*) 
!
!    write (*,*) "Should see F here:"
!    call failedMark()
!  
!    write (*,*) "Should see FF here:"
!    call failedMark()
!    call failedMark()
!  
!    write (*,*) 
!  end subroutine markTest
!  
!  ! ----
!  subroutine assertTrueResultTest 
!    use fruit
!    implicit none;
!    logical :: inputValue;
!    logical :: resultValue = .FALSE.;
!    
!    inputValue = .true.
!    
!    call assertTrue (inputValue, resultValue)
!    
!    IF (resultValue .eqv. .true.) then 
!      write (*,*) 'assertTrueResultTest Successful'
!    else 
!      write (*,*) 'assertTrueResultTest FAILED!!!'
!    end IF
!    
!    return
!  end subroutine assertTrueResultTest
!  
!  ! ----
!  subroutine assertTrueResultTest_invalid
!    use fruit
!    implicit none;
!    logical :: inputValue;
!    logical :: resultValue = .FALSE.;
!    
!    inputValue = .FALSE.
!    
!    call assertTrue (inputValue, resultValue)
!    
!    IF (resultValue .neqv. .true.) then 
!      write (*,*) 'assertTrueResultTest_invalid Successful'
!    else 
!      write (*,*) 'assertTrueResultTest_invalid FAILED!!!'
!    end IF
!    
!    return
!  end subroutine assertTrueResultTest_invalid
!
!  !------------
!  ! Test assert with result and message
!  !------------  
!  subroutine assertTrueResultMessageTest
!    use fruit
!    implicit none;
!    logical :: inputValue;
!    logical :: resultValue;
!    
!    inputValue = .true.
!    resultValue = .FALSE.
!    
!    call assertTrue (inputValue, 'Test assertTrue (input, msg, result) message.', resultValue)
!    
!    IF (resultValue .eqv. .true.) then 
!      write (*,*) 'assertTrueResultMessageTest Successful'
!    else 
!      write (*,*) 'assertTrueResultMessageTest FAILED!!!'
!    end IF
!    
!    write (*,*) 'Should see 1 successful case';
!    call getTestSummary
!    
!    return
!  end subroutine assertTrueResultMessageTest
!  
!  ! ----
!  ! Test assertTrue
!  ! ----
!  subroutine assertTrueTest 
!    use fruit
!    implicit none;
!
!    write (*,*) 'Should see 1 successful case';
!    call initializeFruit;
!    call assertTrue (.true.);
!    call getTestSummary
!
!    write (*,*) 'Shoule see 1 failed case';
!    call initializeFruit;
!    call assertTrue (.FALSE.);
!    call getTestSummary
!    
!    return
!  end subroutine assertTrueTest
!  
!  ! ----
!  ! Test assertTrue with message
!  ! ----
!  subroutine assertTrueMessageTest 
!    use fruit
!    implicit none;
!    
!    write (*,*) 'Should see 1 successful case';
!    call initializeFruit;
!    call assertTrue (.true., 'message in assertTrueTest_message true test');
!    call getTestSummary
!
!    write (*,*) 'Should see 1 failed case';
!    call initializeFruit;
!    call assertTrue (.FALSE., 'message in assertTrueTest_message false test');
!    call getTestSummary
!    
!    return
!  end subroutine assertTrueMessageTest
!  
!  ! -----
!  ! Test addSuccess and isAllSuccessful
!  ! -----
!  subroutine addSuccessTest
!    use fruit
!    implicit none;
!    
!    logical :: result = .FALSE.
!
!    call initializeFruit;
!    call addSuccess
!    call isAllSuccessful (result);
!
!    IF (result .neqv. .true.) then
!      write (*, *);
!      write (*, *) 'FAILED addSuccess!!!';
!      write (*, *);
!    end IF
!    
!  end subroutine addSuccessTest
!  
!  subroutine addSuccessMessageTest
!    use fruit
!    implicit none;
!
!    call addSuccess ('Success in this subroutine: addSuccessMessageTest');
!    
!    return
!  end subroutine addSuccessMessageTest
!  
!  ! -----
!  ! Test addSuccess and isAllSuccessful
!  ! -----
!  subroutine addFailTest
!    use fruit
!    implicit none;
!    
!    logical :: result = .FALSE.
!
!    call initializeFruit;
!    call addFail
!    call isAllSuccessful (result);
!
!    IF (result .neqv. .FALSE.) then
!      write (*, *);
!      write (*, *) 'FAILED addFail !!!';
!      write (*, *);
!    end IF
!    
!  end subroutine addFailTest
!  
!  ! -----
!  ! Test addSuccess and isAllSuccessful
!  ! -----
!  subroutine addFailMessageTest
!    use fruit
!    implicit none;
!    
!    logical :: result = .FALSE.
!
!    call initializeFruit;
!    call addFail ('Add a failed case');
!    call isAllSuccessful (result);
!
!    IF (result .neqv. .FALSE.) then
!      write (*, *);
!      write (*, *) 'FAILED addFail !!!';
!      write (*, *);
!    end IF
!    
!  end subroutine addFailMessageTest
!  
!  ! ----
!  ! Test successful and failed summary
!  ! ----
!  subroutine getTestSummaryTest  
!    use fruit
!    implicit none;
!    
!    write (*,*) 'Summary for successful cases without message: ';
!    write (*,*) 'Should see 1 successful case';
!    call initializeFruit;
!    call addSuccess;
!    call getTestSummary
!    
!    write (*,*) 'Summary for successful cases and 2 messages: ';
!    write (*,*) 'Should see 3 successful cases';
!    call initializeFruit;
!    call addSuccess;
!    call addSuccess ('Success message 1 from test case.');
!    call addSuccess ('Success message 2 from test case.');
!    call getTestSummary
!    
!    write (*,*) 'Summary for failed cases: ';
!    write (*,*) 'Should see 2 failed case amd 1 message';
!    call initializeFruit;
!    call addFail;
!    call addFail('Fail message from test case.');
!    call getTestSummary
!    
!    return
!  end subroutine getTestSummaryTest
!  
!  ! ----
!  ! Add one failed case and assert false
!  ! Add all success case, and assert true
!  ! ----
!  subroutine isAllSuccessfulTest  
!    use fruit
!    implicit none;
!    
!    logical :: result = .true.
!    
!    call initializeFruit;
!    call addSuccess
!    call addFail
!    call addSuccess
!    call isAllSuccessful (result);
!    
!    IF (result .neqv. .FALSE.) then
!      write (*, *);
!      write (*, *) 'FAILED isAllSuccessfulTest!!! (addFail)';
!      write (*, *);
!    end IF
!
!    call initializeFruit;
!    call addSuccess
!    call addSuccess
!    call addSuccess
!    call isAllSuccessful (result);
!    
!    IF (result .neqv. .true.) then
!      write (*, *);
!      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)';
!      write (*, *);
!    end IF
!
!    call initializeFruit;
!    IF (result .neqv. .true.) then
!      write (*, *);
!      write (*, *) 'FAILED isAllSuccessfulTest!!! (addSuccess)';
!      write (*, *);
!    end IF
!
!    return
!  end subroutine isAllSuccessfulTest
!  
!  subroutine showOutputTest 
!    use fruit
!    implicit none;
!    logical :: trueValue = .TRUE.;
!    logical :: falseValue = .FALSE.;
!    integer :: i;
!    integer :: count;
!    
!    DO i=1,100
!      call assertTrue (trueValue)
!    END DO
!    
!    DO i=1,100
!      call assertTrue (falseValue)
!    END DO
!    
!    return
!  end subroutine showOutputTest
!  
!  subroutine showOutputForReport 
!    use fruit
!    implicit none;
!    logical :: trueValue = .TRUE.;
!    logical :: falseValue = .FALSE.;
!    integer :: i;
!    integer :: count;
!    character (100), DIMENSION (3)  :: msgs; 
!    
!    DO i=1,2
!      call assertTrue (trueValue, 'msg')
!    END DO
!    
!    DO i=1,2
!      call assertTrue (falseValue, 'msg')
!    END DO
!    
!    call getTotalCount (count)
!    write (*, *) 'Total count is: ' , count; 
!    
!    call getFailedCount (count)
!    write (*, *) 'Failed count is: ' , count; 
!    
!    ! to be implemented
!    !call getMessages (msgs);
!    write (*, *) 'Messages are: '; 
!    write (*, *) msgs; 
!    
!    return
!  end subroutine showOutputForReport
!  
!  subroutine testAssertEqualsFloat
!    use fruit
!    implicit none
!    
!    real :: variable = 2.3
!    real :: result = 2.3
!    
!    call initializeFruit;
!    call assertEquals (variable, result);
!    call assertEquals (variable + 0.1, result);
!    call getTestSummary;
!    
!  end subroutine testAssertEqualsFloat
  
end module dataset_1d_test