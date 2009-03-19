!------------------------
! FORTRAN unit test utility
!
! Author: Andrew H. Chen chena@westinghouse.com
! Modified for use by BAI:  B. Frank, and likely others
!------------------------
!!
! Unit test framework for FORTRAN.  (FoRtran UnIT)
!
! This package is to perform unit test for FORTRAN subroutines
!
! The method used most is: call assertTrue (logical [, message]);
!
!  Things that I think need to be done here (BRF):
!    1) the messageArray stack should be turned into a linked list,
!       so we don't hae to worry about hitting some upper limit
!       Of course, an upper limit could be set internally.
!    2) Figure out what type testError is here for - use it or not?
!    3) Is there a way to hanle the message strings better?
!    4) Make available some public variables like maybe failedAssertCount
!       Then, the test program could check this and stop with a non-zero
!       return code so Unix scripts can check on it
!    5) At the place where there is a STOP in FRUIT, use a non-zero 
!       return code.
!    7) Here or in a utility mod/library, add a float equals function.
!    8) Add some "array equals" functions for real and integer arrays of
!       arbitrary rank
!    9) Look at what's provided by Junit and provide that if posible
!       This might include assertEquals?
!   10) Maybe read some options from a "fruit.ini" type file - like the
!       maximum number of messages to allow before exiting.
!   11) Provide an option to generate a simple HTML page showing the success
!       or failure data.
!            AHC - this should be outside of FRUIT.  It is awkward to generate html in FORTRAN
!                  FRUIT will provide interfaces to export number of success/fail
!
!
module fruit

  use Util

  integer, parameter :: MSG_LENGTH = 100
  integer, parameter :: MSG_STACK_SIZE = 150
  
  integer, private, save :: successfulAssertCount = 0;
  integer, private, save :: failedAssertCount = 0;
  character (len = MSG_LENGTH), private, DIMENSION (MSG_STACK_SIZE), save :: messageArray; 
  integer, private, save :: messageIndex = 1;
  
  !-------
  ! Assert true 
  !-------
  interface assertTrue
      module procedure assertTrue_single;
      module procedure assertTrue_result;
      module procedure assertTrue_result_message;
      module procedure assertTrue_single_message;
  end interface
  
  !------------
  ! Assert variables are equal  
  !------------
  interface assertEquals
      module procedure assertEquals_single_int
      module procedure assertEquals_1darray_int

      module procedure assertEquals_single_real
      module procedure assertEquals_1darray_real

      module procedure assertEquals_single_double
      module procedure assertEquals_1darray_double

      module procedure assertEquals_single_int_message
      module procedure assertEquals_1darray_int_message

      module procedure assertEquals_single_real_message
      module procedure assertEquals_1darray_real_message

      module procedure assertEquals_single_double_message
      module procedure assertEquals_1darray_double_message
  end interface
  
  !------------
  ! Add a successful case.  
  !------------
  interface addSuccess
    module procedure addSuccess_no_message;
    module procedure addSuccess_message;
  end interface
  
  !------------
  ! Add a failed case.  == fail() in JUnit
  !------------
  interface addFail
    module procedure addFail_no_message;
    module procedure addFail_message;
  end interface
  
  !------------
  ! Retrieve number of total cases.  Used for future report generator
  !------------
  interface getTotalCount
    module procedure getTotalCount_;
  end interface
  
  !------------
  ! Retrieve number of failed cases.  Used for future report generator
  !------------
  interface getFailedCount
    module procedure getFailedCount_;
  end interface
  
  !------------
  ! Retrieve all error messages.  Used for future report generator
  !------------
  interface getMessages
    module procedure getMessages_;
  end interface
  
  private ::     assertTrue_single     ,&
    assertTrue_result                  ,&
    assertTrue_result_message          ,&
    assertTrue_single_message          ,&
    addSuccess_no_message              ,&
    addSuccess_message                 ,&
    addFail_no_message                 ,&
    addFail_message                    ,&
    increaseMessageStack               ,&
    assertEquals_single_int            ,&
    assertEquals_1darray_int           ,&
    assertEquals_single_real           ,&
    assertEquals_1darray_real          ,&
    assertEquals_single_double         ,&
    assertEquals_1darray_double        ,&
    assertEquals_single_int_message    ,&
    assertEquals_1darray_int_message   ,&
    assertEquals_single_real_message   ,&
    assertEquals_1darray_real_message  ,&
    assertEquals_single_double_message ,&
    assertEquals_1darray_double_message;
                                       
contains

  ! ----
  ! Initialize all test modules
  ! ----
  subroutine initializeFruit
    successfulAssertCount = 0;
    failedAssertCount = 0;
    messageIndex = 1;
    write (*,*) 
    write (*,*) "Test module initialized"
    write (*,*) 
    write (*,*) "   . : successful assert,   F : failed assert "
    write (*,*) 
  end subroutine initializeFruit

  ! ----
  ! Assert the values are true, with return value
  ! print error messages and return error
  ! ----
  subroutine assertTrue_result (inputBoolValue, resultBoolValue)
    logical, intent (in) :: inputBoolValue
    logical, intent (out) :: resultBoolValue
    
    if ( inputBoolValue .eqv. .true.) then
      resultBoolValue = .true.
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      resultBoolValue = .false.
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertTrue_result
  
  ! ----
  ! Assert the values are true
  ! print error messages and return error
  ! ----
  subroutine assertTrue_result_message (inputBoolValue, message, resultBoolValue)
    logical, intent (in) :: inputBoolValue
    logical, intent (out) :: resultBoolValue
    character (*), intent (in) :: message;
    
    if ( inputBoolValue .eqv. .true.) then
      resultBoolValue = .true.
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      resultBoolValue = .false.
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
    
    call increaseMessageStack(message)
    
  end subroutine assertTrue_result_message
  
  ! ----
  ! Assert the values are true and message
  ! print error messages and return error
  ! ----
  subroutine assertTrue_single (inputBoolValue)
    implicit none;
    logical, intent (in) :: inputBoolValue
    
    if ( inputBoolValue .eqv. .true.) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
    call failedMark()
    end if
   
  end subroutine assertTrue_single
  
  ! ----
  ! Assert the values are true and message
  ! This subroutine is used most
  ! print error messages and return error
  ! ----
  subroutine assertTrue_single_message (inputBoolValue, message)
    implicit none;
    logical, intent (in) :: inputBoolValue
    character (*), intent (in) :: message;
    
    if ( inputBoolValue .eqv. .true.) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
  end subroutine assertTrue_single_message
  
  ! -----
  ! Just add one successful case
  ! -----
  subroutine addSuccess_no_message
    implicit none;
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
  end subroutine addSuccess_no_message
  
  ! -----
  ! Just add one successful case with message
  ! -----
  subroutine addSuccess_message (message)
    implicit none;
    character (*), intent (in) :: message;
    successfulAssertCount = successfulAssertCount + 1;
    
    call increaseMessageStack(message)
    call successfulMark()
    
  end subroutine addSuccess_message
  
  ! -----
  ! Just add one failed case
  ! -----
  subroutine addFail_no_message
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
  end subroutine addFail_no_message
  
  ! -----
  ! Just add one failed case
  ! -----
  subroutine addFail_message (message)
    character (*), intent (in) :: message;
    failedAssertCount = failedAssertCount + 1;
    call increaseMessageStack(message)
    call failedMark()
  end subroutine addFail_message
  
  ! -----
  ! Just add one successful case
  ! -----
  subroutine isAllSuccessful (result)
    implicit none;
      logical, intent (out) :: result
      if ( failedAssertCount > 0 ) then
        result = .false.
      else 
        result = .true.
      end if
      
      return
  end subroutine isAllSuccessful
  
  ! -----
  ! Return summary of all tests in this instance
  ! -----
  subroutine getTestSummary
    implicit none;
    
    integer :: i;
  

    write (*,*) 
    write (*,*) '    Start of FRUIT summary: '
    write (*,*) 
    
    if (failedAssertCount > 0) then
      write (*,*) 'Some tests failed!';
    else 
      write (*,*) 'SUCCESSFUL!';
    end if;
    
    !----------------
    ! Dump message stack
    !----------------
    if ( messageIndex > 1) then
      write (*,*) '  -- Error messages are:';
    
      DO i = 1, messageIndex
        write (*,"(A)") messageArray(i);
      end DO

      write (*,*) '  -- end of messages.';
    else 
      write (*,*) '  No messages ';
    end if;
      
    if (successfulAssertCount + failedAssertCount /= 0) then
      
      write (*,*) 'Total test run :   ', successfulAssertCount + failedAssertCount;
      write (*,*) 'Successful :       ', successfulAssertCount;
      write (*,*) 'Failed :           ', failedAssertCount;
      write (*,*) 'Successful rate:   ', real(successfulAssertCount) * 100.0 / real (successfulAssertCount + failedAssertCount), '%';
      write (*, *) 
      write (*, *) '  -- end of FRUIT summary'
      
    end if;
    
  end subroutine getTestSummary
  
  subroutine successfulMark
   write(*,"(A1)",ADVANCE='NO') '.'
  end subroutine successfulMark
  
  subroutine failedMark
   write(*,"(A1)",ADVANCE='NO') 'F'
  end subroutine failedMark
  
  subroutine increaseMessageStack (message)
    character(*), intent (in) :: message
    
    if (messageIndex > MSG_STACK_SIZE ) then
      write (*, *) "Too many errors to put into stack"
      call getTestSummary ()
      stop                     ! Ought to stop with a non-zero return code
                               ! so Unix scripts can catch it
    end if
    
    messageArray (messageIndex) = message;
    messageIndex = messageIndex + 1;
  end subroutine increaseMessageStack
  
  ! ----
  ! Assert the integer values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_int (var1, var2)
    implicit none;
    integer, intent (in) :: var1, var2
    
    if ( equals (var1, var2)) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_int
  
  ! ----
  ! Assert the integer 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_int (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    
    integer count

    do count = 1, n
       if ( equals (var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_int
  
  ! ----
  ! Assert the real values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_real (var1, var2)
    implicit none;
    real, intent (in) :: var1, var2
    
    if ( equals (var1, var2) ) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_real
  
  ! ----
  ! Assert the real 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_real (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    
    integer count

    do count = 1, n
       if ( equals ( var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_real
  
  ! ----
  ! Assert the double precision values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_double (var1, var2)
    implicit none;
    real(dp), intent (in) :: var1, var2
    
    if ( equals (var1, var2) ) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call failedMark()
    end if
   
  end subroutine assertEquals_single_double
  
  ! ----
  ! Assert the double precision 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_double (var1, var2, n)
    implicit none;
    integer, intent (in) :: n
    real(dp), intent (in) :: var1(n), var2(n)
    
    integer:: count

    do count = 1, n
       if ( equals (var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_double
  ! ----
  ! Assert the integer values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_int_message (var1, var2, message)
    implicit none;
    integer, intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( equals (var1, var2)) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_int_message
  
  ! ----
  ! Assert the integer 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_int_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    integer, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    do count = 1, n
       if ( equals (var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_int_message
  
  ! ----
  ! Assert the real values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_real_message (var1, var2, message)
    implicit none;
    real, intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( equals (var1, var2) ) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_real_message
  
  ! ----
  ! Assert the real 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_real_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    real, intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    do count = 1, n
       if ( equals (var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_real_message
  
  ! ----
  ! Assert the double precision values are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_single_double_message (var1, var2, message)
    implicit none;
    real(dp), intent (in) :: var1, var2
    character (*), intent (in) :: message;
    
    if ( equals (var1, var2)) then
      successfulAssertCount = successfulAssertCount + 1;
      call successfulMark()
    else 
      failedAssertCount = failedAssertCount + 1;
      call increaseMessageStack(message)
      call failedMark()
    end if
   
  end subroutine assertEquals_single_double_message
  
  ! ----
  ! Assert the double precision 1-D arrays are equal
  ! print error messages and return error
  ! ----
  subroutine assertEquals_1darray_double_message (var1, var2, n, message)
    implicit none;
    integer, intent (in) :: n
    real(dp), intent (in) :: var1(n), var2(n)
    character (*), intent (in) :: message;
    
    integer count

    do count = 1, n
       if ( equals (var1(count), var2(count))) then
         successfulAssertCount = successfulAssertCount + 1;
         call successfulMark()
       else 
         failedAssertCount = failedAssertCount + 1;
         call increaseMessageStack(message)
         call failedMark()
       end if
    end do
   
  end subroutine assertEquals_1darray_double_message

  ! ----
  ! Return total number of assert calls
  ! ----
  subroutine getTotalCount_ (count)
  implicit none;
    integer, intent (out) :: count
    
    count = successfulAssertCount + failedAssertCount;
   
  end subroutine getTotalCount_

  ! ----
  ! Return number of failed assert calls
  ! ----
  subroutine getFailedCount_ (count)
  implicit none;
    integer, intent (out) :: count
    
    count = failedAssertCount;
   
  end subroutine getFailedCount_

  ! ----
  ! Return number of failed assert calls
  ! ----
  subroutine getMessages_ ()
  implicit none;
    !character (len = MSG_LENGTH), DIMENSION (MSG_STACK_SIZE), intent (out)  :: msgs; 
    
    ! not implemented yet
    !msgs = messageArray;
   
  end subroutine getMessages_

END module fruit
