!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMstatus
!-----------------------------------------------------------------------------------------------------------------------------------

!> \file IXMstatus.f90 This file contains definitions for the IXMstatus module.


!>
!! Generic status (error) handling routines using the IXTstatus object.
!! Though it will usually be error messages that are logged, the mechanism can
!! handle debug and informational messages as well
!!
!! You only really need to know two things
!!
!! - To test for an error just compare your IXTstatus variable to one
!!   of the IXCseverity codes defined below e.g.
!!\code
!!     type(IXTstatus) :: status
!!     call some_subroutine(status)
!!     if (status /= IXCseverity_error) return
!!\endcode
!!
!! - To record an error use IXFadd_status() specifying the error facility, severity, type and
!!     additional explanetary text
!!\code
!!     type(IXTstatus) :: status
!!     call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, 'additional text')
!!\endcode
!! .
!!
!! - even severity -> errors conditions (larger->worse)
!! - odd severity -> OK/Information message
!! .
!!
!! Error severity codes are docmented below - facility codes  and  Error code is a 32 bit number made up of
!! -  lowest 4 bits severity (1=OK, 2=warning, 3=info, 4=error, 5=debug, 6=fatal_error)
!! - next 20 bits are error type code
!! - top 8 bits are facility/category code
!! .
!!
!! \author Freddie Akeroyd, ISIS
!! \see IXMerrorcodes
!! \bug None known
!! \todo Something
!!
!! \ingroup ERRORHANDLING
!! @{
!!
module IXMstatus
   use IXMerrorcodes
   implicit none
    !private

   public :: IXTstatus, IXFadd_status, IXFreport_status, IXFcheck_status, &
             IXFinit_status, operator(==), operator(/=)
   
   integer, parameter :: IXCseverity_ok = 1		!< No current error
   integer, parameter :: IXCseverity_warning = 2	!< Warning condition
   integer, parameter :: IXCseverity_info = 3		!< Informational message
   integer, parameter :: IXCseverity_error = 4		!< Error conditon
   integer, parameter :: IXCseverity_debug = 5		!< Debug message
   integer, parameter :: IXCseverity_fatal = 6		!< Fatal (non-recoverable) error condition

   !> Contains all the information about a single error 
   type IXTstatus_condition         
     private
       integer :: severity = IXCseverity_ok	    	!< Error severity - an IXCseverity_ code
	   integer :: code = IXCerr_unknown		!< Error type - defined in the IXMerrorcodes module
	   integer :: facility = IXCfacility_none	!< Error raising facility - defiend in the IXMerrorcodes module
	   character(len=80) :: source	= ' '		!< Name of subroutine/function/module reporting the condition
	   character(len=256) :: message = ' '		!< Additional error message text
   end type IXTstatus_condition
   
   !> Holds a stack of error object 
   type IXTstatus
     private
	   integer :: err_size = 0	!< maximum size of error stack
	   integer :: err_top = 0   	!< current top of error stack (=number of errors held)
	   integer :: severity = IXCseverity_ok !< The combined overall severity of all errors on this stack
	                         		!! This is updated each time an error is added and used to quickly
						!! determine the result of e.g. (status /= IXCseverity_error)
           type(IXTstatus_condition), allocatable :: err_list(:) !< list of all errors in this object
	   integer :: source_size = 0
	   integer :: source_top = 0
	   character(len=80), allocatable :: source_list(:) !< default error source stack
   end type IXTstatus

   !> Abbreviated names for the error severities - used when printing
   character(len=1), parameter :: IXCseverity_names(6) = (/ 'S','W','I','E','D','F' /)

!> \name pubint
!! Public interfaces
!! @{

   interface operator(==) !< compare an IXTstatus object with an IXCseverity_* code 
      module procedure equal_status
   end interface operator(==)

   interface operator(/=) !< compare an IXTstatus object with an IXCseverity_* code
      module procedure notequal_status
   end interface operator(/=)

!> Add a status condition to a status object e.g.
!!\code
!!    call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, 'additional text')
!!\endcode
!! Add a status condition to the global status object (IXGstatus)
!!\code
!!    call IXFadd_status(IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, 'additional text')
!!\endcode
!! The IXmerrorcodes module contains the list of valid IXCfacility and IXCerr codes
!!
   interface IXFadd_status
       module procedure add_local_status, add_global_status
   end interface IXFadd_status
   
   interface IXFcheck_status
       module procedure check_local_status, check_global_status
   end interface IXFcheck_status

   interface IXFclear_status
       module procedure clear_local_status, clear_global_status
   end interface IXFclear_status
   
   interface IXFreport_status
       module procedure report_local_status, report_global_status
   end interface IXFreport_status

! finish public interfaces
!> @}
   
   type(IXTstatus), save :: IXGstatus     !< global status value
   
contains

! public IXF routines !

!> Initialise a status object. The object \a status is populated using the supplied \a source
!! \param status   Resultant status object
!! \param[in] source   Source of status object
   subroutine IXFinit_status(source, status)
  implicit none
  type(ixtstatus), intent(out) :: status
  character(len=*), intent(in) :: source 
  status%err_size = 100
  allocate(status%err_list(status%err_size))
  status%err_top = 0
  status%severity = IXCseverity_ok
  status%source_size = 100
  allocate(status%source_list(status%source_size))
  status%source_top = 1
  status%source_list(1) = source
  end subroutine


! private (non-IXF) !

!> \name privatefuncs
!! Private functions
!! @{

!> compare an IXTstatus object with an IXCseverity_* code - used by the == operator
  function equal_status(a,b) result(r)
  implicit none
  type(IXTstatus), intent(in) :: a
  integer, intent(in) :: b !< [in] arg b
  logical :: r
  r = IXFcheck_status(a, b)
  end function

  function check_global_status(status_type) result(r)
  implicit none
  integer, intent(in) :: status_type
  logical :: r
  r = IXFcheck_status(IXGstatus, status_type)
  end function

!> compare an IXTstatus object with an IXCseverity_* code - used by the /= operator
  function notequal_status(a,b) result(r)
  implicit none
  type(IXTStatus), intent(in) :: a
  integer, intent(in) :: b
  logical :: r
  r = .not. equal_status(a, b)
  end function


!> Clear status object, reporting error messages
  subroutine clear_local_status(status, report)
  implicit none
  type(IXTstatus), intent(inout) :: status
  logical, intent(in), optional :: report
  logical :: do_report
  do_report = .true.
  if (present(report)) then
      if (report) then
		do_report = .true.
	  else
		do_report = .false.
	  endif
  endif
  if (do_report .and. (status%err_top .gt. 0)) then
      call IXFreport_status(status)
  endif
  if (allocated(status%err_list)) deallocate(status%err_list)
  status%err_top = 0
  status%err_size = 0
  status%severity = IXCseverity_ok
  if (allocated(status%source_list)) deallocate(status%source_list)
  status%source_top = 0
  status%source_size = 0
  end subroutine

  subroutine clear_global_status(report)
  implicit none
  logical, intent(in), optional :: report
  if (present(report)) then
      call IXFclear_status(IXGstatus, report)
  else
      call IXFclear_status(IXGstatus)
  endif
  end subroutine
  
!> Report status messages and reset message list to empty; however the
!! current overall error condition is maintained so that any unwinding
!! process will continue as normal
  subroutine report_local_status(status)
  implicit none
  type(IXTstatus), intent(inout) :: status
  external IXIwrite_line ! becaue cannot use IXMio and IXFwrite_line
  integer i
  character(len=512) message
  if (.not. allocated(status%err_list)) then
      return
  endif
  do i = 1,status%err_top
	  call make_message_status(status%err_list(i), message)
      call IXIwrite_line(trim(message),status)
  enddo
  status%err_top = 0
  end subroutine 

  subroutine report_global_status()
  implicit none
  call IXFreport_status(IXGstatus)
  end subroutine

  subroutine make_message_status(condition, message)
  implicit none
  type(IXTstatus_condition), intent(in) :: condition
  character(len=*), intent(out) :: message
  integer :: i, j
!  write(message, '(''%'',A,''-'',A,'', '',A)')  & 
!         facility_names(condition%facility), &
  i = len_trim(condition%message)
  write(message, '(''%ISISEXC-'',A,'', '',A)')  & 
		 IXCseverity_names(condition%severity), condition%message(:i)
  end subroutine

  subroutine add_source_status(status, source)
  implicit none
  type(IXTstatus), intent(inout) :: status
  character(len=*), intent(in) :: source
  status%source_top = status%source_top + 1
  status%source_list(status%source_top) = source
  end subroutine

  subroutine remove_source_status(status)
  implicit none
  type(IXTstatus), intent(inout) :: status
  status%source_list(status%source_top) = ' '
  status%source_top = status%source_top - 1
  end subroutine

  subroutine add_local_status(status, facility, severity, code, message)
  implicit none
  type(IXTStatus), intent(inout) :: status
  integer, intent(in) :: facility, severity, code
  character(len=*), intent(in) :: message
  if (.not. allocated(status%err_list)) then
      call IXFinit_status('unknown', status)
  endif
  status%err_top = status%err_top + 1
  status%err_list(status%err_top)%message = message
  status%err_list(status%err_top)%facility = facility
  status%err_list(status%err_top)%code = code
  status%err_list(status%err_top)%severity = severity
  call make_Traceback_status(status, status%err_list(status%err_top)%source)
! Update cached error code
  if (mod(severity,2) .eq. 0) then   ! even severity -> error message
      if (mod(status%severity, 2) .eq. 0) then
	      status%severity = max(severity, status%severity) ! if error condition already, take worse case
	  else
		  status%severity = severity ! if no previous error, then take this
	  endif
  else
	  status%severity = max(severity, status%severity) ! debug or info message
  endif
! report status messages to the screen immediately (for now)
  call IXFreport_status(status)
  end subroutine

  subroutine add_global_status(facility, severity, code, message)
  implicit none
  integer, intent(in) :: facility, severity, code
  character(len=*), intent(in) :: message
  call IXFadd_status(IXGstatus, facility, severity, code, message)
  end subroutine

!> Check status for warning (or worse) condition
  function check_warning_status(status, default_source) result(r)
  implicit none
  type(IXTStatus), intent(inout) :: status
  character(len=*), intent(in), optional :: default_source
  logical :: r
  if (present(default_source)) then
      r = IXFcheck_status(status, IXCseverity_warning, default_source)
  else
      r = IXFcheck_status(status, IXCseverity_warning)
  endif
  end function

!> Check status for error (or worse) condition
  function check_error_status(status, default_source) result(r)
  implicit none
  type(IXTstatus), intent(inout) :: status
  character(len=*), intent(in), optional :: default_source
  logical :: r
  if (present(default_source)) then
      r = IXFcheck_status(status, IXCseverity_error, default_source)
  else
      r = IXFcheck_status(status, IXCseverity_error)
  endif
  end function

  subroutine make_traceback_status(status, traceback)
  implicit none
  type(IXTStatus), intent(in) :: status
  integer :: i, j, k
  character(len=*) traceback
  traceback = ' '
  do i = status%source_top, 1, -1
      j = len_trim(traceback)
	  k = len_trim(status%source_list(i))
	  write(traceback(j+1:), '(a,''/'')') status%source_list(i)(:k) 
  enddo
  end subroutine

!> Check status for condition status_type (or worse)
  function check_local_status(status, status_type, default_source) result(r)
  implicit none
  type(IXTstatus), intent(in) :: status
  character(len=*), intent(in), optional :: default_source
  integer, intent(in) :: status_type
  logical :: r
  r = .false.
!  if (.not. allocated(status%list)) then
!      error
!  endif
  if (mod(status_type,2) .eq. 0) then ! we are checking for an error status
      if ((mod(status%severity,2) .eq. 0) .and. (status%severity .ge. status_type)) r = .true.
  else ! We are checking for an OK type
      if (mod(status%severity,2) .ne. 0) r = .true. 
  endif
!  if (present(default_source)) then
!	call IXSAddSource(status, default_source)
!  endif
  end function
!> @}
end module IXMstatus

!> @}
