!-------------------------------------------------------------------------------
! MODULE: IXMlibcore
!-------------------------------------------------------------------------------
!> \file IXMlibcore.f90
!! \brief LIBCORE Initialisation
!!
!! Core library basic functions (write to the screen, initialise the library etc.)
!!
!! This module should be included in any stand-alone fortran program
!!
!! It defines the functions IXFlibrary_init() and IXFlibrary_finish()
!! which should be called respectivbely before and after using the library 
!!
!! \author Freddie Akeroyd, ISIS
!! \version $Revision: 1350 $ ($Date: 2008-04-29 13:21:39 -0400 (Tue, 29 Apr 2008) $)
!! \see IXMmemory
!! \see IXMstatus

!> \defgroup LIBCORE Core Library Routine

!> \defgroup LIBCLASSES Classes Library

!> \defgroup ERRORHANDLING Error Handling functions
!! \ingroup LIBCORE

!> \mainpage LIBISIS Index
!! \section intro_sec Introduction
!! In the text below  
!! -     Namespaces are modules
!! -     Classes are derived data types and interfaces
!! -     Module functions are written as module::function   
!! -     derived data types are written as module::type
!! -     Interface functions are module::interface::function
!! \subpage intro
!! \subpage advanced

!> \page intro "Introduction"
!!

!>\page advanced "Advanced"
!!

!>\page another "Another"
!!

!> \brief libcore module
!!
!! This is the libcore module
!! \ingroup LIBCORE
!!
module IXMlibcore
  use IXMtype_definitions
  use IXMstatus
  use IXMio
  use IXMmemory
  implicit none
  integer, parameter, private :: IXCmemory_stack_size = 5000 !< initial size of IXFalloc() memory stack 

contains

    !> \brief initialise the library
    !!
    !! This must be called before using the library as it
    !! performs important initialisations for things such as the
    !! memory allocator. It returns 0 on success, anything else on error
    function IXFlibrary_init() result(r)
      implicit none
      integer r !< the result
      r = 0	! success
      call IXFmemory_init(IXCmemory_stack_size)
    end function IXFlibrary_init

    !> This should be called when you have finished using the library -
    !! it cleans up structures used by e.g. the memory allocator and prints out any
    !! unread status messages
    subroutine IXFlibrary_finish(status)
      implicit none
      type(IXTstatus)::status
      call IXFmemory_cleanup(status)
      call IXFreport_status(status)
	  call IXFclear_status(status)
    end subroutine IXFlibrary_finish

end module IXMlibcore

