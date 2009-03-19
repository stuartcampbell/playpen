! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMerrorcodes
!-----------------------------------------------------------------------------------------------------------------------------------
!! This module defines codes for error classes/types and names of error reporting facilities 
!! It is used within the *IXTstatus* type and associated module routines
!! 
!! Error facilities are indicated by an *IXCfacility_* constant.
!! They are numbered sequentially starting with 1 and the name is
!! included in the *IXCfacility_names* array at the same array location as the number 
!!
!! Error codes are indicated by an *IXCerr_* constant.
!! They are numbered sequentially starting with 1 and the description
!! is included the *IXCerror_names* array at the same array location as the number 
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 503 $ ($Date: 2005-10-24 14:04:40 -0400 (Mon, 24 Oct 2005) $)
!! @see IXMstatus
!!
module IXMerrorcodes
   implicit none
   public

   ! Facility name codes - start at 1 and number sequentiall
   ! Also put a string representation of the name in the IXCfacility_names() array

   integer, parameter :: IXCfacility_none = 1    !! no facility specified
   integer, parameter :: IXCfacility_libisis = 2  !! Error raised in "LIBISIS library" 
                                                  !! (a general catchall for when we are not specific)
   integer, parameter :: IXCfacility_bindings = 3   !! Error raised in external bindings interface
												    !! (e.g. in any MATLAB or PYTHON library wrappers)
   integer, parameter :: IXCfacility_memory = 4 !! Error raised in memory alloc/dealloc interface

   integer, parameter :: IXCfacility_wrapvar = 5 !! Error raised in variable wrapping routines

   integer, parameter :: IXCfacility_file = 6 !! Error raised in file access routines

   integer, parameter :: IXCfacility_max_name_len = 20 !! Maximum number of characters in a facility name

   !! List for printing the facility name given its IXCfacility_* code
   character(len=IXCfacility_max_name_len), parameter :: IXCfacility_names(6) = (/ &
    'NONE    ', &
    'LIBISIS ', &
    'BINDINGS', &
    'MEMORY  ', &
    'WRAPVAR ', &
    'FILE    ' &
/)
   
   ! Error type codes - start at 1 and number sequentiall
   ! Also put a string representation of the name in the IXCerr_names() array
   integer, parameter :: IXCerr_unknown = 1
   integer, parameter :: IXCerr_outofmem = 2 !! Out of Memory
   integer, parameter :: IXCerr_invparam = 3 !! Invalid Parameter/Argument Passed
   integer, parameter :: IXCerr_filenotfound = 4 !! File not found

   integer, parameter :: IXCerr_max_name_len = 30 !! Maximum number of characters in an error name
   !! List for printing the error description given an IXCerr_* code
   character(len=IXCerr_max_name_len), parameter :: IXCerr_names(4) = (/ &
      'Unknown                   ', & 
      'Out of Memory             ', & 
      'Invalid Argument/Parameter', &
      'File Not Found            ' &
/)

end module IXMerrorcodes
