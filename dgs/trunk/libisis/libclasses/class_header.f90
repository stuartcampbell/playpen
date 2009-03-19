! $Id: class_header.f90 1371 2008-05-13 07:50:25Z Dickon Champion $

!
! These must be provided by each class writer
!
! default object must NEVER be modified


#if defined(IXD_TYPE)

public :: IXFoperation_run, IXFoperation_run_ptr, IXFcheck, IXFdisplay, IXFdestroy,IXFfile_read,IXFfile_write
public :: IXFcheck_and_valid
interface IXFoperation_run
	    module procedure IXFoperation_run_&/**/
                                              &IXD_TYPE
end interface IXFoperation_run

interface IXFcheck
	    module procedure IXFcheck_&/**/
		                         &IXD_TYPE
	    module procedure IXFcheckArray&/**/
		                                &IXD_TYPE
end interface IXFcheck

interface IXFcheck_and_valid
	    module procedure IXFcheck_and_valid_&/**/
		                         &IXD_TYPE
	    module procedure IXFcheckarray_and_valid_&/**/
		                         &IXD_TYPE
end interface IXFcheck_and_valid

#ifndef IXD_NO_BASE
public :: IXFinitialised,IXFinitialise ! ,IXFdefault

!interface IXFdefault
!	    module procedure IXFinitialise_&/**/
!		                           &IXD_TYPE
!	    module procedure IXFinitialise_array_&/**/
!		                                 &IXD_TYPE		                         
!end interface IXFdefault

interface IXFinitialise
	    module procedure IXFinitialise_&/**/
		                           &IXD_TYPE
	    module procedure IXFinitialise_array_&/**/
		                                 &IXD_TYPE		
end interface IXFinitialise

interface IXFinitialised
	    module procedure IXFinitialised_&/**/
		                         &IXD_TYPE
	    module procedure IXFinitialised_array_&/**/
		                         &IXD_TYPE		                         
end interface IXFinitialised


public :: IXFvalid
private :: IXFclear_valid,IXFmark_valid

interface IXFvalid
	    module procedure IXFvalid_&/**/
		                         &IXD_TYPE
	    module procedure IXFvalid_array_&/**/
		                         &IXD_TYPE		                         
end interface IXFvalid

interface IXFmark_valid
	    module procedure IXFmark_valid_&/**/
		                         &IXD_TYPE
	    module procedure IXFmark_valid_array_&/**/
		                         &IXD_TYPE
end interface IXFmark_valid

interface IXFclear_valid
	    module procedure IXFclear_valid_&/**/
		                         &IXD_TYPE
	    module procedure IXFclear_valid_array_&/**/
		                         &IXD_TYPE		                         
end interface IXFclear_valid


interface IXFalloc
        module procedure IXFalloc_&/**/
                               &IXD_TYPE
end interface IXFalloc

interface IXFrealloc
        module procedure IXFrealloc_&/**/
                               &IXD_TYPE
end interface IXFrealloc
#endif

interface IXFget
	    module procedure get_&/**/
		                 &IXD_TYPE
	    module procedure get_array_&/**/
		                 &IXD_TYPE
	    module procedure IXFget_&/**/
		                 &IXD_TYPE		                 
end interface IXFget

interface IXFdestroy
	    module procedure IXFdestroy_&/**/
		                            &IXD_TYPE
	    module procedure IXFdestroy_array_&/**/
		                                  &IXD_TYPE
end interface IXFdestroy

interface IXFcreate
	    module procedure IXFcreate_&/**/
		                            &IXD_TYPE
end interface IXFcreate

interface IXFset
	    module procedure gset_&/**/
		                      &IXD_TYPE
	    module procedure IXFset_&/**/
		                      &IXD_TYPE
		                      
end interface IXFset

interface IXFcopy
        module procedure IXFcopy_&/**/
                              &IXD_TYPE
        module procedure IXFcopyarray_&/**/
                              &IXD_TYPE                               
end interface IXFcopy                              

interface IXFdealloc
        module procedure IXFdealloc_&/**/
                               &IXD_TYPE
end interface IXFdealloc

!
! These are generated automatically by including class_base.f90
!
interface IXFdisplay
	    module procedure IXFdisplay&/**/
		                           &IXD_TYPE
	    module procedure IXFdisplayArray&/**/
		                                &IXD_TYPE
end interface IXFdisplay


interface IXFwrap_var
        module procedure wrap_&/**/
                              &IXD_TYPE
end interface IXFwrap_var

interface IXFunwrap_var
        module procedure unwrap_&/**/
                                &IXD_TYPE
end interface IXFunwrap_var

interface IXFunwrap_var_ptr
        module procedure unwrap_ptr_&/**/
                                    &IXD_TYPE
end interface IXFunwrap_var_ptr

type IXPT&/**/
         &IXD_TYPE
    type(IXT&/**/
            &IXD_TYPE ), pointer :: ptr => null()
end type

interface IXFgget
	    module procedure gget_&/**/
		                      &IXD_TYPE
end interface IXFgget

interface IXFgset
	    module procedure gset_&/**/
		                      &IXD_TYPE
end interface IXFgset

interface IXFoperation_run
	    module procedure IXFOperation_run_array_&/**/
						    &IXD_TYPE
end interface IXFoperation_run

interface IXFoperation_run_alloc
	    module procedure IXFOperation_run_array_alloc_&/**/
							  &IXD_TYPE
end interface IXFoperation_run_alloc

interface IXFoperation_run_ptr
	    module procedure IXFoperation_run_ptr_&/**/
						   &IXD_TYPE
	    module procedure IXFoperation_run_array_ptr_&/**/
						        &IXD_TYPE
end interface IXFoperation_run_ptr 

interface IXFpopulate_file
        module procedure IXFpopulate_file_&/**/
						   &IXD_TYPE
end interface IXFpopulate_file						 

interface IXFfile_read
	    module procedure IXFfile_read_&/**/
					  &IXD_TYPE
end interface IXFfile_read

interface IXFfile_write
	    module procedure IXFfile_write_&/**/
					  &IXD_TYPE
end interface IXFfile_write

type IXTL&/**/
         &IXD_TYPE
    private
        type(IXT&/**/
                &IXD_TYPE ) :: value
        type(IXT&/**/
                &IXD_TYPE ), pointer :: next => null()
end type

interface IXFadd
    module procedure add_&/**/
                         &IXD_TYPE
end interface

#undef IXD_TYPE

#endif /* defined(IXD_TYPE) */
