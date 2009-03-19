!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMtestclass
!-----------------------------------------------------------------------------------------------------------------------------------
!> \file IXMtestclass.f90
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1382 $ ($Date: 2008-05-19 09:07:54 -0400 (Mon, 19 May 2008) $)
!!
!! Example of a class module
!!
!! A class should be defined as follows
!!
!! - A derived TYPE starting with the IXT prefix should
!! be created in a module with the same name but prefixed IXM
!! - within this module various functions should be defined for
!!   class operations e.g. addition and display
!! - the class object should be the first argument of any function
!! - module functions should make no explicit reference to matlab -
!!   they should be passed constructed objects
!! - wrapper functions outside the module should be defined to
!!   pass data to the module functions
!! 
!! All classes must define the module function the implements the IXFoperationRun
!! interface
!!
!> @{
module IXMtestclass
! include the modules which are relied upon by the module  
  use IXMbase
  use IXMspectra
  use IXMdataset_2d
!> IXTtestclass is an object which contains all the types of variables which exist in the framework, it also 
!! contains implementations of all the standard subroutines which MUST be included by any new module which is
!! added to the framework. In each of these routines there are particular ways of treating each different
!! type of variable, and the code in IXTtestclass can be used as a model for other modules, containing all the 
!! standard code required.
  type IXTtestclass 
     private ! all component elements of the module are private and encapsulated
     ! all objects have a base class
     type(IXTbase) :: base !< base class object
     real(dp) :: val    !< real variable
     integer(i4b) :: nx !< integer variable
     real(dp) :: val_static(3)=0.0 !< static real array
     integer(i4b)::int_static(4)=0.0 !< static integer array
     real(dp), pointer :: val_array(:) => NULL()    !<  variable length 1d real array, always declared as NULL by default
     integer(i4b),pointer :: int_arr(:,:)=>NULL() !< variable length 2d integer array, always declared as NULL by default               
     type(IXTspectra):: spectra !< nested object
     logical :: xhist=.FALSE. !< logical variable
     character(len=short_len) :: label='x-label' !< character string variable
     character(len=long_len),allocatable :: cell_string(:) !< allocatable array of strings
     type(IXTdataset_2d), allocatable :: d2d(:) !<allocatable array of objects
  end type IXTtestclass


!> \name pubint
!! Public interfaces
!! @{
! include the interfaces required by routines declared in class_base.f90
#define IXD_TYPE testclass
#include "class_header.f90"

!> interface to the IXFplus function
  interface IXFplus
     module procedure IXFplus_testclass
  end interface IXFplus
! finish public interfaces
!> @}
contains

!! include the generic subroutines every class requires

#define IXD_DESCRIPTION	"IXTtestclass class"
#define IXD_TYPE testclass
#define IXD_SQTYPE 'testclass'
#include "class_base.f90"

!>  All classes must provide this operation; it loops through
!! all members of the class doing the supplied operation
  recursive subroutine IXFoperation_run_testclass(op, field, arg, status)
    implicit none
    type(IXTtestclass) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTtestclass', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing argemnts passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op, 'val', arg%val, status)
    call IXFoperation_run(op, 'nx', arg%nx, status)
    call IXFoperation_run(op, 'val_static', arg%val_static, status)
    call IXFoperation_run(op, 'int_static', arg%int_static, status)
    call IXFoperation_run_ptr(op, 'val_array', arg%val_array, status)!this is a pointer array so the ptr suffix is used
    call IXFoperation_run_ptr(op, 'int_arr', arg%int_arr, status)!this is a pointer array so the ptr suffix is used      
    call IXFoperation_run(op, 'spectra', arg%spectra, status)
    call IXFoperation_run(op, 'xhist', arg%xhist, status)
    call IXFoperation_run(op, 'label', arg%label, status)
    call IXFoperation_run_alloc(op, 'cell_string', arg%cell_string, status)
    call IXFoperation_run_alloc(op, 'd2d', arg%d2d, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_testclass
  
!-----------------------------------------------------------------------------------------------------------------------
!> The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
!! object. If an element of the object is another class then it MUST be initialised. 
  subroutine IXFcreate_testclass(arg, val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d, status)
    implicit none
    type(IXTtestclass),intent(out) :: arg
    type(IXTstatus),intent(inout) :: status
     real(dp),intent(in) :: val    !< real variable
     integer(i4b),intent(in) :: nx !< integer variable
     real(dp),intent(in) :: val_static(3) !< static real array
     integer(i4b),intent(in)::int_static(4) !< static integer array
     real(dp),intent(in) :: val_array(:)    !<  variable length 1d real array, always declared as NULL by default
     integer(i4b),intent(in) :: int_arr(:,:) !< variable length 2d integer array, always declared as NULL by default               
     type(IXTspectra),intent(in):: spectra !< nested object
     logical,intent(in) :: xhist !< logical variable
     character(len=short_len),intent(in) :: label !< character string variable
     character(len=long_len),intent(in) :: cell_string(:) !< allocatable array of strings
     type(IXTdataset_2d),intent(in) :: d2d(:) !<allocatable array of objects


    ! nested objects should be tested for initialisation, this shows they have been created properly
   
    if( IXFvalid(spectra) .neqv. .true.)then
            call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTspectra failure, all nested objects MUST be initialised (IXFcreate_testclass)')
    endif
    if(status == IXCseverity_error)return
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(arg)
       
    call IXFset_testclass(arg,status,val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d)    
    
  end subroutine IXFcreate_testclass
  
  
!-----------------------------------------------------------------------------------------------------------------------
!>The IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, it can also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.
  subroutine IXFdestroy_testclass(arg, status)
    implicit none
    type(IXTtestclass),intent(inout) :: arg
    type(IXTstatus),intent(inout) :: status
    
    call IXFdestroy(arg%base,status)
! destroy pointer arrays
    call IXFdealloc(arg%val_array,status)    
    call IXFdealloc(arg%int_arr,status)
       
    arg%xhist=.FALSE.
    ! for nested objects check it hasn't been destroyed already
    if(IXFvalid(arg%spectra))call IXFdestroy(arg%spectra,status)
    if(allocated(arg%cell_string))call IXFdeallocfortran(arg%cell_string,status)
    if(IXFvalid(arg%d2d))then
       if(allocated(arg%d2d))call IXFdealloc(arg%d2d,status)
    endif
    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(arg)
    
  end subroutine IXFdestroy_testclass

!-----------------------------------------------------------------------------------------------------------------------
!> IXFcheck will make internal consistency checks in the object, such as array length checking to make
!! sure the object is properly filled.
  subroutine IXFcheck_Testclass(arg, status)
    implicit none
    type(IXTtestclass),intent(in) ::arg
    type(IXTstatus),intent(inout) :: status
    if (size(arg%val_array) /= size(arg%int_arr,1)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'sizes of value and int arrays do not match(IXFcheck_testclass)')
    endif
  end subroutine IXFcheck_Testclass

!-----------------------------------------------------------------------------------------------------------------------
!>The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
!! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
!! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
!! The order of arguments should match the order of declaration, except for the IXTbase type which is not declared.
!! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
!! does not have to be initialised, but the reference object MUST be initialised.
  recursive subroutine IXFset_testclass(arg, status, val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d,ref)
    implicit none
    type(IXTtestclass),intent(inout) :: arg
    type(IXTstatus),intent(inout) :: status
    ! all the supplied variables are declared as optional with intent(in)
    type(IXTtestclass),intent(in),optional :: ref
     real(dp),optional,intent(in) :: val    !< real variable
     integer(i4b),optional,intent(in) :: nx !< integer variable
     real(dp),optional,intent(in) :: val_static(3) !< static real array
     integer(i4b),optional,intent(in)::int_static(4) !< static integer array
     real(dp),optional,intent(in) :: val_array(:)  !<  variable length 1d real array, always declared as NULL by default
     integer(i4b),optional,intent(in) :: int_arr(:,:) !< variable length 2d integer array, always declared as NULL by default               
     type(IXTspectra),optional,intent(in):: spectra !< nested object
     logical,optional,intent(in) :: xhist !< logical variable
    ! input strings are treated as an unknown length, if the supplied string is longer than the declared length
    ! then it will be truncated. If it is shorter, then the new variable will be padded with spaces.
     character(len=*),optional,intent(in) :: label !< character string variable
     character(len=*),optional,intent(in) :: cell_string(:) !< array of strings
     type(IXTdataset_2d),optional,intent(in) :: d2d(:) !<array of objects
  
   ! check that either the reference object is initialised
   ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_invparam, 'Reference object MUST be initialised (IXFset_testclass)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(arg)
    else    
       if(IXFvalid(arg) .neqv. .true.) then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_invparam, 'Set can only be called on an initialised object (IXFset_testclass)')
       endif
       if(status == IXCseverity_error)return
    endif
   
   ! This command will copy all of the attributes of the reference object to the object being modified
   ! it calls set with all the components of the reference object
    if (present(ref))call IXFset_testclass(arg,status,ref%val,ref%nx,ref%val_static,ref%int_static,ref%val_array, &
                        ref%int_arr,ref%spectra,ref%xhist,ref%label,ref%cell_string,ref%d2d)
   
    ! single variables are simply overwritten by the supplied variables
    if (present(val))arg%val=val
    if (present(nx))arg%nx=nx

    ! static arrays are simply copied
    ! if the length of val_stat is not 3, then the program will break at
    ! runtime, a check cannot be made for this
    if (present(val_static)) arg%val_static=val_static
    if (present(int_static)) arg%int_static=int_static

    call IXFset_real_array(arg%val_array,status,val_array)        
    call IXFset_integer_array(arg%int_arr,status,int_arr)    
    
    if (present(spectra))call IXFcopy(spectra,arg%spectra,status)
    
    ! logicals and strings are treated in the same way as single variables
    if(present(xhist))arg%xhist=xhist
    if(present(label))arg%label=label
    
    if (present(cell_string))then
       call IXFreallocFortran(arg%cell_string,size(cell_string),.false.,status)
       arg%cell_string=cell_string
    endif    
    if(present(d2d))then
        call IXFrealloc(arg%d2d,size(d2d),.false.,status)
        call IXFcopy(d2d,arg%d2d,status)
    endif
    ! the check routine MUST always be called at the end of the set routine
    call IXFcheck(arg,status)
    
  end subroutine IXFset_testclass

!-----------------------------------------------------------------------------------------------------------------------
!> The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
!! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
!! contents of a whole object to a new one.
  subroutine IXFget_testclass(arg,status, val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d,wout)
    implicit none
    type(IXTtestclass),intent(in) :: arg
    type(IXTtestclass),optional,intent(out)::wout
    type(IXTstatus),intent(inout) :: status
    ! all the supplied variables are declared as optional with intent(out)
     real(dp),optional,intent(out) :: val    !< real variable
     integer(i4b),optional,intent(out) :: nx !< integer variable
     real(dp),optional,intent(out) :: val_static(3) !< static real array
     integer(i4b),optional,intent(out)::int_static(4) !< static integer array
     real(dp),optional,intent(out) :: val_array(:)  !<  variable length 1d real array, always declared as NULL by default
     integer(i4b),optional,intent(out) :: int_arr(:,:) !< variable length 2d integer array, always declared as NULL by default               
     type(IXTspectra),optional,intent(out):: spectra !< nested object
     logical,optional,intent(out) :: xhist !< logical variable
    ! input strings are treated as an unknown length, if the supplied string is longer than the declared length
    ! then it will be truncated. If it is shorter, then the new variable will be padded with spaces.
     character(len=*),optional,intent(out) :: label !< character string variable
     character(len=*),optional,intent(out) :: cell_string(:) !< array of strings
     type(IXTdataset_2d),optional,intent(out) :: d2d(:) !<array of objects


    !  this makes a call to the appropriate set routine. The IXFcopy routine calls
    !  the same routine underneath.
    if (present(wout))then
      call IXFcopy(arg,wout,status)
    endif    
    
    ! single variables are copied into the supplied arrays
    if (present(val))val=arg%val
    if (present(nx))nx=arg%nx
    
    ! 1D variable length arrays
    ! The supplied array must be the same length as the object array. this test is made and the array filled
      call IXFget_real_array(arg%val_array,status,val_array)
    ! 2D variable length arrays
    ! The supplied array must now be the same shape as the object array. this test is made and the object is filled
    call IXFget_integer_array(arg%int_arr,status,int_arr)
    
    !checks are not made on static arrays, since the routine will break at runtime if a supplied array is not 
    !the same length as that declared
    if (present(val_static))val_static=arg%val_static
    if (present(int_static))int_static=arg%int_static
    

    ! supplied nested objects are filled with an appropriate set routine
    if(present(spectra))call IXFcopy(arg%spectra,spectra,status)

    ! logicals and strings are treated as single variables, with strings being truncated where appropriate
    if(present(xhist))xhist=arg%xhist
    if(present(label))label=arg%label

    if (present(cell_string))cell_string=arg%cell_string    
    if(present(d2d))call IXFcopy(arg%d2d,d2d,status)

    
  end subroutine IXFget_testclass
  
!-----------------------------------------------------------------------------------------------------------------------
!> IXFget_alloc can be called with all the same arguments as IXFget, but the pointer array elements/string array/object array elements
!! can be allocatable arrays. the arrays are allocated to the appropriate length and IXFget is called underneath to populate them.
  subroutine IXFget_alloc_testclass(arg,status, val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d,wout)
    implicit none
    type(IXTtestclass),intent(in) :: arg
    type(IXTtestclass),intent(out),optional::wout
     real(dp),optional,intent(out) :: val    !< real variable
     integer(i4b),optional,intent(out) :: nx !< integer variable
     real(dp),optional,allocatable :: val_static(:) !< static real array
     integer(i4b),optional,allocatable::int_static(:) !< static integer array
     !allocatable arrays cannot be defined as intent(out), this tends to make runtime errors for some reason
     real(dp),optional,allocatable :: val_array(:)  !<  variable length 1d real array, always declared as NULL by default
     integer(i4b),optional,allocatable :: int_arr(:,:) !< variable length 2d integer array, always declared as NULL by default               
     type(IXTspectra),optional,intent(out):: spectra !< nested object
     logical,optional,intent(out) :: xhist !< logical variable
    ! input strings are treated as an unknown length, if the supplied string is longer than the declared length
    ! then it will be truncated. If it is shorter, then the new variable will be padded with spaces.
     character(len=*),optional,intent(out) :: label !< character string variable
     !allocatable arrays cannot be defined as intent(out), this tends to make runtime errors for some reason
     character(len=*),allocatable,optional :: cell_string(:) !< allocatable array of strings
     type(IXTdataset_2d),allocatable,optional :: d2d(:) !<allocatable array of objects
    type(IXTstatus),intent(inout)::status

    if(present(val_static))then
       call IXFreallocfortran(val_static,3,.false.,status)
    endif
    if(present(int_static))then
       call IXFreallocfortran(int_static,4,.false.,status)
    endif


! allocate the appropriate allocatable arrays then call the standard get function
    if (present(val_array))then
       call IXFreallocdimsFortran(val_array,shape(arg%val_array),.false.,status)
    endif
    
    if (present(int_arr))then
       call IXFreallocdimsFortran(int_arr,shape(arg%int_arr),.false.,status)
    endif

    if (present(cell_string))then
       call IXFreallocdimsFortran(cell_string,(/ size(arg%cell_string) /),.false.,status)
    endif

    if(present(d2d))then
       call IXFrealloc(d2d,size(arg%d2d),.false.,status)
    endif
    
    call IXFget_testclass(arg,status,   val, nx,val_static,int_static, val_array, int_arr,spectra, xhist,label, cell_string,d2d,wout)

  end subroutine IXFget_alloc_testclass


!-----------------------------------------------------------------------------------------------------------------------
!> IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
!! The pointer arguments are the same name as the object elements they are pointing to.
!! EXTREME Care must be taken since if the pointers are edited, then the data in the structure will also be edited.
  subroutine IXFget_ptr_testclass(arg,val_array,int_arr,spectra)
    implicit none
    type(IXTtestclass),intent(in),target :: arg
    type(IXTspectra),optional,pointer::spectra
    real(dp),optional,pointer::val_array(:)
    integer(i4b),optional,pointer:: int_arr(:,:)

    if (present(spectra))spectra=>arg%spectra
    
    if (present(val_array))val_array=>arg%val_array
    if (present(int_arr))int_arr=>arg%int_arr

  end subroutine IXFget_ptr_testclass


!!-----------------------------------------------------------------------------------------------------------------------
!!! IXFcreate_special_testclass is a customised constructor subroutine which takes three source components of the object
!!! and fills the rest of the object with customised variables and calls the IXFset subroutine. 
!  subroutine IXFcreate_special_testclass(arg,val_array,err_array,spectra,status)
!    implicit none
!    type(IXTtestclass)::arg
!    real(dp),intent(in)::val_array(:),err_array(:)
!    type(IXTspectra),intent(in):: spectra 
!    real(dp)::val_stat(3),val
!    type(IXTstatus)::status
!    integer(i4b)::int_arr(2,2),nx
!    logical :: xhist
!    character(len=short_len) :: label
!
!    !set customised variables
!    xhist=.true.
!    nx=33
!    val=666.66
!    label='customised object'
!    int_arr(1,:)=2
!    int_arr(2,:)=445
!    call random_number(val_stat)
!    
!    !conceivably the IXFcreate subroutine could now be called here
!    
!    !input arguments are now checked 
!    
!    !make the check on nested objects
!    if( IXFvalid(spectra) .neqv. .true.)then
!            call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
!            IXCerr_outofmem, 'IXTspectra failure, all nested objects MUST be initialised (IXFset_testclass)')
!    endif
!
!    
!    ! the set routine can ONLY be called on an initialised object
!    ! so in this *special* case it is initialised before it is filled
!    call IXFmark_valid(arg)         
!    call IXFset_testclass(arg,status,val, nx, val_array, err_array,val_stat, int_arr,spectra, xhist,label)    
!    
!  end subroutine IXFcreate_special_testclass
!!-----------------------------------------------------------------------------------------------------------------------
!> This is an example plus operation for the IXTtestclass object
 subroutine IXFplus_testclass(wres, w1, w2,array, status)

!result objects are generally declared as intent(out)

    type(IXTtestclass),intent(out):: wres
    type(IXTtestclass),intent(in) :: w1, w2
    real(dp),intent(in)::array(:)
    type(IXTstatus),intent(inout) :: status
    integer(i4b)::i,len1,len2

!static and singular values can be added without any checks or allocation of memory

    wres%val = w1%val + w2%val
    wres%nx = w1%nx + w2%nx
    wres%val_static=w1%val_static+w2%val_static
    wres%int_static=w1%int_static+w2%int_static

!allocation of the result pointer arrays

    call IXFalloc(wres%val_array, size(w1%val_array), status)
    call IXFallocdims(wres%int_arr, shape(w1%int_arr) , status)

!before combining the pointer arrays we need to check if they can be safely combined, ie by checking their shapes are the same.
!in this example there are no errors to calculate, and a simple addition can be performed. if there are error functions to be determined 
!the standard array operations can be called for standard manipulations (plus/subtract/multiply/divide/power), 
!these are defined in the IXMarraymanips module

    if (sum(abs(shape(w1%val_array) - shape(w2%val_array)))/=0 ) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'val_array elements not commensurate in operation (IXFplus_testclass)')
        return
    endif
    if (sum(abs(shape(w1%int_arr) - shape(w2%int_arr)))/=0 ) then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'int_arr elements not commensurate in operation (IXFplus_testclass)')
        return
    endif
    wres%int_arr= w1%int_arr + w2%int_arr 
    
    
    if(size(w1%val_array) /= size(array))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
        IXCerr_invparam, 'array not commensurate with val_array (IXFplus_testclass)')
        return
    else
        wres%val_array = w1%val_array + w2%val_array + array
    endif
       

    

!no sensible combination of spectra objects, so take the left hand side one for example, the same with logicals , and append 
!the strings together

    call IXFcopy(w1%spectra,wres%spectra,status)
    wres%xhist=.true.    
    wres%label=trim(adjustl(w1%label))//' '//trim(adjustl(w2%label))   

!a simple combination of arrays of strings

    len1=size(w1%cell_string)
    len2=size(w2%cell_string)
    call IXFallocfortran(wres%cell_string,(len1+len2),status)
    do i=1,len1
      wres%cell_string(i)=w1%cell_string(i)
    enddo
    do i=len1+1,len1+len2
      wres%cell_string(i)=w2%cell_string(i-len1)
    enddo

!standard manipulation of dataset_2d objects, with checking of length

    if(size(w1%d2d) /= size(w2%d2d))then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'd2d failure in operation (IXFplus_testclass)')
        return
    else    
        call IXFalloc(wres%d2d,size(w1%d2d),status)
        do i=1,size(w1%d2d)
            call IXFplus(wres%d2d(i),w1%d2d(i),w2%d2d(i),status)
        enddo
    endif
    call IXFmark_valid(wres)
    
  end subroutine IXFplus_testclass

    
end module IXMtestclass
!> @}