!------------------------------
! MODULE: IXMmoments
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1304 $ ($Date: 2008-01-29 11:55:00 -0500 (Tue, 29 Jan 2008) $)
!!
!! FORTRAN definition of IXMmoments

module IXMmoments
  use IXMtype_definitions
  use IXMstatus
  use IXMdatum_array
  use IXMdataset_common
  implicit none
  public:: IXTmoments
  !----------------------------
  ! TYPE: moments
  !----------------------------
  !! Various information about moments of a peak
  !! (For definitions of skewness and kurtosis, 
  !! see Kendall and Stewart Vo1.1 pp 87-89; Note that Kurtosis = 0 for a Gaussian)
  type IXTmoments
     private
     type(IXTbase):: base
     type(IXTdatum_array):: area	!!	area of peak and its error
     real(dp),pointer:: bkgd_xmean(:)=>NULL()	!!	background level at the value of the first moment of x
     real(dp),pointer:: bkgd_error(:)=>NULL()	!!	slope of background at that position
     real(dp),pointer:: xmax(:)=>NULL()		!!	position of the peak maximum
     real(dp),pointer:: c_fwhh(:)=>NULL()		!!	the half-way position between the peak half-heights
     real(dp),pointer:: fwhh(:)=>NULL()		!!	peak fwhh
     type(IXTdatum_array):: xmean	!!	first moment, <x>
     type(IXTdatum_array):: sigma	!!	second moment, <(x-<x>)^2>
     type(IXTdatum_array):: g1		!!	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
     type(IXTdatum_array):: g2		!!	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -  3
  end type IXTmoments

#define IXD_TYPE moments
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTmoments class"
#define IXD_TYPE moments
#define IXD_SQTYPE 'moments'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_moments(arg, status)
    implicit none
    type(IXTmoments) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)

    ! IXTdatum does not have a base class, therefore cannot be initialised
    call IXFdestroy(arg%area,status)
    call IXFdealloc(arg%bkgd_xmean,status)
    call IXFdealloc(arg%bkgd_error,status)
    call IXFdealloc(arg%xmax,status)
    call IXFdealloc(arg%c_fwhh,status)
    call IXFdealloc(arg%fwhh,status)
    call IXFdestroy(arg%xmean,status)
    call IXFdestroy(arg%sigma,status)
    call IXFdestroy(arg%g1,status)
    call IXFdestroy(arg%g2,status)

    call IXFclear_valid(arg)

  end subroutine IXFdestroy_moments

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_moments(arg, status)
    implicit none
    type(IXTmoments) :: arg
    type(IXTstatus) :: status

    call IXFcheck_base(arg%base,status)
    call IXFcheck(arg%area,status)
    call IXFcheck(arg%xmean,status)
    call IXFcheck(arg%sigma,status)
    call IXFcheck(arg%g1,status)
    call IXFcheck(arg%g2,status)

  end subroutine IXFcheck_moments

  recursive subroutine IXFoperation_run_moments(op, field, arg, status)
    implicit none
    type(IXTmoments) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTmoments', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing argemnts passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op, 'area', arg%area, status)
    call IXFoperation_run_ptr(op, 'bkgd_xmean', arg%bkgd_xmean, status)
    call IXFoperation_run_ptr(op, 'bkgd_error', arg%bkgd_error, status)
    call IXFoperation_run_ptr(op, 'xmax', arg%xmax, status)
    call IXFoperation_run_ptr(op, 'c_fwhh', arg%c_fwhh, status)
    call IXFoperation_run_ptr(op, 'fwhh', arg%fwhh, status)
    call IXFoperation_run(op, 'xmean', arg%xmean, status)
    call IXFoperation_run(op, 'sigma', arg%sigma, status)
    call IXFoperation_run(op, 'g1', arg%g1, status)
    call IXFoperation_run(op, 'g2', arg%g2, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_moments

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_moments(arg,area,bkgd_xmean,bkgd_error,xmax,c_fwhh,fwhh,xmean,sigma,g1,g2,status)
    implicit none
    type(IXTmoments) :: arg
    type(IXTstatus) :: status
    type(IXTdatum_array),intent(in):: area	!!	area of peak and its error
    real(dp),intent(in):: bkgd_xmean(:)	!!	background level at the value of the first moment of x
    real(dp),intent(in):: bkgd_error(:)	!!	slope of background at that position
    real(dp),intent(in):: xmax(:)		!!	position of the peak maximum
    real(dp),intent(in):: c_fwhh(:)		!!	the half-way position between the peak half-heights
    real(dp),intent(in):: fwhh(:)		!!	peak fwhh
    type(IXTdatum_array),intent(in):: xmean	!!	first moment, <x>
    type(IXTdatum_array),intent(in):: sigma	!!	second moment, <(x-<x>)^2>
    type(IXTdatum_array),intent(in):: g1		!!	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
    type(IXTdatum_array),intent(in):: g2		!!	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -  3

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(area) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_moments)')
    endif

    if( IXFvalid(xmean) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_moments)')
    endif

    if( IXFvalid(sigma) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_moments)')
    endif

    if( IXFvalid(g1) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_moments)')
    endif

    if( IXFvalid(g2) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_moments)')
    endif

    if(status == IXCseverity_error)return
    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(arg)

    call IXFset_moments(arg,status,area,bkgd_xmean,bkgd_error,xmax,c_fwhh,fwhh,xmean,sigma,g1,g2)
  end subroutine IXFcreate_moments

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_moments(var, status, area, bkgd_xmean, bkgd_error, xmax, c_fwhh, &
       fwhh, xmean, sigma, g1, g2,ref)
    implicit none
    type(IXTmoments) :: var
    type(IXTmoments), optional,intent(in) :: ref
    type(IXTdatum_array), optional,intent(in) :: area	!!	area of peak and its error
    real(dp), optional,intent(in) :: bkgd_xmean(:)	!!	background level at the value of the first moment of x
    real(dp), optional,intent(in) :: bkgd_error(:)	!!	slope of background at that position
    real(dp), optional,intent(in) :: xmax(:)		!!	position of the peak maximum
    real(dp), optional,intent(in) :: c_fwhh(:)		!!	the half-way position between the peak half-heights
    real(dp), optional,intent(in) :: fwhh(:)		!!	peak fwhh
    type(IXTdatum_array), optional,intent(in) :: xmean	!!	first moment, <x>
    type(IXTdatum_array), optional,intent(in) :: sigma	!!	second moment, <(x-<x>)^2>
    type(IXTdatum_array), optional,intent(in) :: g1		!!	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
    type(IXTdatum_array), optional,intent(in) :: g2		!!	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -  3
    type(IXTstatus),intent(inout) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_moments)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(var)
    else    
       if(IXFvalid(var) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_moments)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_moments(var,status,ref%area, ref%bkgd_xmean, ref%bkgd_error, ref%xmax, ref%c_fwhh, &
         ref%fwhh, ref%xmean, ref%sigma, ref%g1, ref%g2)

    ! could also be with a IXFset_datum_array call
    if (present(area))call IXFcopy(area,var%area,status)
    call IXFset_real_array(var%bkgd_xmean,status,bkgd_xmean)    
    call IXFset_real_array(var%bkgd_error,status,bkgd_error)    
    call IXFset_real_array(var%xmax,status,xmax)    
    call IXFset_real_array(var%c_fwhh,status,c_fwhh)    
    call IXFset_real_array(var%fwhh,status,fwhh)    

    if (present(xmean))call IXFcopy(xmean,var%xmean,status)   
    if (present(sigma))call IXFcopy(sigma,var%sigma,status) 
    if (present(g1)) call IXFcopy(g1,var%g1,status)
    if (present(g2))call IXFcopy(g2,var%g2,status) 

    call IXFcheck(var, status)
  end subroutine IXFset_moments

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_moments(var, status,area, bkgd_xmean, bkgd_error, xmax, c_fwhh, &
       fwhh, xmean, sigma, g1, g2, wout)
    implicit none
    type(IXTmoments) :: var
    type(IXTmoments),intent(out), optional :: wout
    type(IXTstatus) :: status
    type(IXTdatum_array), optional,intent(out) :: area	!!	area of peak and its error
    real(dp), optional,intent(out) :: bkgd_xmean(:)	!!	background level at the value of the first moment of x
    real(dp), optional,intent(out) :: bkgd_error(:)	!!	slope of background at that position
    real(dp), optional ,intent(out):: xmax(:)		!!	position of the peak maximum
    real(dp), optional,intent(out) :: c_fwhh(:)		!!	the half-way position between the peak half-heights
    real(dp), optional ,intent(out):: fwhh(:)		!!	peak fwhh
    type(IXTdatum_array), optional,intent(out) :: xmean	!!	first moment, <x>
    type(IXTdatum_array), optional,intent(out) :: sigma	!!	second moment, <(x-<x>)^2>
    type(IXTdatum_array), optional,intent(out) :: g1		!!	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
    type(IXTdatum_array), optional,intent(out) :: g2		!!	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -  3

    if (present(wout)) call IXFcopy(var,wout,status)
    if (present(area))call IXFcopy(var%area,area,status)
    call IXFget_real_array(var%bkgd_xmean,status,bkgd_xmean)    
    call IXFget_real_array(var%bkgd_error,status,bkgd_error)    
    call IXFget_real_array(var%xmax,status,xmax)    
    call IXFget_real_array(var%c_fwhh,status,c_fwhh)    
    call IXFget_real_array(var%fwhh,status,fwhh)    

    if (present(xmean))call IXFcopy(var%xmean,xmean,status)
    if (present(sigma))call IXFcopy(var%sigma,sigma,status)
    if (present(g1))call IXFcopy(var%g1,g1,status)
    if (present(g2))call IXFcopy(var%g2,g2,status)

  end subroutine IXFget_moments

  subroutine IXFget_ptr_moments(moments,area,bkgd_xmean,bkgd_error,xmax,c_fwhh,fwhh,xmean,sigma,g1,g2)
    implicit none
    type(IXTmoments),intent(in),target::moments
    real(dp),optional,pointer::c_fwhh(:)    
    type(IXTdatum_array), optional,pointer :: area	!!	area of peak and its error
    real(dp), optional,pointer :: bkgd_xmean(:)	!!	background level at the value of the first moment of x
    real(dp), optional,pointer :: bkgd_error(:)	!!	slope of background at that position
    real(dp), optional ,pointer:: xmax(:)		!!	position of the peak maximum
    real(dp), optional ,pointer:: fwhh(:)		!!	peak fwhh
    type(IXTdatum_array), optional,pointer :: xmean	!!	first moment, <x>
    type(IXTdatum_array), optional,pointer :: sigma	!!	second moment, <(x-<x>)^2>
    type(IXTdatum_array), optional,pointer :: g1		!!	skewness = <(x-<x>)^3>/(<(x-<x>)^2>)^(3/2)
    type(IXTdatum_array), optional,pointer :: g2		!!	kurtosis = <(x-<x>)^4>/(<(x-<x>)^2>)^2  -  3

    if(present(c_fwhh))c_fwhh=>moments%c_fwhh
    if(present(area))area=>moments%area
    if(present(bkgd_xmean))bkgd_xmean=>moments%bkgd_xmean
    if(present(bkgd_error))bkgd_error=>moments%bkgd_error
    if(present(xmax))xmax=>moments%xmax
    if(present(fwhh))fwhh=>moments%fwhh
    if(present(xmean))xmean=>moments%xmean
    if(present(sigma))sigma=>moments%sigma
    if(present(g1))g1=>moments%g1
    if(present(g2))g2=>moments%g2
    
  end subroutine
  
  subroutine IXFgetei_moments(moments,monei_info,Lm1,Lm2,ei,status)
    use IXMunits_utils
    implicit none
  ! this method uses the first moment of the monitor peak to determine the energy
! if this is changed then it will also have to be changed in the method defined in
! IXFgetei_dataset_2d
    type(IXTmoments),intent(in)::moments
    integer(i4b),intent(in)::monei_info(3)
    type(IXTstatus),intent(inout)::status
    real(dp),intent(out)::ei
    real(dp),intent(in)::Lm1,Lm2
    real(dp)::cc
    real(dp),pointer::xmean(:)
    character(len=10)::number1
    
    cc=sqrt(c_t_to_emev) 
    call IXFget_ptr_datum_array(moments%xmean,signal=xmean)
    ei=((cc*(Lm2-Lm1))/(xmean(monei_info(2))-xmean(monei_info(1))))**2
    write(number1,'(f10.3)')ei
    call IXFwrite_line('Incident Energy calculated = '//number1//' meV',status)

  end subroutine  
end module IXMmoments
