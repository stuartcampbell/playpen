!------------------------------
! MODULE: IXMpeaks
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1304 $ ($Date: 2008-01-29 11:55:00 -0500 (Tue, 29 Jan 2008) $)
!!
!! FORTRAN definition of IXMpeaks class
! peaks module
module IXMpeaks
  use IXMtype_definitions
  use IXMmoments
  implicit none
  public :: IXTpeaks

  type IXTpeaks
     private
     type(IXTbase):: base	
     integer(i4b),pointer :: monitor_no(:)=>NULL()		!! No of monitor. 
     type(IXTdatum_array):: integral				!! Monitor integral and error
     real(dp),pointer:: irange_low(:)=>NULL()		!! Integration range lower limit
     real(dp),pointer:: irange_high(:)=>NULL()    !! Integration range higher limit
     ! length of array is equal to number of monitors defined, and same as length of moments array
     character(len=short_len):: integral_units='units'	!! Units in which the integration was performed
     type(IXTmoments):: moments		!! Various derived quantities for peaks (see separate derived data type)
     character(len=short_len):: moments_units='units'	!! Units in which the moments are expressed				
  end type IXTpeaks

#define IXD_TYPE peaks
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTpeaks class"
#define IXD_TYPE peaks
#define IXD_SQTYPE 'peaks'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_peaks(op, field, arg, status)
    implicit none
    type(IXTpeaks) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTpeaks', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'monitor_no', arg%monitor_no, status)
    call IXFoperation_run(op,'integral',arg%integral,status)
    call IXFoperation_run_ptr(op,'irange_low', arg%irange_low, status)
    call IXFoperation_run_ptr(op,'irange_high', arg%irange_high, status)
    call IXFoperation_run(op,'integral_units', arg%integral_units, status)
    call IXFoperation_run(op,'moments',arg%moments,status)
    call IXFoperation_run(op,'moments_units',arg%moments_units,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_peaks(peaks,status,monitor_no,integral, &
       irange_low,irange_high,integral_units,moments,moments_units,ref)
    implicit none
    type(IXTpeaks),intent(inout)::peaks
    type(IXTpeaks),optional,intent(in)::ref
    integer(i4b),optional,intent(in) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),optional,intent(in):: integral				!! Monitor integral and error
    real(dp),optional,intent(in):: irange_low(:)					!! Integration range low limit
    real(dp),optional,intent(in):: irange_high(:)					!! Integration range high limit
    character(len=*),optional,intent(in):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),optional,intent(in):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),optional,intent(in):: moments_units	!! Units in which the moments are expressed				
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_peaks)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(peaks)
    else    
       if(IXFvalid(peaks) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_peaks)')
       endif
       if(status == IXCseverity_error)return
    endif


    if (present(ref))call IXFset_peaks(peaks,status,&
         ref%monitor_no,ref%integral,ref%irange_low,ref%irange_high,&
         ref%integral_units,ref%moments,ref%moments_units)

    call IXFset_integer_array(peaks%monitor_no,status,monitor_no)

    if (present(integral))call IXFcopy(integral,peaks%integral,status)
   
    call IXFset_real_array(peaks%irange_low,status,irange_low)
    call IXFset_real_array(peaks%irange_high,status,irange_high)

    if (present(integral_units))peaks%integral_units=integral_units
    if (present(moments)) call IXFcopy(moments,peaks%moments,status)   
    if (present(moments_units))peaks%moments_units=moments_units
    
    call IXFcheck_peaks(peaks,status)

  end subroutine IXFset_peaks
  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_peaks(peaks,status,monitor_no,integral,&
       irange_low,irange_high,integral_units,moments,moments_units,wout)
    implicit none
    type(IXTpeaks),intent(in)::peaks
    type(IXTpeaks),optional,intent(out)::wout
    integer(i4b),optional,intent(out) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),optional,intent(out):: integral				!! Monitor integral and error
    real(dp),optional,intent(out):: irange_low(:)					!! Integration range low limit
    real(dp),optional,intent(out):: irange_high(:)					!! Integration range high limit
    character(len=*),optional,intent(out):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),optional,intent(out):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),optional,intent(out):: moments_units	!! Units in which the moments are expressed				
    type(IXTstatus)::status
    
    if (present(wout))call IXFcopy(peaks,wout,status)

    call IXFget_integer_array(peaks%monitor_no,status,monitor_no)
 

    if (present(integral))call IXFcopy(peaks%integral,integral,status)
    call IXFget_real_array(peaks%irange_low,status,irange_low)
    call IXFget_real_array(peaks%irange_high,status,irange_high)
    if (present(integral_units))integral_units=peaks%integral_units
    if(present(moments)) call IXFcopy(peaks%moments,moments,status)
    if (present(moments_units))moments_units=peaks%moments_units

  end subroutine IXFget_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_peaks(peaks,status)
    implicit none  
    type(IXTpeaks)::peaks
    type(IXTstatus)::status

    call IXFcheck_base(peaks%base,status)
    call IXFcheck(peaks%integral,status)
    call IXFcheck(peaks%moments,status)

  end subroutine IXFcheck_peaks

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_peaks(peaks,status)
    implicit none
    type(IXTpeaks)::peaks
    type(IXTstatus)::status
    call IXFdestroy(peaks%base,status)

    if(IXFvalid(peaks%integral))call IXFdestroy(peaks%integral,status)
    call IXFdealloc(peaks%monitor_no,status)
    call IXFdealloc(peaks%irange_low,status)
    call IXFdealloc(peaks%irange_high,status)
    if(IXFvalid(peaks%moments)) call IXFdestroy(peaks%moments,status)

    call IXFclear_valid(peaks)
  end subroutine IXFdestroy_peaks


  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_peaks(peaks,monitor_no,integral,irange_low,&
       irange_high,integral_units,moments,moments_units,status)  
    implicit none
    type(IXTpeaks)::peaks
    type(IXTstatus)::status
    integer(i4b),intent(in) :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),intent(in):: integral				!! Monitor integral and error
    real(dp),intent(in):: irange_low(:)					!! Integration range low limit
    real(dp),intent(in):: irange_high(:)					!! Integration range high limit
    character(len=*),intent(in):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),intent(in):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),intent(in):: moments_units	!! Units in which the moments are expressed				

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(integral) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdatum_array failure, all nested objects MUST be initialised (IXFcreate_peaks)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(moments) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTmoments failure, all nested objects MUST be initialised (IXFcreate_peaks)')
    endif

    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(peaks)

    call IXFset_peaks(peaks,status,monitor_no,integral,irange_low,irange_high,integral_units,moments,moments_units)

  end subroutine IXFcreate_peaks


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_peaks(peaks,monitor_no,irange_low,irange_high,moments)
    real(dp),pointer,optional::irange_low(:) !!output: irange_low array pointer
    real(dp),pointer,optional::irange_high(:) !!output: irange_high array pointer
    integer(i4b),pointer,optional::monitor_no(:)
    type(IXTpeaks),intent(in),target::peaks !!input peaks object 
    type(IXTmoments),pointer,optional::moments

    if (present(monitor_no))monitor_no=>peaks%monitor_no
    if (present(irange_low))irange_low=>peaks%irange_low
    if (present(irange_high))irange_high=>peaks%irange_high
    if (present(moments))moments=>peaks%moments
    

  end subroutine IXFget_ptr_peaks
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_peaks(peaks,status,monitor_no,integral,&
       irange_low,irange_high,integral_units,moments,moments_units,wout)
    implicit none
    type(IXTpeaks),intent(in)::peaks
    type(IXTpeaks),optional,intent(out)::wout
    integer(i4b),optional,allocatable :: monitor_no(:)		!! No of monitor. 
    type(IXTdatum_array),optional,intent(out):: integral				!! Monitor integral and error
    real(dp),optional,allocatable:: irange_low(:)					!! Integration range low limit
    real(dp),optional,allocatable:: irange_high(:)					!! Integration range high limit
    character(len=*),optional,intent(out):: integral_units	!! Units in which the integration was performed
    type(IXTmoments),optional,intent(out):: moments					!! Various derived quantities for peaks (see separate derived data type)
    character(len=*),optional,intent(out):: moments_units	!! Units in which the moments are expressed				
    type(IXTstatus)::status
    if (present(monitor_no))then
       call IXFreallocdimsFortran(monitor_no,shape(peaks%monitor_no),.false.,status)    
    endif

    if (present(irange_high))then
       call IXFreallocdimsFortran(irange_high,shape(peaks%irange_high),.false.,status)    
    endif

    if (present(irange_low))then
       call IXFreallocdimsFortran(irange_low,shape(peaks%irange_low),.false.,status)    
    endif
    
    call IXFget_peaks(peaks,status,monitor_no,integral,&
       irange_low,irange_high,integral_units,moments,moments_units,wout)

  end subroutine IXFget_alloc_peaks

  subroutine IXFpopulate_peaks(peaks,mondata,ei,L1,status)
    use IXMdata
    use IXMdetector  
    implicit none
    type(IXTdata),intent(in)::mondata
    type(IXTpeaks),intent(inout)::peaks
    type(IXTstatus),intent(inout)::status
    type(IXTworkspace),pointer::worksp_ptr
    type(IXTdataset_2d),pointer::datasets(:)
    real(dp),intent(in)::ei,L1
    type(IXTdetector),pointer::edet
    real(dp),pointer::L2(:),mon_no(:)
    integer(i4b)::index,len
    real(dp)::Tmin,Tmax
    real(dp),allocatable::xmean(:),area(:),sig_area(:),bkgd_xmean(:),bkgd_error(:),xmax(:),c_fwhh(:),&
    fwhh(:),sig_xmean(:),sig(:),sig_sig(:),g1(:),sig_g1(:),g2(:),sig_g2(:)
    type(IXTdatum_array)::areaDA,xmeanDA,sigmaDA,g1DA,g2DA
    call IXFget_ptr_data(mondata,datasets=datasets)
    
    
    if (size(datasets) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'mon_data datasets not properly rebinned,(IXFpeakarea_data)')
       return
    endif

    call IXFget_ptr_data(mondata,workspace=worksp_ptr)
    call IXFget_ptr_workspace(worksp_ptr,eff_det=edet)
    call IXFget_ptr_detector(edet,L2=L2)
    edet=>NULL() 

    len=size(L2)
    allocate(xmean(len),area(len),sig_area(len),bkgd_xmean(len),bkgd_error(len),xmax(len),&
    c_fwhh(len),fwhh(len),sig_xmean(len),sig(len),sig_sig(len),g1(len),sig_g1(len),g2(len),sig_g2(len))

    call IXFalloc(peaks%monitor_no,len,status)
    call IXFget_ptr_dataset_2d(datasets(1),y=mon_no)
     
    do index=1,len
      peaks%monitor_no(index)=mon_no(index)
    ! fill up peak information for monitors using ei_guess
      Tmin=0.9*sqrt(c_t_to_emev)*(L1+L2(index))/sqrt(ei)  
      Tmax=1.1*sqrt(c_t_to_emev)*(L1+L2(index))/sqrt(ei)    
      call IXFmoments_dataset_2d(datasets(1),index,Tmin,Tmax,xmean(index),area(index),status, &
      sig_area(index),bkgd_xmean(index),bkgd_error(index),xmax(index),c_fwhh(index),fwhh(index),&
      sig_xmean(index),sig(index),sig_sig(index),g1(index),sig_g1(index),g2(index),sig_g2(index))
! extract datasets(1)%x_units from dataset and set to peaks%moments_units
    ! if it fails for any monitor (monitor 1 particularly) then it is a warning, but cleared      
      if (status == IXCseverity_error)then        
        call IXFclear_status(status)        
        call IXFwrite_line('Failure with Monitor 1 or 4 is expected',status)
      endif       
    enddo
    mon_no=>NULL()
    
    call IXFcreate_datum_array(areaDA,area,sig_area,status)
    call IXFcreate_datum_array(xmeanDA,xmean,sig_xmean,status)
    call IXFcreate_datum_array(sigmaDA,sig,sig_sig,status)
    call IXFcreate_datum_array(g1DA,g1,sig_g1,status)
    call IXFcreate_datum_array(g2DA,g2,sig_g2,status)
    
    call IXFcreate_moments(peaks%moments,areaDA,bkgd_xmean,bkgd_error,xmax,c_fwhh,fwhh,xmeanDA,sigmaDA,g1DA,g2DA,status)
    call IXFmark_valid(peaks)
    L2=>NULL()
    
    deallocate(xmean,area,sig_area,bkgd_xmean,bkgd_error,xmax,c_fwhh,fwhh,sig_xmean,sig,sig_sig,g1,sig_g1,g2,sig_g2)
  
  end subroutine IXFpopulate_peaks
  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFgetei_peaks(peaks,monei_info,ei,Lm1,Lm2,status)
    implicit none
  ! this method uses the first moment of the monitor peak to determine the energy
! if this is changed then it will also have to be changed in the method defined in
! IXFgetei_dataset_2d
    type(IXTpeaks),intent(in)::peaks
    integer(i4b),intent(in)::monei_info(3)
    type(IXTstatus),intent(inout)::status
    real(dp)::ei
    real(dp),intent(in)::Lm1,Lm2
  
    if (monei_info(3)==1)call IXFgetei_moments(peaks%moments,monei_info,Lm1,Lm2,ei,status)
  end subroutine  


end module IXMpeaks
