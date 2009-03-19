!------------------------------
! MODULE: IXMrunfile
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1419 $ ($Date: 2008-07-08 09:57:47 -0400 (Tue, 08 Jul 2008) $)
!!
!! FORTRAN definition of IXMrunfile class
module IXMrunfile
  use IXMuser
  use IXMsample
  use IXMinstrument
  use IXMdata
  use IXMpeaks
  use IXMdata_source
  use IXMoptions
  !  use IXMisotime
  implicit none
  public :: IXTrunfile
  type IXTrunfile
     private
     type(IXTbase):: base
     !!header information
     character(len=long_len)::title='title'
     !     type(IXTisotime)::start_time
     !     type(IXTisotime)::end_time
     character(len=short_len):: start_time
     character(len=short_len):: end_time
     integer(i4b)::run_number
     real(dp)::total_charge
     integer(i4b)::total_raw_frames
     integer(i4b)::total_good_frames
     type(IXThistory)::program_name
     type(IXThistory)::command_line
     type(IXTuser),allocatable::users(:)
     type(IXTsample)::sample
     type(IXTinstrument)::inst
     type(IXTdata)::det_data
     type(IXTdata)::mon_data
     type(IXTpeaks)::peaks
  end type IXTrunfile
  integer(i4b),parameter,private:: IXCcomline_initlength=5 !!initial length of array list elements

#define IXD_TYPE runfile
#include "class_header.f90"

  private :: units_runfile, units_rebinXdesc_runfile,units_rebinXref_runfile,get_d_correction
  private :: loadmonmap,loaddetmap,loadmask,loadrawfile,loadheaderinfo_isis !,setcommandline
  private ::ts_power_runfile, tt_power_runfile, st_power_runfile
  private ::ts_divide_runfile, tt_divide_runfile, st_divide_runfile
  private ::ts_plus_runfile, tt_plus_runfile, st_plus_runfile
  private ::ts_minus_runfile, tt_minus_runfile, st_minus_runfile
  private ::ts_times_runfile, tt_times_runfile, st_times_runfile
  private :: finish_op_runfile,maskratio,maskintegral,getlinearsignalerror
  interface IXFunits_runfile
     module procedure units_runfile, units_rebinXdesc_runfile,units_rebinXref_runfile
  end interface

  interface IXFplus_runfile
     module procedure ts_plus_runfile, tt_plus_runfile, st_plus_runfile
  end interface
  interface IXFplus
     module procedure ts_plus_runfile, tt_plus_runfile, st_plus_runfile
  end interface

  interface IXFminus_runfile
     module procedure ts_minus_runfile, tt_minus_runfile, st_minus_runfile
  end interface
  interface IXFminus
     module procedure ts_minus_runfile, tt_minus_runfile, st_minus_runfile
  end interface

  interface IXFtimes_runfile
     module procedure ts_times_runfile, tt_times_runfile, st_times_runfile
  end interface
  interface IXFtimes
     module procedure ts_times_runfile, tt_times_runfile, st_times_runfile
  end interface

  interface IXFdivide_runfile
     module procedure ts_divide_runfile, tt_divide_runfile, st_divide_runfile
  end interface
  interface IXFdivide
     module procedure ts_divide_runfile, tt_divide_runfile, st_divide_runfile
  end interface

  interface IXFpower_runfile
     module procedure ts_power_runfile, tt_power_runfile, st_power_runfile
  end interface
  interface IXFpower
     module procedure ts_power_runfile, tt_power_runfile, st_power_runfile
  end interface

  interface IXFsin
     module procedure IXFsin_runfile
  end interface

  interface IXFcos
     module procedure IXFcos_runfile
  end interface

  interface IXFtan
     module procedure IXFtan_runfile
  end interface

  interface IXFsinh
     module procedure IXFsinh_runfile
  end interface

  interface IXFcosh
     module procedure IXFcosh_runfile
  end interface

  interface IXFtanh
     module procedure IXFtanh_runfile
  end interface

  interface IXFlog
     module procedure IXFlog_runfile
  end interface

  interface IXFlog10
     module procedure IXFlog10_runfile
  end interface

  interface IXFexp
     module procedure IXFexp_runfile
  end interface
  
contains

#define IXD_DESCRIPTION	"IXTrunfile class"
#define IXD_TYPE runfile
#define IXD_SQTYPE 'runfile'
#include "class_base.f90"    
  recursive subroutine IXFoperation_run_runfile(op, field, arg, status)
    implicit none
    type(IXTrunfile) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTrunfile', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op,'title', arg%title, status)
    call IXFoperation_run(op,'start_time', arg%start_time, status)
    call IXFoperation_run(op,'end_time', arg%end_time, status)
    call IXFoperation_run(op,'run_number', arg%run_number, status)
    call IXFoperation_run(op,'total_charge', arg%total_charge, status)
    call IXFoperation_run(op,'total_raw_frames', arg%total_raw_frames, status)
    call IXFoperation_run(op,'total_good_frames', arg%total_good_frames, status)
    call IXFoperation_run(op,'program_name', arg%program_name, status)
    call IXFoperation_run(op,'command_line', arg%command_line, status)
    call IXFoperation_run_alloc(op,'users',arg%users,status)
    call IXFoperation_run(op,'sample', arg%sample, status)
    call IXFoperation_run(op,'inst', arg%inst, status)
    call IXFoperation_run(op,'det_data',arg%det_data,status)
    call IXFoperation_run(op,'mon_data',arg%mon_data,status)
    call IXFoperation_run(op,'peaks',arg%peaks,status)    
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_runfile
  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.
  subroutine IXFget_runfile(runfile,status,title,users,sample,inst,det_data,mon_data,peaks,wout)
    implicit none
    type(IXTrunfile),intent(in)::runfile
    type(IXTrunfile),optional,intent(out)::wout
    character(len=*),optional,intent(out)::title
    type(IXTuser),optional,intent(out)::users(:)
    type(IXTsample),optional,intent(out)::sample
    type(IXTinstrument),optional,intent(out)::inst
    type(IXTdata),optional,intent(out)::mon_data,det_data
    type(IXTpeaks),optional,intent(out)::peaks
    type(IXTstatus)::status

    if(present(wout))call IXFcopy(runfile,wout,status)
    if(present(title))title=runfile%title
    if(present(users))call IXFcopy(runfile%users,users,status)
    if(present(sample)) call IXFcopy(runfile%sample,sample,status)
    if(present(inst)) call IXFcopy(runfile%inst,inst,status)
    if(present(det_data)) call IXFcopy(runfile%det_data,det_data,status)
    if(present(mon_data)) call IXFcopy(runfile%mon_data,mon_data,status)
    if(present(peaks)) call IXFcopy(runfile%peaks,peaks,status)
  end subroutine IXFget_runfile

  !! IXFgetmondata_runfile will extract the monitor datasets from the runfile%mon_data IXTdata object if 
  !! it has been appropriately filled, and give a warning otherwise. It will return an allocatable array of IXTdataset_2d 
  !! objects of appropriate length
  subroutine IXFgetmondata_runfile(runfile,data2d,status)
    !the data2d argument is to be intent(out), but implicit deallocation of the objects causes problems    
    implicit none
    type(IXTrunfile),intent(in)::runfile
    type(IXTdataset_2d),allocatable::data2d(:)
    type(IXTstatus)::status

    if(IXFvalid(runfile%mon_data))then
       call IXFget_alloc_data(runfile%mon_data,status,datasets=data2d)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'runfile%mon_data must be a valid IXTdata object(IXFgetmondata_runfile)')        
    endif

  end subroutine IXFgetmondata_runfile

  !! IXFgetdetdata_runfile will extract the detector datasets from the runfile%det_data IXTdata object if 
  !! it has been appropriately filled, and give a warning otherwise. It will return an allocatable array of IXTdataset_2d 
  !! objects of appropriate length
  subroutine IXFgetdetdata_runfile(runfile,data2d,status)
    !the data2d object is to be intent(out), but implicit deallocation of the objects causes problems
    implicit none
    type(IXTrunfile),intent(in)::runfile
    type(IXTdataset_2d),allocatable::data2d(:)
    type(IXTstatus)::status

    if (IXFvalid(runfile%det_data))then
       call IXFget_alloc_data(runfile%det_data,status,datasets=data2d)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'runfile%det_data must be a valid IXTdata object(IXFgetdetdata_runfile)')    
    endif

  end subroutine IXFgetdetdata_runfile

  !! IXFgeteival_runfile will extract the energy of the chopper_instrument fermi chopper if it has 
  !! been appropriately filled, and give a warning otherwise.
  subroutine IXFgeteival_runfile(runfile,eival,status)
    implicit none
    type(IXTrunfile),intent(in)::runfile
    real(dp),intent(out)::eival
    type(IXTfermi_chopper),pointer::FC
    type(IXTchopper_instrument),pointer::ci
    type(IXTstatus)::status    

    call IXFget_ptr_instrument(runfile%inst,ci=ci)    
    if (IXFvalid(ci))then
       call IXFget_ptr_chopper_instrument(ci,monochromator=FC)
       call IXFget_fermi_chopper(FC,status,energy=eival)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'runfile%inst%ci must be a valid IXTchopper_instrument(IXFgeteival_runfile)')    
    endif

    ci=>NULL()
    if(associated(FC))FC=>NULL()
  end subroutine IXFgeteival_runfile

  !! IXFseteival_runfile will set the energy of the chopper_instrument fermi chopper if it has 
  !! been appropriately filled, and give a warning otherwise.
  subroutine IXFseteival_runfile(runfile,eival,status)
    implicit none
    type(IXTrunfile),intent(inout)::runfile
    real(dp),intent(in)::eival
    type(IXTstatus),intent(inout)::status    
    type(IXTfermi_chopper),pointer::FC
    type(IXTchopper_instrument),pointer::ci

    call IXFget_ptr_instrument(runfile%inst,ci=ci)    
    if (IXFvalid(ci))then
       call IXFget_ptr_chopper_instrument(ci,monochromator=FC)
       call IXFset_fermi_chopper(FC,status,energy=eival)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'runfile%inst%ci must be a valid IXTchopper_instrument(IXFseteival_runfile)')    
    endif

    ci=>NULL()
    if(associated(FC))FC=>NULL()
  end subroutine IXFseteival_runfile
  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_runfile(runfile,status,title,users,sample,inst,det_data,mon_data,peaks,ref)
    implicit none
    type(IXTrunfile)::runfile
    type(IXTrunfile),optional,intent(in)::ref
    character(len=*),optional,intent(in)::title
    type(IXTuser),optional,intent(in)::users(:)
    type(IXTsample),optional,intent(in)::sample
    type(IXTinstrument),optional,intent(in)::inst
    type(IXTdata),optional,intent(in)::mon_data,det_data
    type(IXTpeaks),optional,intent(in)::peaks
    type(IXTstatus)::status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_runfile)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(runfile)
    else    
       if(IXFvalid(runfile) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_runfile)')
       endif
       if(status == IXCseverity_error)return
    endif


    if(present(ref))then
       call IXFset_runfile(runfile,status,ref%title,ref%users,ref%sample,ref%inst,ref%det_data,ref%mon_data,ref%peaks)
    endif

    if(present(title))runfile%title=title

    if (present(users))then   
       if(allocated(runfile%users))call IXFdealloc(runfile%users,status)
       call IXFalloc(runfile%users,size(users),status) ! this calls the IXFalloc_aperture to allocate an array of structures
       call IXFcopy(users,runfile%users,status)  ! this calls IXFcopyarray_users to copy the array of structures, interfaced to IXFcopy
    endif

    if(present(sample))call IXFcopy(sample,runfile%sample,status)    
    if(present(inst))call IXFcopy(inst,runfile%inst,status)
    if(present(det_data))call IXFcopy(det_data,runfile%det_data,status)
    if(present(mon_data))call IXFcopy(mon_data,runfile%mon_data,status)
    if(present(peaks))call IXFcopy(peaks,runfile%peaks,status)

    call IXFcheck_runfile(runfile,status)

  end subroutine IXFset_runfile

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_runfile(runfile,status)
    implicit none
    type(IXTrunfile)::runfile
    type(IXTstatus)::status

    call IXFdestroy(runfile%base,status)
    if(IXFvalid(runfile%users))then
      if(allocated(runfile%users))call IXFdealloc(runfile%users,status)
    endif
    if(IXFvalid(runfile%sample)) call IXFdestroy(runfile%sample,status)
    if(IXFvalid(runfile%inst)) call IXFdestroy(runfile%inst,status)
    if(IXFvalid(runfile%det_data)) call IXFdestroy(runfile%det_data,status)
    if(IXFvalid(runfile%mon_data)) call IXFdestroy(runfile%mon_data,status)
    if(IXFvalid(runfile%peaks)) call IXFdestroy(runfile%peaks,status)
    call IXFclear_valid(runfile)

  end subroutine IXFdestroy_runfile

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_runfile(runfile,title,users,sample,inst,det_data,mon_data,peaks,status)
    implicit none
    type(IXTrunfile)::runfile
    type(IXTstatus)::status
    character(len=*),intent(in)::title
    type(IXTuser),intent(in)::users(:)
    type(IXTsample),intent(in)::sample
    type(IXTinstrument),intent(in)::inst
    type(IXTdata),intent(in)::det_data,mon_data
    type(IXTpeaks),intent(in)::peaks


    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(users) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTuser failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(sample) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTsample failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(inst) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTinstrument failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(det_data) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdata failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(mon_data) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdata failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif

    if( IXFvalid(peaks) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTpeaks failure, all nested objects MUST be initialised (IXFcreate_runfile)')
    endif

    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(runfile)

    call IXFset_runfile(runfile,status,title,users,sample,inst,det_data,mon_data,peaks) 

  end subroutine IXFcreate_runfile


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_runfile(runfile,status) 
    implicit none
    type(IXTrunfile)::runfile
    type(IXTstatus)::status

    call IXFcheck_base(runfile%base,status)
    if(allocated(runfile%users).and. IXFvalid(runfile%users))call IXFcheck(runfile%users,status)
    call IXFcheck(runfile%sample,status)
    call IXFcheck(runfile%inst,status)
    call IXFcheck(runfile%det_data,status)
    call IXFcheck(runfile%mon_data,status)                
    call IXFcheck(runfile%peaks,status)

  end subroutine IXFcheck_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  ! this subroutine will specifically only populate the mon_data part of a runfile
  subroutine IXFpopulate_mon_runfile(runfile,status,dso,period,ei,m_axis,m_rebin,opt)
    use IXMmap
    use IXMmask
    implicit none
    type(IXTrunfile),intent(inout)::runfile
    type(IXTdata_source),intent(in)::dso
    integer(i4b),intent(in)::period    
    type(IXToptions),optional,intent(in)::opt
    type(IXTaxis),optional,intent(in)::m_axis
    real(dp),intent(in),optional::m_rebin(:),ei(2)
    type(IXTstatus),intent(inout)::status
    type(IXTmap)::mon_map
    type(IXTmask)::mon_mask        
    type(IXTraw_file),allocatable::rawfile(:)
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    real(dp)::efixed,L1
    integer(i4b)::emode            

    ! most importantly check if rawfile present
    call loadrawfile(rawfile,dso,status)   
    if (status == IXCseverity_error)return

    !load information into monitor IXTmap object: mon_map    
    call loadmonmap(mon_map,dso,rawfile(1),status)
    if (status == IXCseverity_error)return
    call IXFverify_period_map(mon_map,period,rawfile(1),status)    
    call loadmask(mon_mask,dso,IXCmonmask,status)
    if (status == IXCseverity_error)return  

    call loadheaderinfo_isis(runfile,dso,rawfile,status)
    !-----------------------------------------------------------------------------------------------------------------------         
    call IXFpopulate_instrument(runfile%inst,rawfile(1),dso,status,mon_map=mon_map)
    if(IXFpresent(opt,ei=ei))call IXFseteival_runfile(runfile,ei(1),status)

    call IXFget_ptr_instrument(runfile%inst,spectra=spe_ptr,detector=det_ptr)
    ! L1 efixed and emode will be relative to whether emode=0,1,2
    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)
    if (status == IXCseverity_error) return    
    !populate mon_data object        
    call IXFpopulate_data(runfile%mon_data,period,mon_map,mon_mask,status,rawfile,dso,&
         det_ptr, spe_ptr,efixed,emode,L1,.false.,IXFpresent(opt,m_axis=m_axis),IXFpresent(opt,m_rebin=m_rebin),&
         m_rebin,m_axis,opt,(/ -1_i4b, 0 /)) !-1 is nchunk for catch
    !while the runfile is not completely populated ie with sample/users/peaks it is notionally valid for operations on datasets
    call IXFmark_valid(runfile)

    call closerawfile(rawfile,status)

  end subroutine IXFpopulate_mon_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  ! this subroutine will specifically only populate the det_data part of a runfile
  subroutine IXFpopulate_det_runfile(runfile,status,dso,period,nchunk,ei,d_axis,d_rebin,bgrd,i_lim,opt)
    use IXMmap
    use IXMmask
    implicit none
    type(IXTrunfile),intent(inout)::runfile
    type(IXTdata_source),intent(in)::dso
    integer(i4b),intent(in)::period,nchunk(2)    
    type(IXToptions),optional,intent(in)::opt
    type(IXTaxis),optional,intent(in)::d_axis
    real(dp),intent(in),optional::d_rebin(:),bgrd(2),ei(2),i_lim(2)
    type(IXTstatus)::status
    type(IXTmap)::det_map
    type(IXTmask)::det_mask                
    type(IXTraw_file),allocatable::rawfile(:)
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    real(dp)::efixed,L1
    integer(i4b)::emode
    ! most importantly check if rawfile present
    call loadrawfile(rawfile,dso,status)   
    if (status == IXCseverity_error)return

    !load information into detector IXTmap object: det_map    
    call loaddetmap(det_map,dso,rawfile(1),status)
    if (status == IXCseverity_error)return
    call IXFverify_period_map(det_map,period,rawfile(1),status)
    call loadmask(det_mask,dso,IXCdetmask,status)
    if (status == IXCseverity_error)return  
    call loadheaderinfo_isis(runfile,dso,rawfile,status)
    !-----------------------------------------------------------------------------------------------------------------------         
    call IXFpopulate_instrument(runfile%inst,rawfile(1),dso,status,det_map=det_map)
    if(IXFpresent(opt,ei=ei))call IXFseteival_runfile(runfile,ei(1),status)

    call IXFget_ptr_instrument(runfile%inst,spectra=spe_ptr,detector=det_ptr)
    ! L1 efixed and emode will be relative to whether emode=0,1,2
    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)          
    if (status == IXCseverity_error) return    

    !populate det_data object
    call IXFpopulate_data(runfile%det_data,period,det_map,det_mask,status,rawfile,dso,&
         det_ptr, spe_ptr,efixed,emode,L1,IXFpresent(opt,d_int=i_lim),IXFpresent(opt,d_axis=d_axis),IXFpresent(opt,d_rebin=d_rebin),&
         d_rebin,d_axis,opt,nchunk,bgrd,i_lim)
    !while the runfile is not completely populated ie with sample/users/peaks it is notionally valid for operations on datasets
    call IXFmark_valid(runfile)
    call closerawfile(rawfile,status)
  end subroutine IXFpopulate_det_runfile
  !-----------------------------------------------------------------------------------------------------------------------  

  subroutine units_runfile(runfile,status,axis_out)
    use IXMaxis
    implicit none
    type(IXTrunfile)::runfile
    type(IXTaxis),intent(in)::axis_out
    type(IXTstatus)::status
    integer(i4b)::emode
    real(dp)::efixed,L1,d_correction(2)
    type(IXTdataset_2d),pointer::d2d(:)
    type(IXTaxis),pointer::xaxis

    if( IXFvalid(runfile%det_data) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTrunfile%det_data object must be valid for units conversion (IXFunits_runfile)')
       return
    endif

    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)

    if(emode == 1)then !if it is a chopper instrument setup
       call IXFget_ptr_data(runfile%det_data,datasets=d2d)
       call IXFget_ptr_dataset_2d(d2d(1),x_axis=xaxis)
       if(IXFisunitcode(xaxis,IXCcode_t))then !if det_data are in time
          call get_d_correction(runfile,d_correction,status)
          call IXFunits_data(runfile%det_data,status,emode,efixed,L1,axis_out,d_correction)
       else
          call IXFunits_data(runfile%det_data,status,emode,efixed,L1,axis_out)
       endif
    else
       call IXFunits_data(runfile%det_data,status,emode,efixed,L1,axis_out)
    endif
  end subroutine units_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine units_rebinXdesc_runfile(runfile,status,axis_out,Xdesc)
    use IXMaxis
    implicit none
    type(IXTrunfile)::runfile
    type(IXTaxis),intent(in)::axis_out
    real(dp),intent(in)::Xdesc(:)
    type(IXTstatus)::status
    integer(i4b)::emode
    real(dp)::efixed,L1,d_correction(2)
    type(IXTdataset_2d),pointer::d2d(:)
    type(IXTaxis),pointer::xaxis
    
    if( IXFvalid(runfile%det_data) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTrunfile%det_data object must be valid for units conversion (IXFunits_runfile)')
       return
    endif

    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)

    if(emode == 1)then !if it is a chopper instrument setup
       call IXFget_ptr_data(runfile%det_data,datasets=d2d)
       call IXFget_ptr_dataset_2d(d2d(1),x_axis=xaxis)
       if(IXFisunitcode(xaxis,IXCcode_t))then !if det_data are in time
          call get_d_correction(runfile,d_correction,status)
          call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xdesc=Xdesc,d_correction=d_correction)
       else
          call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xdesc=Xdesc)
       endif
    else
       call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xdesc=Xdesc)
    endif

  end subroutine units_rebinXdesc_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine units_rebinXref_runfile(runfile,status,axis_out,Xref)
    use IXMaxis
    implicit none
    type(IXTrunfile)::runfile
    type(IXTaxis),intent(in)::axis_out
    type(IXTdataset_2d)::Xref    
    type(IXTstatus)::status
    integer(i4b)::emode
    real(dp)::efixed,L1,d_correction(2)
    type(IXTdataset_2d),pointer::d2d(:)
    type(IXTaxis),pointer::xaxis
    if( IXFvalid(runfile%det_data) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTrunfile%det_data object must be valid for units conversion (IXFunits_runfile)')
       return
    endif
    
    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)

    if(emode == 1)then !if it is a chopper instrument setup
       call IXFget_ptr_data(runfile%det_data,datasets=d2d)
       call IXFget_ptr_dataset_2d(d2d(1),x_axis=xaxis)
       if(IXFisunitcode(xaxis,IXCcode_t))then !if det_data are in time
          call get_d_correction(runfile,d_correction,status)
          call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xref=Xref,d_correction=d_correction)
       else
          call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xref=Xref)
       endif
    else
       call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xref=Xref)
    endif

    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)

    call IXFunits_rebin_data(runfile%det_data,status,emode,efixed,L1,axis_out,Xref=Xref)

  end subroutine units_rebinXref_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFrebin_runfile this will rebin the detector datasets given either a reference IXTdataset_2d object or an array
  !! describing the new histogram boundaries
  subroutine IXFrebin_x_det_runfile(runfile,status,Xdesc,Xref)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in),optional::Xdesc(:)
    type(IXTdataset_2d),optional::Xref
    type(IXTstatus)::status

    call IXFrebin_data(runfile%det_data,status,Xdesc,Xref)

  end subroutine IXFrebin_x_det_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFrebin_mon_runfile this will rebin the detector datasets given either a reference IXTdataset_2d object or an array
  !! describing the new histogram boundaries
  subroutine IXFrebin_x_mon_runfile(runfile,status,Xdesc,Xref)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in),optional::Xdesc(:)
    type(IXTdataset_2d),optional::Xref
    type(IXTstatus)::status

    call IXFrebin_data(runfile%mon_data,status,Xdesc,Xref)

  end subroutine IXFrebin_x_mon_runfile

  subroutine IXFrebin_x_runfile(runfile,status,Xdesc,Xref)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in),optional::Xdesc(:)
    type(IXTdataset_2d),optional::Xref
    type(IXTstatus)::status

    call IXFrebin_data(runfile%mon_data,status,Xdesc,Xref)
    if(status == IXCseverity_error)return
    call IXFrebin_data(runfile%det_data,status,Xdesc,Xref)    

  end subroutine IXFrebin_x_runfile


  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFrebunch_x_mon_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    integer(i4b),intent(in)::Xdesc
    type(IXTstatus)::status

    call IXFrebunch_data(runfile%mon_data,Xdesc,status)

  end subroutine IXFrebunch_x_mon_runfile

  subroutine IXFrebunch_x_det_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    integer(i4b),intent(in)::Xdesc
    type(IXTstatus)::status

    call IXFrebunch_data(runfile%det_data,Xdesc,status)

  end subroutine IXFrebunch_x_det_runfile

  subroutine IXFrebunch_x_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    integer(i4b),intent(in)::Xdesc
    type(IXTstatus)::status
    
    call IXFrebunch_data(runfile%mon_data,Xdesc,status)
    if(status == IXCseverity_error)return
    call IXFrebunch_data(runfile%det_data,Xdesc,status)

  end subroutine IXFrebunch_x_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFregroup_x_mon_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in)::Xdesc(:)
    type(IXTstatus)::status

    call IXFregroup_data(runfile%mon_data,Xdesc,status)

  end subroutine IXFregroup_x_mon_runfile

  subroutine IXFregroup_x_det_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in)::Xdesc(:)
    type(IXTstatus)::status

    call IXFregroup_data(runfile%det_data,Xdesc,status)

  end subroutine IXFregroup_x_det_runfile

  subroutine IXFregroup_x_runfile(runfile,Xdesc,status)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in)::Xdesc(:)
    type(IXTstatus)::status
    
    call IXFregroup_data(runfile%mon_data,Xdesc,status)
    if(status == IXCseverity_error)return
    call IXFregroup_data(runfile%det_data,Xdesc,status)

  end subroutine IXFregroup_x_runfile

  subroutine IXFshift_x_runfile(runfile,shift,status)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in)::shift
    type(IXTstatus)::status
    
    call IXFshift_x_data(runfile%mon_data,shift,status)
    if(status == IXCseverity_error)return
    call IXFshift_x_data(runfile%det_data,shift,status)

  end subroutine IXFshift_x_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFintegrate_x_mon_runfile this will integrate the monitor datasets given appropriate integration limits  
  subroutine IXFintegrate_x_mon_runfile(rfile_out,rfile_in,i_lim,status)
    implicit none
    type(IXTrunfile)::rfile_out,rfile_in
    real(dp)::i_lim(2)
    type(IXTstatus)::status

    call IXFintegrate_x_data(rfile_out%mon_data,rfile_in%mon_data,i_lim,status)
    if (status == IXCseverity_error) return
    call IXFcopy(rfile_in%det_data,rfile_out%det_data,status)
    call finish_op_runfile(rfile_out,rfile_in,status)
  end subroutine IXFintegrate_x_mon_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFintegrate_x_mon_runfile this will integrate the detector datasets given appropriate integration limits  
  subroutine IXFintegrate_x_det_runfile(rfile_out,rfile_in,i_lim,status)
    implicit none
    type(IXTrunfile)::rfile_out,rfile_in
    real(dp)::i_lim(2)
    type(IXTstatus)::status

    call IXFintegrate_x_data(rfile_out%det_data,rfile_in%det_data,i_lim,status)
    if (status == IXCseverity_error) return
    call IXFcopy(rfile_in%mon_data,rfile_out%mon_data,status)
    call finish_op_runfile(rfile_out,rfile_in,status)
  end subroutine IXFintegrate_x_det_runfile
  !-----------------------------------------------------------------------------------------------------------------------  

  subroutine IXFintegrate_x_runfile(rfile_out,rfile_in,i_lim,status)
    implicit none
    type(IXTrunfile)::rfile_out,rfile_in
    real(dp)::i_lim(2)
    type(IXTstatus)::status

    call IXFintegrate_x_data(rfile_out%mon_data,rfile_in%mon_data,i_lim,status)
    if (status == IXCseverity_error) return
    call IXFintegrate_x_data(rfile_out%det_data,rfile_in%det_data,i_lim,status)
    if (status == IXCseverity_error) return
    call IXFcopy(rfile_in%mon_data,rfile_out%mon_data,status)
    call finish_op_runfile(rfile_out,rfile_in,status)
  end subroutine IXFintegrate_x_runfile


  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFbackground_runfile this will calculate and subtract a flat background from the detector datasets given two 
  !! background limits
  subroutine IXFbackground_runfile(runfile,bmin,bmax,status)
    implicit none
    type(IXTrunfile)::runfile
    real(dp),intent(in)::bmin,bmax !!backgorund limits
    type(IXTstatus)::status

    call IXFbackground_data(runfile%det_data,bmin,bmax,status)

  end subroutine IXFbackground_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFcompare_runfile(rfile1,rfile2,ident,status)
    implicit none
    type(IXTrunfile),intent(in)::rfile1,rfile2
    type(IXTstatus)::status
    logical::ident
    ident=.false.
    ident = IXFcompare_instrument(rfile1%inst,rfile2%inst)

    if(.not. ident)call IXFwrite_line('detector mapping not commensurate',status)
  end subroutine IXFcompare_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  !!IXFremap_runfile, subroutine which will remap a runfile provided bridges contain subsiduary spectra to new map
  subroutine IXFremap_runfile(runfile,dso,status)
    use IXMmap
    use IXMmask
    implicit none
    type(IXTrunfile)::runfile
    type(IXTdata_source),intent(in)::dso
    type(IXTstatus)::status
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    type(IXTmap)::dmap
    type(IXTmask)::dmask
    logical::found
    character(len=long_len)::fpath,dumpath

    ! map and mask information loaded from reference data_source object
    !-----------------------------------------------------------------------------------------------------------------------               
    !load information into detector IXTmap object: det_map    
    found=.false.
    call IXFfindpath_data_source(dso,IXCdetmap,fpath,dumpath,found,status)
    if(found)then       
       call IXFpopulate_map(dmap,fpath,status)
       if (status == IXCseverity_error)return
       call IXFwrite_line('Detector map file loaded from '//fpath,status)                  
    else
       !failure here - should not happen
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_filenotfound,'Fatal error, remapping file not found in dso(IXFremap_runfile)')
       return
    endif

    call loadmask(dmask,dso,IXCdetmask,status)
    if (status == IXCseverity_error)return
    !-----------------------------------------------------------------------------------------------------------------------                          
    call IXFget_ptr_instrument(runfile%inst,spectra=spe_ptr,detector=det_ptr)
    call IXFremap_data(runfile%det_data,dso,dmap,dmask,det_ptr,spe_ptr,status)

  end subroutine IXFremap_runfile
  !-----------------------------------------------------------------------------------------------------------------------  

  subroutine IXFgetei_runfile(runfile,Ei,ei_extras,monitor_no,status)
    implicit none
    type(IXTrunfile),intent(in)::runfile
    real(dp),intent(inout)::Ei
    real(dp),intent(out)::ei_extras(6)
    integer(i4b),intent(inout)::monitor_no(2)
    type(IXTstatus),intent(inout)::status
    real(dp)::L1,L2(2)

    ! one would normally inspect the instrument using:
    ! runfile.inst.moderator.L1 
    ! runfile.inst.detector.L2
    ! to determine Lm1 and Lm2 
    call IXFei_info_instrument(runfile%inst,L1,status)
    if(status == IXCseverity_error)then
       Ei=0.0_dp
       return 
    endif
    !Lm1=10.205_dp
    !Lm2=17.51_dp
    ! send down total charge for text output on screen
    call IXFgetei_data(runfile%mon_data,Ei,ei_extras,monitor_no,L1,runfile%total_charge,status)    
  end subroutine IXFgetei_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFsolid_runfile(rf,dso,wbrf,status)
    implicit none
    type(IXTrunfile)::rf,wbrf
    type(IXTstatus)::status
    type(IXTdata_source)::dso
    type(IXTdataset_2d),pointer::wbdataset(:),rfdataset(:)
    type(IXTdataset_2d)::normdata(1)
    real(dp),pointer::vec_s(:,:),vec_e(:,:)
    real(dp),allocatable::val(:),err(:)

    logical::ident1,ident2
    !check instruments IXTspectra object identity, ie precise spectrum to detector mapping
    ident1=.false.
    ident2=.false.
    ! don't need to check each spectra object the same, rather that common spectra have same mapping
    !    call IXFcompare_runfile(rf,wbrf,ident1,status)
    ident1=IXFwhitecompare_runfile(rf,wbrf)


    if(ident1) then
       !check bridges are the same
       call IXFcomparebridge_data(rf%det_data,wbrf%det_data,ident2,status)
       ! if bridges are not the same then if possible remap
       if ( .not. ident2)then
          call IXFwrite_line('workspace-spectra bridge mapping is incomensurate, &
               &  whitebeam runfile will be remapped if possible',status)

          call IXFremap_runfile(wbrf,dso,status)
       endif
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'White beam rawfile not commensurate with sample rawfile,(IXFsolid_runfile)')
       return
    endif
    ! extract dataset_2d's from each runfile
    ! arrays should both be of length 1
    call IXFget_ptr_data(rf%det_data,rfdataset)
    call IXFget_ptr_data(wbrf%det_data,wbdataset)

    if (size(wbdataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'white beam data runfile not properly rebinned,(IXFsolid_runfile)')
       return
    endif
    if (size(rfdataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'rawdata runfile not properly rebinned,(IXFsolid_runfile)')
       return
    endif

    call IXFget_ptr_dataset_2d(wbdataset(1),signal=vec_s,error=vec_e)

    if (size(vec_s,1) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'White beam runfile not properly rebinned,(IXFsolid_runfile)')
       return
    endif

    allocate(val(size(vec_s,2)),err(size(vec_s,2)))    
    val=vec_s(1,:)
    err=vec_e(1,:)    
    ! divide val vector along Y direction respectively
    ! this calls a routine which does not propagate the errors in the normalised
    ! signal in the division since the err() array is not called as an argument
    call IXFarray_Y_Divide_dataset_2d(normdata(1),rfdataset(1),val,status)
    call IXFset_data(rf%det_data,status,datasets=normdata)
    call IXFdestroy(normdata,status)
    wbdataset=>NULL()
    rfdataset=>NULL()
    vec_s=>NULL()
    vec_e=>NULL()
    deallocate(val,err)
  end subroutine IXFsolid_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  logical function IXFwhitecompare_runfile(sample,whitebeam)result(ident)
    implicit none
    type(IXTrunfile),intent(in)::sample,whitebeam   
    ident=IXFwhitecompare_instrument(sample%inst,whitebeam%inst)   
  end function IXFwhitecompare_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFmon_norm_runfile will perform monitor specific normalisation of a runfile
  subroutine IXFmon_norm_runfile(rf,wk_ind,limits,scale,status)
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    integer(i4b),intent(in)::wk_ind
    type(IXTdataset_2d),pointer::mon_dataset(:),det_dataset(:)
    real(dp),intent(in)::limits(2),scale
    real(dp),pointer::signal(:)
    type(IXTdataset_1d)::int_res
    type(IXTdataset_2d)::res2d
    real(dp)::normfactor

    ! extract dataset_2d's from runfile
    ! both arrays should be of length 1
    call IXFget_ptr_data(rf%mon_data,mon_dataset)
    call IXFget_ptr_data(rf%det_data,det_dataset)

    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'det_data datasets not properly rebinned,(IXFmon_norm_runfile)')
       return
    endif

    if (size(mon_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'mon_data datasets not properly rebinned,(IXFmon_norm_runfile)')
       return
    endif

    ! for this example the whole monitor dataset is integrated, there are
    ! only four spectra populated so there is no extra overhead
    ! we could extract the signal and error arrays, and only integrate the i'th spectrum....
    call IXFintegrate_x_dataset_2d(int_res,mon_dataset(1),limits(1),limits(2),status)
    call IXFget_ptr_dataset_1d(int_res,signal=signal)
    normfactor=signal(wk_ind)

    call IXFtimes_dataset_2d(res2d,det_dataset(1),scale/normfactor,status)
    call IXFdestroy_dataset_2d(det_dataset(1),status)
    call IXFcopy(res2d,det_dataset(1),status)
    call IXFdestroy_dataset_2d(res2d,status)
    mon_dataset=>NULL()
    det_dataset=>NULL()

  end subroutine IXFmon_norm_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFpeak_norm_runfile(rf,wk_ind,ei,scale,status)
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    integer(i4b),intent(in)::wk_ind
    real(dp),intent(in)::ei,scale
    type(IXTdataset_2d),pointer::mon_dataset(:),det_dataset(:)
    type(IXTdataset_2d)::res2d
    real(dp)::area,L1 ! T_M is a dummy argument in this respect

    call IXFei_info_instrument(rf%inst,L1,status)
    if(status == IXCseverity_error)return
    !no keyword since first argument in suibroutine call
    call IXFget_ptr_data(rf%det_data,det_dataset)

    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'det_data datasets not properly rebinned,(IXFpeak_norm_runfile)')
       return
    endif


    ! find peak area in mon_dataset
    call IXFpeakarea_data(rf%mon_data,L1,ei,wk_ind,area,status)
    if(status == IXCseverity_error)return

    call IXFtimes_dataset_2d(res2d,det_dataset(1),scale/area,status)
    call IXFdestroy_dataset_2d(det_dataset(1),status)
    call IXFcopy(res2d,det_dataset(1),status)
    call IXFdestroy_dataset_2d(res2d,status)
    mon_dataset=>NULL()
    det_dataset=>NULL()

  end subroutine IXFpeak_norm_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFcharge_norm_runfile(rf,scale,status)
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    real(dp),intent(in)::scale
    type(IXTdataset_2d),pointer::det_dataset(:)
    type(IXTdataset_2d)::res2d

    !no keyword since first argument in suibroutine call
    call IXFget_ptr_data(rf%det_data,det_dataset)

    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'det_data datasets not properly rebinned,(IXFcharge_norm_runfile)')
       return
    endif

    call IXFtimes_dataset_2d(res2d,det_dataset(1),scale/rf%total_charge,status)

    call IXFdestroy_dataset_2d(det_dataset(1),status)
    call IXFcopy(res2d,det_dataset(1),status)
    call IXFdestroy_dataset_2d(res2d,status)
    det_dataset=>NULL()

  end subroutine IXFcharge_norm_runfile

  !-----------------------------------------------------------------------------------------------------------------------  

  subroutine IXFmonovan_runfile(rf,zero,tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig,abs_val,status)
    use IXMmask_array
    implicit none
    type(IXTrunfile),intent(in)::rf
    type(IXTstatus),intent(inout)::status    
    real(dp),intent(in)::tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig
    logical,intent(in)::zero
    real(dp),intent(out)::abs_val
    real(dp)::average,sigma
    logical,allocatable::mask(:)
    type(IXTdataset_2d),pointer::det_dataset(:)    
    real(dp),pointer::signal(:,:),error(:,:)

    abs_val=0.0d0
    !no keyword since first argument in subroutine call
    call getlinearsignalerror(rf,status,signal,error)

    allocate(mask(size(signal,2)))
    mask=.true.
    call IXFmask_positive_array(signal(1,:),error(1,:),mask,zero,tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig)        
    call IXFaverage_array(signal(1,:),error(1,:),mask,average=average,sigma=sigma,median=abs_val)
    deallocate(mask)
    det_dataset=>NULL()
    signal=>NULL()
    error=>NULL()
  end subroutine IXFmonovan_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFdiag_runfile(rf_run,rf_v1,tiny,huge,s_zero,s_out_lo,s_out_hi,sv_lo,sv_hi,sv_sig,  &
       v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,  &
       out_mask,o_cause,o_value,o_error,o_file,r,r_sig,status,rf_v2,bank,hmask,rf_ZC)
    use IXMmask_array
    use IXMmask
    use IXMmap
    implicit none
    type(IXTrunfile),intent(in)::rf_run,rf_v1
    real(dp),intent(in)::s_out_lo,s_out_hi,sv_lo,sv_hi,sv_sig,tiny,huge
    real(dp),intent(in)::v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,r,r_sig    
    logical,intent(in)::v_zero,s_zero
    type(IXTmask),intent(out)::out_mask
    ! no intent since allocatable problems
    integer(i4b),allocatable::o_cause(:),cause(:),o_file(:),c_file(:)
    real(dp),allocatable::o_value(:),c_value(:),o_error(:),c_error(:)
    type(IXTrunfile),optional,intent(in)::rf_v2,rf_ZC
    type(IXTmap),optional,intent(in)::bank
    type(IXTmask),optional,intent(in)::hmask
    type(IXTstatus),intent(inout)::status
    logical,allocatable::mask(:)
    real(dp),pointer::run_signal(:,:),run_error(:,:),v1_signal(:,:),v1_error(:,:),v2_signal(:,:),v2_error(:,:),v_wkno(:),s_wkno(:)
    integer(i4b),pointer::t_spec(:),s_ind(:),s_no(:),hmask_arr(:)
    integer(i4b)::i,j,data_len,n_cause
    logical::v2,bank_fl,jj
    real(dp),allocatable::run_s_all(:,:),run_e_all(:,:)
    real(dp)::median,hmean

    bank_fl=.false.
    v2=.false.
    !get bank info if present
    if(present(bank))then
       if(IXFvalid(bank))then
          call IXFget_ptr_map(bank,total_spec=t_spec,spec_ind=s_ind,spec_no=s_no)
          bank_fl=.true.
          if(.not. IXFverify_spectra_map(bank))then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
                  IXCerr_invparam,'Spectra appears twice in same bank or another bank(IXFdiag_runfile)')
             return
          endif
       endif
    endif

    ! get mono sample run data
    call getlinearsignalerror(rf_run,status,run_signal,run_error,s_wkno)
    data_len=size(run_signal,2)

    !create mask + cause array, and populate with initial values
    allocate (mask(data_len),cause(data_len),c_value(data_len),c_error(data_len),c_file(data_len))
    mask=.true.
    cause=0
    c_file=0
    c_value=0.0d0
    c_error=0.0d0

    !get first vanadium run
    call getlinearsignalerror(rf_v1,status,v1_signal,v1_error,v_wkno)
    if (data_len /= size(v1_signal,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'data run and first vanadium run not commensurate,(IXFdiag_runfile)')
       return
    elseif(sum(abs(v_wkno-s_wkno)) /= 0.0)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'data run and first vanadium workspace numbers not commensurate,(IXFdiag_runfile)')
       return
    endif

    !populate hard mask if present, matching spectra to mask using wkno array
    if(present(hmask))then
       if(IXFvalid(hmask))then
          call IXFget_ptr_mask(hmask,hmask_arr)
          !cannot use subscripting because some spectra do not have contributing detector
          ! ands have therefore not been populated
          ! it is assumed that whitebeam vanadium has not been populated with any strange map file
          ! and that proper workspace numbers corresponding to spectra are in wkno array         
          i=1
          j=1
          jj=.true.
          do while(i <= size(hmask_arr) )
             do while(jj)
                if(hmask_arr(i)==int(v_wkno(j)))then
                   mask(j)=.false.
                   cause(j)=IXCmask_hard
                   jj=.false.                     
                endif
                j=j+1
             end do
             jj=.true.
             i=i+1
          end do
          hmask_arr=>NULL()
       endif
    endif

    !get second vanadium run if present
    if(present(rf_v2))then
       if(IXFvalid(rf_v2))then
          call getlinearsignalerror(rf_v2,status,v2_signal,v2_error)
          if (data_len /= size(v2_signal,2))then
             call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
                  IXCerr_invparam,'data run and second vanadium run not commensurate,(IXFdiag_runfile)')
             return
          endif
          v2=.true.
       endif
    endif

    if(bank_fl)then
       !masking of first vanadium run    
       call maskintegral(v1_signal,v1_error,mask,cause,c_value,c_error,c_file,1,v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,tiny,huge,t_spec,s_ind,s_no)    
       if(v2)then
          !masking of second vanadium run
          call maskintegral(v2_signal,v2_error,mask,cause,c_value,c_error,c_file,2,v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,tiny,huge,t_spec,s_ind,s_no)
          call maskratio(v1_signal,v1_error,v2_signal,v2_error,mask,cause,c_value,r,r_sig,t_spec,s_ind,s_no)       
!          !make harmonic mean and normalise whole dataset

!          allocate(hmean(data_len))
!          allocate(run_s_all(1,data_len),run_e_all(1,data_len))
!          hmean=2.0d0/(  (1.0/v1_signal(1,:)) +  (1.0/v2_signal(1,:))  )
!          run_s_all(1,:)=run_signal(1,:)/hmean
!          !taken from VMS DIAG
!          run_e_all(1,:)=sqrt(run_signal(1,:))/hmean
!          deallocate(hmean)

          allocate(run_s_all(1,data_len),run_e_all(1,data_len))
          do i=1,data_len
             if(v1_signal(1,i)/=0.0 .and. v2_signal(1,i)/=0.0)then
! calculate hmean for each spectrum if no zeros           
                hmean=2.0d0/(  (1.0/v1_signal(1,i)) +  (1.0/v2_signal(1,i))  )
                run_s_all(1,i)=run_signal(1,i)/hmean
          !error determination taken from VMS DIAG
                run_e_all(1,i)=sqrt(run_signal(1,i))/hmean
             else
                run_s_all(1,i)=0.0
                run_e_all(1,i)=0.0
             endif
          enddo                              
       else
          ! no ratio masking, but normalise wrt v1 for each bank           
          !normalise mono smaple run

          allocate(run_s_all(1,data_len),run_e_all(1,data_len))
!!$
!!$          run_s_all(1,:)=run_signal(1,:)/v1_signal(1,:)
!!$          !taken from VMS DIAG
!!$          run_e_all(1,:)=sqrt(run_signal(1,:))/v1_signal(1,:)

          do i=1,data_len
             if (v1_signal(1,i)/= 0)then
                run_s_all(1,i)=run_signal(1,i)/v1_signal(1,i)
          !taken from VMS DIAG
                run_e_all(1,i)=sqrt(run_signal(1,i))/v1_signal(1,i)
             else
                run_s_all(1,i)=0.0
                run_e_all(1,i)=0.0
             endif
          enddo

       endif

       if(present(rf_ZC) .and. IXFvalid(rf_ZC))then
          call maskZC(mask,cause,rf_ZC,status)
       endif


       call masksample(run_s_all,run_e_all,mask,cause,c_value,c_error,s_zero,sv_hi,sv_sig,t_spec,s_ind,s_no)

       deallocate(run_s_all,run_e_all)
       run_signal=>NULL()
       run_error=>NULL()
       v1_signal=>NULL()
       v1_error=>NULL()

       if (associated(v2_signal))v2_signal=>NULL()
       if (associated(v2_error))v2_error=>NULL()

    else
       ! no banking information do everything all at once

       !masking of first vanadium run    
       call maskintegral(v1_signal,v1_error,mask,cause,c_value,c_error,c_file,1,v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,tiny,huge)    
       !masking of second vanadium run
       if(v2)then
          call maskintegral(v2_signal,v2_error,mask,cause,c_value,c_error,c_file,2,v_zero,v_out_lo,v_out_hi,vv_lo,vv_hi,vv_sig,tiny,huge) 
          call maskratio(v1_signal,v1_error,v2_signal,v2_error,mask,cause,c_value,r,r_sig)       

          allocate(run_s_all(1,data_len),run_e_all(1,data_len))
          do i=1,data_len
             if(v1_signal(1,i)/=0.0 .and. v2_signal(1,i)/=0.0)then
! calculate hmean for each spectrum if no zeros           
                hmean=2.0d0/(  (1.0/v1_signal(1,i)) +  (1.0/v2_signal(1,i))  )
                run_s_all(1,i)=run_signal(1,i)/hmean
          !error determination taken from VMS DIAG
                run_e_all(1,i)=sqrt(run_signal(1,i))/hmean
             else
                run_s_all(1,i)=0.0
                run_e_all(1,i)=0.0
             endif
          enddo
!          deallocate(hmean) 
       else                             
          ! no ratio masking, but normalise wrt v1 for each bank           
          !normalise mono smaple run
          allocate(run_s_all(1,data_len),run_e_all(1,data_len))
          do i=1,data_len
             if (v1_signal(1,i)/= 0)then
                run_s_all(1,i)=run_signal(1,i)/v1_signal(1,i)
          !taken from VMS DIAG
                run_e_all(1,i)=sqrt(run_signal(1,i))/v1_signal(1,i)
             else
                run_s_all(1,i)=0.0
                run_e_all(1,i)=0.0
             endif
          enddo

       endif

       if(present(rf_ZC))then
          if(IXFvalid(rf_ZC))then
             call maskZC(mask,cause,rf_ZC,status)
          endif
       endif

       call masksample(run_s_all,run_e_all,mask,cause,c_value,c_error,s_zero,sv_hi,sv_sig)
       deallocate(run_s_all,run_e_all)

       run_signal=>NULL()
       run_error=>NULL()
       v1_signal=>NULL()
       v1_error=>NULL()

       if (associated(v2_signal))v2_signal=>NULL()
       if (associated(v2_error))v2_error=>NULL()

    endif

    !with the assumption that the runfile has been populated with one spectrum per workspace
    !get workspace numbers from dataset and apply to mask array
    call IXFcreate_mask(out_mask,pack(int(s_wkno), .not.mask),status)
    n_cause=count(.not.mask)
    allocate(o_cause(n_cause),o_value(n_cause),o_error(n_cause),o_file(n_cause))
    o_cause=pack(cause,.not.mask)
    o_value=pack(c_value,.not.mask)
    o_error=pack(c_error,.not.mask)
    o_file=pack(c_file,.not.mask)
    s_wkno=>NULL()
    v_wkno=>NULL()
    deallocate(cause,c_file,c_value,c_error)
  end subroutine IXFdiag_runfile
  !masking from normalised monchromatic sample run

  ! to load in sample rawdata on the fly
  ! most useful to be supplied IXTraw_file object
  ! fill up a 1:1 IXTmap object with call below
  !    call IXFrawfile_popdet_map(det_map,rawfile,status)
  !    call IXFopen_raw(some_path_to_raw_file, rf, status)
  ! then fill d2d with spectra
  !    call IXFget_ptr_map(det_map,specno=specno)
  !    call IXFget_raw(rf, specno ,d2d,period, status)
  !    call IXFget_ptr_dataset_2d(d2d,signal=run_signal,error=run_error)
  !    allocate(sumspec(IXTmap%specno))
  !    sumspec=sum(run_signal,1)
  !    where (sumspec .eq. 0.0d0)
  !       mask =.FALSE.
  !       cause= IXCmask_zero_total
  !    end where
  !    sig=>NULL()
  !    deallocate(sumspec)

  ! then integrate according to bmin, bmax (VMS arguments)
  !    call IXFintegrate_x_dataset_2d(d1d,d2d,bmin,bmax,status)
  ! pointers should change with the object data
  ! and ready for normalisation wrt vanadium signal and masking


  ! first few optional arguments are in the order, keywords are specified later on in the list
  !      call maskintegral(run_s_all,run_e_all,mask,cause,s_zero,s_out_lo,s_out_hi,sv_lo,sv_hi,sv_sig,t_spec=t_spec,s_ind=s_ind,s_no=s_no)  


  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine maskintegral(sig,err,mask,cause,c_value,c_error,c_file,file_no,zero, out_lo, out_hi, v_lo, v_hi,v_sig, tiny, huge,t_spec,s_ind,s_no)
    use IXMmask_array
    implicit none
    real(dp),intent(in)::sig(:,:),err(:,:)
    logical,intent(in)::zero
    integer,intent(in)::file_no
    real(dp),optional,intent(in)::out_lo,out_hi,v_lo,v_hi,v_sig,tiny,huge
    integer(i4b),optional,intent(in)::t_spec(:),s_ind(:),s_no(:)
    integer(i4b),intent(inout)::cause(:),c_file(:)
    real(dp),intent(inout)::c_value(:),c_error(:)
    logical,intent(inout)::mask(:)        

    integer(i4b)::i,r1,r2

    if(present(t_spec))then
       do i=1,size(t_spec)
          r1=s_ind(i)
          r2=s_ind(i)+t_spec(i)-1
          call IXFmask_positive_array(sig(1,r1:r2),err(1,r1:r2),mask(r1:r2),zero,tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig,&
               cause(r1:r2),c_value(r1:r2),c_error(r1:r2),c_file(r1:r2))      
       enddo
    else
       call IXFmask_positive_array(sig(1,:),err(1,:),mask,zero,tiny,huge,out_lo,out_hi,v_lo,v_hi,v_sig,&
            cause,c_value,c_error,c_file)
    endif
    where (c_file  == -1 )
       c_file = file_no
    end where


  end subroutine maskintegral

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine masksample(sig,err,mask,cause,c_value,c_error,zero, v_hi,v_sig, t_spec,s_ind,s_no)
    use IXMmask_array
    implicit none
    real(dp),intent(in)::sig(:,:),err(:,:)
    logical,intent(in)::zero
    real(dp),intent(in)::v_hi,v_sig
    real(dp),intent(inout)::c_value(:),c_error(:)
    integer(i4b),optional,intent(in)::t_spec(:),s_ind(:),s_no(:)
    integer(i4b),intent(inout)::cause(:)
    logical,intent(inout)::mask(:)        

    integer(i4b)::i,r1,r2

    if(present(t_spec))then
       do i=1,size(t_spec)
          r1=s_ind(i)
          r2=s_ind(i)+t_spec(i)-1
          call IXFmask_sample_array(sig(1,r1:r2),err(1,r1:r2),mask(r1:r2),zero,v_hi,v_sig,&
               cause(r1:r2),c_value(r1:r2),c_error(r1:r2))      
       enddo
    else
       call IXFmask_sample_array(sig(1,:),err(1,:),mask,zero,v_hi,v_sig,cause,c_value,c_error)
    endif
  end subroutine masksample
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine maskZC(mask,cause,rf_ZC,status)
    use IXMmask_array
    implicit none
    type(IXTrunfile)::rf_ZC
    type(IXTstatus)::status
    integer(i4b)::cause(:)
    real(dp),pointer::ZC_signal(:,:)
    logical,intent(inout)::mask(:)        

    call getlinearsignalerror(rf_ZC,status,ZC_signal)

    where (mask .and.  ZC_signal(1,:)==0.0d0)
       mask = .FALSE.
       cause = IXCmask_zero_total
    end where

  end subroutine maskZC

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine maskratio(sig1,err1,sig2,err2,mask,cause,c_value,r,r_sig,t_spec,s_ind,s_no)
    use IXMmask_array
    implicit none
    real(dp),intent(in)::sig1(:,:),err1(:,:),sig2(:,:),err2(:,:)
    real(dp),intent(inout)::c_value(:)    
    real(dp),optional,intent(in)::r,r_sig
    integer(i4b),optional,intent(in)::t_spec(:),s_ind(:),s_no(:)
    integer(i4b),intent(inout)::cause(:)
    logical,intent(inout)::mask(:)        

    integer(i4b)::i,r1,r2

    if(present(t_spec))then
       do i=1,size(t_spec)
          r1=s_ind(i)
          r2=s_ind(i)+t_spec(i)-1
          call IXFmask_ratio_positive_array(sig1(1,r1:r2),err1(1,r1:r2),sig2(1,r1:r2),err2(1,r1:r2),mask(r1:r2),r,r_sig,cause(r1:r2),c_value(r1:r2))      
       enddo
    else
       call IXFmask_ratio_positive_array(sig1(1,:),err1(1,:),sig2(1,:),err2(1,:),mask,r,r_sig,cause,c_value)
    endif
  end subroutine maskratio

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine getlinearsignalerror(rfin,status,sigout,errout,y)
    implicit none
    type(IXTrunfile)::rfin
    real(dp),pointer::sigout(:,:)
    real(dp),optional,pointer::y(:),errout(:,:)
    type(IXTdataset_2d),pointer::det_dataset(:)
    type(IXtstatus)::status

    call IXFget_ptr_data(rfin%det_data,det_dataset)    
    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'run det_data datasets not properly rebinned,(getlinearsignalerror)')
       return
    endif
    call IXFget_ptr_dataset_2d(det_dataset(1),signal=sigout,error=errout,y=y)
    if(size(sigout,1)/=1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'run dataset signal/error array not linear,(getlinearsignalerror)')
       return
    endif
    det_dataset=>NULL()    
  end subroutine getlinearsignalerror
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFabsolute_norm_runfile(rf,factor,status)
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    real(dp),intent(in)::factor
    type(IXTdataset_2d),pointer::det_dataset(:)
    type(IXTdataset_2d)::res2d

    !no keyword since first argument in subroutine call
    call IXFget_ptr_data(rf%det_data,det_dataset)

    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'det_data datasets not properly rebinned,(IXFabsolute_norm_runfile)')
       return
    endif

    call IXFdivide_dataset_2d(res2d,det_dataset(1),factor,status)

    call IXFdestroy_dataset_2d(det_dataset(1),status)
    call IXFcopy(res2d,det_dataset(1),status)
    call IXFdestroy_dataset_2d(res2d,status)
    det_dataset=>NULL()

  end subroutine IXFabsolute_norm_runfile


  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFcorr_runfile(rf,status)
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    type(IXTdataset_2d),pointer::det_dataset(:)
    type(IXTchopper_instrument),pointer::ci_ptr
    type(IXTfermi_chopper),pointer::fc_ptr
    real(dp),parameter::atms=10.0d0
    real(dp)::energy
    integer(i4b)::emode

    call IXFunitsinfo_instrument(rf%inst,status,emode)

    !no keyword since first argument in suibroutine call
    call IXFget_ptr_data(rf%det_data,det_dataset)


    call IXFget_ptr_instrument(rf%inst,ci=ci_ptr)
    call IXFget_ptr_chopper_instrument(ci_ptr,monochromator=fc_ptr)
    call IXFget_Fermi_chopper(fc_ptr,status,energy=energy)
    if (size(det_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'det_data datasets not properly rebinned,(IXFeffic_norm_runfile)')
       return
    endif
    ! in the future we will have a dynamic calculation of efficiency according to the detector geometry
    ! for the moment all detectors are assumed to be He3 and have pressure of 10atms
    call IXFcorr_dataset_2d(det_dataset(1),atms,energy,emode,status)

    det_dataset=>NULL()
    ci_ptr=>NULL()
    fc_ptr=>NULL()
  end subroutine IXFcorr_runfile
  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFmoncorr_runfile(rf,abs_norm,energy,line_limit,status)
    use IXMefficiency
    implicit none
    type(IXTrunfile)::rf
    type(IXTstatus)::status
    real(dp),intent(out)::abs_norm
    type(IXTdataset_2d),pointer::mon_dataset(:)   
    type(IXTdataset_1d)::int_res
    type(IXTworkspace),pointer::workspace
    type(IXTdetector),pointer::det
    real(dp),pointer::L2(:),signal(:)
    real(dp),parameter::atms=10.0d0 !in microsecs
    real(dp),intent(in)::energy,line_limit
    real(dp)::efficiency,L1,time,time1,time2
    integer(i4b)::emode    

    !no keyword since first argument in suibroutine call
    call IXFget_ptr_data(rf%mon_data,mon_dataset)
    emode=1
    if (size(mon_dataset) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'mon_data datasets not properly rebinned,(IXFeffic_norm_runfile)')
       return
    endif
    efficiency=EFF(energy,atms)
    ! in the future we will have a dynamic calculation of efficiency according to the detector geometry
    ! for the moment all detectors are assumed to be He3 and have pressure of 10atms
    call IXFget_ptr_data(rf%mon_data,workspace=workspace)
    call IXFget_ptr_workspace(workspace,eff_det=det)
    call IXFget_ptr_detector(det,L2=L2)
    
    if(size(L2) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'monitor data should have only one workspace,(IXFeffic_norm_runfile)')
       return
    endif
    call IXFunitsinfo_instrument(rf%inst,status,emode,L1)
    !calculate integration limits over elastic line in time using energy and L1 & L2(mons)
    time=(252.8*  (L1+L2(1))  *9.044/sqrt(energy))
    time1=time-line_limit
    time2=time+line_limit
    
    call IXFintegrate_x_dataset_2d(int_res,mon_dataset(1),time1,time2,status) ! or whatever
    call IXFget_ptr_dataset_1d(int_res,signal=signal)
    ! divide by efficiency value for energy, for monitor 2 considering mapping from 4 detectors
    abs_norm=signal(1)/efficiency
    mon_dataset=>NULL()
  end subroutine IXFmoncorr_runfile

  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFpopulate_runfile(runfile,status,dso,period,nchunk,monei_info,ei,m_axis,m_rebin,d_axis,d_rebin,i_lim,bgrd,opt)
    use IXMmap
    use IXMmask
    implicit none
    type(IXTrunfile),intent(inout)::runfile
    type(IXTdata_source),intent(in)::dso
    integer(i4b),intent(in)::period,nchunk(2),monei_info(3)    
    real(dp),intent(in),optional::ei(2) !! a real EI value, calculated or not............................
    type(IXToptions),optional,intent(in)::opt
    type(IXTaxis),optional,intent(in)::m_axis,d_axis
    real(dp),intent(in),optional::m_rebin(:),d_rebin(:),bgrd(2),i_lim(2)
    type(IXTstatus)::status
    type(IXTmap)::det_map,mon_map
    type(IXTmask)::mon_mask,det_mask
    type(IXTraw_file),allocatable::rawfile(:)
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    real(dp)::efixed,L1,d_correction(2)
    integer(i4b)::emode
!!! frig parts
    type(IXTdetector),pointer::det
    type(IXTworkspace),pointer::workspace
    real(dp),pointer::L2array(:),xmean(:),mon_delay(:)
    integer(i4b),pointer::mon_nos(:)
    type(IXTmoments),pointer::moments   
    type(IXTdatum_array),pointer::darray    

    ! most importantly check if rawfile present
    ! if multiple rawfiles to be loaded this will allocate length
    call loadrawfile(rawfile,dso,status)   
    if (status == IXCseverity_error)return
    !load IXTmap objects
    call loadmonmap(mon_map,dso,rawfile(1),status)
    if (status == IXCseverity_error)return
    call loaddetmap(det_map,dso,rawfile(1),status)    
    if (status == IXCseverity_error)return
    !load IXTmask objects
    call loadmask(mon_mask,dso,IXCmonmask,status)
    if (status == IXCseverity_error)return  
    call loadmask(det_mask,dso,IXCdetmask,status)
    if (status == IXCseverity_error)return
    call IXFverify_period_map(det_map,period,rawfile(1),status)
    call IXFverify_period_map(mon_map,period,rawfile(1),status)
    if (status == IXCseverity_error)return
    ! this will combine total charge/good frames/bad frames for multiple rawfile input
    call loadheaderinfo_isis(runfile,dso,rawfile,status)
    !-----------------------------------------------------------------------------------------------------------------------         
    !perhaps call some function to populate peaks/moments structure
    ! common population tasks   
    call IXFpopulate_instrument(runfile%inst,rawfile(1),dso,status,det_map,mon_map)
    !once monochromator info has been filled then put in energy value
    !in theory one could also add frequency etc, in this process.....     
    if(IXFpresent(opt,ei=ei))call IXFseteival_runfile(runfile,ei(1),status)

    call IXFget_ptr_instrument(runfile%inst,spectra=spe_ptr,detector=det_ptr)
    ! L1 efixed and emode will be relative to whether emode=0,1,2
    ! for changing units of mon_data does not matter what efixed is....
    call IXFunitsinfo_instrument(runfile%inst,status,emode,L1,efixed)      
    if (status == IXCseverity_error) return    

    !populate mon_data object        
    call IXFpopulate_data(runfile%mon_data,period,mon_map,mon_mask,status,rawfile,dso,&
         det_ptr, spe_ptr,efixed,emode,L1,.false.,IXFpresent(opt,m_axis=m_axis),IXFpresent(opt,m_rebin=m_rebin),&
         m_rebin,m_axis,opt,(/ -1_i4b, 0 /)) ! -1 is nchunk for mon data catch
!!!    
    if(status == IXCseverity_error)return
    if(IXFpresent(opt,ei=ei))then
       call IXFget_ptr_data(runfile%mon_data,workspace=workspace)
       call IXFget_ptr_workspace(workspace,eff_det=det)
       call IXFget_ptr_detector(det,L2=L2array,delay_time=mon_delay)
       det=>NULL()
       workspace=>NULL()

       call IXFpopulate_peaks(runfile%peaks,runfile%mon_data,ei(1),L1,status)    
       !monei_info is only referenced if ei argument is present
       if (status == IXCseverity_error)return
       efixed=ei(1)

       if( (size(L2array) < monei_info(1)) .or. (size(L2array) < monei_info(2)))then
         call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam,'monitor info array defines indices which exceed the length of the monitor array (IXFpopulate_runfile)')
         return
       endif
       ! a guess/fix goes in a value is determined if required
       call IXFgetei_peaks(runfile%peaks,monei_info,efixed,L1+L2array(monei_info(1)),L1+L2array(monei_info(2)),status)
       call IXFseteival_runfile(runfile,efixed,status)
       if (status == IXCseverity_error) return   

       call IXFget_ptr_peaks(runfile%peaks,moments=moments,monitor_no=mon_nos)
       call IXFget_ptr_moments(moments,xmean=darray)
       call IXFget_ptr_datum_array(darray,signal=xmean)
       !the origin in time and space will be defined according to the first argument in the
       !monei_info array which is supplied. these are indices
       if (monei_info(3)==0)then !therefore a fixei value is set
       d_correction(1)=L2array(monei_info(1))
         if(ei(2)>0.0)then         
           d_correction(2)=ei(2) - mon_delay(monei_info(1))
         else          
         ! calculate time of fixed energy in microseconds, there is no delay in a determined measurement 
           d_correction(2)=(252.8*  (L1+d_correction(1))  *9.044/sqrt(ei(1))) ! - mon_delay(monei_info(1))
         endif
       else
         d_correction(1)=L2array(monei_info(1))
         d_correction(2)=xmean(monei_info(1)) - mon_delay(monei_info(1))
       endif
       L2array=>NULL()
       xmean=>NULL()
       moments=>NULL()
       mon_nos=>NULL()
       mon_delay=>NULL()
       !populate det_data object WITH d_correction argument
       call IXFpopulate_data(runfile%det_data,period,det_map,det_mask,status,rawfile,dso,&
            det_ptr, spe_ptr,efixed,emode,L1,IXFpresent(opt,d_int=i_lim),IXFpresent(opt,d_axis=d_axis),IXFpresent(opt,d_rebin=d_rebin),&
            d_rebin,d_axis,opt,nchunk,bgrd,i_lim,d_correction)       
    else
       !populate det_data object WITHOUT d_correction argument
       call IXFpopulate_data(runfile%det_data,period,det_map,det_mask,status,rawfile,dso,&
            det_ptr, spe_ptr,efixed,emode,L1,IXFpresent(opt,d_int=i_lim),IXFpresent(opt,d_axis=d_axis),IXFpresent(opt,d_rebin=d_rebin),&
            d_rebin,d_axis,opt,nchunk,bgrd,i_lim)
    endif
    ! if an error has been thrown during population it does not matter since it will get picked up in the binding and not sent back
    !while the runfile is not completely populated ie with sample/users/peaks it is notionally valid for operations on datasets
    call IXFmark_valid(runfile)
    call closerawfile(rawfile,status)
  end subroutine IXFpopulate_runfile
  !-----------------------------------------------------------------------------------------------------------------------    
  !load information into detector IXTmap object: mon_map      
  subroutine loadmonmap(mon_map,dso,rawfile,status)
    use IXMmap
    implicit none
    type(IXTmap),intent(out)::mon_map
    type(IXTdata_source)::dso
    type(IXTraw_file),intent(in)::rawfile    
    type(IXTstatus)::status
    logical::found
    character(len=long_len)::fpath,dumpath

    found=.false.
    call IXFfindpath_data_source(dso,IXCmonmap,fpath,dumpath,found,status)
    if(found)then
       call IXFpopulate_map(mon_map,fpath,status)
       if (status == IXCseverity_error)return
       call IXFwrite_line('Monitor map file loaded from '//trim(adjustl(fpath)),status)                 
    else
       ! call function to determine mapfile from rawfile if not supplied
       call IXFwrite_line('No monitor map file specified, 1:1 mapping',status)
       call IXFrawfile_popmon_map(mon_map,rawfile,status)      
    endif
  end subroutine loadmonmap

  !load information into detector IXTmap object: det_map      
  subroutine loaddetmap(det_map,dso,rawfile,status)
    use IXMmap
    implicit none
    type(IXTmap),intent(out)::det_map
    type(IXTdata_source)::dso
    type(IXTraw_file),intent(in)::rawfile    
    type(IXTstatus)::status
    logical::found
    character(len=long_len)::fpath,dumpath
    !load information into detector IXTmap object: det_map    
    found=.false.
    call IXFfindpath_data_source(dso,IXCdetmap,fpath,dumpath,found,status)
    if(found)then       
       call IXFpopulate_map(det_map,fpath,status)
       if (status == IXCseverity_error)return
       call IXFwrite_line('Detector map file loaded from '//trim(adjustl(fpath)),status)           
    else
       ! call function to determine mapfile from rawfile if not supplied
       call IXFwrite_line('No detector map file specified, 1:1 mapping',status)
       call IXFrawfile_popdet_map(det_map,rawfile,status)      
    endif
  end subroutine loaddetmap
  !load information into detector IXTmask object: mask  
  subroutine loadmask(mask,dso,masktype,status)
    use IXMmask
    implicit none
    type(IXTmask),intent(out)::mask
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    logical::found
    character(len=long_len)::fpath,dumpath
    character(len=*),intent(in)::masktype
    found=.false.
    call IXFfindpath_data_source(dso,masktype,fpath,dumpath,found,status)
    if(found)then       
       call IXFpopulate_mask(mask,fpath,status)
       call IXFwrite_line(trim(adjustl(masktype))//' loaded from '//trim(adjustl(fpath)),status)           
    else
       call IXFwrite_line('No '//trim(adjustl(masktype))//' specified',status)
       call IXFcreate_mask(mask,(/ 0 /),status )      
    endif
  end subroutine loadmask

  subroutine loadrawfile(rawfile,dso,status)
    implicit none
    type(IXTraw_file),allocatable::rawfile(:)
    type(IXTdata_source)::dso
    type(IXTstatus)::status
    logical::found
    integer(i4b)::i
    character(len=long_len)::fpath,dumpath
    character(len=long_len),allocatable::fpaths(:),dumpaths(:)

    fpath=''    
    found=.false.
    ! look for a single rawfile defintion
    call IXFfindpath_data_source(dso,IXCrawfile,fpath,dumpath,found,status)
    if(found)then
       allocate(rawfile(1))
       call IXFopen_raw(fpath,rawfile(1), status)       
       return
    endif
    ! then look for multiple raw file definition
    call IXFfindpaths_data_source(dso,IXCrawfile_mult,fpaths,dumpaths,found,status)
    if(found)then    
       allocate(rawfile(size(fpaths)))
       do i=1,size(rawfile)
          call IXFopen_raw(fpaths(i),rawfile(i), status)  
       enddo
       deallocate(fpaths)      
       return
    endif
    if(.not. found) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'no raw data defined,(loadrawfile)')
    endif

  end subroutine loadrawfile

  subroutine closerawfile(rawfile,status)
    implicit none
    type(IXTraw_file)::rawfile(:)
    type(IXTstatus)::status

    call IXFclose_raw_file(rawfile,status)
  end subroutine closerawfile

  subroutine loadheaderinfo_isis(rfile,dso,inputsource,status)
    implicit none
    type(IXTrunfile)::rfile
    type(IXTdata_source)::dso
    type(IXTraw_file),intent(in)::inputsource(:)
    type(IXTstatus)::status
    integer(i4b)::irpb(32),i
    real(dp)::rrpb(32)
    logical::found
!    character(len=long_len)::command_line
    character(len=long_len)::crpb(32)
    character(len=80)::hdr(1)
    !doesn't matter if it's found or not implicitly create
    !    call IXFfindpath_data_source(dso,rfile%program_name,IXCprogname,found,status)
    !    call setcommandline(rfile,command_line,status)
    call IXFget_raw(inputsource(1), 'HDR',hdr , status)
    call IXFget_raw(inputsource(1), 'CRPB',crpb , status)
    rfile%start_time=hdr(1)(53:64)//hdr(1)(65:72)
    rfile%end_time=trim(crpb(17))//' '//trim(crpb(20))
    
    rfile%total_charge=0.0_dp
    rfile%total_raw_frames=0
    rfile%total_good_frames=0
    do i=1,size(inputsource)
      call IXFget_raw(inputsource(i), 'RRPB',rrpb , status)
      call IXFget_raw(inputsource(i), 'IRPB',irpb , status)
      call IXFget_raw(inputsource(i), 'RUN', rfile%run_number, status)
      rfile%total_charge=rfile%total_charge+rrpb(8)
      rfile%total_raw_frames=rfile%total_raw_frames+irpb(11)
      rfile%total_good_frames=rfile%total_good_frames+irpb(10)
    enddo
    
    
  end subroutine loadheaderinfo_isis

  subroutine IXFtubes_to_spectra_runfile(runfile,tubes,spectra,status)
    use IXMmask
    implicit none
    type(IXTrunfile),intent(in)::runfile
    type(IXTmask),intent(in)::tubes
    type(IXTmask),intent(out)::spectra
    type(IXTstatus)::status
    
    call IXFtubes_to_spectra_data(runfile%det_data,tubes,spectra,status)
  
  end subroutine

  subroutine get_d_correction(runfile,d_correction,status)
    implicit none
    type(IXTrunfile),intent(in)::runfile
    real(dp),intent(out)::d_correction(2)
    type(IXTstatus)::status
    type(IXTworkspace),pointer::workspace
    type(IXTdetector),pointer::det
    type(IXTmoments),pointer::moments
    real(dp),pointer::mon_delay(:),L2(:),array2(:),xmean(:)
    integer(i4b),pointer::mon_nos(:)
    type(IXTdatum_array),pointer::d_array    
    logical::mon2found
    integer(i4b)::i
    d_correction(1)=0.0_dp
    d_correction(2)=0.0_dp
    call IXFget_ptr_data(runfile%mon_data,workspace=workspace)
    call IXFget_ptr_workspace(workspace,eff_det=det)
    call IXFget_ptr_detector(det,L2=L2,delay_time=mon_delay)
    det=>NULL()
    workspace=>NULL()

    call IXFget_ptr_peaks(runfile%peaks,moments=moments,monitor_no=mon_nos)
    call IXFget_ptr_moments(moments,xmean=d_array)       
    call IXFget_ptr_datum_array(d_array,signal=xmean)
    mon2found=.false.
    do i=1,size(mon_nos)
       ! select c-fwhh of monitor 2
       if(mon_nos(i)==2)then
          d_correction(2)=xmean(i) - mon_delay(i)
          d_correction(1)=L2(i)
          mon2found=.true.
       endif
    end do
    if(.not. mon2found)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'monitor 2 not found in monitor list,(IXFpopulate_runfile)')
       return
    endif
    L2=>NULL()
    xmean=>NULL()
    moments=>NULL()
    mon_nos=>NULL()
    mon_delay=>NULL() 
  end subroutine get_d_correction


  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Plus
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Minus
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Times
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Divide
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Power
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Log
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Log10
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------

#define RUNFILE_OPERATION   Exp
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Sin
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Cos
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Tan
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Sinh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Cosh
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   Tanh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   deriv1x
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define RUNFILE_OPERATION   deriv2x
#include "unary_ops.f90"

  subroutine finish_op_runfile(rfile_res,rfile_base,status)
    implicit none
    type(IXTrunfile),intent(inout)::rfile_res
    type(IXTrunfile),intent(in)::rfile_base
    type(IXTstatus),intent(inout)::status

    rfile_res%title=rfile_base%title
    ! the following values in a binary runfile operation do not make sense
    ! but we follow the rule of inherit information from the left hand side
    rfile_res%start_time=rfile_base%start_time
    rfile_res%end_time=rfile_base%end_time
    rfile_res%run_number=rfile_base%run_number
    rfile_res%total_charge=rfile_base%total_charge
    rfile_res%total_raw_frames=rfile_base%total_raw_frames
    rfile_res%total_good_frames=rfile_base%total_good_frames
    !specialised set subrouitine called for IXThistory objeect
    call IXFset_history(rfile_res%program_name,status,ref=rfile_base%program_name)
    call IXFset_history(rfile_res%command_line,status,ref=rfile_base%command_line)    
    !validity of original rfile_base object is checked for in the copy command
    !    if(allocated(rfile_base%users))call IXFset_runfile(rfile_res,users=rfile_base%users,status)
    !    if(allocated(rfile_base%users).and. IXFvalid(rfile_base%users))call IXFcopy(rfile_base%users,rfile_res%users,status)
    !    call IXFcopy(rfile_base%users,rfile_res%users,status)
    call IXFrealloc(rfile_res%users,size(rfile_base%users),.false.,status) ! this calls the IXFalloc_users to allocate an array of structures
    call IXFcopy(rfile_base%users,rfile_res%users,status)
    call IXFcopy(rfile_base%sample,rfile_res%sample,status)
    call IXFcopy(rfile_base%inst,rfile_res%inst,status)
    call IXFcopy(rfile_base%peaks,rfile_res%peaks,status)    
    ! rest of object is ok so make valid
    call IXFmark_valid(rfile_res)

  end subroutine finish_op_runfile
  !  subroutine setcommandline(rfile,command_line,status)
  !    implicit none
  !    type(IXTrunfile)::rfile
  !    type(IXTstatus)::status
  !    character(len=*)::command_line
  !    integer(i4b)::length
  !    
  !    if(.not. allocated(rfile%command_line))rfile%entries=0
  !    if(rfile%entries == 0)then            
  !      call IXFreallocFortran(rfile%command_line, IXCcomline_initlength ,.false., status)
  !      rfile%command_line=' '
  !    endif
  !    if ( rfile%entries == size(rfile%command_line) )then
  !    ! the array is full so add some more space to the array list
  !      call IXFreallocFortran(rfile%command_line, rfile%entries+IXCcomline_initlength ,.true.,status)
  !    endif
  !    rfile%entries=rfile%entries+1
  !    rfile%command_line(rfile%entries)=command_line

  !  end subroutine setcommandline


end module IXMrunfile

