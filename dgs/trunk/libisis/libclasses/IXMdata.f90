!------------------------------
! MODULE: IXMdata
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1417 $ ($Date: 2008-07-04 13:04:09 -0400 (Fri, 04 Jul 2008) $)
!!
!! FORTRAN definition of IXMdata class
module IXMdata
  use IXMdataset_2d
  use IXMworkspace
  use IXMbridge
  use IXMraw_file
  use IXMdetector
  use IXMspectra
  use IXMdata_source
  use IXMoptions
  implicit none
  public :: IXTdata
  type IXTdata
     private
     type(IXTbase):: base
     ! this will be an allocatable array of length 1, or a full array
     ! of many structures
     type(IXTdataset_2d),allocatable::datasets(:)
     type(IXTworkspace)::workspace
     type(IXTbridge)::bridge
  end type IXTdata

  private ::  ts_plus_data, tt_plus_data, st_plus_data 
  private ::  ts_minus_data, tt_minus_data, st_minus_data
  private ::  ts_times_data, tt_times_data, st_times_data
  private ::  ts_divide_data, tt_divide_data, st_divide_data
  private ::  ts_power_data, tt_power_data, st_power_data
  private :: popunitsrebin_datasets,popunitsint_datasets,sum_data,units_only_data_chunks
  interface IXFplus_data
     module procedure ts_plus_data, tt_plus_data, st_plus_data
  end interface
  interface IXFplus
     module procedure ts_plus_data, tt_plus_data, st_plus_data
  end interface

  interface IXFminus_data
     module procedure ts_minus_data, tt_minus_data, st_minus_data
  end interface
  interface IXFminus
     module procedure ts_minus_data, tt_minus_data, st_minus_data
  end interface

  interface IXFtimes_data
     module procedure ts_times_data, tt_times_data, st_times_data
  end interface
  interface IXFtimes
     module procedure ts_times_data, tt_times_data, st_times_data
  end interface

  interface IXFdivide_data
     module procedure ts_divide_data, tt_divide_data, st_divide_data
  end interface
  interface IXFdivide
     module procedure ts_divide_data, tt_divide_data, st_divide_data
  end interface

  interface IXFpower_data
     module procedure ts_power_data, tt_power_data, st_power_data
  end interface
  interface IXFpower
     module procedure ts_power_data, tt_power_data, st_power_data
  end interface
  
  interface IXFsin
     module procedure IXFsin_data
  end interface

  interface IXFcos
     module procedure IXFcos_data
  end interface

  interface IXFtan
     module procedure IXFtan_data
  end interface

  interface IXFsinh
     module procedure IXFsinh_data
  end interface

  interface IXFcosh
     module procedure IXFcosh_data
  end interface

  interface IXFtanh
     module procedure IXFtanh_data
  end interface

  interface IXFlog
     module procedure IXFlog_data
  end interface

  interface IXFlog10
     module procedure IXFlog10_data
  end interface


  interface IXFexp
     module procedure IXFexp_data
  end interface

  interface IXFderiv1x
     module procedure IXFderiv1x_data
  end interface

  interface IXFderiv2x
     module procedure IXFderiv2x_data
  end interface


#define IXD_TYPE data
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTdata class"
#define IXD_TYPE data
#define IXD_SQTYPE 'data'
#include "class_base.f90"
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.
  subroutine IXFget_ptr_data(data,datasets,workspace,bridge)
    implicit none
    type(IXTdata),target::data
    type(IXTdataset_2d),optional,pointer::datasets(:)
    type(IXTworkspace),optional,pointer::workspace
    type(IXTbridge),optional,pointer::bridge
    if(present(datasets))datasets=>data%datasets
    if(present(workspace))workspace=>data%workspace
    if(present(bridge))bridge=>data%bridge
  end subroutine IXFget_ptr_data
  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.
  subroutine IXFget_data(data,status,datasets,workspace,bridge,wout)
    implicit none
    type(IXTdata),intent(in)::data
    type(IXTdata),intent(out),optional::wout
    type(IXTdataset_2d),optional::datasets(:)
    type(IXTworkspace),optional,intent(out)::workspace
    type(IXTbridge),optional,intent(out)::bridge
    type(IXTstatus)::status

    if(present(wout))call IXFcopy(data,wout,status)
    if(present(datasets))call IXFcopy(data%datasets,datasets,status)
    if(present(workspace))call IXFcopy(data%workspace,workspace,status)
    if(present(bridge))call IXFcopy(data%bridge,bridge,status)
  end subroutine IXFget_data
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc can be called with all the same arguments as IXFget, but the allocatable array elements can be allocatable arrays.
  !! the arrays are allocated to the appropriate length and IXF get is called underneath to fill them up.

  subroutine IXFget_alloc_data(data,status,datasets,workspace,bridge,wout)
    !the datasets argument is to be intent(out), but implicit deallocation of the objects causes problems
    implicit none
    type(IXTdata),intent(in)::data
    type(IXTdata),intent(out),optional::wout
    !allocatable arrays cannot be defined as intent(out), this tends to make runtime errors for some reason
    type(IXTdataset_2d),optional,allocatable::datasets(:)
    type(IXTworkspace),optional,intent(out)::workspace
    type(IXTbridge),optional,intent(out)::bridge
    type(IXTstatus)::status

    if(present(datasets))then
       call IXFrealloc(datasets,size(data%datasets),.false.,status)
    endif
    call IXFget_data(data,status,datasets,workspace,bridge,wout)
  end subroutine IXFget_alloc_data


  recursive subroutine IXFoperation_run_data(op, field, arg, status)
    implicit none
    type(IXTdata) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdata', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_alloc(op,'datasets',arg%datasets,status)
    call IXFoperation_run(op,'workspace',arg%workspace,status)
    call IXFoperation_run(op,'bridge',arg%bridge,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_data

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_data(data,status,datasets,workspace,bridge,ref)
    implicit none
    type(IXTdata)::data
    type(IXTdata),optional,intent(in)::ref
    type(IXTdataset_2d),optional,intent(in)::datasets(:)
    type(IXTworkspace),optional,intent(in)::workspace
    type(IXTbridge),optional,intent(in)::bridge

    type(IXTstatus)::status
    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_data)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(data)
    else    
       if(IXFvalid(data) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_data)')
       endif
       if(status == IXCseverity_error)return
    endif
    if(present(ref))then
       call IXFset_data(data,status,ref%datasets, ref%workspace,ref%bridge)
    endif
    if(present(datasets))then   
       call IXFrealloc(data%datasets,size(datasets),.false.,status) ! this calls the IXFalloc_dataset_2d to allocate an array of structures
       call IXFcopy(datasets,data%datasets,status)  ! this calls IXFcopyarray_datasets to copy the array of structures, interfaced to IXFcopy
    endif

    if(present(workspace))call IXFcopy(workspace,data%workspace,status)
    if(present(bridge))call IXFcopy(bridge,data%bridge,status)

    call IXFcheck_data(data,status)

  end subroutine IXFset_data

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_data(data,status)
    implicit none
    type(IXTdata)::data
    type(IXTstatus)::status

    call IXFdestroy(data%base,status)
    if(IXFvalid(data%datasets))then
       if(allocated(data%datasets))call IXFdealloc(data%datasets,status)
    endif
    if(IXFvalid(data%workspace)) call IXFdestroy(data%workspace,status)
    if(IXFvalid(data%bridge)) call IXFdestroy(data%bridge,status)
    call IXFclear_valid(data)

  end subroutine IXFdestroy_data

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised.

  subroutine IXFcreate_data(data,datasets,workspace,bridge,status)
    implicit none
    type(IXTdata)::data
    type(IXTstatus)::status
    type(IXTdataset_2d),intent(in)::datasets(:)
    type(IXTworkspace),intent(in)::workspace
    type(IXTbridge),intent(in)::bridge

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(datasets) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTdataset_2d failure, all nested objects MUST be initialised (IXFcreate_data)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(workspace) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTworkspace failure, all nested objects MUST be initialised (IXFcreate_data)')
    endif
    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(bridge) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTbridge failure, all nested objects MUST be initialised (IXFcreate_data)')
    endif
    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(data)
    call IXFset_data(data,status,datasets,workspace,bridge)    


  end subroutine IXFcreate_data

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_data(data,status) 
    implicit none
    type(IXTdata)::data
    type(IXTstatus)::status

    call IXFcheck_base(data%base,status)
    if(allocated(data%datasets) .and. IXFvalid(data%datasets))call IXFcheck(data%datasets,status)!this calls a check on the array of structures
    call IXFcheck(data%workspace,status)
    call IXFcheck(data%bridge,status)    

  end subroutine IXFcheck_data

  !-----------------------------------------------------------------------------------------------------------------------
  !! calaculates a flat background between a given minimum and a maximum on the x axis and subtracts it from hte data as appropriate
  subroutine IXFbackground_data(data,min,max,status,ichunk)
    implicit none
    type(IXTdata)::data
    type(IXTdataset_1d)::bgrd,bgrd2
    type(IXTdataset_2d)::resdata(1)
    type(IXTstatus)::status
    integer(i4b),optional::ichunk
    real(dp),intent(in)::min,max

    if (present(ichunk))then
       call IXFintegrate_x_dataset_2d(bgrd,data%datasets(ichunk),min,max,status)
    else
       call IXFintegrate_x_dataset_2d(bgrd,data%datasets(1),min,max,status)
    endif
    call IXFdivide(bgrd2,bgrd,(max-min),status)
    call IXFdestroy_dataset_1d(bgrd,status)
    ! this calls a routine which propagates the errors in the calculation 
    ! of the background in the 'bgrd2' IXTdataset_1d object
    if (present(ichunk))then
       call IXFdataset_1d_Y_Minus_dataset_2d(resdata(1),data%datasets(ichunk),bgrd2,status)
       call IXFcopy(resdata(1),data%datasets(ichunk),status)
    else
       ! there is only one dataset
       call IXFdataset_1d_Y_Minus_dataset_2d(resdata(1),data%datasets(1),bgrd2,status)
       call IXFcopy(resdata(1),data%datasets(1),status)
    endif
    !    call IXFset_dataset_2d(data,status,datasets=resdata)
    call IXFdestroy(bgrd2,status)
    call IXFdestroy(resdata(1),status)

  end subroutine IXFbackground_data


  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFunits_data routine will change the units of the effective detector of a properly filled IXTdata object.
  subroutine IXFunits_data(data,status,emode,efixed,L1,axis_out,d_correction)
    implicit none    
    type(IXTdata)::data
    type(IXTdataset_2d),allocatable::arrayd2d(:)
    type(IXTstatus)::status
    type(IXTaxis),intent(in)::axis_out
    real(dp),intent(in)::L1,efixed
    integer(i4b),intent(in)::emode
    real(dp),optional,intent(in)::d_correction(2)
    integer(i4b)::ny
    real(dp),allocatable::L2(:),theta(:),delay(:)
    integer(i4b),pointer::good_ptr(:)
    type(IXTdetector),pointer::ed_ptr
    type(IXTeffdet_index),pointer::ed_index    
    real(dp)::L1_local
    !determine if it is d2d(1)(n) or d2d(n)
    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFunits_data)')
    endif

    if(status == IXCseverity_error)return
    !determine L2,delay and theta from effective workspace/effective detector
    ! this assumes that a standard data object contains workspace data
    call IXFget_ptr_workspace(data%workspace,eff_det=ed_ptr,effdet_index=ed_index)

    ! this provides the list of indices in the effective detector corresponding
    ! to each workspace's good values for L2,theta,delay etc.
    call IXFget_ptr_effdet_index(ed_index,good_index=good_ptr)
    ! we then use this list of indices to fill the temporary L2,delay,theta arrays
    ! used in the units conversion, in which each index is the appropriate value for each workspace 
    call IXFalloc_section_detector(ed_ptr,status,good_ptr,L2=L2,theta=theta,delay_time=delay)
    good_ptr=>NULL()
    ed_ptr=>NULL()

!!! set M2 to be origin in space and time
       if (present(d_correction))then
          L1_local=abs(d_correction(1))
          delay=delay+d_correction(2)
       else
          L1_local=L1
       endif
!!!

    if (ny == 1)then
       !it is a single dataset_2d containing a 2d array, output is created in arrayd2d(:) allocatble array 
       call IXFunits(data%datasets(1),arrayd2d,status,emode,efixed,L1_local,L2,theta,delay,axis_out)
       if(status == IXCseverity_error)return
       call IXFdealloc_dataset_2d(data%datasets,status)
       ! the data is now in an array of dataset_2d objects
       ! it started as a single dataset_2d
       call IXFset_data(data,status,datasets=arrayd2d)
    else
       !it is an array of dataset_2d objects
       call IXFunits(data%datasets,status,emode,efixed,L1_local,L2,theta,delay,axis_out)
       if(status == IXCseverity_error)return
    endif
    deallocate(L2,theta,delay)
    ! check has been called already in the units change subroutines

  end subroutine IXFunits_data
  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFunits_rebin_data routine will change the units of the effective detector of a properly filled IXTdata object,
  !! and then rebin the data given an appropriate binning descriptor
  subroutine IXFunits_rebin_data(data,status,emode,efixed,L1,axis_out,Xdesc,Xref,d_correction)
    implicit none    
    type(IXTdata)::data
    type(IXTdataset_2d),allocatable::arrayd2d(:),rbd2d(:)
    type(IXTstatus)::status
    type(IXTaxis),intent(in)::axis_out
    real(dp),optional,intent(in)::Xdesc(:)
    type(IXTdataset_2d),optional::Xref
    real(dp),optional::d_correction(2)    

    real(dp),intent(in)::L1,efixed
    integer(i4b),intent(in)::emode
    integer(i4b)::ny,i
    real(dp),allocatable::L2(:),theta(:),delay(:)
    integer(i4b),pointer::good_ptr(:)
    type(IXTdetector),pointer::ed_ptr
    type(IXTeffdet_index),pointer::ed_index    

    !determine if it is d2d(1)(n) or d2d(n)
    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFunits_rebin_data)')
    endif

    if(present(Xref) .and. present(Xdesc) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'a reference and a descriptor variable cannot be passed together (IXFunits_rebin_data)') 
    endif

    if(status == IXCseverity_error)return
    !determine L2,delay and theta from effective workspace/effective detector
    ! this assumes that a standard data object contains workspace data
    call IXFget_ptr_workspace(data%workspace,eff_det=ed_ptr,effdet_index=ed_index)

    ! this provides the list of indices in the effective detector corresponding
    ! to each workspace's good values for L2,theta,delay etc.
    call IXFget_ptr_effdet_index(ed_index,good_index=good_ptr)
    ! we then use this list of indices to fill the temporary L2,delay,theta arrays from the effective detector
    ! used in the units conversion, in which each index is the appropriate value for each workspace 
    call IXFalloc_section_detector(ed_ptr,status,good_ptr,L2=L2,theta=theta,delay_time=delay)
    !clean up pointers used
    good_ptr=>NULL()
    ed_index=>NULL()
    ed_ptr=>NULL()

    ! first we treat  a single dataset_2d containing a 2d array, output is created in arrayd2d(:) allocatable array 
    ! and then rebinned according to either Xdesc or Xref, the rebin process is optimized over the rest of the array 
    if (ny == 1)then
       call IXFunits(data%datasets(1),arrayd2d,status,emode,efixed,L1,L2,theta,delay,axis_out)
       ! units command creates a result of arrayd2d, since rebinning -> output will still be of length (1),
       ! we just destroy the data inside data%datasets(1)
       if(status == IXCseverity_error)return
       call IXFdestroy(data%datasets(1),status)
       ! rebin the data into rb2d2(:)
       call IXFalloc_dataset_2d(rbd2d,size(arrayd2d),status)
       if(present(Xdesc))call IXFrebin_x_dataset_2d(rbd2d(1),status,arrayd2d(1),Xdesc=Xdesc)
       if(present(Xref))call IXFrebin_x_dataset_2d(rbd2d(1),status,arrayd2d(1),Xref=Xref)
       call IXFdestroy(arrayd2d(1),status)
       do i=2,size(arrayd2d)
          call IXFrebin_x_dataset_2d(rbd2d(i),status,arrayd2d(i),Xref=rbd2d(1))
          call IXFdestroy(arrayd2d(i),status)
       enddo
       call IXFcontract_arrayd2d_dataset_2d(rbd2d,data%datasets(1),status)
       call IXFdealloc_dataset_2d(rbd2d,status)       
    else
       ! if not above then it is an array of (ny) dataset_2d objects. the units change can be done in situ
       ! but the rebinning requires an extra array of dataset_2d objects-> rbd2d(:)
       call IXFunits(data%datasets,status,emode,efixed,L1,L2,theta,delay,axis_out)
       if(status == IXCseverity_error)return
       call IXFalloc_dataset_2d(rbd2d,ny,status)
       !only one of the follwing should be called because of consistency check above
       if(present(Xdesc))call IXFrebin_x_dataset_2d(rbd2d(1),status,data%datasets(1),Xdesc=Xdesc)
       if(present(Xref))call IXFrebin_x_dataset_2d(rbd2d(1),status,data%datasets(1),Xref=Xref)
       call IXFdestroy(data%datasets(1),status)
       do i=2,ny
          call IXFrebin_x_dataset_2d(rbd2d(i),status,data%datasets(i),Xref=rbd2d(1))
          call IXFdestroy(data%datasets(i),status)
       enddo
       ! in this case we need to deallocate the data%datasets since we are contracting 
       ! the dataset down to length 1
       call IXFdealloc_dataset_2d(data%datasets,status)
       call IXFalloc_dataset_2d(data%datasets,1,status)       
       call IXFcontract_arrayd2d_dataset_2d(rbd2d,data%datasets(1),status)
       call IXFdealloc_dataset_2d(rbd2d,status)      
    endif

    deallocate(L2,theta,delay)
    ! IXFcontract and rebin functions both perform a IXFcheck operation

  end subroutine IXFunits_rebin_data
  !! rebins the datasets in an IXTdata object according to a given Xdesc array. the length of data%datasets(:) will remain constant. 
  !! ie if it starts as N*d2d(1) or d2d(:,N) it will stay that way
  subroutine IXFrebin_data(data,status,Xdesc,Xref)
    implicit none
    type(IXTdata)::data
    type(IXTdataset_2d),allocatable::rbd2d(:)
    type(IXTdataset_2d)::temp
    type(IXTstatus)::status
    real(dp),intent(in),optional::Xdesc(:)
    type(IXTdataset_2d),intent(in),optional::Xref

    integer(i4b):: ny,i

    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFrebin_data)')
    endif

    if(status == IXCseverity_error)return

    if(ny==1)then
       ! single 2d array
       !       call IXFinitialise(temp,status)
       !       call IXFmark_validkeep(temp)
       call IXFrebin_x_dataset_2d(temp,status,data%datasets(1),Xdesc,Xref)
       ! set temp to be data%datasets(1)- no need to destroy since it will stay 
       ! as an allocatable array of length 1
       call IXFdestroy(data%datasets(1),status)
       call IXFcopy(temp,data%datasets(1),status)
       call IXFdestroy(temp,status)
    else
       ! array of dataset-2d
       call IXFalloc_dataset_2d(rbd2d,ny,status)
       call IXFrebin_x_dataset_2d(rbd2d(1),status,data%datasets(1),Xdesc,Xref)  
       call IXFdestroy(data%datasets(1),status)  
       do i=2,ny
          call IXFrebin_x_dataset_2d(rbd2d(i),status,data%datasets(i),Xref=rbd2d(1))
          call IXFdestroy(data%datasets(i),status)
       enddo
       ! since the x arrays are now all the same we contract the data back down to a unit length dataset_2d array
       call IXFdealloc_dataset_2d(data%datasets,status)
       call IXFalloc_dataset_2d(data%datasets,1,status)
       call IXFcontract_arrayd2d_dataset_2d(rbd2d,data%datasets(1),status)
       !       call IXFset_data(data,status,datasets=rbd2d)
       call IXFdealloc_dataset_2d(rbd2d,status)
    endif
  end subroutine IXFrebin_data
  
  subroutine IXFregroup_data(data,Xdesc,status)
    implicit none
    type(IXTdata)::data
    type(IXTdataset_2d),allocatable::rgd2d(:)
    type(IXTdataset_2d)::temp
    type(IXTstatus)::status
    real(dp),intent(in)::Xdesc(:)    
    integer(i4b):: ny,i

    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFregroup_data)')
       return
    endif

    if(ny==1)then
       ! single 2d array
       !       call IXFinitialise(temp,status)
       !       call IXFmark_validkeep(temp)
       call IXFregroup_x_dataset_2d(temp,data%datasets(1),Xdesc,status)
       ! set temp to be data%datasets(1)- no need to destroy since it will stay 
       ! as an allocatable array of length 1
       call IXFdestroy(data%datasets(1),status)
       call IXFcopy(temp,data%datasets(1),status)
       call IXFdestroy(temp,status)
    else
       ! array of dataset-2d
       call IXFalloc_dataset_2d(rgd2d,ny,status)
       call IXFregroup_x_dataset_2d(rgd2d(1),data%datasets(1),Xdesc,status)  
       call IXFdestroy(data%datasets(1),status)  
       do i=2,ny
          call IXFregroup_x_dataset_2d(rgd2d(i),data%datasets(i),Xdesc,status)
          call IXFdestroy(data%datasets(i),status)
       enddo
       ! since the x arrays are now all the same we contract the data back down to a unit length dataset_2d array
       call IXFdealloc_dataset_2d(data%datasets,status)
       call IXFalloc_dataset_2d(data%datasets,1,status)
       call IXFcontract_arrayd2d_dataset_2d(rgd2d,data%datasets(1),status)
       !       call IXFset_data(data,status,datasets=rbd2d)
       call IXFdealloc_dataset_2d(rgd2d,status)
    endif
  end subroutine IXFregroup_data
  
  subroutine IXFshift_x_data(data,shift,status)
    implicit none
    type(IXTdata)::data
    type(IXTdataset_2d)::shifted
    type(IXTstatus)::status
    real(dp),intent(in)::shift   
    integer(i4b):: ny,i

    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFshift_x_data)')
       return
    endif

    if(ny==1)then
       ! single 2d array
       call IXFshift_dataset_2d(shifted,data%datasets(1),status,shift_x=shift)
       ! set temp to be data%datasets(1)- no need to destroy since it will stay 
       ! as an allocatable array of length 1
       call IXFdestroy(data%datasets(1),status)
       call IXFcopy(shifted,data%datasets(1),status)
       call IXFdestroy(shifted,status)
    else
       ! array of dataset-2d       
       do i=1,ny
         call IXFshift_dataset_2d(shifted,data%datasets(i),status,shift_x=shift)  
         call IXFdestroy(data%datasets(i),status)
         call IXFcopy(shifted,data%datasets(i),status)
       enddo
    endif

  end subroutine IXFshift_x_data  
  
  subroutine IXFrebunch_data(data,Xdesc,status)
    implicit none
    type(IXTdata)::data
    type(IXTdataset_2d),allocatable::rgd2d(:)
    type(IXTdataset_2d)::temp
    type(IXTstatus)::status
    integer(i4b),intent(in)::Xdesc    
    integer(i4b):: ny,i

    if(allocated(data%datasets))then
       ny=size(data%datasets)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'there must be data in datasets element(IXFregroup_data)')
       return
    endif

    if(ny==1)then
       ! single 2d array
       !       call IXFinitialise(temp,status)
       !       call IXFmark_validkeep(temp)
       call IXFrebunch_x_dataset_2d(temp,data%datasets(1),Xdesc,status)
       ! set temp to be data%datasets(1)- no need to destroy since it will stay 
       ! as an allocatable array of length 1
       call IXFdestroy(data%datasets(1),status)
       call IXFcopy(temp,data%datasets(1),status)
       call IXFdestroy(temp,status)
    else
       ! array of dataset-2d
       call IXFalloc_dataset_2d(rgd2d,ny,status)
       call IXFrebunch_x_dataset_2d(rgd2d(1),data%datasets(1),Xdesc,status)  
       call IXFdestroy(data%datasets(1),status)  
       do i=2,ny
          call IXFrebunch_x_dataset_2d(rgd2d(i),data%datasets(i),Xdesc,status)
          call IXFdestroy(data%datasets(i),status)
       enddo
       ! since the x arrays are now all the same we contract the data back down to a unit length dataset_2d array
       call IXFdealloc_dataset_2d(data%datasets,status)
       call IXFalloc_dataset_2d(data%datasets,1,status)
       call IXFcontract_arrayd2d_dataset_2d(rgd2d,data%datasets(1),status)
       !       call IXFset_data(data,status,datasets=rbd2d)
       call IXFdealloc_dataset_2d(rgd2d,status)
    endif
  end subroutine IXFrebunch_data
   
  subroutine IXFintegrate_x_data(data_out,data_in,i_lim,status)
    implicit none
    type(IXTdata)::data_in,data_out
    real(dp),intent(in)::i_lim(2)
    type(IXTstatus)::status
    integer(i4b)::length,i
    
    length=size(data_in%datasets)    
    call IXFrealloc(data_out%datasets,length,.false.,status)    
    do i=1,length
      call IXFintegrate_x_dataset_2d(data_out%datasets(i),data_in%datasets(i),i_lim(1),i_lim(2),status)
    end do    

    call IXFmark_valid(data_out)           
      !first argument gives bridge data with none to choose
    call IXFset_data(data_out,status,workspace=data_in%workspace,bridge=data_in%bridge)
        
  end subroutine IXFintegrate_x_data

  subroutine IXFremap_data(detdata,dso,dmap,dmask,det_ptr,spe_ptr,status)
    use IXMmap
    use IXMmask
    implicit none 
    type(IXTdata_source),intent(in)::dso
    type(IXTdata),intent(inout)::detdata 
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    type(IXTstatus),intent(inout)::status    
    type(IXTws_bridge),pointer::old_wsbrg,new_wsbrg
    type(IXTbridge)::newbridge

    type(IXTmap)::dmap
    type(IXTmask)::dmask    
    integer(i4b),allocatable::lookup(:)

    ! check it is a 1:1 mapped runfile
    ! data%datasets *must* have been populated without a mapfile
    call IXFget_ptr_bridge(detdata%bridge,ws_bridge=old_wsbrg)
    !supplied mask object is empty    
    call IXFpopulate_bridge(newbridge,dmap,dmask,status)
    if(status == IXCseverity_error)return

    call IXFget_ptr_bridge(newbridge,ws_bridge=new_wsbrg)

    call IXFcheck_subsid_ws_bridge(old_wsbrg,new_wsbrg,lookup)

    if(allocated(lookup))then
       call IXFdestroy(detdata%workspace,status)
       call IXFpopulate_workspace(detdata%workspace,new_wsbrg,spe_ptr,det_ptr,status)
       call remap_data(detdata,newbridge,lookup,status)    
       call IXFdestroy(detdata%bridge,status)
       call IXFset_data(detdata,status,bridge=newbridge)
       deallocate(lookup)
    else
       !error statement
       call IXFwrite_line('old workspace-spectra mapping is incomensurate with new remapping',status)
    endif

    new_wsbrg=>NULL()
  end subroutine IXFremap_data
  !-----------------------------------------------------------------------------------------------------------------------
  !! sum_data:: private subroutine which remaps a 1:1 filled dataset, according to a newbridge
  subroutine remap_data(data,newbridge,lookup,status)
    implicit none
    type(IXTdata)::data
    type(IXTstatus)::status
    type(IXTws_bridge),pointer::wsbrg_new,wsbrg_old
    type(IXTbridge),intent(in)::newbridge
    type(IXTeffdet_index),pointer::ed_index
    type(IXTaxis)::Yaxis
    integer(i4b),intent(in)::lookup(:)
    integer(i4b),pointer::wk_no(:),good_ind(:),wkspecs(:)
    integer(i4b)::nwk,wk_ind,nx
    integer(i4b),allocatable::list(:)
    real(dp),allocatable::s_old(:,:),e_old(:,:),s_new(:,:),e_new(:,:)

    !  get ws_bridge pointer for workspace numbers
    ! also needed for array list of good spectra later on
    call IXFget_ptr_bridge(newbridge,ws_bridge=wsbrg_new)
    call IXFget_ptr_bridge(data%bridge,ws_bridge=wsbrg_old)

    call IXFget_ptr_ws_bridge(wsbrg_new,work_no=wk_no)
    call IXFget_ptr_ws_bridge(wsbrg_old,spec_no=wkspecs)

    nwk=size(wk_no)
    ! extract new effective detector index
    call IXFget_ptr_workspace(data%workspace,effdet_index=ed_index)
    call IXFget_ptr_effdet_index(ed_index,good_index=good_ind)
    ! extract existing data into allocatable arrays
    call IXFget_alloc_dataset_2d(data%datasets(1),status,signal=s_old,error=e_old)
    nx=size(s_old,1)+1

    allocate(s_new(nx-1,nwk),e_new(nx-1,nwk))
    s_new=0.0
    e_new=0.0

    ! first square the errors as they will be added together in quadrature    
    e_old=e_old**2

    do wk_ind=1,nwk
       if(good_ind(wk_ind) /= 0 )then
          ! find appropriate spectra to group together 
          call IXFgetspecsgood_ws_bridge(wsbrg_new,wk_ind,specs_out=list)
          ! s_old has been populated by spectra in the order defined by the spec_no array in the ws_bridge object
          ! so we need to use the lookup array to determine the appropriate index in the s_old array
          s_new(:,wk_ind)=sum(s_old(:,lookup(list)),2)             
          e_new(:,wk_ind)=sum(e_old(:,lookup(list)),2)
       else
          !if all spectra in the workspace are masked then the workspace is created with IXCundef_dp=NaN
          s_new(:,wk_ind)=IXCundef_dp    
          e_new(:,wk_ind)=IXCundef_dp
       endif
    enddo
    ! now root them since they've been added
    e_new=sqrt(e_new)       

    call IXFcreate_code_axis(Yaxis,IXCworknoC,status)
    call IXFset_dataset_2d(data%datasets(1),status,error=e_new,signal=s_new,y=dble(wk_no),y_axis=Yaxis)
    deallocate(s_old,e_old,s_new,e_new)
  end subroutine remap_data
  !  !-----------------------------------------------------------------------------------------------------------------------  
  subroutine IXFtubes_to_spectra_data(data,tubes,spectra,status)
    use IXMmask
    implicit none
    type(IXTdata),intent(in)::data
    type(IXTmask),intent(in)::tubes
    type(IXTmask),intent(out)::spectra
    type(IXTstatus)::status
    type(IXTws_bridge),pointer::wsbrg_ptr

    call IXFget_ptr_bridge(data%bridge,ws_bridge=wsbrg_ptr)
    
    call IXFtubes_to_spectra_ws_bridge(wsbrg_ptr,tubes,spectra,status)
  end subroutine
  
  !  !-----------------------------------------------------------------------------------------------------------------------
  subroutine IXFgetei_data(mondata,Ei,ei_extras,monitor_no,L1,charge,status)
    implicit none
    type(IXTdata),intent(in)::mondata
    real(dp),intent(in)::L1,charge
    real(dp),intent(inout)::Ei
    real(dp),intent(out)::ei_extras(6)
    integer(i4b),intent(in)::monitor_no(2)
    type(IXTstatus),intent(inout)::status
    type(IXTdetector),pointer::edet
    real(dp),pointer::L2(:)
    integer(i4b)::i_mon

    call IXFget_ptr_workspace(mondata%workspace,eff_det=edet)
    call IXFget_ptr_detector(edet,L2=L2)
    edet=>NULL()

    i_mon=size(L2)

    if ((i_mon < 2) .or. (monitor_no(1) > i_mon) .or. (monitor_no(2) > i_mon) )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam,&
            'getei called on an inappropriate IXTdata object or monitor index values invalid (IXFgetei_data)')      
       Ei=0.0_dp
       L2=>NULL()
       return
    endif

    call IXFgetei_dataset_2d(mondata%datasets(1),Ei,ei_extras,monitor_no,L1+L2(monitor_no(1)),L1+L2(monitor_no(2)),charge,status)
    L2=>NULL()

  end subroutine IXFgetei_data
  !  !-----------------------------------------------------------------------------------------------------------------------
  !will determine the area of a monitor peak given by an index to a spectrum in the mondata dataset
  ! assumes monitor spectrum is in microseconds
  subroutine IXFpeakarea_data(mondata,L1,ei,index,area,status)
    implicit none
    type(IXTdata)::mondata
    real(dp),intent(in)::ei,L1
    real(dp),intent(out)::area
    real(dp)::dummy,Tmin,Tmax
    type(IXTstatus)::status
    type(IXTdetector),pointer::edet
    real(dp),pointer::L2(:)
    integer(i4b),intent(in)::index

    if (size(mondata%datasets) > 1)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_fatal, &
            IXCerr_invparam,'mon_data datasets not properly rebinned,(IXFpeakarea_data)')
       return
    endif

    call IXFget_ptr_workspace(mondata%workspace,eff_det=edet)
    call IXFget_ptr_detector(edet,L2=L2)
    edet=>NULL() 

    Tmin=0.9*sqrt(c_t_to_emev)*(L1+L2(index))/sqrt(ei)  
    Tmax=1.1*sqrt(c_t_to_emev)*(L1+L2(index))/sqrt(ei)
    L2=>NULL()
    call IXFmoments_dataset_2d(mondata%datasets(1),index,Tmin,Tmax,dummy,area,status) 
  end subroutine IXFpeakarea_data
  !-----------------------------------------------------------------------------------------------------------------------

  subroutine IXFoperation_data(data_out,data_in1,data_in2,status)
    implicit none
    type(IXTdata),intent(out)::data_out
    type(IXTdata),intent(in)::data_in1,data_in2
    type(IXTstatus),intent(inout)::status
    integer(i4b)::i,d1_len,d2_len

    d1_len=size(data_in1%datasets)
    d2_len=size(data_in2%datasets)

    if((d1_len == d2_len) .and. (d2_len == 1))then
       ! d2d(1)[n,m] and d2d(1)[n,m]
       ! d2d(1)[n,m] and d2d(1)[n,1] (m times)
       call IXFalloc(data_out%datasets,d2_len,status)
       call IXFplus(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets(1),status)
       if(status == IXCseverity_error)return
       call IXFmark_valid(data_out)
       ! first argument gives bridge data with none to choose
       call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
    endif

    if((d1_len == d2_len) .and. (d2_len > 1))then
       !**** d2d(m)[n,1] and d2d(m)[n,1]
       call IXFalloc(data_out%datasets,d2_len,status)
       do i=1,d2_len
          call IXFplus(data_out%datasets(i),data_in1%datasets(i),data_in2%datasets(i),status)
          if(status == IXCseverity_error)return        
       enddo
       call IXFmark_valid(data_out)           
       !first argument gives bridge data with none to choose
       call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
    endif

    if((d1_len == 1) .and. (d2_len > 1))then    
       !**** d2d(1)(n,m) and d2d(m)[n,1]
       !output will be a single d2d
       call IXFalloc(data_out%datasets,d1_len,status)
       call IXFplus(data_out%datasets(1),data_in1%datasets(1),data_in2%datasets,status)
       if(status == IXCseverity_error)return      
       call IXFmark_valid(data_out)     
       call IXFset_data(data_out,status,workspace=data_in1%workspace,bridge=data_in1%bridge)
    endif

    if((d2_len == 1) .and. (d1_len > 1))then
       !**** d2d(m)[n,1] and d2d(1)(n,m)
       !output will be a single d2d
       call IXFalloc(data_out%datasets,d2_len,status)
       call IXFplus(data_out%datasets(1),data_in1%datasets,data_in2%datasets(1),status)
       if(status == IXCseverity_error)return
       call IXFmark_valid(data_out)     
       call IXFset_data(data_out,status,workspace=data_in2%workspace,bridge=data_in2%bridge)
    endif

  end subroutine IXFoperation_data

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Plus
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Minus
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Times
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Divide
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Power
#include "binary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Log
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Log10
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------

#define DATA_OPERATION   Exp
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Sin
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Cos
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Tan
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Sinh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Cosh
#include "unary_ops.f90"
  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   Tanh
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   deriv1x
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
#define DATA_OPERATION   deriv2x
#include "unary_ops.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcomparebridge_data will drill down through two IXTdata objects and compare their workspace to spectra mapping 
  subroutine IXFcomparebridge_data(data1,data2,ident,status)
    implicit none
    type(IXTdata),intent(in)::data1,data2
    type(IXTstatus)::status
    logical::ident
    ident=.false.
    ident=IXFcompare_bridge(data1%bridge,data2%bridge)

  end subroutine IXFcomparebridge_data
  !-----------------------------------------------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------------------------------------------  
  !! IXFpopulate populates an IXTdata object from an input_source object (RAWFILE) and a mapping file (flname) only
  !! no background subtraction or untis change
  subroutine IXFpopulate_data(data,period,map,mask,status,inputsource,dso,det_ptr,spe_ptr,efixed,emode,L1,&
       iflag,uflag,rflag,rbparams,axis_out,opt,nchunk,bgrd,i_lim,d_correction)
    use IXMmap
    use IXMmask
    implicit none
    logical,intent(in)::iflag,uflag,rflag
    type(IXTdata)::data
    type(IXTraw_file),intent(inout)::inputsource(:)
    type(IXTdata_source),intent(in)::dso
    type(IXTmap),intent(in)::map
    type(IXTmask),intent(in)::mask
    type(IXTdetector),pointer::det_ptr
    type(IXTspectra),pointer::spe_ptr
    integer(i4b),intent(in)::emode,period
    real(dp),intent(in)::efixed,L1
    !optional arguments
    real(dp),optional,intent(in)::rbparams(:),bgrd(2),i_lim(2),d_correction(2)
    type(IXToptions),optional,intent(in)::opt
    type(IXTaxis),optional,intent(in)::axis_out
    integer(i4b),intent(in)::nchunk(2)
    type(IXTstatus),intent(inout)::status
    type(IXTaxis)::initial_axis
    type(IXTdataset_2d)::d2d(1)
    type(IXTdataset_2d),allocatable::arrayd2d(:)
    type(IXTws_bridge),pointer::wsbrg_ptr
    integer(i4b),pointer::specno_ptr(:),workno_ptr(:),specind_ptr(:),tspec_ptr(:)
    integer(i4b)::nwk,ichunk,piece,piece_end,iwork,specind_st,specind_end,st_wk,end_wk,chunks,i,nch_l
    integer(i4b)::timechans,totalbytes,mem_limit,max_spectra,no_of_spectra
    integer(i4b),allocatable::specs(:)    
    logical::units_only,same_units,memory_safe
    character(len=8)::max_spec_char,no_of_spec_char

    units_only=.false.
    same_units=.false.
    memory_safe=.false.
    nch_l=nchunk(1)

    !   not to be chunked
    call IXFpopulate_bridge(data%bridge,map,mask,status)
    if(status == IXCseverity_error)return
    call IXFget_ptr_bridge(data%bridge,ws_bridge=wsbrg_ptr)
    ! spe_ptr/det_ptr are passed down to enable determination of detectors used in each spectrum and workspace
    call IXFpopulate_workspace(data%workspace,wsbrg_ptr,spe_ptr,det_ptr,status)
    if(status == IXCseverity_error)return
    ! chunking only necessary for dataset population
    !need to chunk together workspaces not spectra, otherwise spectra may not be present for sum_data to work properly
    !find total number of workspaces, divide by nchunk

    call IXFget_ptr_ws_bridge(wsbrg_ptr,spec_no=specno_ptr,work_no=workno_ptr,spec_ind=specind_ptr,total_spec=tspec_ptr)
    nwk=size(workno_ptr)
!determine number of time channels to verify total memory to be used does not exceed limits
    call IXFget_raw_file(inputsource(1),status,ntc1=timechans)    


do while((.not. memory_safe) .and. nch_l>0)
!first iteration
  chunks=nwk/nch_l       
  if(chunks*nch_l==nwk)then
    piece=nch_l
    piece_end=nch_l         
  else
    chunks=chunks+1
    piece=nch_l
    piece_end=mod(nwk,nch_l)
  endif   
  do i=1,chunks
    st_wk=((i-1)*piece)+1
    if(i/=chunks)then 
      iwork=piece
    else
      iwork=piece_end
    endif
    end_wk=st_wk+iwork-1
     !totalbytes =#spec * #timechans * 2 (signal+error arrays) * 8b (reals)      
    no_of_spectra = sum(tspec_ptr(st_wk:end_wk))
    totalbytes = no_of_spectra * timechans * 2 * 8       
    mem_limit= nchunk(2) * 1024 * 1024 ! memory limit is user defined  
    if(totalbytes>mem_limit)then
      nch_l=nch_l/2
      exit    
    endif
    ! then it did all the chunks in the memory
    if(i==chunks)memory_safe=.true.
  end do  
  if(nch_l==0)then
    max_spectra=mem_limit/(timechans*2*8)
    write(no_of_spec_char,'(i8)')no_of_spectra
    write(max_spec_char,'(i8)')max_spectra
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_outofmem,&
          'too many spectra:'//trim(adjustl(no_of_spec_char))//', for pre-defined memory limit, maximum possible per chunk: '//trim(adjustl(max_spec_char))//' (IXFpopulate_data)')       
    return      
  endif
end do
  
    !nchunk is set from subroutine in IXMrunfile
    !if nchunk=1 carry on as before
    !else chunk it 
    ! if nchunk is number of workspaces to use in each chunk
    ! piece is default number of workspaces in each chunk
    ! piece_end is number of workspaces in last chunk
! run first iteration for defined value of nchunk

    if( nch_l > 0 )then   
       !to destroy bits of dataset_2d inside array
       if(allocated(data%datasets))then
          call IXFdealloc(data%datasets,status)
       endif
       ! allocate array of d2d's to hold chunked data
       call IXFalloc(data%datasets, chunks ,status) 
    else
    ! nwk < nchunk  and/or nchunk=-1(monitor data setting)
       !don't do any chunking    
       chunks=1
       piece_end=nwk              
       !to destroy bits of dataset_2d inside array
       if(allocated(data%datasets))then
          call IXFdealloc(data%datasets,status)
       endif
       ! allocate array of d2d's to hold chunked data
       call IXFalloc(data%datasets, chunks ,status)               
    endif

    !this is for the SPECIAL case for units only population without rebinning or integration
    if ( (.not. rflag).and. (.not. iflag) .and. uflag)then
       units_only=.true.
       call IXFwrite_line('units change on dataset during population without rebinning or integrating, are you sure? (IXFpopulate_data)',status)
       if (chunks /=1) call IXFalloc(arrayd2d,nwk,status)
    endif

    ! piece is default number of workspaces in each chunk
    ! piece_end is number of workspaces in last chunk
    do ichunk=1,chunks
       if(ichunk == chunks) then !this will catch one chunk only as well as last chunk in a list
          iwork=piece_end
       else
          iwork=piece
       endif
       st_wk=((ichunk-1)*piece)+1
       end_wk=st_wk+iwork-1 !st_wk:end_wk is range of indices of workspaces in the chunk
       call IXFreallocfortran(specs,sum(tspec_ptr(st_wk:end_wk)),.false.,status)
       !determines spectrum numbers from specno_ptr for iwork workspaces
       specind_st=specind_ptr(st_wk)
       specind_end=specind_ptr(end_wk)+tspec_ptr(end_wk)-1
       specs=specno_ptr(specind_st:specind_end)                     
       ! this fills data%datasets(1) with selected spectra
       call IXFget_raw(inputsource, specs, data%datasets(ichunk),period, status)
       !datasets are initialised inside the get_raw process
       ! but IXTdata object needs to be initialised
       if(ichunk==1)then
          !check for same_units -> same_units   
          call IXFget_dataset_2d(data%datasets(1),status,x_axis=initial_axis)
          !if units out not supplied then untis do not change.....
          if(present(axis_out))then
             if(IXFcompare_units(initial_axis,axis_out))same_units=.true.
          else
             same_units=.true.
          endif
          if(same_units .and. units_only .and. allocated(arrayd2d))call IXFdealloc(arrayd2d,status)
       endif

       call IXFmark_valid(data)
       if (IXFpresent(opt,bgrd))call IXFbackground_data(data,bgrd(1),bgrd(2),status,ichunk)
       if(uflag)then
          if(rflag)then
             ! units and rebinning
             call popunitsrebin_datasets(data,spe_ptr,det_ptr,L1,efixed,emode,rbparams,axis_out,ichunk,same_units,status,d_correction)
             if(status == IXCseverity_error)return
             call sum_data(data,(/ (i,i=st_wk,end_wk) /),status,ichunk,chunks,piece) 
          elseif(iflag)then
             !units and integration
             call popunitsint_datasets(data,spe_ptr,det_ptr,L1,efixed,emode,i_lim,axis_out,ichunk,same_units,status,d_correction)
             if(status == IXCseverity_error)return
             call sum_data(data,(/ (i,i=st_wk,end_wk) /),status,ichunk,chunks,piece)
          else
             !units only
             !data must be summed together before you do a units change on whole dataset
             call sum_data(data,(/ (i,i=st_wk,end_wk) /),status,ichunk,chunks,piece)
             if(.not. same_units)then
                if (chunks ==1) then
                   !this is for the SPECIAL case for units only population without rebinning or integration and only one chunk
                   ! no need for concatenation or setting of datasets
                   call IXFunits_data(data,status,emode,efixed,L1,axis_out,d_correction)
                   if(status == IXCseverity_error)return
                else
                   !this is for the SPECIAL case for units only population without rebinning or integration and more than one chunk
                   call units_only_data_chunks(data,ichunk,iwork,piece,emode,efixed,L1,axis_out,arrayd2d,status,d_correction)
                   if(status == IXCseverity_error)return
                endif
             endif
          endif !(if uflag)
       elseif(rflag)then
          !rebin only
          call IXFrebin_x_dataset_2d(d2d(1),status,data%datasets(ichunk),Xdesc=rbparams)
          call IXFcopy(d2d(1),data%datasets(ichunk),status)
          call IXFdestroy(d2d(1),status)
          call sum_data(data,(/(i,i=st_wk,end_wk) /),status,ichunk,chunks,piece)
       elseif(iflag)then
          !integrate only
          !d2d(1) is an argument since IXFset_data is expecting an array of IXTdataset_2d
          call IXFintegrate_x_dataset_2d(d2d(1),data%datasets(ichunk),i_lim(1),i_lim(2),status)
          call IXFcopy(d2d(1),data%datasets(ichunk),status)
          call IXFdestroy(d2d(1),status)
          call sum_data(data,(/ (i,i=st_wk,end_wk) /),status,ichunk,chunks,piece)
       else
          ! sum data only
          call sum_data(data,(/ (i,i=st_wk,end_wk) /),status,ichunk,chunks,piece) 
       endif
    enddo
    call IXFdeallocfortran(specs,status)

    ! concatenate everything back together again...... unless units only population
    if(chunks > 1) then
       if (units_only .and. (.not. same_units))then ! will only catch arrayd2d case         
          call IXFset_data(data,status,datasets=arrayd2d)
       else
          call IXFcontract_arrayd2d_dataset_2d(data%datasets,d2d(1),status)
          if(status == IXCseverity_error)return
          call IXFset_data(data,status,datasets=d2d)
          call IXFdestroy(d2d,status)
       endif
    endif

  end subroutine IXFpopulate_data

  subroutine units_only_data_chunks(data,ichunk,iwork,piece,emode,efixed,L1,axis_out,arrayd2d,status,d_correction)
    implicit none    
    type(IXTdata)::data
    type(IXTdataset_2d)::arrayd2d(:)
    type(IXTdataset_2d),allocatable::arrayd2d_u(:)
    type(IXTstatus)::status
    type(IXTaxis),intent(in)::axis_out
    real(dp),intent(in)::L1,efixed
    integer(i4b),intent(in)::emode,iwork,ichunk,piece
    real(dp),intent(in),optional::d_correction(2)
    real(dp),allocatable::L2(:),theta(:),delay(:)
    integer(i4b),pointer::goodlist_ptr(:)
    type(IXTdetector),pointer::ed_ptr
    type(IXTeffdet_index),pointer::ed_index    
    integer(i4b)::st_ind,end_ind
    real(dp)::L1_local

    !determine L2,delay and theta from effective workspace/effective detector
    ! this assumes that a standard data object contains workspace data
    call IXFget_ptr_workspace(data%workspace,eff_det=ed_ptr,effdet_index=ed_index)

    ! this provides the list of indices in the effective detector corresponding
    ! to each workspace's good values for L2,theta,delay etc.
    call IXFget_ptr_effdet_index(ed_index,good_index=goodlist_ptr)

    ! we then use this list of indices to fill the temporary L2,delay,theta arrays
    ! the list of indices refers directly to workspace
    ! used in the units conversion, in which each index is the appropriate value for each workspace 
    ! this function can be called since the spctra have already been grouped together in sum_data, and the 
    ! effective detector arrays are appropriate
    st_ind=((ichunk-1)*piece)+1
    end_ind=st_ind+iwork-1
    call IXFalloc_section_detector(ed_ptr,status,goodlist_ptr(st_ind:end_ind),L2=L2,theta=theta,delay_time=delay)
    goodlist_ptr=>NULL()
    ed_ptr=>NULL()

!!! set M2 to be origin in space and time
       if (present(d_correction))then
          L1_local=abs(d_correction(1))
          delay=delay+d_correction(2)
       else
          L1_local=L1
       endif
!!!

    !it is a single dataset_2d containing a 2d array, output is created in arrayd2d(:) allocatble array 
    call IXFunits(data%datasets(ichunk),arrayd2d_u,status,emode,efixed,L1_local,L2,theta,delay,axis_out)
    if(status == IXCseverity_error)return
    arrayd2d(st_ind:end_ind)=arrayd2d_u

    deallocate(L2,theta,delay,arrayd2d_u)
    ! check has been called already in the units change subroutines

  end subroutine units_only_data_chunks

  subroutine popunitsint_datasets(data,spe_ptr,det_ptr,L1,efixed,emode,i_lim,axis_out,ichunk,same_units,status,d_correction)
    implicit none
    type(IXTdata),intent(inout)::data
    type(IXTspectra),intent(in)::spe_ptr
    type(IXTdetector),intent(in)::det_ptr
    integer(i4b),intent(in)::emode  
    type(IXTaxis),intent(in)::axis_out
    real(dp),intent(in)::efixed,L1,i_lim(2)
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::ichunk
    logical,intent(in)::same_units
    real(dp),intent(in),optional::d_correction(2)
    type(IXTdataset_2d),allocatable::arrayd2d(:),intd2d(:)
    integer(i4b),allocatable::dets_out(:)
    real(dp),allocatable::L2(:),theta(:),delay(:)
    integer(i4b)::i,specno(1)
    real(dp),pointer::i_ptr(:)
    real(dp)::L1_local


    if(same_units)then !integrate only
       allocate(intd2d(1))
       call IXFintegrate_x_dataset_2d(intd2d(1),data%datasets(ichunk),i_lim(1),i_lim(2),status)
       call IXFdestroy_dataset_2d(data%datasets(ichunk),status)
       call IXFcopy(intd2d(1),data%datasets(ichunk),status)
       call IXFdestroy(intd2d,status)
    else
       call IXFget_ptr_dataset_2d(data%datasets(ichunk),y=i_ptr)    
       allocate(theta(size(i_ptr)),L2(size(i_ptr)),delay(size(i_ptr)))
       ! need to create average detector values for ungrouped spectra   
       do i=1,size(i_ptr)
          specno(1)=int(i_ptr(i))
          call IXFgetdets_spectra(spe_ptr,specno,dets_out,status)    
          theta(i)= IXFavgtheta_detector(det_ptr,dets_out)
          L2(i)= IXFavgL2_detector(det_ptr,dets_out)
          delay(i)=IXFavgdelaytime_detector(det_ptr,dets_out)
       enddo

!!! set M2 to be origin in space and time
       if (present(d_correction))then
          L1_local=abs(d_correction(1))
          delay=delay+d_correction(2)
       else
          L1_local=L1
       endif
!!!       
       call IXFunits(data%datasets(ichunk),arrayd2d,status,emode,efixed,L1_local,L2,theta,delay,axis_out)
       if(status == IXCseverity_error)return
       !destroy old data arrays
       call IXFdestroy_dataset_2d(data%datasets(ichunk),status)
       !integrate datasets destrying each line as you go
       allocate(intd2d(size(arrayd2d)))       
       do i=1,size(arrayd2d)
          call IXFintegrate_x_dataset_2d(intd2d(i),arrayd2d(i),i_lim(1),i_lim(2),status)
          call IXFdestroy_dataset_2d(arrayd2d(i),status)
       enddo
       call IXFcontract_arrayd2d_dataset_2d(intd2d,data%datasets(ichunk),status)
       ! each d2d has been destroyed, deallocate array of structures
       deallocate(arrayd2d)
       call IXFdestroy(intd2d,status)
    endif
  end subroutine popunitsint_datasets
  !-----------------------------------------------------------------------------------------------------------------------
  !!popunitsrebin_datasets is a private routine which changes the units on an array of dataset_2d's, rebins the datasets and
  !! recombines them back into a single dataset_2d
  !! the rebin description is hardcoded
  subroutine popunitsrebin_datasets(data,spe_ptr,det_ptr,L1,efixed,emode,rbparams,axis_out,ichunk,same_units,status,d_correction)
    implicit none
    type(IXTdata)::data
    ! for testing purposes
    type(IXTdataset_2d),allocatable::arrayd2d(:),rbd2d(:)
    type(IXTspectra),intent(in)::spe_ptr
    type(IXTdetector),intent(in)::det_ptr
    real(dp),intent(in)::efixed,L1,rbparams(:)
    real(dp),intent(in),optional::d_correction(2)
    integer(i4b),intent(in)::emode,ichunk
    logical,intent(in)::same_units
    type(IXTaxis),intent(in)::axis_out
    type(IXTstatus),intent(inout)::status
    integer(i4b),allocatable::dets_out(:)
    real(dp),allocatable::x_out(:),s_out(:,:),e_out(:,:)
    real(dp),allocatable::L2(:),theta(:),delay(:)
    integer(i4b)::i,specno(1)
    real(dp),pointer::i_ptr(:)
    real(dp)::L1_local

    if(same_units)then !rebin only
       allocate(rbd2d(1))
       call IXFrebin_x_dataset_2d(rbd2d(1),status,data%datasets(ichunk),Xdesc=rbparams)
       call IXFcopy(rbd2d(1),data%datasets(ichunk),status)
       call IXFdestroy(rbd2d(1),status)
    else
       call IXFget_ptr_dataset_2d(data%datasets(ichunk),y=i_ptr)    
       allocate(theta(size(i_ptr)),L2(size(i_ptr)),delay(size(i_ptr)))
       ! need to create average detector values for ungrouped spectra   
       do i=1,size(i_ptr)
          specno(1)=int(i_ptr(i))
          call IXFgetdets_spectra(spe_ptr,specno,dets_out,status)    
          theta(i)= IXFavgtheta_detector(det_ptr,dets_out)
          L2(i)= IXFavgL2_detector(det_ptr,dets_out)
          delay(i)=IXFavgdelaytime_detector(det_ptr,dets_out)
       enddo
!!! set M2 to be origin in space and time
       if (present(d_correction))then
          L1_local=abs(d_correction(1))
          delay=delay+d_correction(2)
       else
          L1_local=L1
       endif
!!!
       call IXFunits_dataset_2d(data%datasets(ichunk),arrayd2d,status,emode,efixed,L1_local,L2,theta,delay,axis_out)
       !call IXFunits_data(data,status,emode,efixed,L1,units_out)

       if(status == IXCseverity_error)return
       call IXFdestroy(data%datasets(ichunk),status)
       call IXFalloc_dataset_2d(rbd2d,size(arrayd2d),status)
 
       ! first time we determine the x-array for rebinning using arrayd2d(1) and rbd2d(1) 
       call IXFrebin_x_dataset_2d(rbd2d(1),status,arrayd2d(1),Xdesc=rbparams)
       call IXFdestroy(arrayd2d(1),status)

       ! for all others we use rebin with the x-array determined for rbd2d(1) (using the Xref keyword)
       do i=2,size(arrayd2d)
          call IXFrebin_x_dataset_2d(rbd2d(i),status,arrayd2d(i),Xref=rbd2d(1))    
          call IXFdestroy(arrayd2d(i),status)
       enddo
       call IXFcontract_arrayd2d_dataset_2d(rbd2d,data%datasets(ichunk),status)
       call IXFdealloc_dataset_2d(rbd2d,status)
       ! not necessary to call check since this is done in contract array
    endif
  end subroutine popunitsrebin_datasets
  !-----------------------------------------------------------------------------------------------------------------------
  !! sum_data:: private subroutine which sums together contributing spectra for each workspace
  !! it has to get a copy of the total datasets and then uses these to sum together
  subroutine sum_data(data,work_inds,status,ichunk,chunks,piece)
    implicit none
    type(IXTdata)::data
    integer(i4b),intent(in)::work_inds(:) !! index numbers of workspaces to populate
    integer(i4b),optional,intent(in)::ichunk,chunks,piece
    type(IXTstatus)::status
    type(IXTws_bridge),pointer::wsbrg_ptr
    type(IXTeffdet_index),pointer::ed_index
    type(IXTaxis)::Yaxis
    integer(i4b),pointer::good_ind(:),spec_ind(:) ,work_no(:)
    integer(i4b)::nwk,wk_ind,nx
    integer(i4b),allocatable::list(:)
    real(dp),allocatable::s_new(:,:),e_new(:,:)
    real(dp),pointer::s_old(:,:),e_old(:,:)

    !  get ws_bridge pointer for workspace numbers
    ! also needed for array list of good spectra later on
    call IXFget_ptr_bridge(data%bridge,ws_bridge=wsbrg_ptr)
    nwk=size(work_inds)

    ! spec_ind is only required if there are chunks, work_no is always required, chunks or not
    ! so get both and use one or both
    call IXFget_ptr_ws_bridge(wsbrg_ptr,spec_ind=spec_ind ,work_no=work_no)


    ! extract effective detector index
    call IXFget_ptr_workspace(data%workspace,effdet_index=ed_index)
    call IXFget_ptr_effdet_index(ed_index,good_index=good_ind)
    ! reset pointer
    ed_index=>NULL()
    ! extract existing data into pointer arrays

    call IXFget_ptr_dataset_2d(data%datasets(ichunk),signal=s_old,error=e_old)
    nx=size(s_old,1)+1

    allocate(s_new(nx-1,nwk),e_new(nx-1,nwk))
    s_new=0.0
    e_new=0.0

    ! first square the errors as they will be added together in quadrature    
    e_old=e_old**2

    do wk_ind=1,nwk
       if(good_ind(work_inds(wk_ind)) /= 0 )then
          ! find appropriate spectra to group together and their appropriate index in the old signal array
          call IXFgetspecsgood_ws_bridge(wsbrg_ptr,work_inds(wk_ind),ind_out=list)
          ! s_old has been populated by spectra in the order defined by the spec_no array in the ws_bridge object   [index(list)]
          if(chunks > 1)then
             !ichunk = 1 ->chunks chunks > 1, will get called each chunk, does not matter if last piece or not
             if(size(list)/=1)then
                s_new(:,wk_ind)=sum(s_old(:,list-(spec_ind(((ichunk-1)*piece)+1)-1)),2)             
                e_new(:,wk_ind)=sum(e_old(:,list-(spec_ind(((ichunk-1)*piece)+1)-1)),2)
             else
                s_new(:,wk_ind)=s_old(:,(list(1)-(spec_ind(((ichunk-1)*piece)+1)-1)))  
                e_new(:,wk_ind)=e_old(:,(list(1)-(spec_ind(((ichunk-1)*piece)+1)-1)))  
             endif
          else
             ! ichunk =1 and chunks=1, will only get called once for IXTdata object
             if(size(list)/=1)then
                s_new(:,wk_ind)=sum(s_old(:,list),2)             
                e_new(:,wk_ind)=sum(e_old(:,list),2)
             else
                s_new(:,wk_ind)=s_old(:,list(1))             
                e_new(:,wk_ind)=e_old(:,list(1))
             endif
          endif
       else
          !if all spectra in the workspace are masked then the workspace is created with IXCundef_dp=NaN
          s_new(:,wk_ind)=IXCundef_dp    
          e_new(:,wk_ind)=IXCundef_dp
       endif
    enddo
    ! now root them since they've been added
    e_new=sqrt(e_new)       
    ! reset pointer
    good_ind=>NULL()
    s_old=>NULL()
    e_old=>NULL()
    wsbrg_ptr=>NULL()
    spec_ind=>NULL()
    call IXFcreate_code_axis(Yaxis,IXCworknoC,status)
    call IXFset_dataset_2d(data%datasets(ichunk),status,error=e_new,signal=s_new,y=dble(work_no(work_inds)),y_axis=Yaxis)
    deallocate(s_new,e_new)
  end subroutine sum_data
  !-----------------------------------------------------------------------------------------------------------------------

end module IXMdata
