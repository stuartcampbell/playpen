!------------------------------
! MODULE: IXM
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1418 $ ($Date: 2008-07-08 08:20:55 -0400 (Tue, 08 Jul 2008) $)
!!
!! FORTRAN definition of IXMdetector
! detector module 
module IXMdetector
  use IXMtype_definitions
  use IXMbase
  use IXMgeometry
  use IXMdet_he3
  use IXMdet_solid
  implicit none
  integer(i4b), parameter, public ::  IXCdet_null=0,IXCdet_solid=1,IXCdet_he3=2,IXCcomposite=3
  integer(i4b),parameter,public :: IXCmin_2theta=0,IXCmax_2theta=1,IXCweighted=2
  ! default is all variables and functions are hidden
  !	private
  ! public types
  public :: IXTdetector
  ! public interfaces
  !   public :: 
  type IXTdetector
     private ! The type is public, but the contents not
     type(IXTbase):: base
     integer(i4b), pointer  :: checksum(:)=>NULL()	!! to compare different detector object values
     integer(i4b), pointer  :: det_no(:)=>NULL()                  !! actual detector number
     !! this element is not being used at the moment
     integer(i4b)  :: ref_count=1                  !! reference count from spectra and multiple runfile
     !! to be incremented when the detector structure is pointed at
     type(IXTgeometry):: geometry         
     real(dp), pointer  :: delay_time(:)=>NULL()              !! electronic delay time (microseconds)
     real(dp), pointer  :: dead_time(:)=>NULL()          !! detector dead time (microseconds)
     real(dp), pointer  :: theta(:)=>NULL()
     real(dp), pointer  :: L2(:)=>NULL()
     real(dp), pointer  :: phi(:)=>NULL()
     integer(i4b), pointer  :: group_index(:)=>NULL()
     integer(i4b), pointer  :: det_type(:)=>NULL()					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
     type(IXTdet_he3)  :: det_he3
     type(IXTdet_solid) :: det_solid
     integer(i4b), pointer  :: type_index(:)=>NULL()
     real(dp),pointer::omega(:)=>NULL()     
  end type IXTdetector

  ! ngroup_max = max. no. groups of detectors for any one bank
  ! ncalib_par_max = max. number of calibration parameters per entry
  ! ncalib_entry_max  = max. number of calibration entries in any one bank
  ! ndet_totmax   = max. no. detectors in whole DETECTOR.DAT file

  integer ngroup_max, ncalib_par_max, ncalib_entry_max
  parameter (ngroup_max=200, ncalib_par_max=10, ncalib_entry_max=10000)

  integer ndet_totmax
  parameter (ndet_totmax=1000000)

  integer nspec_totmax
  parameter (nspec_totmax=ndet_totmax)

  integer nwork_totmax
  parameter (nwork_totmax=ndet_totmax)

  !! global detector objects
  !XXX  type(IXTdetector),private,save,target::inst_det !! instrument detector object
  !XXX  type(IXTdetector),private,save,target::eff_det !! effective detector object

  private:: IXFdecref_detector,IXFincref_detector, single_detector_average

#define IXD_TYPE detector
#include "class_header.f90"
  interface IXFpopulate_reference_detector
     module procedure IXFpopulate_reference_detector_list, IXFpopulate_reference_detector_spectra
  end interface
contains

#define IXD_DESCRIPTION	"IXTdetector class"
#define IXD_TYPE detector
#define IXD_SQTYPE 'detector'
#include "class_base.f90"


  recursive subroutine IXFoperation_run_detector(op, field, arg, status)
    implicit none
    type(IXTdetector) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdetector', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_ptr(op,'checksum', arg%checksum, status)
    call IXFoperation_run_ptr(op,'det_no',arg%det_no,status)    
    call IXFoperation_run(op,'ref_count', arg%ref_count, status)
    call IXFoperation_run(op,'geometry',arg%geometry,status)
    call IXFoperation_run_ptr(op,'delay_time',arg%delay_time,status)
    call IXFoperation_run_ptr(op,'dead_time',arg%dead_time,status)
    call IXFoperation_run_ptr(op,'theta',arg%theta,status)
    call IXFoperation_run_ptr(op,'L2',arg%L2,status)
    call IXFoperation_run_ptr(op,'phi',arg%phi,status)
    call IXFoperation_run_ptr(op,'group_index',arg%group_index,status)
    call IXFoperation_run_ptr(op,'det_type',arg%det_type,status)
    call IXFoperation_run(op,'det_he3',arg%det_he3,status)
    call IXFoperation_run(op,'det_solid',arg%det_solid,status)
    call IXFoperation_run_ptr(op,'type_index',arg%type_index,status)
    call IXFoperation_run_ptr(op,'omega',arg%omega,status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_detector(arg,status)
    implicit none
    type(IXTdetector)::arg
    type(IXTstatus)::status
    call IXFcheck_base(arg%base,status)
    if(IXFvalid(arg%det_solid))call IXFcheck(arg%det_solid,status)
    if(IXFvalid(arg%det_he3))call IXFcheck(arg%det_he3,status)
    if(IXFvalid(arg%geometry))call IXFcheck(arg%geometry,status)
    ! write(6,*)size(arg%checksum),size(arg%det_no),size(arg%delay_time),size(arg%dead_time),size(arg%theta)
    ! write(6,*)size(arg%L2),size(arg%phi),size(arg%group_index),size(arg%det_type),size(arg%type_index)

  end subroutine IXFcheck_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_detector(detector,status)
    implicit none
    type(IXTdetector)::detector
    type(IXTstatus)::status
    ! if the detector exists to be used then ref_count=1
    ! if it is pointed at by 1 then ref_count=2
    ! if it is pointed at by 2 then ref_count=3  etc..
    ! it can only be destroyed when it is not pointed at by any other objects
    ! first decrement the reference counter
    !    detector%ref_count=detector%ref_count-1
    ! then check its value to see if it is pointed at by something else

    !    if (detector%ref_count .eq. 1) then ! not pointed at by any other structures 
    call IXFdestroy(detector%base,status)
    call IXFdealloc(detector%checksum,status)
    call IXFdealloc(detector%det_no,status)
    if(IXFvalid(detector%geometry))call IXFdestroy(detector%geometry,status)
    call IXFdealloc(detector%delay_time,status)
    call IXFdealloc(detector%dead_time,status)
    call IXFdealloc(detector%theta,status)
    call IXFdealloc(detector%L2,status)
    call IXFdealloc(detector%phi,status)
    call IXFdealloc(detector%group_index,status)
    call IXFdealloc(detector%det_type,status)
    if(IXFvalid(detector%det_he3))call IXFdestroy(detector%det_he3,status)
    if(IXFvalid(detector%det_solid))call IXFdestroy(detector%det_solid,status)
    call IXFdealloc(detector%type_index,status)
    call IXFdealloc(detector%omega,status)
    ! and now clear initialize flag
    call IXFclear_valid(detector)
    !    endif

  end subroutine IXFdestroy_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFincref will increment the reference counter of a detector, when another object points at it

  subroutine IXFincref_detector(detector)
    implicit none
    type(IXTdetector)::detector

    detector%ref_count=detector%ref_count+1

  end subroutine IXFincref_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFdecref will decrement the reference counter of a detector, 

  subroutine IXFdecref_detector(detector)
    implicit none
    type(IXTdetector)::detector

    detector%ref_count=detector%ref_count-1

  end subroutine IXFdecref_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_detector(detector,status,checksum,det_no,geometry,delay_time,dead_time, &
       theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,ref)
    implicit none
    type(IXTdetector),intent(inout)::detector
    type(IXTdetector),optional,intent(in)::ref
    integer(i4b),optional,intent(in) :: checksum(:)	!! to compare different detector object values
    integer(i4b),optional,intent(in):: det_no(:)                  !! actual detector number
    type(IXTgeometry),optional,intent(in):: geometry          !! array of structures
    real(dp),optional,intent(in):: delay_time(:)              !! electronic delay time (microseconds)
    real(dp),optional,intent(in):: dead_time(:)          !! detector dead time (microseconds)
    real(dp),optional,intent(in):: theta(:)
    real(dp),optional,intent(in):: L2(:)
    real(dp),optional,intent(in):: phi(:)
    real(dp),optional,intent(in):: omega(:)
    integer(i4b),optional,intent(in):: group_index(:)
    integer(i4b),optional,intent(in):: det_type(:)					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
    type(IXTdet_he3),optional,intent(in):: det_he3
    type(IXTdet_solid),optional,intent(in):: det_solid
    integer(i4b),optional,intent(in):: type_index(:)
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_detector)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(detector)
    else    
       if(IXFvalid(detector) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_detector)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))then
       ! this is called either from an IXFcopy subroutine or directly using IXFset_detector(ref=###) 
       ! therefore creating a copy of a detector. this should be an independent entity and therefore
       ! it's ref_count=1 with the default value
       call IXFset_detector(detector,status,ref%checksum,ref%det_no,ref%geometry,ref%delay_time,ref%dead_time,ref%theta, &
            ref%L2,ref%phi,ref%group_index,ref%det_type,ref%det_he3,ref%det_solid,ref%type_index)
    endif

    if(present(geometry))call IXFcopy(geometry,detector%geometry,status)
    if (present(det_he3))then
       call IXFcopy(det_he3,detector%det_he3,status) ! use copy routine to copy det_he3 into detector subunit 
    endif
    if (present(det_solid))then
       call IXFcopy(det_solid,detector%det_solid,status) ! use copy routine to copy det_solid into detector subunit 
    endif
    call IXFset_integer_array(detector%checksum,status,checksum)
    call IXFset_integer_array(detector%det_no,status,det_no)    
    call IXFset_integer_array(detector%group_index,status,group_index)
    call IXFset_integer_array(detector%det_type,status,det_type)    
    call IXFset_integer_array(detector%type_index,status,type_index)        

    call IXFset_real_array(detector%delay_time,status,delay_time)    
    call IXFset_real_array(detector%dead_time,status,dead_time)    
    call IXFset_real_array(detector%theta,status,theta)    
    call IXFset_real_array(detector%L2,status,L2)    
    call IXFset_real_array(detector%phi,status,phi)    
    call IXFset_real_array(detector%omega,status,omega)    

    call IXFcheck(detector,status)

  end subroutine IXFset_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. In this case for the IXTdetector object, the IXTdet_he3 and IXTdet_solid objects are optional,
  !! so long as at least one IXTdet_*** object is present

  subroutine IXFcreate_detector(detector,checksum,det_no,geometry,delay_time,dead_time, &
       theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,status)
    implicit none
    type(IXTdetector),intent(inout)::detector
    type(IXTgeometry),intent(in)::geometry
    integer(i4b),intent(in) :: checksum(:)	!! to compare different detector object values
    integer(i4b),intent(in):: det_no(:)                  !! actual detector number
    !    integer(i4b):: ref_count=0                  !! reference count from spectra and multiple runfile
    real(dp),intent(in):: delay_time(:)              !! electronic delay time (microseconds)
    real(dp),intent(in):: dead_time(:)          !! detector dead time (microseconds)
    real(dp),intent(in):: theta(:)
    real(dp),intent(in):: L2(:)
    real(dp),intent(in):: phi(:)
    real(dp),intent(in):: omega(:)
    integer(i4b),intent(in):: group_index(:)
    integer(i4b),intent(in):: det_type(:)					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
    type(IXTdet_he3),intent(in),optional:: det_he3
    type(IXTdet_solid),intent(in),optional:: det_solid
    integer(i4b),intent(in):: type_index(:)
    type(IXTstatus),intent(inout)::status

    if((.not. present(det_he3)) .and. (.not. present(det_solid)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'either IXTdet_solid or IXTdet_he3 objects MUST be present (IXFcreate_detector)')
    endif
    if (status == IXCseverity_error) return

    if (present(det_he3))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(det_he3) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'IXTdet_he3 failure, all nested objects MUST be initialised (IXFcreate_detector)')
       endif
    endif
    if (present(det_solid))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(det_solid) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'IXTdet_solid failure, all nested objects MUST be initialised (IXFcreate_detector)')
       endif
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(geometry) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'IXTgeometry, all nested objects MUST be initialised (IXFcreate_detector)')
    endif



    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(detector)

    call IXFset_detector(detector,status,checksum,det_no,geometry,delay_time,dead_time, &
         theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega)

    ! the reference counter is now only incremented when the detector is pointed at
    ! the ref_count variable has a default 'existence' value of 1

  end subroutine IXFcreate_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_detector(detector,status,checksum,det_no,geometry,delay_time,dead_time, &
       theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,wout)
    implicit none
    type(IXTdetector),intent(in)::detector
    type(IXTdetector),optional,intent(out)::wout
    type(IXTgeometry),optional,intent(out)::geometry
    integer(i4b),optional,intent(out) :: checksum(:)	!! to compare different detector object values
    integer(i4b),optional,intent(out):: det_no(:)                  !! actual detector number
    real(dp),optional,intent(out):: delay_time(:)              !! electronic delay time (microseconds)
    real(dp),optional,intent(out):: dead_time(:)          !! detector dead time (microseconds)
    real(dp),optional,intent(out):: theta(:)
    real(dp),optional,intent(out):: L2(:)
    real(dp),optional,intent(out):: phi(:)
    real(dp),optional,intent(out):: omega(:)
    integer(i4b),optional,intent(out):: group_index(:)
    integer(i4b),optional,intent(out):: det_type(:)					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
    type(IXTdet_he3),optional,intent(out):: det_he3
    type(IXTdet_solid),optional,intent(out):: det_solid
    integer(i4b),optional,intent(out):: type_index(:)
    type(IXTstatus),intent(inout)::status

    if (present(det_he3))call IXFcopy(detector%det_he3,det_he3,status)
    if (present(det_solid))call IXFcopy(detector%det_solid,det_solid,status)
    if(present(geometry))call IXFcopy(detector%geometry,geometry,status)
    call IXFget_integer_array(detector%checksum,status,checksum)
    call IXFget_integer_array(detector%det_no,status,det_no)    
    call IXFget_integer_array(detector%group_index,status,group_index)
    call IXFget_integer_array(detector%det_type,status,det_type)    
    call IXFget_integer_array(detector%type_index,status,type_index)        

    call IXFget_real_array(detector%delay_time,status,delay_time)    
    call IXFget_real_array(detector%dead_time,status,dead_time)    
    call IXFget_real_array(detector%theta,status,theta)    
    call IXFget_real_array(detector%L2,status,L2)    
    call IXFget_real_array(detector%phi,status,phi)    
    call IXFget_real_array(detector%omega,status,omega)    

  end subroutine IXFget_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_alloc will fill optionally supplied allocatable arrays with the data contained in the 
  !! object array elements. The supplied arrays can be either allocated or not. If they are the wrong
  !! length then they are adjusted accordingly. This is a routine only for internal Fortran use.

  subroutine IXFget_alloc_detector(detector,status,checksum,det_no,geometry,delay_time,dead_time, &
       theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,wout)
    implicit none
    type(IXTdetector),intent(in)::detector
    type(IXTgeometry),intent(out),optional::geometry    
    type(IXTdetector),intent(out)::wout
    type(IXTstatus)::status
    integer(i4b),optional,allocatable :: checksum(:)	!! to compare different detector object values
    integer(i4b),optional,allocatable:: det_no(:)                  !! actual detector number
    real(dp),optional,allocatable:: delay_time(:)              !! electronic delay time (microseconds)
    real(dp),optional,allocatable:: dead_time(:)          !! detector dead time (microseconds)
    real(dp),optional,allocatable:: theta(:)
    real(dp),optional,allocatable:: L2(:)
    real(dp),optional,allocatable:: phi(:)
    real(dp),optional,allocatable:: omega(:)
    integer(i4b),optional,allocatable:: group_index(:)
    integer(i4b),optional,allocatable:: det_type(:)					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
    integer(i4b),optional,allocatable:: type_index(:)
    type(IXTdet_he3),intent(out),optional  :: det_he3
    type(IXTdet_solid),intent(out),optional :: det_solid

    if (present(checksum))then
       call IXFreallocdimsFortran(checksum,shape(detector%checksum),.false.,status)    
    endif

    if (present(det_no))then
       call IXFreallocdimsFortran(det_no,shape(detector%det_no),.false.,status)        
    endif

    if (present(delay_time))then
       call IXFreallocdimsFortran(delay_time,shape(detector%delay_time),.false.,status)
    endif

    if (present(dead_time))then
       call IXFreallocdimsFortran(dead_time,shape(detector%dead_time),.false.,status)
    endif

    if (present(theta))then
       call IXFreallocdimsFortran(theta,shape(detector%theta),.false.,status)
    endif

    if (present(L2))then
       call IXFreallocdimsFortran(L2,shape(detector%L2),.false.,status)
    endif

    if (present(phi))then
       call IXFreallocdimsFortran(phi,shape(detector%phi),.false.,status)
    endif

    if (present(group_index))then
       call IXFreallocdimsFortran(group_index,shape(detector%group_index),.false.,status)
    endif

    if (present(det_type))then
       call IXFreallocdimsFortran(det_type,shape(detector%det_type),.false.,status)
    endif

    if (present(type_index))then
       call IXFreallocdimsFortran(type_index,shape(detector%type_index),.false.,status)
    endif

    if (present(omega))then
       call IXFreallocdimsFortran(omega,shape(detector%omega),.false.,status)
    endif


    call IXFget_detector(detector,status,checksum,det_no,geometry,delay_time,dead_time, &
         theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,wout)    

  end subroutine IXFget_alloc_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFget_ptr will return a pointer to a structure or an array, from an optional argument.
  !! The pointer arguments are generally the same name as the object elements they are pointing to.
  !! Care must be taken since if the pointers are edited, then the data in the structure will also be edited.

  subroutine IXFget_ptr_detector(detector,checksum,det_no,delay_time,dead_time,theta,L2,phi,group_index,&
       & det_type,type_index,omega)
    implicit none
    type(IXTdetector),intent(in):: detector
    integer(i4b),optional,pointer::checksum(:),det_no(:),group_index(:),det_type(:),type_index(:)
    real(dp),optional,pointer::delay_time(:),dead_time(:),theta(:),L2(:),phi(:),omega(:)

    if(present(checksum))checksum=>detector%checksum
    if(present(det_no))det_no=>detector%det_no
    if(present(delay_time))delay_time=>detector%delay_time
    if(present(dead_time))dead_time=>detector%delay_time
    if(present(theta))theta=>detector%theta
    if(present(L2))L2=>detector%L2
    if(present(phi))phi=>detector%phi
    if(present(group_index))group_index=>detector%group_index
    if(present(det_type))det_type=>detector%det_type
    if(present(type_index))type_index=>detector%type_index
    if(present(omega))omega=>detector%omega

  end subroutine IXFget_ptr_detector

  pure function IXFavgL2_detector(arg,list_in)result(out_av)
    implicit none
    type(IXTdetector),intent(in)::arg
    integer(i4b),intent(in)::list_in(:)
    real(dp):: out_av

    out_av=IXFaverage_detector(arg%L2,list_in)

  end function IXFavgL2_detector

  pure function IXFavgphi_detector(arg,list_in)result(out_av)
    implicit none
    type(IXTdetector),intent(in)::arg
    integer(i4b),intent(in)::list_in(:)
    real(dp):: out_av

    out_av=IXFaverage_detector(arg%phi,list_in)

  end function IXFavgphi_detector

  pure function IXFavgtheta_detector(arg,list_in)result(out_av)
    implicit none
    type(IXTdetector),intent(in)::arg
    integer(i4b),intent(in)::list_in(:)
    real(dp):: out_av

    out_av=IXFaverage_detector(arg%theta,list_in)

  end function IXFavgtheta_detector

  pure function IXFavgdelaytime_detector(arg,list_in)result(out_av)
    implicit none
    type(IXTdetector),intent(in)::arg
    integer(i4b),intent(in)::list_in(:)
    real(dp):: out_av

    out_av=IXFaverage_detector(arg%delay_time,list_in)

  end function IXFavgdelaytime_detector

  pure function IXFavgdeadtime_detector(arg,list_in)result(out_av)
    implicit none
    type(IXTdetector),intent(in)::arg
    integer(i4b),intent(in)::list_in(:)
    real(dp):: out_av

    out_av=IXFaverage_detector(arg%dead_time,list_in)

  end function IXFavgdeadtime_detector

  !! IXFaverage_detector will take the average of a supplied component of a detector object
  !! eg the phi array (var_array), for a given list of detector numbers (list_in)


  pure function IXFaverage_detector(var_array,list_in)result(var_avg)
    real(dp)::var_avg
    real(dp),intent(in)::var_array(:)
    integer(i4b),intent(in)::list_in(:)
    !list_in is an array of detector numbers
    !we want to average over lookup table reference
    var_avg=0.0
    var_avg=sum(var_array(list_in))
    var_avg=var_avg/real(size(list_in))

  end function IXFaverage_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFupdate_detector, if a detector object exists already this will add extra entries to the arrays, if not it will call
  !! create and fill the arrays from scratch
  subroutine IXFupdate_detector(det,good,total,checksum,det_no,geometry,delay_time,dead_time, &
       theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,status)
    implicit none
    type(IXTdetector)::det
    type(IXTdet_he3),optional,intent(in)::det_he3
    type(IXTdet_solid),optional,intent(in)::det_solid
    type(IXTgeometry),intent(in)::geometry
    integer(i4b),intent(in):: checksum(:)	!! to compare different detector object values
    integer(i4b),intent(in):: det_no(:)                 !! actual detector number
    real(dp),intent(in):: delay_time(:), dead_time(:), theta(:),L2(:), phi(:),omega(:)
    integer(i4b),intent(in):: group_index(:), det_type(:), type_index(:)
    integer(i4b)::newlength,oldlength,good(:),total(:)
    type(IXTstatus)::status

    if(IXFvalid(det))then
    ! detector has been properly filled previously (by an effective monitor population), it is updated
       oldlength=size(det%checksum)
       newlength=oldlength+size(checksum)
       call IXFrealloc(det%checksum,newlength,.true.,status)
       call IXFrealloc(det%det_no,newlength,.true.,status)
       call IXFrealloc(det%delay_time,newlength,.true.,status)
       call IXFrealloc(det%dead_time,newlength,.true.,status)
       call IXFrealloc(det%theta,newlength,.true.,status)
       call IXFrealloc(det%L2,newlength,.true.,status)
       call IXFrealloc(det%phi,newlength,.true.,status)
       call IXFrealloc(det%group_index,newlength,.true.,status)
       call IXFrealloc(det%det_type,newlength,.true.,status)
       call IXFrealloc(det%type_index,newlength,.true.,status)
       call IXFrealloc(det%omega,newlength,.true.,status)

       det%checksum(oldlength+1:newlength)=checksum
       det%det_no(oldlength+1:newlength)=det_no
       det%delay_time(oldlength+1:newlength)=delay_time
       det%dead_time(oldlength+1:newlength)=dead_time
       det%theta(oldlength+1:newlength)=theta
       det%L2(oldlength+1:newlength)=L2
       det%phi(oldlength+1:newlength)=phi
       det%group_index(oldlength+1:newlength)=group_index
       det%det_type(oldlength+1:newlength)=det_type
       det%type_index(oldlength+1:newlength)=type_index
       det%omega(oldlength+1:newlength)=omega

       ! create call with new arrays
       call IXFset_detector(det,status,det_he3=det_he3,det_solid=det_solid)
       ! this next line may change when we think about not reproducing extra information
       ! but in the way it works at the moment it is necessary to update the existing effective detector index arrays
       where(good /= 0)good=good + oldlength
       total=total + oldlength       
       call IXFset_detector(det,status,geometry=geometry)

    else
       ! create call with new arrays
       call IXFcreate_detector(det,checksum,det_no,geometry,delay_time,dead_time, &
            theta,L2,phi,group_index,det_type,det_he3,det_solid,type_index,omega,status)
    endif

  end subroutine IXFupdate_detector

  !-----------------------------------------------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------------------------------------------
  !! allocates an optional number of output arrays with detector information according to the index 'list' input array
  subroutine IXFalloc_section_detector(arg,status,list,checksum,det_no,delay_time,dead_time,theta,L2,phi,  &
       group_index,det_type,type_index,omega)
    implicit none
    type(IXTdetector)::arg
    type(IXTstatus)::status
    integer(i4b),optional,allocatable :: checksum(:)	!! to compare different detector object values
    integer(i4b),optional,allocatable:: det_no(:)                  !! actual detector number
    real(dp),optional,allocatable:: delay_time(:)              !! electronic delay time (microseconds)
    real(dp),optional,allocatable:: dead_time(:)          !! detector dead time (microseconds)
    real(dp),optional,allocatable:: theta(:)
    real(dp),optional,allocatable:: L2(:)
    real(dp),optional,allocatable:: phi(:)
    real(dp),optional,allocatable:: omega(:)
    integer(i4b),optional,allocatable:: group_index(:)
    integer(i4b),optional,allocatable:: det_type(:)					!! type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...), this can be he3 or solid or composite. Composite is used for effective detector purpose in which there can be a combination of detectors of different type.
    integer(i4b),optional,allocatable:: type_index(:)
    integer(i4b),intent(in)::list(:)
    integer(i4b)::nwk

    nwk=size(list)
    if (present(checksum))then
       call IXFreallocdimsfortran(checksum,(/ nwk /),.false.,status)
       where(list/= 0)
          checksum=arg%checksum(list) 
       elsewhere
          checksum=1.0
       endwhere
    endif

    if (present(det_no))then
       call IXFreallocdimsfortran(det_no ,(/ nwk /),.false.,status)
       where(list/= 0)        
          det_no=arg%det_no(list) 
       elsewhere
          det_no=1.0
       endwhere
    endif

    if (present(delay_time))then
       call IXFreallocdimsfortran(delay_time ,(/ nwk /),.false.,status)
       where(list/= 0) 
          delay_time=arg%delay_time(list) 
       elsewhere
          delay_time=1.0
       endwhere
    endif

    if (present(dead_time))then
       call IXFreallocdimsfortran(dead_time ,(/ nwk /),.false.,status)
       where(list/= 0)
          dead_time= arg%dead_time(list) 
       elsewhere
          dead_time=1.0
       endwhere

    endif

    if (present(theta))then
       call IXFreallocdimsfortran(theta ,(/ nwk /),.false.,status)
       where(list/= 0)
          theta= arg%theta(list) 
       elsewhere
          theta=1.0
       endwhere

    endif

    if (present(L2))then
       call IXFreallocdimsfortran(L2 ,(/ nwk /),.false.,status)
       where(list/= 0)
          L2=arg%L2(list) 
       elsewhere
          L2=1.0
       endwhere

    endif

    if (present(phi))then
       call IXFreallocdimsfortran( phi,(/ nwk /),.false.,status)
       where(list/= 0)
          phi=arg%phi(list) 
       elsewhere
          phi=1.0
       endwhere

    endif
    if (present(omega))then
       call IXFreallocdimsfortran(omega,(/ nwk /),.false.,status)
       where(list/= 0)
          omega=arg%omega(list) 
       elsewhere
          omega=1.0
       endwhere

    endif

    if (present(group_index))then
       call IXFreallocdimsfortran(group_index ,(/ nwk /),.false.,status)
       where(list/= 0) 
          group_index=arg%group_index(list)   
       elsewhere
          group_index=1.0
       endwhere

    endif

    if (present(det_type))then
       call IXFreallocdimsfortran(det_type ,(/ nwk /),.false.,status)
       where(list/= 0)
          det_type=arg%det_type(list) 
       elsewhere
          det_type=1.0
       endwhere

    endif

    if (present(type_index))then
       call IXFreallocdimsfortran(type_index,(/ nwk /),.false.,status)
       where(list/= 0)
          type_index=arg%type_index(list) 
       elsewhere
          type_index=1.0
       endwhere

    endif

  end subroutine IXFalloc_section_detector
  !-----------------------------------------------------------------------------------------------------------------------

  subroutine IXFpopulate_detector(detector,rawfile,index_list,status)
    use IXMraw_file
    implicit none
    type(IXTdetector)::detector
    type(IXTraw_file)::rawfile
    type(IXTbase) :: base  
    integer(i4b),intent(in),allocatable:: index_list(:)                  !! actual detector number index into data arrays from raw file
    real(dp),allocatable:: delaytime(:)              !! electronic delay time (microseconds)
    real(dp),allocatable:: deadtime(:)          !! detector dead time (microseconds)
    real(dp),allocatable:: twotheta(:)
    real(dp),allocatable:: L2(:)
    real(dp),allocatable:: phi(:) 
    type(IXTdet_he3):: det_he3
    type(IXTdet_solid):: det_solid
    integer(i4b),allocatable:: spec(:),temp1(:),temp2(:),udet(:)
    integer(i4b)::ndets,i,len
    type(IXTstatus)::status

    call IXFget_raw(rawfile, 'NDET', ndets, status)
    allocate(spec(ndets),L2(ndets),twotheta(ndets),delaytime(ndets),phi(ndets))
    allocate (deadtime(ndets),udet(ndets))
    call IXFget_raw(rawfile, 'SPEC', spec, status)
    call IXFget_raw(rawfile, 'UDET', udet, status)    
    call IXFget_raw(rawfile, 'LEN2', L2, status)
    call IXFget_raw(rawfile, 'TTHE', twotheta, status)
    call IXFget_raw(rawfile, 'DELT', delaytime, status)
    call IXFget_raw(rawfile, 'UT01', phi, status)
    !*MAPS* specific UT11 , no idea for MARI not included??
    call IXFget_raw(rawfile, 'UT11',deadtime,status)

    allocate (temp1(ndets))
    temp1=(/ (i,i=1,ndets) /)
    !temporary arrays used to fill type_index and group_index sections
    ! this will come from the raw file or another source of input in the future
    allocate(temp2(ndets))
    temp2=1

    ! make no assumptions about detector from the raw file, therefore it is type composite
    ! and type_index=0
    

    ! allocate lengths of arrays in detector
    len=size(index_list)
    call IXFrealloc(detector%L2,len,.false.,status)
    call IXFrealloc(detector%theta,len,.false.,status)
    call IXFrealloc(detector%checksum,len,.false.,status)
    call IXFrealloc(detector%det_no,len,.false.,status)
    call IXFrealloc(detector%delay_time,len,.false.,status)
    call IXFrealloc(detector%dead_time,len,.false.,status)
    call IXFrealloc(detector%phi,len,.false.,status)
    call IXFrealloc(detector%group_index,len,.false.,status)
    call IXFrealloc(detector%det_type,len,.false.,status)
    call IXFrealloc(detector%type_index,len,.false.,status)
    call IXFrealloc(detector%omega,len,.false.,status)
    ! now populate pointer arrays
    ! cannot dereference array list in a command call, so populate without temporary allocatables one at a time....
    detector%L2=L2(index_list)
    detector%det_no=udet(index_list)
    detector%delay_time=delaytime(index_list)
    detector%phi=phi(index_list)
    detector%theta=twotheta(index_list)
    detector%dead_time=deadtime(index_list)
    ! dummy integer values
    detector%checksum=temp1(index_list)
    detector%type_index=0
    detector%group_index=temp1(index_list)
    detector%det_type=IXCcomposite
    detector%omega=1.0
    deallocate(temp1,temp2,phi,L2,twotheta,delaytime,deadtime,udet)
    call IXFmark_valid(detector)
    
    !no shape information from raw file, we create a dummy geometry compatible with the combine geometry subroutine used 
    ! to create the effectivw detector later on
    call IXFdummy_geometry(detector%geometry,index_list,status)

  end subroutine IXFpopulate_detector

  subroutine IXFpopulate_reference_detector_list(detector,path,det_list,rawfile,status)
    use IXMraw_file
    use IXMsort
    implicit none
    type(IXTdetector),intent(out)::detector
    type(IXTstatus),intent(inout)::status
    integer(i4b),intent(in)::det_list(:)
    type(IXTraw_file),intent(in)::rawfile
    character(len=long_len),intent(in)::path
    type(IXTdetector)::ref_det
    integer(i4b)::len,ndet,mx_raw(1),i,j,ref_len
    integer(i4b),allocatable::udet(:),raw_ranks(:),raw_sorted(:),new_index(:), type_list(:),lookup(:),temp_index(:)
    logical,allocatable::dtype(:)

    call IXFpopulate_file_detector(ref_det,path,status,IXCdet_ref)
    
    call IXFget_raw(rawfile, 'NDET', ndet, status)
    allocate(udet(ndet),raw_ranks(ndet),raw_sorted(ndet))
    !call IXFget_raw(inputsource, 'SPEC', spec, status)
    call IXFget_raw(rawfile, 'UDET', udet, status)
    
    call IXFrank(udet,raw_ranks)
    raw_sorted=udet(raw_ranks)
    mx_raw=maxloc(udet(det_list))
    
    len=size(det_list)
    allocate(new_index(len))
    ref_len=size(ref_det%det_no)
    
    allocate(lookup(maxval(det_list)))
    
do i=1,len    
    lookup(det_list(i))=i
enddo
    
      i=1
      j=1
      do while(j <= len .and. i <= ref_len )
        if (raw_sorted(j) == ref_det%det_no(i))then
          new_index(j)=i   
          j=j+1       
        endif
        i=i+1
      end do 
        
      if(j-1 /= len)then
        call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'reference detector incompatible with rawfile (IXFpopulate_reference_detector) ')
        return      
      endif
    

! new_index(i) is now an index into ref_det% arrays
! raw_ranks is the same length as new_index
! lookup is as long as the highest det_list index used
! lookup(raw_ranks(i)) -> an index into detector% arrays, since may not be the same length as ref_det% arrays

    ! allocate lengths of arrays in detector
    
    call IXFalloc(detector%L2,len,status)
    call IXFalloc(detector%theta,len,status)
    call IXFalloc(detector%checksum,len,status)
    call IXFalloc(detector%det_no,len,status)
    call IXFalloc(detector%delay_time,len,status)
    call IXFalloc(detector%dead_time,len,status)
    call IXFalloc(detector%phi,len,status)
    call IXFalloc(detector%group_index,len,status)
    call IXFalloc(detector%type_index,len,status) 
    call IXFalloc(detector%det_type,len,status)
    call IXFalloc(detector%omega,len,status)

do i=1,len
    detector%L2(lookup(raw_ranks(i)))=ref_det%L2(new_index(i))
    detector%det_no(lookup(raw_ranks(i)))=ref_det%det_no(new_index(i))
    detector%delay_time(lookup(raw_ranks(i)))=ref_det%delay_time(new_index(i))
    detector%phi(lookup(raw_ranks(i)))=ref_det%phi(new_index(i))
    detector%theta(lookup(raw_ranks(i)))=ref_det%theta(new_index(i))
    detector%dead_time(lookup(raw_ranks(i)))=ref_det%dead_time(new_index(i)) 
    detector%checksum(lookup(raw_ranks(i)))=ref_det%checksum(new_index(i))
    detector%group_index(lookup(raw_ranks(i)))=ref_det%group_index(new_index(i))
    detector%det_type(lookup(raw_ranks(i)))=ref_det%det_type(new_index(i))
    detector%type_index(lookup(raw_ranks(i)))=ref_det%type_index(new_index(i))
    detector%omega(lookup(raw_ranks(i)))=ref_det%omega(new_index(i))
enddo
! need to determine type of detector which are included by det_list/new_index
! by inspection of det_type array



    allocate(temp_index(len))
 
       allocate(dtype(len))
       dtype=.false.
       ! IXCdet_solid              
       where(detector%det_type == IXCdet_solid) dtype=.true.       
       if(any(dtype))then
         allocate(type_list(count(dtype)))
         type_list=pack(detector%type_index,dtype)
         !type_list is an index into the reference det_solid object
         call IXFpopulate_reference_det_solid(detector%det_solid,ref_det%det_solid,type_list,status)         
         temp_index=unpack( (/ (i,i=1,size(type_list)) /),dtype,0)
         deallocate(type_list)
       endif

       dtype=.false.      
       ! IXCdet_he3
       where(detector%det_type == IXCdet_he3) dtype=.true.
       if(any(dtype))then
         allocate(type_list(count(dtype)))
         type_list=pack(detector%type_index,dtype)
         !type_list is an index into the reference det_he3 object
         call IXFpopulate_reference_det_he3(detector%det_he3,ref_det%det_he3,type_list,status)                  
         detector%type_index=unpack( (/ (i,i=1,size(type_list)) /),dtype,temp_index)         
         deallocate(type_list)
       endif      

       deallocate(dtype,temp_index)
       
    call IXFdestroy(ref_det%det_he3,status)
    call IXFdestroy(ref_det%det_solid,status)

    call IXFpopulate_reference_geometry(detector%geometry,ref_det%geometry,lookup,raw_ranks,new_index,status)

    call IXFmark_valid(detector)
    call IXFdestroy_detector(ref_det,status)
    deallocate(new_index,lookup)

  end subroutine IXFpopulate_reference_detector_list

  subroutine IXFpopulate_reference_detector_spectra(detector,path,spectra,rawfile,status)
    use IXMspectra
    use IXMraw_file
    implicit none
    type(IXTdetector),intent(out)::detector
    type(IXTstatus),intent(inout)::status
    type(IXTspectra),intent(in)::spectra
    character(len=long_len),intent(in)::path
    type(IXTraw_file),intent(in)::rawfile
    integer(i4b),allocatable::det_list(:)

    !det_list needs to be generated from spectrum numbers and raw_file, this is done during the spectra population                            
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'this function cannot be called(IXFpopulate_reference_detector)')
    return   
  end subroutine IXFpopulate_reference_detector_spectra

  subroutine IXFreference_file_create_detector(data_file, calib_file, data_out_file, detpar_file, short_detpar_file,status,detector)
    use IXMsort
    implicit none
    character(len=*),intent(in):: data_file, calib_file, data_out_file, detpar_file, short_detpar_file
    type(IXTdetector),optional,intent(out)::detector
    type(IXTstatus)::status
    real(dp),pointer::L2(:),theta(:),phi(:),delta(:),alpha_x(:),alpha_y(:),alpha_z(:)
    real(dp),pointer::det_1(:),det_2(:),det_3(:),det_4(:),w_x(:),w_y(:),w_z(:)
    integer(i4b),pointer::detno(:),code(:)
    integer(i4b)::ndet,i,n_solid,n_he3,ntype,index_count
    logical,allocatable::dtype(:)
    integer(i4b),allocatable::udet_ranks(:)
  
    if(.not. present(detector))then
    ! create file output only
       call create_detector_data(data_file, calib_file, data_out_file, detpar_file, short_detpar_file,status)
    else    
    ! create file output and populate supplied detector object
       call create_detector_data(data_file, calib_file, data_out_file, detpar_file, short_detpar_file,status,&
           L2,theta,phi,delta,alpha_x,alpha_y,alpha_z,det_1,det_2,det_3,det_4,code,detno,w_x,w_y,w_z,ndet)       
       
! arrange everything according to detector number
allocate(udet_ranks(ndet))
call IXFrank(detno,udet_ranks)
detno=detno(udet_ranks)
L2=L2(udet_ranks)
phi=phi(udet_ranks)
theta=theta(udet_ranks)
delta=delta(udet_ranks)
alpha_x=alpha_x(udet_ranks)
alpha_y=alpha_y(udet_ranks)
alpha_z=alpha_z(udet_ranks)
det_1=det_1(udet_ranks)
det_2=det_2(udet_ranks)
det_3=det_3(udet_ranks)
det_4=det_4(udet_ranks)
code=code(udet_ranks)
w_x=w_x(udet_ranks)
w_y=w_y(udet_ranks)
w_z=w_z(udet_ranks)
deallocate(udet_ranks)
       
       call IXFrealloc(detector%det_type,ndet,.false.,status)
       call IXFrealloc(detector%type_index,ndet,.false.,status)

       ! IXCnull              
       where(code == 0) detector%det_type=IXCdet_null

       allocate(dtype(ndet))
       dtype=.false.
       ! IXCdet_solid              
       where(code == 1) 
         detector%det_type=IXCdet_solid
         dtype=.true.
       end where
       if(any(dtype))then
         call IXFcreate_reference_det_solid(detector%det_solid,det_2,dtype,status)      
         index_count=1
         do i=1,ndet
            if(dtype(i) .eqv. .true.)then
              detector%type_index(i)=index_count
              index_count=index_count+1
            endif
         enddo
       endif
       
       dtype=.false.
       ! IXCdet_he3
       where(code == 2)
          detector%det_type=IXCdet_he3
          dtype=.true.
       end where
       where(code == 3)
          detector%det_type=IXCdet_he3
          dtype=.true.
       end where              
       if(any(dtype))then
         call IXFcreate_reference_det_he3(detector%det_he3,det_2,det_3,dtype,status)
         index_count=1
         do i=1,ndet
            if(dtype(i) .eqv. .true.)then
              detector%type_index(i)=index_count
              index_count=index_count+1
            endif
         enddo
       endif
       deallocate(dtype)
          
       call IXFreference_file_geometry(detector%geometry,w_x,w_y,w_z,alpha_x,alpha_y,alpha_z,phi,theta,L2,ndet,detector%det_type,status)

       detector%L2=>L2
       detector%delay_time=>delta
       detector%dead_time=>det_1       
       detector%phi=>phi
       detector%theta=>theta
       detector%det_no=>detno

       call IXFrealloc(detector%group_index,ndet,.false.,status)
       detector%group_index=int(det_4)

       call IXFdealloc(code,status)
       call IXFdealloc(det_4,status)
       call IXFdealloc(det_3,status)
       call IXFdealloc(det_2,status)       
       call IXFdealloc(w_x,status)       
       call IXFdealloc(w_y,status)       
       call IXFdealloc(w_z,status)
       call IXFdealloc(alpha_x,status)       
       call IXFdealloc(alpha_y,status)       
       call IXFdealloc(alpha_z,status)

! solid angle information
call IXFalloc(detector%omega,ndet,status)

call IXFsolid_angle_geometry(detector%geometry,(/ 0.0d0, 0.0d0, 0.0d0 /),detector%omega,status)

! filled with null data at present       
       call IXFrealloc(detector%checksum,ndet,.false.,status) ! fills with zeros
       
       call IXFmark_valid(detector)
    endif
  end subroutine IXFreference_file_create_detector

subroutine IXFaverageparts_detector(det_list,posn_index,detector,effdet,solid_counter,he3_counter,w_type,w_dimension,status)
use IXMmaths_utils
implicit none
integer(i4b),intent(in)::det_list(:),posn_index
type(IXTdetector),intent(in)::detector
type(IXTdetector)::effdet
type(IXTstatus)::status
logical::try_combine,combine_ok
logical::pars_true,same_type,same_group
integer(i4b),intent(inout)::solid_counter,he3_counter
integer(i4b)::mintype,w_type(:)
real(dp)::w_dimension(:,:)
real(dp)::omega_sum,effL2(1)
type(IXTtranslation)::trans_eff

if(size(det_list) ==1)then
  call single_detector_average(effdet,detector,posn_index,det_list(1),solid_counter,he3_counter,w_type,w_dimension,status)
  return
endif

omega_sum=sum(detector%omega(det_list))
effdet%dead_time(posn_index)=sum(detector%dead_time(det_list) * detector%omega(det_list)) / omega_sum
effdet%delay_time(posn_index)=sum(detector%delay_time(det_list) * detector%omega(det_list)) / omega_sum

try_combine=.false.
combine_ok=.false.
same_type=.false.
same_group=.false.
pars_true=.false.
!det_type all the same
call IXFfind_if_same_value(detector%det_type(det_list),same_type,mintype)

!if(mintype <= 0)then
!   call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
!      IXCerr_invparam, 'an invalid or IXCdet_null detector has been included in the spectrum mapping(IXFpopulate_effdet_index)')
!   return
!endif

!det_type
!if(maxval(detector%det_type(det_list)) == mintype ) same_type=.true.

if(mintype /=IXCdet_null)then
  if(same_type)then
    effdet%det_type(posn_index)=mintype    
  else
    effdet%det_type(posn_index)=IXCcomposite
    effdet%type_index(posn_index)=0
  endif
else
! set values for null detectors
  effdet%det_type(posn_index)=IXCdet_null
  effdet%type_index(posn_index)=0
endif

! take first value in detector list
effdet%det_no(posn_index)=detector%det_no(det_list(1))
effdet%checksum(posn_index)=detector%checksum(det_list(1))
!this can be changed later by combine geometry
effdet%omega(posn_index)=omega_sum

! group_index
!if(maxval(detector%group_index(det_list)) == minval(detector%group_index(det_list)) ) same_group=.true.
call IXFfind_if_same_value(detector%group_index(det_list),same_group)

if(same_group)then
  effdet%group_index(posn_index)=detector%group_index(det_list(1))
else
  effdet%group_index(posn_index)=0
endif

! parameters can only be true if the detector is not null
if(effdet%det_type(posn_index) == IXCdet_solid)then
  call IXFchecksamepars_det_solid(detector%det_solid,detector%type_index(det_list),pars_true)
else
  if(effdet%det_type(posn_index) == IXCdet_he3)then
    call IXFchecksamepars_det_he3(detector%det_he3,detector%type_index(det_list),pars_true) 
  !else
    !detectors are null 
    ! do something??
  endif
endif



if(pars_true .and. same_group .and. same_type)try_combine=.true.

!try_combine => true, full combine will be attempted
!            => false, generic combine only attempted

call IXFcombine_geometry(detector%geometry,posn_index,det_list,try_combine,w_type,w_dimension,effdet%geometry,combine_ok,status,trans_eff)

! if combine_ok => true, then successfully combined using standard manner otherwise a generic combination performed
! combine_ok => false, a generic combination performed (irrespective of incident try_combine value)


  if(pars_true)then
  ! can only happen if detectors are not null
  ! if detector is one type is one or the other
    if(effdet%det_type(posn_index)==IXCdet_he3)then
      effdet%type_index(posn_index)=he3_counter
      call IXFcopy_index_from_det_he3(detector%det_he3,detector%type_index(det_list(1)),he3_counter,effdet%det_he3,status)
      he3_counter=he3_counter+1
    endif
  
    if(effdet%det_type(posn_index)==IXCdet_solid)then
      effdet%type_index(posn_index)=solid_counter    
      call IXFcopy_index_from_det_solid(detector%det_solid,detector%type_index(det_list(1)),solid_counter,effdet%det_solid,status)
      solid_counter=solid_counter+1
    endif
  else
  !could still be null detectors
    if(mintype/=IXCdet_null)then
    ! it did not combine properly so therefore changed to be a composite type
      effdet%det_type(posn_index)=IXCcomposite
      effdet%type_index(posn_index)=0
    endif
  endif

!if(try_combine .and. (.not.combine_ok))then
! detector is still the same type if all the detector parameters were the same
! it will therefore not change its type
! effdet%det_type(posn_index)=IXCcomposite
! group index does not need to change
!endif

if(combine_ok)then

! this is just here to stop a particular problem which will raise its head with merlin data

  ! translate spherical polar coordinates from new object
  call IXFfind_l2thetaphi_translation(trans_eff,effdet%L2(posn_index),effdet%theta(posn_index),effdet%phi(posn_index),status)
! successfull combination calculate solid angle of new shape
!  call IXFgetsolid_angle_i_geometry(effdet%geometry,(/ 0.0d0, 0.0d0, 0.0d0 /),effdet%omega(posn_index),posn_index,status)

else 
! this will catch the case of a monitor with multiple detectors in the map a la merlin
! try_combine will never have been true since group_index would have been different
  call combine_position_weighted(effdet,detector,posn_index,det_list,omega_sum,IXCweighted,status)
endif



end subroutine IXFaverageparts_detector


subroutine combine_position_weighted(effdet,detector,posn_index,det_list,omega_sum,avtype,status)
implicit none
type(IXTstatus)::status
integer(i4b),intent(in)::posn_index,det_list(:),avtype
real(dp),intent(in)::omega_sum
type(IXTdetector),intent(in)::detector
type(IXTdetector)::effdet
real(dp)::val,omega_t
logical,allocatable::index_list(:)
integer(i4b)::i,list_len

if(avtype == IXCweighted)then
    effdet%L2(posn_index)=sum(detector%L2(det_list)*detector%omega(det_list))/omega_sum
    effdet%phi(posn_index)=sum(detector%phi(det_list)*detector%omega(det_list))/omega_sum
    effdet%theta(posn_index)=sum(detector%theta(det_list)*detector%omega(det_list))/omega_sum
    return
endif

if(avtype == IXCmax_2theta)then
list_len=size(det_list)
allocate(index_list(list_len))

call IXFfind_max_and_indexmask(detector%theta(det_list),index_list,val)
effdet%theta(posn_index)=val
omega_t=sum(detector%omega(pack(det_list,index_list)))
effdet%L2(posn_index)=sum(detector%L2(pack(det_list,index_list))*detector%omega(pack(det_list,index_list)))/omega_t
effdet%phi(posn_index)=sum(detector%phi(pack(det_list,index_list))*detector%omega(pack(det_list,index_list)))/omega_t
deallocate (index_list)
return
endif

if(avtype == IXCmin_2theta)then
list_len=size(det_list)
allocate(index_list(list_len))

call IXFfind_min_and_indexmask(detector%theta(det_list),index_list,val)
effdet%theta(posn_index)=val
omega_t=sum(detector%omega(pack(det_list,index_list)))
effdet%L2(posn_index)=sum(detector%L2(pack(det_list,index_list))*detector%omega(pack(det_list,index_list)))/omega_t
effdet%phi(posn_index)=sum(detector%phi(pack(det_list,index_list))*detector%omega(pack(det_list,index_list)))/omega_t
deallocate (index_list)
return
endif

end subroutine combine_position_weighted

subroutine single_detector_average(effdet,detector,index_eff,index_full,solid_counter,he3_counter,w_type,w_dimension,status)
  implicit none
  type(IXTdetector)::effdet,detector
  integer(i4b),intent(in)::index_eff,index_full
  integer(i4b),intent(inout)::solid_counter,he3_counter,w_type(:)
  real(dp)::w_dimension(:,:)
  logical ::combine_ok
  type(IXTstatus)::status
  effdet%L2(index_eff)=detector%L2(index_full)
  effdet%theta(index_eff)=detector%theta(index_full)
  effdet%phi(index_eff)=detector%phi(index_full)
  effdet%det_no(index_eff)=detector%det_no(index_full)
  effdet%checksum(index_eff)=detector%checksum(index_full)
  effdet%delay_time(index_eff)=detector%delay_time(index_full)
  effdet%dead_time(index_eff)=detector%dead_time(index_full)
  effdet%group_index(index_eff)=detector%group_index(index_full)
  effdet%det_type(index_eff)=detector%det_type(index_full)
  effdet%omega(index_eff)=detector%omega(index_full)
  
  ! type_index....
  ! geometry
    if(effdet%det_type(index_eff)==IXCdet_he3)then
      effdet%type_index(index_eff)=he3_counter
      call IXFcopy_index_from_det_he3(detector%det_he3,detector%type_index(index_full),he3_counter,effdet%det_he3,status)
      he3_counter=he3_counter+1
    endif
  
    if(effdet%det_type(index_eff)==IXCdet_solid)then
      effdet%type_index(index_eff)=solid_counter    
      call IXFcopy_index_from_det_solid(detector%det_solid,detector%type_index(index_full),solid_counter,effdet%det_solid,status)
      solid_counter=solid_counter+1
    endif
    call IXFcombine_geometry(detector%geometry,index_eff,(/ index_full /),.false.,w_type,w_dimension,effdet%geometry,combine_ok,status)
  
end subroutine single_detector_average

!> allocates memory for most of detector, except the IXTshape/IXTdet_he3/IXTdet_solid objects
! which can only be filled after inspection of populated effective detector arrays (det_type/type_index)
subroutine IXFsetup_effective_detector(effdet,detfull,len,status)
implicit none
type(IXTdetector)::effdet,detfull
integer(i4b),intent(in)::len
type(IXTstatus)::status
integer(i4b)::mintype,maxtype,i

    call IXFrealloc(effdet%L2,len,.false.,status)
    call IXFrealloc(effdet%theta,len,.false.,status)
    call IXFrealloc(effdet%checksum,len,.false.,status)
    call IXFrealloc(effdet%det_no,len,.false.,status)
    call IXFrealloc(effdet%delay_time,len,.false.,status)
    call IXFrealloc(effdet%dead_time,len,.false.,status)
    call IXFrealloc(effdet%phi,len,.false.,status)
    call IXFrealloc(effdet%group_index,len,.false.,status)
    call IXFrealloc(effdet%type_index,len,.false.,status)
    call IXFrealloc(effdet%det_type,len,.false.,status)
    call IXFrealloc(effdet%omega,len,.false.,status)

! allocate appropriate memory in geometry object
!shape is ignored until finish_effective_geometry
    call IXFsetup_effective_geometry(effdet%geometry,len,status)
    
    call IXFmake_det_solid(effdet%det_solid,len,.false.,status)
    call IXFmake_det_he3(effdet%det_he3,len,.false.,status)

end subroutine IXFsetup_effective_detector
  
subroutine IXFfinish_effective_detector(effdet,solid_counter,he3_counter,w_type,w_dimension,status)
  implicit none
  type(IXTdetector)::effdet
  type(IXTstatus)::status
  integer(i4b),intent(in)::solid_counter,he3_counter,w_type(:)
  real(dp)::w_dimension(:,:)
 
  
  if(solid_counter == 0)then
    call IXFdestroy(effdet%det_solid,status)
  else
    call IXFmake_det_solid(effdet%det_solid,solid_counter,.true.,status)
  endif
  
  if(he3_counter == 0)then
    call IXFdestroy(effdet%det_he3,status)
  else
    call IXFmake_det_he3(effdet%det_he3,he3_counter,.true.,status)  
  endif
  
  
call IXFfinish_effective_geometry(effdet%geometry,w_type,w_dimension,status)
    
  if (status == IXCseverity_error)then
    return
  else
    call IXFmark_valid(effdet)
  endif
 
end subroutine IXFfinish_effective_detector
  
  
  
  subroutine create_detector_data (data_file, calib_file, data_out_file, detpar_file, short_detpar_file,status,&
             L2_o,theta_o,phi_o,delta_o,alpha_x_o,alpha_y_o,alpha_z_o,det_1_o,det_2_o,det_3_o,det_4_o,code_o,detno_o,&
             w_x_o,w_y_o,w_z_o,ndet_o)
    use IXMtools
    use IXMfileio
    use IXMsort
    character(len=*),intent(in):: data_file, calib_file, data_out_file, detpar_file, short_detpar_file
    real(dp),optional,pointer::L2_o(:),theta_o(:),phi_o(:),delta_o(:),alpha_x_o(:),alpha_y_o(:),alpha_z_o(:)
    real(dp),optional,pointer::det_1_o(:),det_2_o(:),det_3_o(:),det_4_o(:),w_x_o(:),w_y_o(:),w_z_o(:)
    integer(i4b),optional,pointer::detno_o(:),code_o(:)
    integer(i4b),optional::ndet_o
    type(IXTstatus)::status
    integer,parameter:: ntype=8, n_user_tables=14, unity=1,ndet_totmax=1200000       
    ! Variables for detector.dat file:
    integer(i4b):: ndet, detno(ndet_totmax), code(ndet_totmax)
    real(dp):: delta(ndet_totmax), l2(ndet_totmax), theta(ndet_totmax), phi(ndet_totmax),w_x(ndet_totmax), w_y(ndet_totmax),&
         w_z(ndet_totmax),f_x(ndet_totmax), f_y(ndet_totmax), f_z(ndet_totmax),alpha_x(ndet_totmax), alpha_y(ndet_totmax),&
         alpha_z(ndet_totmax), det_1(ndet_totmax), det_2(ndet_totmax), det_3(ndet_totmax), det_4(ndet_totmax)
    ! Other variables:
    integer(i4b):: iu_data, iu_calib, iu_data_out, iunit, istatus, len_line, i, l_bank_type, l_bank_name, itype,&
         iwork(ndet_totmax), noff, ndet_bank, idet, ierr_write, ierr
    character(len=64):: bank_type_list(ntype)
    character(len=255)::  trans_name, line, bank_type, bank_name, data_out_file_full

    data bank_type_list /'PSD_TUBE_PLANE','PSD_TUBE_STRIP','PSD_TUBE_GCIRCLE','TUBE_PLANE','TUBE_STRIP','TUBE_GCIRCLE',&
         'MONITOR', 'DUMMY'/

    ierr = OK
    iu_data=0
    iu_calib=0
    iu_data_out=0
    iunit=0
    istatus=0

    ! Get to start of a data block:
    ! =============================

    ! Open data file
    call IXFunitno (iu_data)
    trans_name=IXFtranslate_read(data_file)	
    if (istatus .ne. 0) then
       call remark ('ERROR: Unable to translate data file name.')
       ierr=ERR
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       return
    endif
    istatus = oldfl (iu_data, trans_name, READ)
    if (istatus .ne. 0) then
       call remark ('ERROR: Unable to open data file.')
       ierr=ERR
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       return
    endif

    ! Open calibration file, if any
    if (lenstr(calib_file) .gt. 0) then
       call IXFunitno (iu_calib)
       trans_name=IXFtranslate_read(calib_file)			
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate calibration file name.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       istatus = oldfl (iu_calib, trans_name, READ)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to open calibration file.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
    else
       iu_calib=0
    endif

    ! Open output data file, if any
    if (lenstr(data_out_file) .gt. 0) then
       call IXFunitno (iu_data_out)
       !		istatus = sys_trans_filename (data_out_file, trans_name)
       trans_name=IXFtranslate_write(data_out_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate output data file name.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       istatus = newfl (iu_data_out, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to open output data file.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       inquire(iu_data_out,name=data_out_file_full)
    else
       iu_data_out=0
    endif

    ndet = 0
1000 continue
    noff = ndet + 1
    call read_line_write (iu_data, line, len_line, iu_data_out, .true., ierr_write)
    if (len_line .gt. 0) then
       i = 1
       l_bank_type = GETSTR (line, i, bank_type)
       l_bank_name = GETSTR (line, i, bank_name)
       itype = ICMD (bank_type_list, ntype, bank_type)
       if (itype .lt. 0) then
          call remark ('ERROR: Ambiguous detector bank type in data file.')
          if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
          ierr = ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return

       else if (itype .eq. 0) then
          call remark ('ERROR: Unrecognised detector bank type in data file.')
          if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
          ierr = ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return

          !   Get detector bank type:
          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_PLANE
       else if (itype .eq. 1) then
          call process_psd_tube_plane_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name,& 
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_STRIP
       else if (itype .eq. 2) then
          call process_psd_tube_strip_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_GCIRCLE
       else if (itype .eq. 3) then
          call process_psd_tube_gcircle_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_PLANE
       else if (itype .eq. 4) then
          call process_tube_plane_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_STRIP
       else if (itype .eq. 5) then
          call process_tube_strip_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_GCIRCLE
       else if (itype .eq. 6) then
          call process_tube_gcircle_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	MONITOR
       else if (itype .eq. 7) then
          call process_monitor_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	DUMMY
       else if (itype .eq. 8) then
          call process_dummy_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------

       else
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          call remark ('*** ERROR in source code. Contact T.G.Perring.')
          stop
       endif

       !	--------------------------------------------------------------------------------------------
    else if (len_line .eq. EOF) then
       goto 2000			! end-of-file reached
    else if (len_line .eq. ERR) then
       if (ierr_write .ne. OK) then
          call remark ('ERROR: Problem writing to output data file')
       else
          call remark ('ERROR: Problem reading from data file.')
       endif
       if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    else
       call remark ('ERROR: Unknown error reading from data file.')
       if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    endif
    goto 1000

2000 continue
    istatus = shutfl (iu_data)
    if (iu_calib .ne. 0) istatus = shutfl(iu_calib)
    if (iu_data_out .ne. 0) istatus = shutfl(iu_data_out)

    ! Check entries for repeated detector numbers:
    ! ============================================
    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read from data file.')
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    endif

    !	call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique in data file.')
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             ierr = ERR
             return
          endif
       end do
    endif
    ! Write short format detector.dat file:
    ! ======================================
    if (lenstr(short_detpar_file) .gt. 0) then
       call IXFunitno (iunit)
       !istatus = sys_trans_filename (short_detpar_file, trans_name)
       trans_name=IXFtranslate_write(short_detpar_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate short format detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr = ERR
          return
       endif
       istatus = newfl (iunit, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to write short format detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr = ERR
          return
       endif

       write (iunit, '(''Short format DETECTOR.DAT generated by CREATE_DETECTOR_FILE'')')
       write (iunit, '(i8,i8)') ndet, unity
       write (iunit, '(a)')'  det no.  offset    l2     code     theta        phi'
       do i = 1, ndet
          idet = iwork(i)
          write (iunit, '(i9, f8.3, f10.5, i6, 2f12.5)') detno(idet), delta(idet), l2(idet), code(idet), theta(idet), phi(idet)
       end do
       istatus = shutfl (iunit)
    endif

    ! Write full format detector.dat file:
    ! ======================================
    if (lenstr(detpar_file) .gt. 0) then
       call IXFunitno (iunit)
       !		istatus = sys_trans_filename (detpar_file, trans_name)
       trans_name=IXFtranslate_write(detpar_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate output detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr=ERR
          return
       endif
       istatus = newfl (iunit, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to write output detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr=ERR
          return
       endif

       write (iunit, '(''Full format DETECTOR.DAT generated by CREATE_DETECTOR_FILE'')')
       write (iunit, '(i8,i8)') ndet, n_user_tables
       write (iunit, '(a,a,a)') '  det no.  offset    l2     code     theta        phi         w_x ',&
            '        w_y         w_z         f_x         f_y         f_z         a_x         a_y',&
            '         a_z        det_1       det_2       det_3       det4'
       do i = 1, ndet
          idet = iwork(i)
          write (iunit, '(i9, f8.3, f10.5, i6, 13f12.5, f12.5, f12.0)')&
               detno(idet), delta(idet), l2(idet), code(idet),&
               theta(idet), phi(idet), w_x(idet), w_y(idet), w_z(idet), f_x(idet), f_y(idet), f_z(idet),&
               alpha_x(idet), alpha_y(idet), alpha_z(idet), det_1(idet), det_2(idet), det_3(idet), det_4(idet)
       end do
       istatus = shutfl (iunit)
    endif
    
    if(present(alpha_x_o))then
      call IXFalloc(alpha_x_o,ndet,status)
      alpha_x_o=alpha_x(1:ndet)
    endif
    if(present(alpha_y_o))then
      call IXFalloc(alpha_y_o,ndet,status)
      alpha_y_o=alpha_x(1:ndet)
    endif
    if(present(alpha_z_o))then
      call IXFalloc(alpha_z_o,ndet,status)
      alpha_z_o=alpha_z(1:ndet)
    endif    
    
    if(present(det_1_o))then
      call IXFalloc(det_1_o,ndet,status)
      det_1_o=det_1(1:ndet)
    endif
    if(present(det_2_o))then
      call IXFalloc(det_2_o,ndet,status)
      det_2_o=det_2(1:ndet)
    endif
    if(present(det_3_o))then
      call IXFalloc(det_3_o,ndet,status)
      det_3_o=det_3(1:ndet)
    endif
    if(present(det_4_o))then
      call IXFalloc(det_4_o,ndet,status)
      det_4_o=det_4(1:ndet)
    endif
    if(present(w_x_o))then
      call IXFalloc(w_x_o,ndet,status)
      w_x_o=w_x(1:ndet)
    endif
    if(present(w_y_o))then
      call IXFalloc(w_y_o,ndet,status)
      w_y_o=w_y(1:ndet)
    endif
    if(present(w_z_o))then
      call IXFalloc(w_z_o,ndet,status)
      w_z_o=w_z(1:ndet)
    endif
    if(present(phi_o))then
      call IXFalloc(phi_o,ndet,status)
      phi_o=phi(1:ndet)
    endif
    if(present(theta_o))then
      call IXFalloc(theta_o,ndet,status)
      theta_o=theta(1:ndet)
    endif
    if(present(l2_o))then
      call IXFalloc(l2_o,ndet,status)
      l2_o=l2(1:ndet)
    endif    
    if(present(delta_o))then
      call IXFalloc(delta_o,ndet,status)
      delta_o=delta(1:ndet)
    endif    
    if(present(code_o))then
      call IXFalloc(code_o,ndet,status)
      code_o=code(1:ndet)
    endif    
    if(present(l2_o))then
      call IXFalloc(l2_o,ndet,status)
      l2_o=l2(1:ndet)
    endif    
    if(present(detno_o))then
      call IXFalloc(detno_o,ndet,status)
      detno_o=detno(1:ndet)
    endif    

    if(present(ndet_o))ndet_o=ndet  
    
  end subroutine create_detector_data



  !-----------------------------------------------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------------------------------------------
  ! the following subroutines and functions will load properties of a detector.dat file and write it out if desired
  ! they are written mostly in FORTRAN 77 and are taken directly from TGP archives
  !-----------------------------------------------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------------------------------------------

  subroutine detector_table_main (data_file, calib_file, data_out_file, detpar_file, short_detpar_file, status)
    use IXMtools
    use IXMfileio
    use IXMsort

    integer ntype, n_user_tables, unity,ndet_totmax
    type(IXTstatus)::status
    parameter (ntype=8, n_user_tables=14, unity=1,ndet_totmax=1200000)

    integer ierr
    character*(*) data_file, calib_file, data_out_file, detpar_file, short_detpar_file

    ! Variables for detector.dat file:
    integer ndet, detno(ndet_totmax), code(ndet_totmax)
    real(dp):: delta(ndet_totmax), l2(ndet_totmax), theta(ndet_totmax), phi(ndet_totmax),w_x(ndet_totmax), w_y(ndet_totmax),&
         w_z(ndet_totmax),f_x(ndet_totmax), f_y(ndet_totmax), f_z(ndet_totmax),alpha_x(ndet_totmax), alpha_y(ndet_totmax),&
         alpha_z(ndet_totmax), det_1(ndet_totmax), det_2(ndet_totmax), det_3(ndet_totmax), det_4(ndet_totmax)

    ! Other variables:
    integer iu_data, iu_calib, iu_data_out, iunit, istatus, len_line, i, l_bank_type, l_bank_name, itype,&
         iwork(ndet_totmax), noff, ndet_bank, idet, ierr_write
    character*64 bank_type_list(ntype)
    character*255  trans_name, line, bank_type, bank_name, data_out_file_full

    data bank_type_list /'PSD_TUBE_PLANE', 'PSD_TUBE_STRIP', 'PSD_TUBE_GCIRCLE','TUBE_PLANE', 'TUBE_STRIP', 'TUBE_GCIRCLE',&
         'MONITOR', 'DUMMY'/

    ierr = OK
    iu_data=0
    iu_calib=0
    iu_data_out=0
    iunit=0
    istatus=0

    ! Get to start of a data block:
    ! =============================

    ! Open data file
    call IXFunitno (iu_data)
    !istatus = sys_trans_filename (data_file, trans_name)
    trans_name=IXFtranslate_read(data_file)	
    if (istatus .ne. 0) then
       call remark ('ERROR: Unable to translate data file name.')
       ierr=ERR
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       return
    endif
    istatus = oldfl (iu_data, trans_name, READ)
    if (istatus .ne. 0) then
       call remark ('ERROR: Unable to open data file.')
       ierr=ERR
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       return
    endif

    ! Open calibration file, if any
    if (lenstr(calib_file) .gt. 0) then
       call IXFunitno (iu_calib)
       !istatus = sys_trans_filename (calib_file, trans_name)
       trans_name=IXFtranslate_read(calib_file)			
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate calibration file name.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       istatus = oldfl (iu_calib, trans_name, READ)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to open calibration file.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
    else
       iu_calib=0
    endif

    ! Open output data file, if any
    if (lenstr(data_out_file) .gt. 0) then
       call IXFunitno (iu_data_out)
       !		istatus = sys_trans_filename (data_out_file, trans_name)
       trans_name=IXFtranslate_write(data_out_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate output data file name.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       istatus = newfl (iu_data_out, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to open output data file.')
          ierr=ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return
       endif
       inquire(iu_data_out,name=data_out_file_full)
    else
       iu_data_out=0
    endif

    ndet = 0
1000 continue
    noff = ndet + 1
    call read_line_write (iu_data, line, len_line, iu_data_out, .true., ierr_write)
    if (len_line .gt. 0) then
       i = 1
       l_bank_type = GETSTR (line, i, bank_type)
       l_bank_name = GETSTR (line, i, bank_name)
       itype = ICMD (bank_type_list, ntype, bank_type)
       if (itype .lt. 0) then
          call remark ('ERROR: Ambiguous detector bank type in data file.')
          if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
          ierr = ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return

       else if (itype .eq. 0) then
          call remark ('ERROR: Unrecognised detector bank type in data file.')
          if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
          ierr = ERR
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          return

          !   Get detector bank type:
          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_PLANE
       else if (itype .eq. 1) then
          call process_psd_tube_plane_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name,& 
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_STRIP
       else if (itype .eq. 2) then
          call process_psd_tube_strip_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	PSD_TUBE_GCIRCLE
       else if (itype .eq. 3) then
          call process_psd_tube_gcircle_data (iu_data, iu_calib, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_PLANE
       else if (itype .eq. 4) then
          call process_tube_plane_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_STRIP
       else if (itype .eq. 5) then
          call process_tube_strip_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	TUBE_GCIRCLE
       else if (itype .eq. 6) then
          call process_tube_gcircle_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	MONITOR
       else if (itype .eq. 7) then
          call process_monitor_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------
          !	DUMMY
       else if (itype .eq. 8) then
          call process_dummy_data (iu_data, iu_data_out, ndet_totmax-ndet, iwork, bank_name, &
               ndet_bank, detno(noff), delta(noff), l2(noff), code(noff), theta(noff), phi(noff),&
               w_x(noff), w_y(noff), w_z(noff), f_x(noff), f_y(noff), f_z(noff),&
               alpha_x(noff), alpha_y(noff), alpha_z(noff),&
               det_1(noff), det_2(noff), det_3(noff), det_4(noff), ierr)
          if (ierr .ne. 0) then
             if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             return
          endif
          ndet = ndet + ndet_bank

          !	--------------------------------------------------------------------------------------------

       else
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          call remark ('*** ERROR in source code. Contact T.G.Perring.')
          stop
       endif

       !	--------------------------------------------------------------------------------------------
    else if (len_line .eq. EOF) then
       goto 2000			! end-of-file reached
    else if (len_line .eq. ERR) then
       if (ierr_write .ne. OK) then
          call remark ('ERROR: Problem writing to output data file')
       else
          call remark ('ERROR: Problem reading from data file.')
       endif
       if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    else
       call remark ('ERROR: Unknown error reading from data file.')
       if (iu_data_out .ne. 0) istatus = sys_delete_file(data_out_file_full)
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    endif
    goto 1000

2000 continue
    istatus = shutfl (iu_data)
    if (iu_calib .ne. 0) istatus = shutfl(iu_calib)
    if (iu_data_out .ne. 0) istatus = shutfl(iu_data_out)

    ! Check entries for repeated detector numbers:
    ! ============================================
    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read from data file.')
       call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
       ierr = ERR
       return
    endif

    !	call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique in data file.')
             call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
             ierr = ERR
             return
          endif
       end do
    endif

    ! Write short format detector.dat file:
    ! ======================================
    if (lenstr(short_detpar_file) .gt. 0) then
       call IXFunitno (iunit)
       !istatus = sys_trans_filename (short_detpar_file, trans_name)
       trans_name=IXFtranslate_write(short_detpar_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate short format detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr = ERR
          return
       endif
       istatus = newfl (iunit, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to write short format detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr = ERR
          return
       endif

       write (iunit, '(''Short format DETECTOR.DAT generated by CREATE_DETECTOR_FILE'')')
       write (iunit, '(i8,i8)') ndet, unity
       write (iunit, '(a)')'  det no.  offset    l2     code     theta        phi'
       do i = 1, ndet
          idet = iwork(i)
          write (iunit, '(i9, f8.3, f10.5, i6, 2f12.5)') detno(idet), delta(idet), l2(idet), code(idet), theta(idet), phi(idet)
       end do
       istatus = shutfl (iunit)
    endif

    ! Write full format detector.dat file:
    ! ======================================
    if (lenstr(detpar_file) .gt. 0) then
       call IXFunitno (iunit)
       !		istatus = sys_trans_filename (detpar_file, trans_name)
       trans_name=IXFtranslate_write(detpar_file,IXC_WRITE)		
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to translate output detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr=ERR
          return
       endif
       istatus = newfl (iunit, trans_name, READWR)
       if (istatus .ne. 0) then
          call remark ('ERROR: Unable to write output detector.dat file.')
          call detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
          ierr=ERR
          return
       endif

       write (iunit, '(''Full format DETECTOR.DAT generated by CREATE_DETECTOR_FILE'')')
       write (iunit, '(i8,i8)') ndet, n_user_tables
       write (iunit, '(a,a,a)') '  det no.  offset    l2     code     theta        phi         w_x ',&
            '        w_y         w_z         f_x         f_y         f_z         a_x         a_y',&
            '         a_z        det_1       det_2       det_3       det4'
       do i = 1, ndet
          idet = iwork(i)
          write (iunit, '(i9, f8.3, f10.5, i6, 13f12.5, f12.5, f12.0)')&
               detno(idet), delta(idet), l2(idet), code(idet),&
               theta(idet), phi(idet), w_x(idet), w_y(idet), w_z(idet), f_x(idet), f_y(idet), f_z(idet),&
               alpha_x(idet), alpha_y(idet), alpha_z(idet), det_1(idet), det_2(idet), det_3(idet), det_4(idet)
          !			IF (L2(IDET) .LE. 1.0D-4) PRINT *, DETNO(IDET), L2(IDET)
          !			IF (ABS(THETA(IDET)) .GT. 180.0D0) PRINT *, DETNO(IDET), THETA(IDET)
       end do
       istatus = shutfl (iunit)
    endif

  end subroutine detector_table_main


  !--------------------------------------------------------
  subroutine detector_file_main_shutdown (iu_data, iu_calib, iu_data_out, iunit)
    !	Orderly shutdown of files, if open
    use IXMfileio
    integer iu_data, iu_calib, iu_data_out, iunit, istatus
    istatus = shutfl (iu_data)
    istatus = shutfl (iu_calib)
    istatus = shutfl (iu_data_out)
    istatus = shutfl (iunit)
    return
  end subroutine detector_file_main_shutdown
  !--------------------------------------------------------
  !--------------------------------------------------------

  subroutine process_dummy_data (iu_data, iu_data_out, ndet_max, iwork, name,   ndet, detno, delta, l2,&
       code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    use IXMsort
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_data_out, ndet_max, iwork(ndet_max)
    character*(*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    real(dp):: delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max)
    real(dp):: f_x(ndet_max), f_y(ndet_max), f_z(ndet_max)
    real(dp):: alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max)
    real(dp):: det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:

    integer l, i, n, nline,  lstr, iarray(4), nd, id, nrepeat, del_d, k, iline, ierr_write
    character*132 line, string


    ndet = 0
    ierr = 0

    ! Fill diagnostic message string:
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed dummy detector bank)', string)
       lstr = lenstr(string)
    endif

    ! Read no. of lines
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) then
       call remark ('ERROR: Problem writing output data file when reading bank: '//string(1:lstr))
       ierr = 1
       return
    endif
    if (l .gt. 0) then
       i = 1
       n = getis (line, i, iarray, 4)
       if (n .eq. 1) then
          if (n .gt. 0) then
             nline = iarray(1)
          else
             call remark ('ERROR: Must have positive no. of data entries in bank: '//string(1:lstr))
             call remark ('      line: '//line(1:l))
             ierr = 1
             return
          endif
       else
          call remark ('ERROR: Must give number of data entries in bank: '//string(1:lstr))
          call remark ('      line: '//line(1:l))
          ierr = 1
          return
       endif
    else
       call remark ('ERROR: No data entries for bank: '//string(1:lstr))
       ierr = 1
       return
    endif

    ! Read lines
    do iline = 1, nline
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) then
          call remark ('ERROR: Problem writing output data file when reading bank: '//string(1:lstr))
          ierr = 1
          return
       endif
       if (l .gt. 0) then
          i = 1
          n = getis (line, i, iarray, 4)
          if (n .eq. 1) then		! single detector
             ndet = ndet + 1
             if (ndet .gt. ndet_max) then
                call remark ('ERROR: Too many detectors for internal arrays in bank: '//string(1:lstr))
                call remark ('      line: '//line(1:l))
                ierr = 1
                return
             endif
             detno(ndet) = iarray(1)
          else if (n .eq. 2) then		! single run of sequential detector numbers
             nd = iarray(1)
             id = iarray(2)
             if (nd .le. 0) then
                call remark ('ERROR: No. detectors elements must be positive in bank: '//string(1:lstr))
                call remark ('      line: '//line(1:l))
                ierr = 1
                return
             endif
             do i = 1, nd
                ndet = ndet + 1
                if (ndet .gt. ndet_max) then
                   call remark ('ERROR: Too many detectors for internal arrays in bank: '//string(1:lstr))
                   call remark ('      line: '//line(1:l))
                   ierr = 1
                   return
                endif
                detno(ndet) = id
                id = id + 1
             end do
          else if (n .eq. 4) then		! multiple sets of sequential detector numbers
             nd = iarray(1)
             id = iarray(2)
             nrepeat = iarray(3)
             del_d = iarray(4)
             if (nd .le. 0) then
                call remark ('ERROR: No. detectors elements must be positive in bank: '//string(1:lstr))
                call remark ('      line: '//line(1:l))
                ierr = 1
                return
             endif
             if (nrepeat .le. 0) then
                call remark ('ERROR: No. repetitions must be positive in bank: '//string(1:lstr))
                call remark ('      line: '//line(1:l))
                stop
             endif
             do k = 1, nrepeat
                do i = 1, nd
                   ndet = ndet + 1
                   if (ndet .gt. ndet_max) then
                      call remark ('ERROR: Too many detectors for internal arrays in bank: '//string(1:lstr))
                      call remark ('      line: '//line(1:l))
                      ierr = 1
                      return
                   endif
                   detno(ndet) = id
                   id = id + 1
                end do
                id = id - nd + del_d
             end do
          else
             call remark ('ERROR: Uninterpretable line for bank: '//string(1:lstr))
             call remark ('      line: '//line(1:l))
             ierr = 1
             return
          endif
       else
          call remark ('ERROR: Error reading from data file, bank: '//string(1:lstr))
          ierr = 1
          return
       endif
    end do

    ! Calculate monitor parameters:
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed monitor)', string)
       lstr = lenstr(string)
    endif

    if (ndet_max .lt. 1) then
       call remark ('ERROR: No internal storage left for '//string(1:lstr))
       ierr = 1
       return
    endif

    do i = 1, ndet
       delta(i) = 0.0d0
       l2(i) = 0.001d0
       code(i) = 0
       theta(i) = 0.0d0
       phi(i) = 0.0d0
       w_x(1) = 0.0d0
       w_y(1) = 0.0d0
       w_z(1) = 0.0d0
       f_x(1) = 0.0d0
       f_y(1) = 0.0d0
       f_z(1) = 0.0d0
       alpha_x(1) = 0.0d0
       alpha_y(1) = 0.0d0
       alpha_z(1) = 0.0d0
       det_1(1) = 0.0d0
       det_2(1) = 0.0d0
       det_3(1) = 0.0d0
       det_4(1) = 0.0d0
    end do

    !
    ! Check that detector numbers are unique (and warn if not 1:1 mapping to spectra):

    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read '//string(1:lstr))
       ierr = 1
       return
    endif

    !	call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)

    if (detno(iwork(1)) .le. 0) then
       call remark ('ERROR: Zero or negative detector number '//string(1:lstr))
       ierr = 1
       return
    endif
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique '//string(1:lstr))
             ierr = 1
             return
          endif
       end do
    endif

    return
  end subroutine process_dummy_data

  subroutine process_monitor_data (iu_data, iu_data_out, ndet_max, iwork, name,& 
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    real(dp)::delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max)
    real(dp)::    f_x(ndet_max), f_y(ndet_max), f_z(ndet_max)
    real(dp)::        alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max)
    real(dp)::    det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:

    integer lstr, detno_in, code_in
    double precision l2_in, width, height, thick, xsect_mac, delay, dead
    character*132 string

    ndet = 0
    ierr = 0

    ! Read monitor data:
    call READ_MONITOR_DATA (iu_data,iu_data_out,name,l2_in,detno_in,code_in,width,height,thick,xsect_mac,delay,dead,ierr)
    if (ierr .ne. 0) return

    ! Calculate monitor parameters:
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed monitor)', string)
       lstr = lenstr(string)
    endif

    if (ndet_max .lt. 1) then
       call remark ('ERROR: No internal storage left for '//string(1:lstr))
       ierr = 1
       return
    endif

    ndet = 1
    detno(1) = detno_in
    delta(1) = delay
    l2(1) = l2_in
    code(1) = code_in
    if (l2(1) .ge. 0.0d0) then
       theta(1) = 0.0d0
    else
       theta(1) = 180.0d0
    endif
    phi(1) = 0.0d0
    w_x(1) = width
    w_y(1) = height
    w_z(1) = thick
    f_x(1) = width
    f_y(1) = height
    f_z(1) = thick
    alpha_x(1) = 0.0d0
    alpha_y(1) = 0.0d0
    alpha_z(1) = 0.0d0
    det_1(1) = dead
    det_2(1) = xsect_mac
    det_3(1) = 0.0d0
    det_4(1) = 0.0d0

  end subroutine process_monitor_data

  subroutine READ_MONITOR_DATA (iu_data, iu_data_out, name, l2, detno, code, width, height, thick, xsect_mac, delay, dead, ierr)
    use IXMtools
    integer iu_data, iu_data_out, detno, code, ierr, ierr_write
    double precision l2, width, height, thick, xsect_mac, delay, dead
    character(len=*) name

    integer i, l
    character*255 line

    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) l2, detno

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) width, height, thick, xsect_mac, delay, dead

    return

900 call detector_message ('ERROR: Problem writing output data file.', 'MONITOR', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'MONITOR', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_MONITOR_DATA

  subroutine process_psd_tube_gcircle_data (iu_data, iu_calib, iu_data_out, ndet_max, iwork, name,&
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    !	include 'tables.par'
    !
    ! Reads data and calculates entries for detector.dat for a great circle of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_calib, iu_data_out, ndet_max, iwork(ndet_max)
    character*(*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:
    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    double precision l2_in, phi_circle, theta_circle, diameter, length, he_pressure, wall, delay, dead,&
         beta_init(ngroup_max), del_beta(ngroup_max), calib(ncalib_par_max,ncalib_entry_max)
    logical use_calib

    ndet = 0
    ierr = 0

    call READ_PSD_TUBE_GCIRCLE_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, l2_in, phi_circle, theta_circle,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, beta_init, del_beta, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, ncalib_entry_max, calib, ierr)
    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_GCIRCLE_DATA (ndet_max, iwork, name, l2_in, phi_circle, theta_circle,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, beta_init, del_beta, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

  end subroutine process_psd_tube_gcircle_data

  subroutine READ_PSD_TUBE_GCIRCLE_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, l2, phi_circle, theta_circle,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, beta_init, del_beta,&
       npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, ntube_max, calib, ierr)
    use IXMtools
    integer(i4b):: iu_data, iu_calib, iu_data_out, ngroup_max, code, ngroup, ntube(ngroup_max), npix(ngroup_max),&
	 detno_init(ngroup_max), del_detno(ngroup_max),index_init(ngroup_max), del_index(ngroup_max), ncalib_par_max,&
         ntube_max, ierr, ierr_write
    real(dp):: l2, phi_circle, theta_circle, diameter, length, he_pressure, wall, delay, dead,&
         beta_init(ngroup_max), del_beta(ngroup_max), calib(ncalib_par_max,ntube_max)
    logical use_calib
    character*(*) name

    integer(i4b):: i, l, j, nvals, imatch, igroup, itube
    logical calibrated
    character*255 line, str


    ! Read header block
    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) l2, phi_circle, theta_circle

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'PSD_TUBE_GCIRCLE', name)
       ierr = 1
       return
    endif

    ! Read number of groups and calibration status
    call read_line_write (iu_data, line, l, iu_data_out, .false., ierr_write)
    if (ierr_write .ne. 0) goto 900
    if (l .le. 0) goto 999
    i=1
    nvals=geti(line,i,ngroup)
    if (nvals .ne. 1) goto 999	! ngroup not read

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'PSD_TUBE_GCIRCLE', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'PSD_TUBE_GCIRCLE', name)
       ierr = 1
       return
    endif

    l=getstr(line,i,str)
    if (l .gt. 0) then	! string read
       call locase(str)
       imatch=icmd((/ 'calibrated' /),1,str)
       if (imatch .le. 0) then
          call detector_message ('ERROR: Unrecognised option following number of groups.','PSD_TUBE_GCIRCLE',name)
          ierr = 1
          return
       endif
       calibrated=.true.
    else
       calibrated=.false.
    endif

    if (iu_data_out .ne. 0) then
       if (iu_calib .eq. 0) then	! no calibration file
          ierr_write = putlf(iu_data_out,line)
       else
          i=itoc(ngroup,line)
          line(9:18)='calibrated'
          ierr_write = putlf(iu_data_out,line)
       endif
       if (ierr_write .ne. 0) goto 900
    endif

    use_calib = calibrated .or. (iu_calib .ne. 0)	! use calibration array after return

    ! Loop over the detector groups
    itube = 0
    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) ntube(igroup), beta_init(igroup), del_beta(igroup)
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'PSD_TUBE_GCIRCLE', name)
          ierr = 1
          return
       endif

       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) npix(igroup),detno_init(igroup),del_detno(igroup),index_init(igroup),del_index(igroup)
       if (npix(igroup) .eq. 0) then
          call detector_message ('ERROR: No. pixels must be non-zero for all entries.', 'PSD_TUBE_GCIRCLE', name)
          ierr = 1
          return
       endif

       if (calibrated .or. (iu_calib .ne. 0)) then		! indicates calibration parameters are to be read
          if (itube+ntube(igroup) .gt. ntube_max) then
             call detector_message ('ERROR: No. tubes in detector bank too large.', 'PSD_TUBE_GCIRCLE', name)
             ierr = 1
             return
          endif
          do j=1, ntube(igroup)
             itube = itube + 1
             if (iu_calib .ne. 0) then	! read from calibration file
                if (calibrated) call read_line (iu_data, line, l)
                if (l .le. 0) goto 999
                call read_line_write (iu_calib, line, l, iu_data_out, .true., ierr_write)
             else						! read from input file
                call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
             endif
             if (ierr_write .ne. 0) goto 900
             if (l .le. 0) goto 999
             i=1
             nvals=getds(line,i,calib(:,itube),ncalib_par_max)
             if (nvals .eq. 0) goto 999
          end do
       endif

    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'PSD_TUBE_GCIRCLE', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'PSD_TUBE_GCIRCLE', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_PSD_TUBE_GCIRCLE_DATA
  subroutine CALC_PSD_TUBE_GCIRCLE_DATA (ndet_max, iwork, name, l2_in, phi_circle, theta_circle,&
       code_in, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, beta_init, del_beta, npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, calib,&
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    use IXMmaths_utils
    use IXMmaths_basis
    use IXMsort
    !
    !  Calculates detector.dat parameters for a great circle of position sensitive detectors.
    !  If all OK, then IERR=0, otherwise IERR=1
    !  Variables obvious, except: 
    !  Input:
    !     ndet_max    maximum no. of detectors that can be stored
    !  Output:
    !     ndet        total no. detectors added by the subroutine
    !

    ! input variables:
    integer ndet_max, iwork(ndet_max), code_in, ngroup, ntube(ngroup), npix(ngroup), detno_init(ngroup), del_detno(ngroup),&
         index_init(ngroup), del_index(ngroup), ncalib_par_max
    double precision l2_in, phi_circle, theta_circle, diameter, length, he_pressure, wall, delay, dead,&
         beta_init(ngroup), del_beta(ngroup), calib(ncalib_par_max,*)
    logical use_calib
    character*(*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! parameters:
    double precision small 
    parameter (small = 1.0d-10)

    ! internal variables:
    integer i, igroup,  lstr, itube, jtube, ipix, jpix, kpix
    double precision beta, beta_mat(3,3), theta_mat(3,3), phi_mat(3,3), temp_mat(3,3), amat(3,3), xdet(3),&
         xspec(3), bmat(3,3), cmat(3,3), sig, norm, nmat(3,3), rotvec(3), pix_pos, pix_len
    character*132 string


    ierr = 0
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed detector bank)', string)
       lstr = lenstr(string)
    endif

    !
    ! Loop for each detector and each pixel within a detector:
    jtube=0
    jpix = 0
    do igroup = 1, ngroup
       do itube = 1, ntube(igroup)
          jtube = jtube + 1
          beta = beta_init(igroup) + del_beta(igroup)*dble(itube-1)
          !implicit conversion of degrees to radians in the call
          call IXFrotation_matrix_y (-beta/rad_to_deg_dp, beta_mat)
          call IXFrotation_matrix_x (-theta_circle/rad_to_deg_dp, theta_mat)
          call IXFrotation_matrix_z (phi_circle/rad_to_deg_dp-0.5d0*pi_dp, phi_mat)
          !              call mmmult (phi_mat, theta_mat, 3, 3, 3, temp_mat)
          temp_mat=matmul(phi_mat,theta_mat)				
          !              call mmmult (temp_mat, beta_mat, 3, 3, 3, amat)
          amat=matmul(temp_mat,beta_mat)				
          do ipix = 1, abs(npix(igroup))
             jpix = jpix + 1
             if (jpix .gt. ndet_max) then
                call remark ('ERROR: Too many detectors for internal storage '//string(1:lstr))
                ierr = 1
                return
             endif
             detno(jpix)  = detno_init(igroup) + del_detno(igroup)*(itube-1) + sign(ipix-1,npix(igroup))
             delta(jpix)  = delay

             xdet(1) = 0.0d0
             if (use_calib) then
                kpix=(abs(npix(igroup))-npix(igroup)+1+sign(2*ipix-1,npix(igroup)))/2 ! pixel no. from low pixel number end
                call psd_tube_position(kpix, abs(npix(igroup)), calib(1,jtube), pix_pos, pix_len )
                xdet(2)=sign(1,npix(igroup))*pix_pos
             else
                xdet(2) = length*(dble(2*ipix-1-abs(npix(igroup)))/dble(2*abs(npix(igroup))))
                pix_len = length / dble(abs(npix(igroup)))
             endif
             xdet(3) = l2_in
             !call mvmult (amat, xdet, 3, 3, xspec)
             xspec=matmul(amat,xdet)				                  
             l2(jpix) = IXFnorm(xspec)
             code(jpix) = code_in
             if (sqrt(xspec(1)**2+xspec(2)**2) .gt. small) then
                theta(jpix) = atan2(sqrt(xspec(1)**2+xspec(2)**2), xspec(3)) * rad_to_deg_dp
                phi(jpix)= atan2(xspec(2),xspec(1)) * rad_to_deg_dp
             else
                theta(jpix) = atan2(0.0d0, xspec(1)) * rad_to_deg_dp  ! in-beam monitor
                phi(jpix)= 0.0d0
             endif
             w_x(jpix) = diameter
             w_y(jpix) = diameter
             w_z(jpix) = pix_len
             f_x(jpix) = 2.0d0*l2_in*tan(0.5d0*abs(del_beta(igroup))/rad_to_deg_dp)
             f_y(jpix) = 2.0d0*l2_in*tan(0.5d0*abs(del_beta(igroup))/rad_to_deg_dp)
             f_z(jpix) = pix_len

             !         construct natural detector frame:
             call IXFtrans_spherical_polars (phi(jpix)/rad_to_deg_dp, theta(jpix)/rad_to_deg_dp, bmat)  ! spec coords to sphere coords
             !                  call mmmult (bmat, amat, 3, 3, 3, cmat)     ! matrix to transform from det-plane to sphere coords
             cmat=matmul(bmat,amat)
             sig = sign(1.0d0,cmat(2,2))
             norm = sqrt(cmat(1,2)**2 + cmat(2,2)**2)
             nmat(1,1) = sig*cmat(2,2)/norm
             nmat(1,2) =-sig*cmat(1,2)/norm
             nmat(1,3) = 0.0d0
             nmat(2,1) = sig*cmat(1,2)*cmat(3,2)/norm
             nmat(2,2) = sig*cmat(2,2)*cmat(3,2)/norm
             nmat(2,3) =-sig*norm
             nmat(3,1) = cmat(1,2)
             nmat(3,2) = cmat(2,2)
             nmat(3,3) = cmat(3,2)

             !                  call rotmat_to_rotvec (nmat, rotvec)
             !                  alpha_x(jpix) = rotvec(1)
             !                  alpha_y(jpix) = rotvec(2)
             !                  alpha_z(jpix) = rotvec(3)
             ! this will give rotvec in radians -> to reproduce old stylee tables need to convert it back to degrees
             rotvec= IXFrotmat_to_rotvec (nmat)
             alpha_x(jpix) = rotvec(1)*rad_to_deg_dp
             alpha_y(jpix) = rotvec(2)*rad_to_deg_dp
             alpha_z(jpix) = rotvec(3)*rad_to_deg_dp

             if (code_in .eq. 2) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = 0.0d0
             else if (code_in .eq. 3) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = index_init(igroup) + del_index(igroup)*(itube-1)
             else
                call remark ('ERROR: Invalid CODE type in detector bank '//string(1:lstr))
                ierr = 1
                return
             endif
          end do
       end do
    end do
    ndet = jpix

    !
    ! Check that detector numbers are unique (and warn if not 1:1 mapping to spectra):

    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read '//string(1:lstr))
       ierr = 1
       return
    endif

    !call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)      
    if (detno(iwork(1)) .le. 0) then
       call remark ('ERROR: Zero or negative detector number '//string(1:lstr))
       ierr = 1
       return
    endif
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique '//string(1:lstr))
             ierr = 1
             return
          endif
       end do
    endif

    return
  end subroutine CALC_PSD_TUBE_GCIRCLE_DATA

  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine process_psd_tube_plane_data (iu_data, iu_calib, iu_data_out, ndet_max, iwork, name, &
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    !	include 'tables.par'
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_calib, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*):: name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    real(dp):: delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max), alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:
    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    real(dp):: x0_in(3), xu_in(3), xv_in(3), diameter, length, he_pressure, wall, delay, dead,&
         dist(ngroup_max), h(ngroup_max), pitch(ngroup_max), calib(ncalib_par_max,ncalib_entry_max)
    logical use_calib

    ndet = 0
    ierr = 0

    call READ_PSD_TUBE_PLANE_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, x0_in, xu_in, xv_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, dist, h, pitch, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, ncalib_entry_max, calib, ierr)
    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_PLANE_DATA (ndet_max, iwork, name, x0_in, xu_in, xv_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, dist, h, pitch, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

    return
  end subroutine process_psd_tube_plane_data
  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine READ_PSD_TUBE_PLANE_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, x0, xu, xv,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, dist, h, pitch, npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, ntube_max, calib, ierr)
    use IXMtools
    integer iu_data, iu_calib, iu_data_out,  ngroup_max, code, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max),&
	 del_detno(ngroup_max), index_init(ngroup_max), del_index(ngroup_max), ncalib_par_max, ntube_max, ierr, ierr_write
    real(dp):: x0(3), xu(3), xv(3), diameter, length, he_pressure, wall, delay, dead,&
         dist(ngroup_max), h(ngroup_max), pitch(ngroup_max), calib(ncalib_par_max,ntube_max)
    logical use_calib
    character(len=*) name

    integer i, l, j, nvals, imatch, igroup, itube
    logical calibrated
    character*255 line, str

    ! Read header block
    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (x0(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (xu(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (xv(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'PSD_TUBE_PLANE', name)
       ierr = 1
       return
    endif

    ! Read number of groups and calibration status
    call read_line_write (iu_data, line, l, iu_data_out, .false., ierr_write)
    if (ierr_write .ne. 0) goto 900
    if (l .le. 0) goto 999
    i=1
    nvals=geti(line,i,ngroup)
    if (nvals .ne. 1) goto 999	! ngroup not read

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'PSD_TUBE_GCIRCLE', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'PSD_TUBE_GCIRCLE', name)
       ierr = 1
       return
    endif

    l=getstr(line,i,str)
    if (l .gt. 0) then	! string read
       call locase(str)
       imatch=icmd((/ 'calibrated' /),1,str)
       if (imatch .le. 0) then
          call detector_message ('ERROR: Unrecognised option following number of groups.','PSD_TUBE_GCIRCLE',name)
          ierr = 1
          return
       endif
       calibrated=.true.
    else
       calibrated=.false.
    endif

    if (iu_data_out .ne. 0) then
       if (iu_calib .eq. 0) then	! no calibration file
          ierr_write = putlf(iu_data_out,line)
       else
          i=itoc(ngroup,line)
          line(9:18)='calibrated'
          ierr_write = putlf(iu_data_out,line)
       endif
       if (ierr_write .ne. 0) goto 900
    endif

    use_calib = calibrated .or. (iu_calib .ne. 0)	! use calibration array after return

    ! Loop over the detector groups
    itube = 0
    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) ntube(igroup), dist(igroup), h(igroup), pitch(igroup)
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'PSD_TUBE_PLANE', name)
          ierr = 1
          return
       endif

       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) npix(igroup),detno_init(igroup),del_detno(igroup),index_init(igroup),del_index(igroup)
       if (npix(igroup) .eq. 0) then
          call detector_message ('ERROR: No. pixels must be non-zero for all entries.', 'PSD_TUBE_PLANE', name)
          ierr = 1
          return
       endif

       if (calibrated .or. (iu_calib .ne. 0)) then		! indicates calibration parameters are to be read
          if (itube+ntube(igroup) .gt. ntube_max) then
             call detector_message ('ERROR: No. tubes in detector bank too large.', 'PSD_TUBE_PLANE', name)
             ierr = 1
             return
          endif
          do j=1, ntube(igroup)
             itube = itube + 1
             if (iu_calib .ne. 0) then	! read from calibration file
                if (calibrated) call read_line (iu_data, line, l)
                if (l .le. 0) goto 999
                call read_line_write (iu_calib, line, l, iu_data_out, .true., ierr_write)
             else						! read from input file
                call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
             endif
             if (ierr_write .ne. 0) goto 900
             if (l .le. 0) goto 999
             i=1
             nvals=getds(line,i,calib(:,itube),ncalib_par_max)
             if (nvals .eq. 0) goto 999
          end do
       endif

    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'PSD_TUBE_PLANE', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'PSD_TUBE_PLANE', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_PSD_TUBE_PLANE_DATA
  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine CALC_PSD_TUBE_PLANE_DATA (ndet_max, iwork, name, x0_in, xu_in, xv_in,&
       code_in, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, dist, h, pitch, npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, calib,&
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    use IXMmaths_basis
    use IXMsort
    !
    !  Calculates detector.dat parameters for a plane of position sensitive detectors.
    !  If all OK, then IERR=0, otherwise IERR=1
    !  Variables obvious, except: 
    !  Input:
    !	ndet_max	maximum no. of detectors that can be stored
    !  Output:
    !	ndet		total no. detectors added by the subroutine
    !

    ! input variables:
    integer ndet_max, iwork(ndet_max), code_in, ngroup, ntube(ngroup), npix(ngroup), detno_init(ngroup), del_detno(ngroup),&
         index_init(ngroup), del_index(ngroup), ncalib_par_max
    double precision x0_in(3), xu_in(3), xv_in(3), diameter, length, he_pressure, wall, delay, dead,&
         dist(ngroup), h(ngroup), pitch(ngroup), calib(ncalib_par_max,*)
    logical use_calib
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! parameters:
    double precision small
    parameter (small = 1.0d-10)

    ! internal variables:
    integer i, igroup,  lstr, itube, jtube, ipix, jpix, kpix
    double precision x0(3), xu(3), xv(3),  cosang, cotang, e1(3), e2(3), e3(3), vlen, amat(3,3), xdet(3),&
         xspec(3), bmat(3,3), cmat(3,3), sig, norm, nmat(3,3), rotvec(3), pix_pos, pix_len
    character*132 string

    ierr = 0
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed detector bank)', string)
       lstr = lenstr(string)
    endif

    !
    ! Internal work done in frame with z axis along Ki
    do i = 1, 3
       x0(i) = x0_in(mod(i,3)+1)
       xu(i) = xu_in(mod(i,3)+1)
       xv(i) = xv_in(mod(i,3)+1)
    end do

    !
    ! Check definition of detector plane:
    cosang = IXFdot(xu,xv)/(IXFnorm(xu)*IXFnorm(xv))
    if (abs(cosang) .lt. 1.0d0-small) then
       cotang = tan(asin(cosang))
    else
       call remark ('ERROR: Check definition of plane in bank '//string(1:lstr))
       ierr = 1
       return
    endif
    !
    ! Calculate transformation matrix (xspec(i) = A(i,j)*xdet_plane(j)) [e3 || length, e1 in plane of detectors]
    !	call vnorm (xv, e2, vlen)
    !	call vvprod (xu, xv, e3)
    !	call vnorm (e3, e3, vlen)
    !	call vvprod (e2, e3, e1)
    !	call vnorm (e1, e1, vlen)
    call IXFvnorm (xv, e2, vlen)
    e3= IXFcross (xu, xv)
    call IXFvnorm (e3, e3, vlen)
    e1= IXFcross (e2, e3)
    call IXFvnorm (e1, e1, vlen)	
    do i = 1, 3
       amat(i,1) = e1(i)
       amat(i,2) = e2(i)
       amat(i,3) = e3(i)
    end do

    !
    ! Loop for each detector and each pixel within a detector:
    jtube = 0
    jpix = 0
    do igroup = 1, ngroup
       do itube = 1, ntube(igroup)
          jtube = jtube + 1
          do ipix = 1, abs(npix(igroup))
             jpix = jpix + 1
             if (jpix .gt. ndet_max) then
                call remark ('ERROR: Too many detectors for internal storage '//string(1:lstr))
                ierr = 1
                return
             endif
             detno(jpix)  = detno_init(igroup) + del_detno(igroup)*(itube-1) + sign(ipix-1,npix(igroup))
             delta(jpix)  = delay
             xdet(1) = h(igroup) + pitch(igroup)*dble(itube-1)
             if (use_calib) then
                kpix=(abs(npix(igroup))-npix(igroup)+1+sign(2*ipix-1,npix(igroup)))/2 ! pixel no. from low pixel number end
                call psd_tube_position(kpix, abs(npix(igroup)), calib(1,jtube), pix_pos, pix_len )
                xdet(2)=sign(1,npix(igroup))*pix_pos
             else
                xdet(2) = length*(dble(2*ipix-1-abs(npix(igroup)))/dble(2*abs(npix(igroup))))
                pix_len = length / dble(abs(npix(igroup)))
             endif
             xdet(2) = dist(igroup) + xdet(1)*cotang + xdet(2)
             xdet(3) = 0.0d0
             !call mvmult (amat, xdet, 3, 3, xspec)
             xspec=matmul(amat,xdet)								
             xspec(1) = x0(1) + xspec(1)
             xspec(2) = x0(2) + xspec(2)
             xspec(3) = x0(3) + xspec(3)
             l2(jpix) = IXFnorm(xspec)
             code(jpix) = code_in
             if (sqrt(xspec(1)**2+xspec(2)**2) .gt. small) then
                theta(jpix) = atan2(sqrt(xspec(1)**2+xspec(2)**2), xspec(3)) * rad_to_deg_dp
                phi(jpix)= atan2(xspec(2),xspec(1)) * rad_to_deg_dp
             else
                theta(jpix) = atan2(0.0d0, xspec(1)) * rad_to_deg_dp	! in-beam monitor
                phi(jpix)= 0.0d0
             endif
             w_x(jpix) = diameter
             w_y(jpix) = diameter
             w_z(jpix) = pix_len
             f_x(jpix) = abs(pitch(igroup))
             f_y(jpix) = abs(pitch(igroup))
             f_z(jpix) = pix_len

             !		construct natural detector frame:
             call IXFtrans_spherical_polars (phi(jpix)/rad_to_deg_dp, theta(jpix)/rad_to_deg_dp, bmat)	! spec coords to sphere coords
             !				call mmmult (bmat, amat, 3, 3, 3, cmat)		! matrix to transform from det-plane to sphere coords
             cmat=matmul(bmat,amat)
             sig = sign(1.0d0,cmat(2,2))
             norm = sqrt(cmat(1,2)**2 + cmat(2,2)**2)
             nmat(1,1) = sig*cmat(2,2)/norm
             nmat(1,2) =-sig*cmat(1,2)/norm
             nmat(1,3) = 0.0d0
             nmat(2,1) = sig*cmat(1,2)*cmat(3,2)/norm
             nmat(2,2) = sig*cmat(2,2)*cmat(3,2)/norm
             nmat(2,3) =-sig*norm
             nmat(3,1) = cmat(1,2)
             nmat(3,2) = cmat(2,2)
             nmat(3,3) = cmat(3,2)

             !                  call rotmat_to_rotvec (nmat, rotvec)
             !                  alpha_x(jpix) = rotvec(1)
             !                  alpha_y(jpix) = rotvec(2)
             !                  alpha_z(jpix) = rotvec(3)
             ! this will give rotvec in radians -> to reproduce old stylee tables need to convert it back to degrees
             rotvec= IXFrotmat_to_rotvec (nmat)
             alpha_x(jpix) = rotvec(1)*rad_to_deg_dp
             alpha_y(jpix) = rotvec(2)*rad_to_deg_dp
             alpha_z(jpix) = rotvec(3)*rad_to_deg_dp

             if (code_in .eq. 2) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = 0.0d0
             else if (code_in .eq. 3) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = index_init(igroup) + del_index(igroup)*(itube-1)
             else
                call remark ('ERROR: Invalid CODE type in detector bank '//string(1:lstr))
                ierr = 1
                return
             endif
          end do
       end do
    end do
    ndet = jpix

    !
    ! Check that detector numbers are unique (and warn if not 1:1 mapping to spectra):

    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read '//string(1:lstr))
       ierr = 1
       return
    endif

    !	call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)
    if (detno(iwork(1)) .le. 0) then
       call remark ('ERROR: Zero or negative detector number '//string(1:lstr))
       ierr = 1
       return
    endif
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique '//string(1:lstr))
             ierr = 1
             return
          endif
       end do
    endif

    return
  end subroutine CALC_PSD_TUBE_PLANE_DATA
  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine process_psd_tube_strip_data (iu_data, iu_calib, iu_data_out, ndet_max, iwork, name, &
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_calib, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:

    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    double precision l2_in, phi_in, diameter, length, he_pressure, wall, delay, dead,&
         theta_init(ngroup_max), del_theta(ngroup_max), type_par(ngroup_max), calib(ncalib_par_max,ncalib_entry_max)
    logical use_calib
    character*20 type(ngroup_max)

    ndet = 0
    ierr = 0

    call READ_PSD_TUBE_STRIP_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, l2_in, phi_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, theta_init, del_theta, type, type_par,&
         npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, ncalib_entry_max, calib, ierr)

    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_STRIP_DATA (ndet_max, iwork, name, l2_in, phi_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, theta_init, del_theta, type, type_par,&
         npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

    return
  end subroutine process_psd_tube_strip_data
  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine READ_PSD_TUBE_STRIP_DATA (iu_data, iu_calib, iu_data_out, ngroup_max, name, l2, phi,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, theta_init, del_theta, type, type_par,&
       npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, ntube_max, calib, ierr)
    use IXMtools
    integer iu_data, iu_calib, iu_data_out, &
         ngroup_max, code, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max), ncalib_par_max, ntube_max, ierr, ierr_write
    double precision l2, phi, diameter, length, he_pressure, wall, delay, dead,&
         theta_init(ngroup_max), del_theta(ngroup_max), type_par(ngroup_max), calib(ncalib_par_max,ntube_max)
    logical use_calib
    character(len=*):: name, type(ngroup_max)

    integer i, l, j, nvals, imatch, igroup, itube, iwrd
    double precision arr(3)
    logical calibrated
    character*10 wrd
    character*255 line, str


    ! Read header block
    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) l2, phi

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'PSD_TUBE_STRIP', name)
       ierr = 1
       return
    endif

    ! Read number of groups and calibration status
    call read_line_write (iu_data, line, l, iu_data_out, .false., ierr_write)
    if (ierr_write .ne. 0) goto 900
    if (l .le. 0) goto 999
    i=1
    nvals=geti(line,i,ngroup)
    if (nvals .ne. 1) goto 999	! ngroup not read

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'PSD_TUBE_STRIP', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'PSD_TUBE_STRIP', name)
       ierr = 1
       return
    endif

    l=getstr(line,i,str)
    if (l .gt. 0) then	! string read
       call locase(str)
       imatch=icmd((/ 'calibrated' /),1,str)
       if (imatch .le. 0) then
          call detector_message ('ERROR: Unrecognised option following number of groups.','PSD_TUBE_STRIP',name)
          ierr = 1
          return
       endif
       calibrated=.true.
    else
       calibrated=.false.
    endif

    if (iu_data_out .ne. 0) then
       if (iu_calib .eq. 0) then	! no calibration file
          ierr_write = putlf(iu_data_out,line)
       else
          i=itoc(ngroup,line)
          line(9:18)='calibrated'
          ierr_write = putlf(iu_data_out,line)
       endif
       if (ierr_write .ne. 0) goto 900
    endif

    use_calib = calibrated .or. (iu_calib .ne. 0)	! use calibration array after return

    ! Loop over the detector groups
    itube=0
    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900

       if (l .le. 0) goto 999

       i = 1
       nvals = getds (line, i, arr, 3)
       if (nvals .ne. 3) goto 999
       ntube(igroup) = nint(arr(1))
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'PSD_TUBE_STRIP', name)
          ierr = 1
          return
       endif
       theta_init(igroup) = arr(2)
       del_theta(igroup) = arr(3)

       iwrd = inext(line,i)
       if (iwrd.gt.len(line)) then
          type(igroup) = 'PLANE'
          type_par(igroup) = 0.0d0
       else if (line(iwrd:iwrd) .eq. '!') then
          type(igroup) = 'PLANE'
          type_par(igroup) = 0.0d0
       else
          l = getwrd (line, i, wrd)
          if (l .le. 0) goto 999
          call upcase (wrd)
          if (wrd(1:l) .eq. 'ARC') then
             type(igroup) = 'ARC'
          else if (wrd(1:l) .eq. 'PLANE') then
             type(igroup) = 'PLANE'
          else if (wrd(1:l) .eq. 'DSTRIP') then
             type(igroup) = 'DSTRIP'
          else if (wrd(1:l) .eq. 'DEBYE') then
             type(igroup) = 'DEBYE'
          else
             goto 999
          endif
          nvals = getd (line, i,type_par(igroup))
          if (nvals .ne. 1) goto 999
       endif

       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) npix(igroup),detno_init(igroup),del_detno(igroup),index_init(igroup),del_index(igroup)
       if (npix(igroup) .eq. 0) then
          call detector_message ('ERROR: No. pixels must be non-zero for all entries.', 'PSD_TUBE_STRIP', name)
          ierr = 1
          return
       endif

       if (calibrated .or. (iu_calib .ne. 0)) then		! indicates calibration parameters are to be read
          if (itube+ntube(igroup) .gt. ntube_max) then
             call detector_message ('ERROR: No. tubes in detector bank too large.', 'PSD_TUBE_STRIP', name)
             ierr = 1
             return
          endif
          do j=1, ntube(igroup)
             itube = itube + 1
             if (iu_calib .ne. 0) then	! read from calibration file
                if (calibrated) call read_line (iu_data, line, l)
                if (l .le. 0) goto 999
                call read_line_write (iu_calib, line, l, iu_data_out, .true., ierr_write)
             else						! read from input file
                call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
             endif
             if (ierr_write .ne. 0) goto 900
             if (l .le. 0) goto 999
             i=1
             nvals=getds(line,i,calib(:,itube),ncalib_par_max)
             if (nvals .eq. 0) goto 999
          end do
       endif

    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'PSD_TUBE_STRIP', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'PSD_TUBE_STRIP', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_PSD_TUBE_STRIP_DATA
  !C---------------------------------------------------------------------
  !C---------------------------------------------------------------------

  subroutine CALC_PSD_TUBE_STRIP_DATA (ndet_max, iwork, name, l2_in, phi_in,&
       code_in, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, theta_init, del_theta, type, type_par,&
       npix, detno_init, del_detno, index_init, del_index,&
       use_calib, ncalib_par_max, calib,&
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    use IXMsort
    !
    !  Calculates detector.dat parameters for a plane of position sensitive detectors.
    !  If all OK, then IERR=0, otherwise IERR=1
    !  Variables obvious, except: 
    !  Input:
    !	ndet_max	maximum no. of detectors that can be stored
    !  Output:
    !	ndet		total no. detectors added by the subroutine
    !

    ! input variables:
    integer ndet_max, iwork(ndet_max), code_in, ngroup, ntube(ngroup), npix(ngroup), detno_init(ngroup), del_detno(ngroup),&
         index_init(ngroup), del_index(ngroup), ncalib_par_max
    double precision l2_in, phi_in, diameter, length, he_pressure, wall, delay, dead,&
         theta_init(ngroup), del_theta(ngroup), type_par(ngroup), calib(ncalib_par_max,*)
    logical use_calib
    character(len=*) name, type(ngroup)

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! parameters:
    double precision small
    parameter (small = 1.0d-10)

    ! internal variables:
    integer i, igroup,  lstr, itube, jtube, ipix, jpix, kpix
    double precision theta_tube, dphi, phi_mat(3,3), theta_mat(3,3),  beta_mat(3,3), temp_mat(3,3), amat(3,3), xdet(3),&
         xspec(3), bmat(3,3),  vlen, cmat(3,3), norm, sig, nmat(3,3), rotvec(3), pix_pos, pix_len
    character*132 string


    ierr = 0
    if (lenstr(name) .gt. 0) then
       call ctoc (name, string)
       call upcase (string)
       lstr = lenstr(string)
    else
       call ctoc ('(unnamed detector bank)', string)
       lstr = lenstr(string)
    endif

    !
    ! Loop for each detector and each pixel within a detector:
    ! (Internal work done in frame with z axis along Ki)
    jtube = 0
    jpix = 0
    do igroup = 1, ngroup
       do itube = 1, ntube(igroup)
          jtube = jtube + 1
          theta_tube = theta_init(igroup) + del_theta(igroup)*dble(itube-1)
          if (type(igroup) .eq. 'PLANE') then
             !implicit conversion of degrees to radians in the call			
             call IXFrotation_matrix_z (phi_in/rad_to_deg_dp, phi_mat)
             call IXFrotation_matrix_y (theta_tube/rad_to_deg_dp, theta_mat)
             !				call mmmult (phi_mat, theta_mat, 3, 3, 3, amat)
             amat=matmul(phi_mat,theta_mat)				
          else if (type(igroup) .eq. 'ARC') then
             !implicit conversion of degrees to radians in the call
             call IXFrotation_matrix_z (phi_in/rad_to_deg_dp, phi_mat)
             call IXFrotation_matrix_y (theta_tube/rad_to_deg_dp, theta_mat)
             call IXFrotation_matrix_x (-type_par(igroup)/rad_to_deg_dp, beta_mat)
             !				call mmmult (phi_mat, theta_mat, 3, 3, 3, temp_mat)
             temp_mat=matmul(phi_mat,theta_mat)				
             !				call mmmult (temp_mat, beta_mat, 3, 3, 3, amat)
             amat=matmul(temp_mat,beta_mat)				
          else if (type(igroup) .eq. 'DSTRIP') then
             dphi = asin(sin(type_par(igroup)/rad_to_deg_dp)/sin(theta_tube/rad_to_deg_dp))
             !implicit conversion of degrees to radians in the call				
             call IXFrotation_matrix_z (phi_in/rad_to_deg_dp + dphi, phi_mat)
             call IXFrotation_matrix_y (theta_tube/rad_to_deg_dp, theta_mat)
             !				call mmmult (phi_mat, theta_mat, 3, 3, 3, amat)
             amat=matmul(phi_mat,theta_mat)				
          else if (type(igroup) .eq. 'DEBYE') then
             !implicit conversion of degrees to radians in the call			
             call IXFrotation_matrix_z ((phi_in+type_par(igroup))/rad_to_deg_dp, phi_mat)
             call IXFrotation_matrix_y (theta_tube/rad_to_deg_dp, theta_mat)
             !				call mmmult (phi_mat, theta_mat, 3, 3, 3, amat)
             amat=matmul(phi_mat,theta_mat)				
          else
             call remark ('ERROR: Unrecognised option in '//string(1:lstr))
          endif

          do ipix = 1, abs(npix(igroup))
             jpix = jpix + 1
             if (jpix .gt. ndet_max) then
                call remark ('ERROR: Too many detectors for internal storage '//string(1:lstr))
                ierr = 1
                return
             endif

             detno(jpix)  = detno_init(igroup) + del_detno(igroup)*(itube-1) + sign(ipix-1,npix(igroup))
             delta(jpix)  = delay

             xdet(1) = 0.0d0

             if (use_calib) then
                kpix=(abs(npix(igroup))-npix(igroup)+1+sign(2*ipix-1,npix(igroup)))/2 ! pixel no. from low pixel number end
                call psd_tube_position(kpix, abs(npix(igroup)), calib(1,jtube), pix_pos, pix_len )
                xdet(2)=sign(1,npix(igroup))*pix_pos
             else
                xdet(2) = length*(dble(2*ipix-1-abs(npix(igroup)))/dble(2*abs(npix(igroup))))
                pix_len = length / dble(abs(npix(igroup)))
             endif
             if (type(igroup) .eq. 'PLANE') xdet(2) = xdet(2) + type_par(igroup)
             xdet(3) = l2_in
             !				call mvmult (amat, xdet, 3, 3, xspec)
             xspec=matmul(amat,xdet)
             l2(jpix) = IXFnorm(xspec)
             code(jpix) = code_in
             if (sqrt(xspec(1)**2+xspec(2)**2) .gt. small) then
                theta(jpix) = atan2(sqrt(xspec(1)**2+xspec(2)**2), xspec(3)) * rad_to_deg_dp
                phi(jpix)= atan2(xspec(2),xspec(1)) * rad_to_deg_dp
             else
                theta(jpix) = atan2(0.0d0, xspec(1)) * rad_to_deg_dp	! in-beam monitor
                phi(jpix)= 0.0d0
             endif
             w_x(jpix) = diameter
             w_y(jpix) = diameter
             w_z(jpix) = pix_len
             f_x(jpix) = 2.0d0*l2_in*tan(0.5d0*abs(del_theta(igroup))/rad_to_deg_dp)
             f_y(jpix) = 2.0d0*l2_in*tan(0.5d0*abs(del_theta(igroup))/rad_to_deg_dp)
             f_z(jpix) = pix_len

             !		construct natural detector frame:
             call IXFtrans_spherical_polars (phi(jpix)/rad_to_deg_dp, theta(jpix)/rad_to_deg_dp, bmat) ! spec coords to sphere
             !				call mmmult (bmat, amat, 3, 3, 3, cmat)		! matrix to transform from det-plane to sphere
             cmat=matmul(bmat,amat)								
             sig = sign(1.0d0,cmat(2,2))
             norm = sqrt(cmat(1,2)**2 + cmat(2,2)**2)
             nmat(1,1) = sig*cmat(2,2)/norm
             nmat(1,2) =-sig*cmat(1,2)/norm
             nmat(1,3) = 0.0d0
             nmat(2,1) = sig*cmat(1,2)*cmat(3,2)/norm
             nmat(2,2) = sig*cmat(2,2)*cmat(3,2)/norm
             nmat(2,3) =-sig*norm
             nmat(3,1) = cmat(1,2)
             nmat(3,2) = cmat(2,2)
             nmat(3,3) = cmat(3,2)

             !                  call rotmat_to_rotvec (nmat, rotvec)
             !                  alpha_x(jpix) = rotvec(1)
             !                  alpha_y(jpix) = rotvec(2)
             !                  alpha_z(jpix) = rotvec(3)
             ! this will give rotvec in radians -> to reproduce old stylee tables need to convert it back to degrees
             rotvec= IXFrotmat_to_rotvec (nmat)
             alpha_x(jpix) = rotvec(1)*rad_to_deg_dp
             alpha_y(jpix) = rotvec(2)*rad_to_deg_dp
             alpha_z(jpix) = rotvec(3)*rad_to_deg_dp

             if (code_in .eq. 2) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = 0.0d0
             else if (code_in .eq. 3) then
                det_1(jpix) = dead
                det_2(jpix) = he_pressure
                det_3(jpix) = wall
                det_4(jpix) = index_init(igroup) + del_index(igroup)*(itube-1)
             else
                call remark ('ERROR: Invalid CODE type in detector bank '//string(1:lstr))
                ierr = 1
                return
             endif
          end do
       end do
    end do
    ndet = jpix

    !
    ! Check that detector numbers are unique (and warn if not 1:1 mapping to spectra):

    if (ndet .eq. 0) then
       call remark ('ERROR: No detectors read '//string(1:lstr))
       ierr = 1
       return
    endif

    !	call indexxi (ndet, detno, iwork)
    call IXFrank(detno(1:ndet),iwork)
    if (detno(iwork(1)) .le. 0) then
       call remark ('ERROR: Zero or negative detector number '//string(1:lstr))
       ierr = 1
       return
    endif
    if (ndet .gt. 1) then
       do i = 1, ndet-1
          if (detno(iwork(i)) .eq. detno(iwork(i+1))) then
             call remark ('ERROR: Detector numbers are not all unique '//string(1:lstr))
             ierr = 1
             return
          endif
       end do
    endif

  end subroutine CALC_PSD_TUBE_STRIP_DATA

  !----------------------------------------------------------------------------------------------------------------------	

  subroutine process_tube_gcircle_data (iu_data, iu_data_out, ndet_max, iwork, name, &
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools
    !
    ! Reads data and calculates entries for detector.dat for a great circle of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:
    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    double precision l2_in, phi_circle, theta_circle, diameter, length, he_pressure, wall, delay, dead,&
         beta_init(ngroup_max), del_beta(ngroup_max), calib_dummy(ncalib_par_max)
    logical :: use_calib = .false.

    data npix /ngroup_max*1/, index_init /ngroup_max*0/, del_index /ngroup_max*0/, calib_dummy /ncalib_par_max*0/

    ndet = 0
    ierr = 0

    call READ_TUBE_GCIRCLE_DATA (iu_data, iu_data_out, ngroup_max,  l2_in, name, phi_circle, theta_circle,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, beta_init, del_beta, detno_init, del_detno, ierr)
    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_GCIRCLE_DATA (ndet_max, iwork, name, l2_in, phi_circle, theta_circle,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, beta_init, del_beta, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib_dummy,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

    return
  end subroutine process_tube_gcircle_data

  subroutine READ_TUBE_GCIRCLE_DATA (iu_data, iu_data_out, ngroup_max, l2, name, phi_circle, theta_circle,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, beta_init, del_beta, detno_init, del_detno, ierr)
    use IXMtools	
    integer iu_data, iu_data_out, ngroup_max, code, ngroup, ntube(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
	 ierr, ierr_write
    double precision l2, phi_circle, theta_circle, diameter, length, he_pressure, wall, delay, dead,&
         beta_init(ngroup_max), del_beta(ngroup_max)
    character(len=*) name

    integer l, igroup
    character*255 line

    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) l2, phi_circle, theta_circle

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'TUBE_GCIRCLE', name)
       ierr = 1
       return
    endif

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) ngroup

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'TUBE_GCIRCLE', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'TUBE_GCIRCLE', name)
       ierr = 1
       return
    endif

    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) ntube(igroup), beta_init(igroup), del_beta(igroup), detno_init(igroup), del_detno(igroup)
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'TUBE_GCIRCLE', name)
          ierr = 1
          return
       endif
    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'TUBE_GCIRCLE', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'TUBE_GCIRCLE', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_TUBE_GCIRCLE_DATA


  subroutine process_tube_plane_data (iu_data, iu_data_out, ndet_max, iwork, name, &
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools	
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:
    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    double precision x0_in(3), xu_in(3), xv_in(3), diameter, length, he_pressure, wall, delay, dead,&
         dist(ngroup_max), h(ngroup_max), pitch(ngroup_max), calib_dummy(ncalib_par_max)
    logical :: use_calib  = .false.

    data npix /ngroup_max*1/, index_init /ngroup_max*0/, del_index /ngroup_max*0/, calib_dummy /ncalib_par_max*0/

    ndet = 0
    ierr = 0

    call READ_TUBE_PLANE_DATA (iu_data, iu_data_out, ngroup_max, name, x0_in, xu_in, xv_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, dist, h, pitch, detno_init, del_detno, ierr)
    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_PLANE_DATA (ndet_max, iwork, name, x0_in, xu_in, xv_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, dist, h, pitch, npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib_dummy,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

    return
  end subroutine process_tube_plane_data

  subroutine READ_TUBE_PLANE_DATA (iu_data, iu_data_out, ngroup_max, name, x0, xu, xv,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, dist, h, pitch, detno_init, del_detno, ierr)
    use IXMtools
    integer iu_data, iu_data_out,&
         ngroup_max, code, ngroup, ntube(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max), ierr, ierr_write
    double precision x0(3), xu(3), xv(3), diameter, length, he_pressure, wall, delay, dead,&
         dist(ngroup_max), h(ngroup_max), pitch(ngroup_max)
    character(len=*) name

    integer i, l, igroup
    character*255 line

    ierr = 0
    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (x0(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (xu(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) (xv(i), i=1,3)

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'TUBE_PLANE', name)
       ierr = 1
       return
    endif

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) ngroup

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'TUBE_PLANE', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'TUBE_PLANE', name)
       ierr = 1
       return
    endif

    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       read (line, *, end=999, err=999) ntube(igroup),dist(igroup),h(igroup),pitch(igroup),detno_init(igroup),del_detno(igroup)
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'TUBE_PLANE', name)
          ierr = 1
          return
       endif
    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'TUBE_PLANE', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'TUBE_PLANE', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))
    ierr = 1
    return

  end subroutine READ_TUBE_PLANE_DATA

  subroutine process_tube_strip_data (iu_data, iu_data_out, ndet_max, iwork, name, &
       ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
       alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)
    use IXMtools	
    !
    ! Reads data and calculates entries for detector.dat for a plane of PSD detectors
    ! Error is indicated by IERR .ne. 0, and NDET=0
    !

    ! input variables:
    integer iu_data, iu_data_out, ndet_max, iwork(ndet_max)
    character(len=*) name

    ! output variables:
    integer ndet, detno(ndet_max), code(ndet_max), ierr
    double precision delta(ndet_max), l2(ndet_max), theta(ndet_max), phi(ndet_max), w_x(ndet_max), w_y(ndet_max), w_z(ndet_max),&
         f_x(ndet_max), f_y(ndet_max), f_z(ndet_max),&
         alpha_x(ndet_max), alpha_y(ndet_max), alpha_z(ndet_max),&
         det_1(ndet_max), det_2(ndet_max), det_3(ndet_max), det_4(ndet_max)

    ! internal variables:
    integer code_in, ngroup, ntube(ngroup_max), npix(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max),&
         index_init(ngroup_max), del_index(ngroup_max)
    double precision l2_in, phi_in, diameter, length, he_pressure, wall, delay, dead,&
         theta_init(ngroup_max), del_theta(ngroup_max), type_par(ngroup_max), calib_dummy(ncalib_par_max)
    logical :: use_calib = .false.
    character*20 type(ngroup_max)

    data npix /ngroup_max*1/, index_init /ngroup_max*0/, del_index /ngroup_max*0/, calib_dummy /ncalib_par_max*0/

    ndet = 0
    ierr = 0

    call READ_TUBE_STRIP_DATA (iu_data, iu_data_out, ngroup_max, name, l2_in, phi_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, theta_init, del_theta, type, type_par,&
         detno_init, del_detno, ierr)

    if (ierr .ne. 0) return

    call CALC_PSD_TUBE_STRIP_DATA (ndet_max, iwork, name, l2_in, phi_in,&
         code_in, diameter, length, he_pressure, wall, delay, dead,&
         ngroup, ntube, theta_init, del_theta, type, type_par,&
         npix, detno_init, del_detno, index_init, del_index,&
         use_calib, ncalib_par_max, calib_dummy,&
         ndet, detno, delta, l2, code, theta, phi, w_x, w_y, w_z, f_x, f_y, f_z,&
         alpha_x, alpha_y, alpha_z, det_1, det_2, det_3, det_4, ierr)

    return
  end subroutine process_tube_strip_data

  subroutine READ_TUBE_STRIP_DATA (iu_data, iu_data_out, ngroup_max, name, l2, phi,&
       code, diameter, length, he_pressure, wall, delay, dead,&
       ngroup, ntube, theta_init, del_theta, type, type_par,&
       detno_init, del_detno, ierr)
    use IXMtools
    integer iu_data, iu_data_out,&
         ngroup_max, code, ngroup, ntube(ngroup_max), detno_init(ngroup_max), del_detno(ngroup_max), ierr, ierr_write
    double precision l2, phi, diameter, length, he_pressure, wall, delay, dead,&
         theta_init(ngroup_max), del_theta(ngroup_max), type_par(ngroup_max)
    character(len=*):: name, type(ngroup_max)

    integer i, l, nvals, igroup, iwrd
    double precision arr(5)
    character*10 wrd
    character*255 line

    ierr = 0

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) l2, phi

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) code

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) diameter, length, he_pressure, wall, delay, dead
    if ((diameter .lt. 0.0d0) .or. (length .lt. 0.0d0) .or. (he_pressure .lt. 0.0d0) .or. (wall .lt. 0.0d0)&
         .or. (dead .lt. 0.0d0)) then
       call detector_message ('ERROR: Check diameter, length, gas pressure, wall thickness and dead time > 0.',&
            'TUBE_STRIP', name)
       ierr = 1
       return
    endif

    call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
    if (ierr_write .ne. 0) goto 900
    read (line, *, end=999, err=999) ngroup

    if (ngroup .gt. ngroup_max) then
       call detector_message ('ERROR: No. groups in detector bank too large.', 'TUBE_STRIP', name)
       ierr = 1
       return
    else if (ngroup .lt. 0) then
       call detector_message ('ERROR: No. groups less than one.', 'TUBE_STRIP', name)
       ierr = 1
       return
    endif

    do igroup = 1, ngroup
       call read_line_write (iu_data, line, l, iu_data_out, .true., ierr_write)
       if (ierr_write .ne. 0) goto 900
       if (l .le. 0) goto 999
       i = 1
       nvals = getds (line, i, arr, 5)
       if (nvals .ne. 5) goto 999
       ntube(igroup) = nint(arr(1))
       if (ntube(igroup) .le. 0) then
          call detector_message ('ERROR: No. tubes must be > 0 for all entries.', 'TUBE_STRIP', name)
          ierr = 1
          return
       endif
       theta_init(igroup) = arr(2)
       del_theta(igroup)  = arr(3)
       detno_init(igroup) = nint(arr(4))
       del_detno(igroup)  = nint(arr(5))

       iwrd = inext(line,i)
       if (iwrd.gt.len(line)) then
          type(igroup) = 'PLANE'
          type_par(igroup) = 0.0d0
       else if (line(iwrd:iwrd) .eq. '!') then
          type(igroup) = 'PLANE'
          type_par(igroup) = 0.0d0
       else
          l = getwrd (line, i, wrd)
          if (l .le. 0) goto 999
          call upcase (wrd)
          if (wrd(1:l) .eq. 'ARC') then
             type(igroup) = 'ARC'
          else if (wrd(1:l) .eq. 'PLANE') then
             type(igroup) = 'PLANE'
          else if (wrd(1:l) .eq. 'DSTRIP') then
             type(igroup) = 'DSTRIP'
          else if (wrd(1:l) .eq. 'DEBYE') then
             type(igroup) = 'DEBYE'
          else
             goto 999
          endif
          nvals = getd (line, i,type_par(igroup))
          if (nvals .ne. 1) goto 999
       endif
    end do
    return

900 call detector_message ('ERROR: Problem writing output data file.', 'TUBE_STRIP', name)
    ierr = 1
    return

999 call detector_message ('ERROR: Inconsistency found in data file.', 'TUBE_STRIP', name)
    if (l .gt. 0) call remark ('  Line: '//line(1:l))	
    ierr = 1
    return

  end subroutine READ_TUBE_STRIP_DATA

  subroutine detector_message (message, type, name)
    use IXMtools
    character(len=*):: message, type, name

    integer lmess, ltype, lname
    character*255 string

    lmess = lenstr(message)
    ltype = lenstr(type)
    lname = lenstr(name)

    if (lmess .ne. 0) call remark (message(1:lmess))
    if (ltype .eq. 0) return
    if (lname .eq. 0) then
       string = '  Detector bank type: '//type(1:ltype)//'     (bank name not supplied)'
    else
       string = '  Detector bank type: '//type(1:ltype)//'      Bank name: '//name(1:lname)
    endif
    call remark (string)

  end subroutine detector_message

  subroutine psd_tube_position (ipix, npix, calib, posn, width)
    ! Return position along length of psd tube of a pixel.
    ! 
    !      psd_tube_position (i,npix,calib)
    !
    ! ipix	Pixel position, counting from low pixel index end of the tube
    ! npix    No. pixels in the tube
    ! calib	Array length 4 of the coefficients relating
    !           y = calib(1) + calib(2)*(x-0.5) + calib(3)*(x-0.5)**2 + calib(4)*(x-0.5)**3
    !         x = Normalised pixel position along the tube, measured from low pixel number end
    !             i.e. 0.5=<x=<0.5. Recall that this covers the pixel range 0.5 to |npix|+0.5
    !         y = Distance w.r.t. centre of tube in meters
    !
    ! Position is given by average of limits of pixel, to ensure that adjacent pixels exactly align
    integer(i4b):: ipix, npix
    real(dp):: calib(4), posn, width, posn_lo, posn_hi

    posn = dble(2*ipix-1-npix)/dble(2*npix)		! position of centre of pixel
    width=1.0d0/dble(npix)						! width of pixel
    posn_lo = calib(1) + calib(2)*(posn-0.5*width) + calib(3)*((posn-0.5*width)**2) + calib(4)*((posn-0.5*width)**3)
    posn_hi = calib(1) + calib(2)*(posn+0.5*width) + calib(3)*((posn+0.5*width)**2) + calib(4)*((posn+0.5*width)**3)
    posn = 0.5*(posn_hi+posn_lo)
    width= abs(posn_hi-posn_lo)

  end	subroutine psd_tube_position
end module IXMdetector
