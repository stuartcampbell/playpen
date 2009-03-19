!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMdetector
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMdetector object.
!!
!! Nearest equivalent NeXus class: NXdetector
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 778 $ ($Date: 2006-08-18 04:48:03 -0400 (Fri, 18 Aug 2006) $)
!!
module IXMdetector_tgp

  use IXMtype_definitions
  use IXMbase
  use IXMgeometry
  use IXMdet_generic
  use IXMdet_he3
  use IXMdet_solid
  implicit none
  ! default is all variables and functions are hidden
  private
  ! Public parameters
  integer(i4b), parameter, public :: IXCgeneric_det=0, IXChe3_det=1, IXCsolid_det=2  
  ! public types
  public :: IXTdetector
  ! public interfaces
  !   public :: 
  type IXTdetector
     private ! The type is public, but the contents not

     type(IXTbase) :: base
     integer(i4b)  :: ref_count=1                   !! Reference count from spectra and multiple runfile
                                                    !!to be incremented when the detector structure is pointed at
     integer(i4b), pointer  :: checksum(:)=>NULL()	!! To compare different detector object values
     integer(i4b), pointer  :: det_no(:)=>NULL()    !! Actual detector number
     real(dp), pointer  :: delay_time(:)=>NULL()    !! Electronic delay time (microseconds)
     real(dp), pointer  :: dead_time(:)=>NULL()     !! Detector dead time (microseconds)
     real(dp), pointer  :: theta(:)=>NULL()         !! Azimuthal angle (radians)
     real(dp), pointer  :: L2(:)=>NULL()            !! Secondary flightpath (m)
     real(dp), pointer  :: phi(:)=>NULL()           !! Scattering angle (radians)
     type(IXTgeometry)  :: geometry                 !! Contains geometry information about all of the detectors
     integer(i4b), pointer  :: group_index(:)=>NULL()
     integer(i4b), pointer  :: det_type(:)=>NULL()	!! Type of detector (IXChe3_det, IXClsolid_det, IXCcomposite_det ...)
                                                    !!This can be he3 or solid or composite. Composite is used for
                                                    !!effective detector purpose in which there can be a combination of detectors of different type.
     integer(i4b), pointer  :: type_index(:)=>NULL()!! Index into the relevant deteector type
     type(IXTdet_generic)   :: det_generic          !! Contains description of all generic detectors
     type(IXTdet_he3)       :: det_he3              !! Contains description of all the distinct He3 detectors
     type(IXTdet_solid)     :: det_solid            !! Contains description of all the distinct solid detectors
  end type IXTdetector

! include the interfaces required by routines declared in class_base.f90
  #define IXD_TYPE detector
  #include "class_header.f90"

  contains
!=======================================================================================================================
! include the generic subroutines every class requires
  #define IXD_DESCRIPTION	"IXTdetector class"
  #define IXD_TYPE detector
  #define IXD_SQTYPE 'detector'
  #include "class_base.f90"

!=======================================================================================================================
  recursive subroutine IXFoperation_run_detector(op, field, arg, status)
    implicit none
    type(IXTdetector) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdetector', field, status)
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run    (op, 'base', arg%base, status)
    call IXFoperation_run    (op, 'ref_count',  arg%ref_count, status)
    call IXFoperation_run_ptr(op, 'checksum',  arg%checksum, status)
    call IXFoperation_run_ptr(op, 'det_no', arg%det_no, status)
    call IXFoperation_run_ptr(op, 'delay_time', arg%delay_time, status)
    call IXFoperation_run_ptr(op, 'dead_time', arg%dead_time, status)
    call IXFoperation_run_ptr(op, 'theta', arg%theta, status)
    call IXFoperation_run_ptr(op, 'L2', arg%L2, status)
    call IXFoperation_run_ptr(op, 'phi', arg%phi, status)
    call IXFoperation_run    (op, 'geometry', arg%geometry, status)
    call IXFoperation_run_ptr(op, 'group_index', arg%group_index, status)
    call IXFoperation_run_ptr(op, 'det_type', arg%det_type, status)
    call IXFoperation_run_ptr(op, 'type_index', arg%type_index, status)
    call IXFoperation_run    (op, 'det_he3', arg%det_generic, status)
    call IXFoperation_run    (op, 'det_he3', arg%det_he3, status)
    call IXFoperation_run    (op, 'det_solid', arg%det_solid, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_detector

!=======================================================================================================================
!! The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
!! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTdetector
!! declaration will be used, it is therefore the last argument. If an element of the object is another class then
!! it MUST be initialised.
!! In the case for the IXTdetector object, the IXTdet_generic, IXTdet_he3 and IXTdet_solid objects are optional,
!! so long as at least one IXTdet_*** object is present

  subroutine IXFcreate_detector(detector, checksum, det_no, delay_time, dead_time, &
       theta, L2, phi, geometry, group_index, det_type, type_index, det_generic, det_he3, det_solid, status, base)
    implicit none
    type(IXTdetector):: detector
    integer(i4b) :: ref_count=0
    integer(i4b), intent(in) :: checksum(:)
    integer(i4b), intent(in) :: det_no(:)
    real(dp), intent(in) :: delay_time(:)
    real(dp), intent(in) :: dead_time(:)
    real(dp), intent(in) :: theta(:)
    real(dp), intent(in) :: L2(:)
    real(dp), intent(in) :: phi(:)
    type(IXTgeometry), intent(in) :: geometry
    integer(i4b), intent(in) :: group_index(:)
    integer(i4b), intent(in) :: det_type(:)
    integer(i4b), intent(in) :: type_index(:)
    type(IXTdet_generic), intent(in), optional :: det_generic
    type(IXTdet_he3),     intent(in), optional :: det_he3
    type(IXTdet_solid),   intent(in), optional :: det_solid
    type(IXTstatus) :: status
    type(IXTbase), intent(in), optional :: base


    ! At least one of the detector types must be present
    if((.not. present(det_generic)) .and. (.not. present(det_he3)) .and. (.not. present(det_solid)))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'either IXTdet_generic, IXTdet_solid or IXTdet_he3 objects MUST be present (IXFcreate_detector)')
    endif

    if (status == IXCseverity_error) return

    if (present(det_generic))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(det_generic) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'IXTdet_generic failure, all nested objects MUST be initialised (IXFcreate_detector)')
       endif
    endif
    if (present(det_he3))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(det_he3) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'IXTdet_he3 failure, all nested objects MUST be initialised (IXFcreate_detector)')
       endif
    endif
    if (present(det_solid))then
       ! nested objects should be tested for initialisation, this shows they have been created properly   
       if( IXFvalid(det_solid) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'IXTdet_solid failure, all nested objects MUST be initialised (IXFcreate_detector)')
       endif
    endif

    ! The set routine can ONLY be called on an initialised object so in this *special* case it is initialised before it is filled
    call IXFmark_valid(detector)

    ! For the optional present base class then the full set command is called (note: checking is done in set routine)
    if(present(base))then
       ! set is called with all the components of the object
       call IXFset_detector(detector, status, checksum, det_no, delay_time, dead_time,  &
            theta, L2, phi, geometry, group_index, det_type, type_index, det_generic, det_he3, det_solid, base)
    else
       call IXFset_detector(detector, status, checksum, det_no, delay_time, dead_time,  &
            theta, L2, phi, geometry, group_index, det_type, type_index, det_generic, det_he3, det_solid)
    endif

    ! the reference counter is now only incremented when the detector is pointed at
    ! the ref_count variable has a default 'existence' value of 1

  end subroutine IXFcreate_detector

!=======================================================================================================================
! The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
! does not have to be initialised, but the reference object must be initialised instead.
  recursive subroutine IXFset_detector(detector, status, checksum, det_no, delay_time, dead_time, &
       theta, L2, phi, geometry, group_index, det_type,  type_index, det_generic, det_he3, det_solid, base, ref)
    implicit none
    type(IXTdetector), intent(inout) :: detector
    type(IXTstatus) :: status
    integer(i4b), optional, intent(in) :: checksum(:)
    integer(i4b), optional, intent(in):: det_no(:)
    real(dp), optional, intent(in):: delay_time(:)
    real(dp), optional, intent(in):: dead_time(:)
    real(dp), optional, intent(in):: theta(:)
    real(dp), optional, intent(in):: L2(:)
    real(dp), optional, intent(in):: phi(:)
    type(IXTgeometry), optional, intent(in) :: geometry
    integer(i4b), optional, intent(in):: group_index(:)
    integer(i4b), optional, intent(in):: det_type(:)
    integer(i4b), optional, intent(in):: type_index(:)
    type(IXTdet_generic), optional, intent(in):: det_generic
    type(IXTdet_he3),     optional, intent(in):: det_he3
    type(IXTdet_solid),   optional, intent(in):: det_solid
    type(IXTbase), optional, intent(in) :: base
    type(IXTdetector), optional, intent(in)::ref

    ! Check that either the reference object is initialised or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_detector)')
       endif
       if (status == IXCseverity_error) return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(detector)
    else    
       if(IXFvalid(detector) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_detector)')
       endif
       if (status == IXCseverity_error) return
    endif

    ! If a reference object was present, then first copy that over.
    if (present(ref))then
       ! this is called either from an IXFcopy subroutine or directly using IXFset_detector(ref=###) 
       ! therefore creating a copy of a detector. this should be an independent entity and therefore
       ! its ref_count=1 with the default value
       call IXFset_detector(detector, status, ref%checksum, ref%det_no, ref%delay_time, ref%dead_time, ref%theta, &
            ref%L2, ref%phi, ref%geometry, ref%group_index, ref%det_type, ref%type_index, &
            ref%det_generic, ref%det_he3, ref%det_solid, ref%base)
    endif

    ! Now copy over any other optional inputs.
    if (present(base))call IXFset_base(detector%base,status,ref=base)

    if (present(checksum)) then
       call IXFrealloc(detector%checksum,size(checksum),.false.,status)
       detector%checksum=checksum
    endif

    if ( present(det_no))then
       call IXFrealloc(detector%det_no,size(det_no),.false.,status)
       detector%det_no=det_no
    endif

    if ( present(delay_time))then
       call IXFrealloc(detector%delay_time,size(delay_time),.false.,status)
       detector%delay_time=delay_time
    endif

    if ( present(dead_time))then
       call IXFrealloc(detector%dead_time,size(dead_time),.false.,status)
       detector%dead_time=dead_time
    endif

    if ( present(theta))then
       call IXFrealloc(detector%theta,size(theta),.false.,status)
       detector%theta=theta
    endif

    if ( present(L2))then
       call IXFrealloc(detector%L2,size(L2),.false.,status)
       detector%L2=L2
    endif

    if ( present(phi))then
       call IXFrealloc(detector%phi,size(phi),.false.,status)
       detector%phi=phi
    endif

    if (present(geometry)) call IXFcopy(geometry, detector%geometry, status) ! use copy routine to copy geometry  into detector subunit 

    if ( present(group_index))then
       call IXFrealloc(detector%group_index,size(group_index),.false.,status)
       detector%group_index=group_index
    endif

    if ( present(det_type))then
       call IXFrealloc(detector%det_type,size(det_type),.false.,status)
       detector%det_type=det_type
    endif

    if ( present(type_index))then
       call IXFrealloc(detector%type_index,size(type_index),.false.,status)
       detector%type_index=type_index
    endif

    if (present(det_generic)) call IXFcopy(det_generic, detector%det_hgeneric, status) ! use copy routine to copy det_generic into detector subunit 
    if (present(det_he3)) call IXFcopy(det_he3, detector%det_he3, status) ! use copy routine to copy det_he3 into detector subunit 
    if (present(det_solid)) call IXFcopy(det_solid, detector%det_solid, status) ! use copy routine to copy det_solid into detector subunit 

    call IXFcheck(detector, status)

  end subroutine IXFset_detector

!=======================================================================================================================
! IXFcheck will make internal consistency checks in the object, such as array length checking to make
! sure the object is properly filled.

  subroutine IXFcheck_detector(arg, status)
    implicit none
    type(IXTdetector)::arg
    type(IXTstatus)::status

    call IXFcheck_base(arg%base,status)
    call IXFcheck(arg%det_solid,status)
    call IXFcheck(arg%det_he3,status)

  end subroutine IXFcheck_detector

!=======================================================================================================================





#define IXD_TYPE det_he3
#include "class_header.f90"

#define IXD_TYPE det_solid
#include "class_header.f90"

#define IXD_TYPE detector
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

  interface IXFvolume
     module procedure IXFvolume_detector
  end interface

  interface IXFsolid_angle
     module procedure IXFsolid_angle_detector
  end interface

  !----------------------------------------------------------------------------------------------------------------------
contains

#define IXD_DESCRIPTION	"IXTdet_he3 class"
#define IXD_TYPE det_he3
#define IXD_SQTYPE 'det_he3'
#include "class_base.f90"

#define IXD_DESCRIPTION	"IXTdet_solid class"
#define IXD_TYPE det_solid
#define IXD_SQTYPE 'det_solid'
#include "class_base.f90"

#define IXD_DESCRIPTION	"IXTdetector class"
#define IXD_TYPE detector
#define IXD_SQTYPE 'detector'
#include "class_base.f90"


  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_det_he3(op, field, arg, status)
    implicit none
    type(IXTdetector) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdet_he3', field, status)
    call IXFoperation_run(op,'gas_pressure',      arg%gas_pressure, status)
    call IXFoperation_run(op,'wall_thickness',   arg%wall_thickness, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_det_he3

  recursive subroutine IXFoperation_run_det_solid(op, field, arg, status)
    implicit none
    type(IXTdetector) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdet_solid', field, status)
    call IXFoperation_run(op,'macro_xs',      arg%macro_xs, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_det_solid

  recursive subroutine IXFoperation_run_detector(op, field, arg, status)
    implicit none
    type(IXTdetector) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTdetector', field, status)
    call IXFoperation_run(op,'index',      arg%index, status)
    call IXFoperation_run(op,'geometry',   arg%geometry, status)
    call IXFoperation_run(op,'delay_time', arg%delay_time, status)
    call IXFoperation_run(op,'dead_time',  arg%dead_time, status)
    call IXFoperation_run(op,'type',       arg%type, status)
    call IXFoperation_run(op,'det_he3',    arg%det_he3, status)
    call IXFoperation_run(op,'det_solid',  arg%det_solid, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_detector
  !----------------------------------------------------------------------------------------------------------------------
  !! Set detector properties
  !!

  ! Set from another object of same class:
  subroutine IXFset_class_detector (self, detector)
    implicit none
    type(IXTdetector), intent(INOUT) :: self		!! the object we are going to set
    type(IXTdetector), intent(IN) :: detector	    !! object from which to make a copy
    self = detector              ! no checking required, as existing object must already be valid
  end subroutine IXFset_class_detector

  ! Set from attributes:
  subroutine IXFset_attributes_detector (self, status, index, geometry, delay_time, dead_time, type, det_he3, det_solid)
    implicit none
    type(IXTorientation), intent(INOUT) :: self					!! the object we are going to set
    integer(i4b), intent(IN), optional :: index                  !! detector index number
    type(IXTgeometry), intent(IN),  optional :: geometry          !! detector geometry
    real(dp), intent(IN), optional :: delay_time              !! electronic delay time (microseconds)
    real(dp), intent(IN), optional :: dead_time               !! detector dead time (microseconds)
    integer(i4b), intent(IN), optional :: type					!! type of detector
    type(IXTdet_he3), intent(IN), optional :: det_he3
    type(IXTdet_solid), intent(IN), optional :: det_solid
    type(IXTstatus), intent(IN), optional :: status

    if (present(index)) self%index = index
    if (present(geometry)) self%geometry = geometry
    if (present(delay_time)) self%delay_time = delay_time
    if (present(dead_time)) self%dead_time = dead_time
    if (present(type)) self%type = type
    if (present(det_he3)) then
       if (self%type==IXChe3_detector) then
          self%det_he3 = det_he3
       else
          call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
               & 'Supplied detector type parameters inconsistent with detector type')
       endif
    endif
    if (present(det_solid)) then
       if (self%type==IXCsolid_detector) then
          self%det_solid = det_solid
       else
          call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
               & 'Supplied detector type parameters inconsistent with detector type')
       endif
    endif
    call IXFcheck_detector(self, status)

  end subroutine IXFset_attributes_detector

  !! Get components of a detector. 
  !!
  !! Syntax:
  !!   - call IXFget_detector (detector, detector_out)        to get a copy of the object
  !!   - call IXFget_detector (detector, status, [name_1=val_1,] [name_2=val_2,] ...) to get one or more attributes
  !!
  subroutine IXFget_class_detector(self, detector)
    implicit none
    type(IXTdetector), intent(IN) :: self
    type(IXTdetector), intent(OUT):: detector
    geometry = self
  end subroutine IXFget_class_detector

  subroutine IXFget_attributes_geometry(self, status, index, geometry, delay_time, dead_time, type, det_he3, det_solid)
    implicit none
    type(IXTorientation), intent(IN) :: self					!! the orientation object we are going to set
    integer(i4b), intent(OUT), optional :: index                  !! detector index number
    type(IXTgeometry), intent(OUT),  optional :: geometry          !! detector geometry
    real(dp), intent(OUT), optional :: delay_time              !! electronic delay time (microseconds)
    real(dp), intent(OUT), optional :: dead_time               !! detector dead time (microseconds)
    integer(i4b), intent(OUT), optional :: type					!! type of detector
    type(IXTdet_he3), intent(OUT), optional :: det_he3
    type(IXTdet_solid), intent(OUT), optional :: det_solid
    type(IXTstatus), intent(OUT), optional :: status
    ! So long as the objects are valid (as they must be if already created), then any combination is valid
    if (present(index)) index = self%index
    if (present(geometry)) geometry = self%geometry
    if (present(delay_time)) delay_time = self%delay_time
    if (present(dead_time)) dead_time = self%dead_time
    if (present(type)) type = self%type
    if (present(det_he3)) det_he3 = self%det_he3
    if (present(det_solid)) det_solid = self%det_solid
  end subroutine IXFget_attributes_geometry

  !! Create a detector object from components.
  !!
  !! Syntax:
  !!   - call IXFcreate_detector (detector_out, detector_in)	    copy detector_in
  !!   - call IXFcreate_detector (detector_out, status, [name_1=val_1,] [name_2=val_2,] ...) create with these components
  !!   - call IXFcreate_detector (detector_out, status)           default detector object

  subroutine IXFcreate_class_detector (self, detector)
    implicit none
    type(IXTdetector), intent(OUT) :: self			!! the detector object we are going to create
    type(IXTdetector), intent(IN)  :: detector	    !! the detector object we are going to copy
    self = detector
  end subroutine IXFcreate_class_detector

  subroutine IXFcreate_attributes_detector (self, status, index, geometry, delay_time, dead_time, type, det_he3, det_solid)
    implicit none
    type(IXTorientation), intent(INOUT) :: self					!! the object we are going to set
    integer(i4b), intent(IN), optional :: index                  !! detector index number
    type(IXTgeometry), intent(IN),  optional :: geometry          !! detector geometry
    real(dp), intent(IN), optional :: delay_time              !! electronic delay time (microseconds)
    real(dp), intent(IN), optional :: dead_time               !! detector dead time (microseconds)
    integer(i4b), intent(IN), optional :: type					!! type of detector
    type(IXTdet_he3), intent(IN), optional :: det_he3
    type(IXTdet_solid), intent(IN), optional :: det_solid
    type(IXTstatus) :: status

    ! internal variables
    type(IXTdetector):: default_detector
    logical :: first_time = .TRUE.
    save first_time, default_detector

    ! Create default output:
    if (first_time) then
       default_detector%index = 0
       call IXFcreate_geometry (default_detector%geometry, status)
       default_detector%delay_time = delay_time
       default_detector%dead_time = dead_time
       default_detector%type = IXCnull_detector
       call IXFcreate_det_he3 (default_detector%det_he3, status)
       call IXFcreate_solid_det (default_detector%det_solid, status)
       first_time = .FALSE.
    endif
    self = default_detector    		

    ! Now call set routine:
    if (count((/present(index),present(geometry),present(delay_time), &
         present(dead_time),present(type),present(det_he3),present(det_solid)/))/=0) then
       call IXFset_attributes_detector(self, status, index, geometry, delay_time, dead_time, type, det_he3, det_solid)
       if (status /= IXCseverity_ok) then
          call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
               & 'Error in IXFcreate_attributes_detector')
       endif
    endif

  end subroutine IXFcreate_attributes_detector

  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments

  subroutine IXFcheck_shape(shape, status)
    type(IXTshape) :: shape
    type(IXTstatus) :: status

    ! Must hand-craft in the various valid shape types at this point
    if (shape%type == IXCpoint) then
       call IXFcheck_point (shape%dimensions, status)
    elseif (shape%type == IXCbox) then
       call IXFcheck_box (shape%dimensions, status)
    else if (shape%type == IXCcylinder) then
       call IXFcheck_cylinder (shape%dimensions, status)
    else if (shape%type == IXCsphere) then
       call IXFcheck_sphere (shape%dimensions, status)
    else if (shape%type == IXCholcyl) then
       call IXFcheck_holcyl (shape%dimensions, status)
    else if (shape%type == IXCpolygon) then
       call IXFcheck_polygon (shape%dimensions, status)
    else
       call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid shape type - failed in IXFcheck_shape')
    endif

  end subroutine IXFcheck_shape
  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:

  ! Rather annoyingly, the various valid types must be hand-crafted in for each parent method.
  function IXFvolume_shape(shape) result(volume)
    type(IXTshape), intent(in):: shape
    real(dp) volume
    if (shape%type==IXCpoint) volume = IXFvolume_point(shape%dimensions)
    if (shape%type==IXCbox) volume = IXFvolume_box(shape%dimensions)
    if (shape%type==IXCcylinder) volume = IXFvolume_cylinder(shape%dimensions)
    if (shape%type==IXCsphere) volume = IXFvolume_sphere(shape%dimensions)
    if (shape%type==IXCholcyl) volume = IXFvolume_holcyl(shape%dimensions)
    if (shape%type==IXCpolygon) volume = IXFvolume_polygon(shape%dimensions)
  end function IXFvolume_shape

  function IXFsolid_angle_shape(shape, viewpoint) result(omega)
    type(IXTshape), intent(in):: shape
    real(dp), intent(in):: viewpoint(3) !! position in local coords of viewpoint
    real(dp) omega
    if (shape%type==IXCpoint) omega = IXFsolid_angle_point(shape%dimensions, viewpoint)
    if (shape%type==IXCbox) omega = IXFsolid_angle_box(shape%dimensions, viewpoint)
    if (shape%type==IXCcylinder) omega = IXFsolid_angle_cylinder(shape%dimensions, viewpoint)
    if (shape%type==IXCsphere) omega = IXFsolid_angle_sphere(shape%dimensions, viewpoint)
    if (shape%type==IXCholcyl) omega = IXFsolid_angle_holcyl(shape%dimensions, viewpoint)
    if (shape%type==IXCpolygon) omega = IXFsolid_angle_polygon(shape%dimensions, viewpoint)
  end function IXFsolid_angle_shape

  function IXFarea_vertices_shape(shape, rotmat, vector) result(area_vertices)
    type(IXTshape), intent(in):: shape
    real(dp), intent(in):: rotmat(3,3), vector(3)   ! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)
    if (shape%type==IXCpoint) area_vertices => IXFarea_vertices_point(shape%dimensions, rotmat, vector)
    if (shape%type==IXCbox) area_vertices => IXFarea_vertices_box(shape%dimensions, rotmat, vector)
    if (shape%type==IXCcylinder) area_vertices => IXFarea_vertices_cylinder(shape%dimensions, rotmat, vector)
    if (shape%type==IXCsphere) area_vertices => IXFarea_vertices_sphere(shape%dimensions, rotmat, vector)
    if (shape%type==IXCholcyl) area_vertices => IXFarea_vertices_holcyl(shape%dimensions, rotmat, vector)
    if (shape%type==IXCpolygon) area_vertices => IXFarea_vertices_polygon(shape%dimensions, rotmat, vector)
  end function IXFarea_vertices_shape

  subroutine IXFprojarea_vertices_shape (shape, rotmat, vector, projection, px, py, status, radius, axes)
    type(IXTshape), intent(in):: shape
    real(dp), intent(in):: rotmat(3,3), vector(3)   ! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: px(:), py(:)
    integer(i4b), intent(in) :: projection
    real(dp), intent(in), optional :: radius
    integer(i4b), intent(in), optional :: axes(2)
    type(IXTstatus) :: status
    ! internal declarations
    real(dp), pointer :: area_vertices(:,:)

    area_vertices => IXFarea_vertices_shape (shape, rotmat, vector)
    call IXFrealloc (px, size(area_vertices,2), .FALSE., status)
    call IXFrealloc (py, size(area_vertices,2), .FALSE., status)
    call IXFproj_projection (area_vertices, projection, px, py, status, radius, axes)

    return
  end subroutine IXFprojarea_vertices_shape

  !======================================================================================================================
  ! Point functions
  !
  ! These are essentially dummy - they allow for default initialisation of a shape to be a point
  ! The dimensions field is ignored, except that it is initialised to an array length zero.
  !======================================================================================================================
  subroutine IXFcheck_point (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    if (size(dimensions)/=0) then
       call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid point parameters - failed in IXFcheck_point')
    endif
    return
  end subroutine IXFcheck_point

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_point (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    volume = 0.0_dp
    return
  end function IXFvolume_point

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_point (dimensions, vp) result(omega)
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    omega = 0.0_dp
    return
  end function IXFsolid_angle_point
  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_point (dims, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b) :: nvert, ivert
    type(IXTstatus) :: status

    nvert = 1
    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(1,:) =  -vector(1)            ! x coordinates
    area_vertices(2,:) =  -vector(2)            ! y coordinates
    area_vertices(3,:) =  -vector(3)
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_point

  !======================================================================================================================
  ! Box functions
  !   Parameters: dimensions(1:3) = full widths along x, y, z axes
  !   Centred: at centre of box
  !======================================================================================================================
  subroutine IXFcheck_box (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    if (size(dimensions)/=3 .OR. minval(dimensions) < 0.0_dp) then
       call IXFstatus_add(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid box parameters - failed in IXFcheck_box')
    endif
    return
