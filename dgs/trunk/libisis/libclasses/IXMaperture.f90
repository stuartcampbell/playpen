!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMaperture
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1191 $ ($Date: 2007-08-09 11:09:49 -0400 (Thu, 09 Aug 2007) $)
!!
!! Fortran definition of IXMaperture object.
!!
!! Nearest equivalent NeXus class: NXaperture
!!
!! This is currently defined having width, height and radius
!! To prevent unnecessary complication we should have another form of 2dshape module
!! for circular and rectangular objects for instance since the current IXMshape only deals with 3d shapes
!! This module has the set/get and create routines
!! The these routines expect ALL variables to be set if sent a reference object

module IXMaperture
  use IXMtype_definitions
  use IXMbase
  implicit none
  public :: IXTaperture
  type IXTaperture
     private
     type(IXTbase) :: base
     character(len=long_len):: name='name'		!! Name of the aperture
     real(dp):: distance=0.0_dp					!! Distance of centre from sample (m) (-ve if upstream of sample)
     character(len=short_len):: shape='shape'		!! 'rectangular', 'circular'
     real(dp):: horiz_posn=0.0_dp       !!Horizontal position of aperture centre (m) 
     real(dp):: vert_posn=0.0_dp		!! vertical position of aperture centre (m)
     real(dp):: width=0.0_dp			!! Aperture width if rectangular (m)
     real(dp):: height=0.0_dp          !! Aperture height if rectangular (m)
     real(dp):: radius=0.0_dp			!! Aperture radius if circular (m)
  end type IXTaperture

#define IXD_TYPE aperture
#include "class_header.f90"

contains
#define IXD_DESCRIPTION	"IXTaperture class"
#define IXD_TYPE aperture
#define IXD_SQTYPE 'aperture'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_aperture(arg, status)
    implicit none
    type(IXTaperture) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)   
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_aperture

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_aperture(aperture, status)
    type(IXTaperture) :: aperture
    type(IXTstatus) :: status
    ! Do not do any checks at present - just a dummy routine	
    call IXFcheck_base(aperture%base,status)
  end subroutine IXFcheck_aperture

  !----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide the operation IXFxxxxxOperationRun; it loops through
  ! all members of the class doing the supplied operation. Needed by 
  ! Fortran - e.g. used in IXFxxxxxDisplay (the default display function) as well as
  ! Matlab & any other binding language (e.g. Python)

  recursive subroutine IXFoperation_run_aperture(op, field, arg, status)
    implicit none
    type(IXTaperture) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTaperture', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! The order of the fields below must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'distance', arg%distance, status)
    call IXFoperation_run(op,'shape', arg%shape, status)
    call IXFoperation_run(op,'horiz_posn', arg%horiz_posn, status)
    call IXFoperation_run(op,'vert_posn', arg%vert_posn, status)        
    call IXFoperation_run(op,'width', arg%width, status)
    call IXFoperation_run(op,'height', arg%height, status)
    call IXFoperation_run(op,'radius', arg%radius, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_aperture


  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_aperture(aperture, name, distance, shape, horiz_posn, vert_posn, width, height, radius, status)
    implicit none
    type(IXTaperture),intent(out) :: aperture
    character(len=*),intent(in):: name
    character(len=*),intent(in):: shape
    real(dp),intent(in):: distance,  horiz_posn, vert_posn,width, height, radius
    !	    real(dp),optional::width,height, radius ??? how to do???
    type(IXTstatus) :: status

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(aperture)

    ! for the optional present base class then the full set command is called
    call IXFset_aperture(aperture, status, name, distance, shape, horiz_posn, vert_posn, &
            width, height, radius)

  end subroutine IXFcreate_aperture

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_aperture(aperture, status, name, distance, &
       shape, horiz_posn, vert_posn, width, height, radius,ref)
    implicit none
    type(IXTaperture),intent(inout) :: aperture
    type(IXTaperture),optional,intent(in) :: ref
    character(len=*),optional, intent(in):: name
    character(len=*),optional, intent(in):: shape
    real(dp),optional, intent(in):: distance,  horiz_posn, vert_posn,width, height, radius
    !	    real(dp),optional::width, height, radius
    type(IXTstatus),intent(inout) :: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_aperture)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(aperture)
    else    
       if(IXFvalid(aperture) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_aperture)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_aperture(aperture, status, ref%name, &
         ref%distance, ref%shape, ref%horiz_posn, ref%vert_posn, &
         ref%width, ref%height, ref%radius)

    if (present(name))aperture%name=name
    if (present(distance))aperture%distance=distance
    if (present(shape))aperture%shape=shape
    if (present(horiz_posn))aperture%horiz_posn=horiz_posn
    if (present(vert_posn))aperture%vert_posn=vert_posn
    if (present(width))aperture%width=width
    if (present(height))aperture%height=height
    if (present(radius))aperture%radius=radius


    call IXFcheck_aperture(aperture, status)

  end subroutine IXFset_aperture

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_aperture(aperture, status, name, distance, shape, horiz_posn, vert_posn, width, height, radius, wout)
    implicit none
    type(IXTaperture),intent(inout) :: aperture
    type(IXTaperture),optional,intent(out) :: wout
    character(len=*),optional, intent(out):: name
    character(len=*),optional, intent(out):: shape
    real(dp),optional, intent(out):: distance,  horiz_posn, vert_posn,width, height, radius
    !	    real(dp),optional::width,height, radius
    type(IXTstatus),intent(inout) :: status

    if (present(wout))call IXFcopy(aperture,wout, status)

    if (present(name))name=aperture%name
    if (present(distance))distance=aperture%distance
    if (present(shape))shape=aperture%shape
    if (present(horiz_posn))horiz_posn=aperture%horiz_posn
    if (present(vert_posn))vert_posn=aperture%vert_posn
    if (present(width))width=aperture%width
    if (present(height))height=aperture%height
    if (present(radius))radius=aperture%radius

  end subroutine IXFget_aperture

end module IXMaperture

