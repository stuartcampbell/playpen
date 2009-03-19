!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMgeometry
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMgeometry object.
!!
!! Nearest equivalent NeXus class: NXgeometry
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1344 $ ($Date: 2008-04-22 05:53:04 -0400 (Tue, 22 Apr 2008) $)
!!

module IXMgeometry
  use IXMtype_definitions
  use IXMbase
  use IXMtranslation
  use IXMorientation
  use IXMshape
  implicit none
  ! default is all variables and functions are hidden
  !	private
  ! public parameters

  ! public types
  public :: IXTgeometry
  ! public interfaces
  !   public :: 

  type IXTgeometry
     private		! The type is public, but the contents not
     type(IXTtranslation) :: translation
     type(IXTorientation) :: orientation
     type(IXTshape) :: shape
  end type IXTgeometry


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE geometry
#define IXD_NO_BASE	1
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

!  interface IXFcreate
!     module procedure IXFcreate_class_geometry, IXFcreate_attributes_geometry
!  end interface

  interface IXFvolume
     module procedure IXFvolume_geometry
  end interface

  interface IXFsolid_angle
     module procedure IXFsolid_angle_geometry
  end interface

  !----------------------------------------------------------------------------------------------------------------------
contains

#define IXD_DESCRIPTION	"IXTgeometry class"
#define IXD_TYPE geometry
#define IXD_SQTYPE 'geometry'
#include "class_base.f90"

  !! destroy routine 
  subroutine IXFdestroy_geometry(arg, status)
    implicit none
    type(IXTgeometry) :: arg
    type(IXTstatus) :: status

    call IXFdestroy(arg%translation,status)
    call IXFdestroy(arg%orientation,status)
    call IXFdestroy(arg%shape,status)

  end subroutine IXFdestroy_geometry

  !!generic create routine, there are some specialized routines which are interfaced to 'IXFcreate'
  subroutine IXFcreate_geometry(geometry, translation, orientation,shape,status)
    implicit none
    type(IXTgeometry) :: geometry
    type(IXTtranslation)::translation
    type(IXTorientation)::orientation
    type(IXTshape)::shape
    type(IXTstatus) :: status   

    call IXFset_geometry(geometry,status,translation, orientation,shape)


  end subroutine IXFcreate_geometry

  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_geometry(op, field, arg, status)
    implicit none
    type(IXTgeometry) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTshape', field, status)
    call IXFoperation_run(op,'translation', arg%translation, status)
    call IXFoperation_run(op,'orientation', arg%orientation, status)
    call IXFoperation_run(op,'shape', arg%shape, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_geometry

  !----------------------------------------------------------------------------------------------------------------------
  !! Set geometry from one of
  !!   - another geometry object
  !!   - the components of a geometry object
  !!
  !! Syntax:
  !!   - call IXFset_geometry (geom_out, ref=geom_in)
  !!   - call IXFset_geometry (geom_out, status, [translation=tr,] [orientation=or,] [shape=sh])
  !!
  recursive subroutine IXFset_geometry (geometry, status, translation, orientation, shape,ref)
    implicit none
    type(IXTgeometry), intent(INOUT) :: geometry					    !! the geometry object we are going to set
    type(IXTgeometry), optional, intent(IN) :: ref					        !! the reference geometry object which is copied
    type(IXTtranslation), intent(IN), optional :: translation		!! translation object
    type(IXTorientation), intent(IN), optional :: orientation		!! orientation object
    type(IXTshape), intent(IN), optional :: shape		        	!! shape object
    type(IXTstatus):: status
    ! So long as the objects are valid (as they must be if already created), then any combination is valid
    if (present(ref))call IXFset_geometry(geometry,status,ref%translation,ref%orientation,ref%shape)

    if (present(translation)) call IXFcopy(translation,geometry%translation,status)
    if (present(orientation)) call IXFcopy(orientation,geometry%orientation,status)
    if (present(shape)) call IXFcopy(shape,geometry%shape,status)

    call IXFcheck_geometry(geometry,status)
  end subroutine IXFset_geometry


  !! Get components of a geometry object. 
  !!
  !! Syntax:
  !!   - call IXFget_geometry (geometry, wout=geometry_out)        to get a copy of the geometry object
  !!   - call IXFget_geometry (geometry, status, [translation=tr,] [orientation=or,] [shape=sh]) to get one or more attributes       
  !!
  subroutine IXFget_geometry(geometry, status, translation, orientation, shape,wout)
    implicit none
    type(IXTgeometry), intent(IN) :: geometry			    !! the geometry object from which attributes will be extracted
    type(IXTgeometry),optional, intent(out) :: wout			    !! the geometry object which attributes will be copied to
    type(IXTtranslation), intent(OUT), optional :: translation		!! translation object
    type(IXTorientation), intent(OUT), optional :: orientation		!! orientation object
    type(IXTshape), intent(OUT), optional :: shape		            !! shape object
    type(IXTstatus):: status

    if (present(wout))call IXFcopy(geometry,wout,status)
    ! So long as the objects are valid (as they must be if already created), then any combination is valid
    if (present(translation)) translation = geometry%translation
    if (present(orientation)) orientation = geometry%orientation
    if (present(shape)) shape = geometry%shape
  end subroutine IXFget_geometry


  !! Create a geometry object from components.
  !!
  !! Syntax:
  !!   - call IXFcreate_geometry (geom_out, geom_in)	    copy geom_in
  !!   - call IXFcreate_geometry (geom_out, status, [translation=tr,] [orientation=or,] [shape=sh]) create with these components
  !!   - call IXFcreate_geometry (geom_out, status)           default geometry object (no translation, identity rotation, point shape)
  !! this is the old routine which is interfaced to IXFcreate 
  subroutine IXFcreate_class_geometry (self, geometry)
    implicit none
    type(IXTgeometry), intent(OUT) :: self			!! the geometry object we are going to create
    type(IXTgeometry), intent(IN)  :: geometry	    !! the geometry object we are going to copy
    self = geometry
  end subroutine IXFcreate_class_geometry

  subroutine IXFcreate_attributes_geometry (self, status, translation, orientation, shape)
    implicit none
    type(IXTgeometry), intent(INOUT) :: self					    !! the geometry object we are going to set
    type(IXTtranslation), intent(IN), optional :: translation		!! translation object
    type(IXTorientation), intent(IN), optional :: orientation		!! orientation object
    type(IXTshape), intent(IN), optional :: shape		        	!! shape object
    type(IXTstatus):: status
    ! internal variables
    type(IXTgeometry):: default_geometry
    logical :: first_time = .TRUE.
    save first_time, default_geometry

    ! Create default output:
    if (first_time) then
       !    		call IXFcreate_translation (default_geometry%translation, status)
       !    		call IXFcreate_orientation (default_geometry%orientation, status)
       !    		call IXFcreate_shape (default_geometry%shape, status)
       call IXFset_translation (default_geometry%translation, status)
       call IXFset_orientation (default_geometry%orientation, status)
       call IXFset_shape (default_geometry%shape, status)
       first_time = .FALSE.
    endif
    self = default_geometry    		

    ! Now call set routine:
    if (count((/present(translation),present(orientation),present(shape)/))/=0) then
       call IXFset_geometry(self, status, translation, orientation, shape)
       if (status /= IXCseverity_ok) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
               & 'Error in IXFcreate_attributes_geometry')
       endif
    endif

  end subroutine IXFcreate_attributes_geometry

  !----------------------------------------------------------------------------------------------------------------------
  !! Subroutine to check consistency of arguments.

  ! dummy routine, as attributes are already valid, as their own check routines guarantee that. 
  subroutine IXFcheck_geometry(geometry, status)
    type(IXTgeometry) :: geometry
    type(IXTstatus) :: status

    call IXFcheck_translation(geometry%translation,status)
    call IXFcheck_orientation(geometry%orientation,status)
    call IXFcheck_shape(geometry%shape,status)
  end subroutine IXFcheck_geometry

  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:
  !----------------------------------------------------------------------------------------------------------------------
  !! Volume of a geometry object
  function IXFvolume_geometry (geometry) result(volume)
    type(IXTgeometry), intent(in) :: geometry
    real(dp) :: volume
    volume = IXFvolume_shape (geometry%shape)
    return
  end function IXFvolume_geometry

  !----------------------------------------------------------------------------------------------------------------------
  !! Solid angle of a geometric object from a viewpoint other than the origin
  function IXFsolid_angle_geometry (geometry, vp) result(omega)
    type(IXTgeometry), intent(in) :: geometry
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    ! local variables:
    type(IXTstatus) :: status
    real(dp) :: vp_prime(3)

    vp_prime = IXFs2sprime_orientation (geometry%orientation, geometry%translation, vp) ! get viewpoint in shape coordinates
    omega = IXFsolid_angle_shape (geometry%shape, vp_prime)

    return
  end function IXFsolid_angle_geometry

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_geometry (geometry, or, t) result(area_vertices)
    ! i/o arguments:
    type(IXTgeometry), intent(in) :: geometry
    type(IXTorientation), intent(in) :: or          !! orientation matrix of view coord frame
    type(IXTtranslation), intent(in) :: t           !! origin of view coord frame
    real(dp), pointer :: area_vertices(:,:)         !! coords of vertices in view coord frame
    ! local arguments
    type(IXTorientation) :: odiff
    type(IXTtranslation) :: tdiff
    real(dp) :: rotmat(3,3), vector(3)
    type(IXTstatus) status

    ! Get orientation and translation of view frame w.r.t. shape frame
    call IXFdifference_orientation(geometry%orientation, geometry%translation, or, t, odiff, tdiff)
    !*** must tidy here, as should use get_absolute_translation, get_absolute_orientation or soemthing:
    call IXFget_orientation(odiff, status, rotmat=rotmat)
    call IXFget_translation(tdiff, status, vector=vector)
    area_vertices => IXFarea_vertices_shape (geometry%shape, rotmat, vector)

  end function IXFarea_vertices_geometry

  !----------------------------------------------------------------------------------------------------------------------
  subroutine IXFprojarea_vertices_geometry (geometry, or, t, projection, px, py, status, radius, axes)
    type(IXTgeometry), intent(in):: geometry
    type(IXTorientation), intent(in) :: or          !! orientation matrix of view coord frame
    type(IXTtranslation), intent(in) :: t           !! origin of view coord frame
    integer(i4b), intent(in) :: projection
    real(dp), pointer :: px(:), py(:)
    real(dp), intent(in), optional :: radius
    integer(i4b), intent(in), optional :: axes(2)
    type(IXTstatus) :: status
    ! internal declarations
    type(IXTorientation) :: odiff
    type(IXTtranslation) :: tdiff
    real(dp) :: rotmat(3,3), vector(3)

    ! Get orientation and translation of view frame w.r.t. shape frame
    call IXFdifference_orientation(geometry%orientation, geometry%translation, or, t, odiff, tdiff)
    !*** must tidy here, as should use get_absolute_translation, get_absolute_orientation or soemthing:
    call IXFget_orientation(odiff, status, rotmat=rotmat)
    call IXFget_translation(tdiff, status, vector=vector)
    call IXFprojarea_vertices_shape (geometry%shape, rotmat, vector, projection, px, py, status, radius, axes)

    return
  end subroutine IXFprojarea_vertices_geometry

  !----------------------------------------------------------------------------------------------------------------------
end module IXMgeometry
