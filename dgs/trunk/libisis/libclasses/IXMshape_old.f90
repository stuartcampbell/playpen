!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMshape
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMshape object.
!!
!! Nearest equivalent NeXus class: NXshape
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1344 $ ($Date: 2008-04-22 05:53:04 -0400 (Tue, 22 Apr 2008) $)
!!

module IXMshape
  use IXMtype_definitions
  use IXMbase
  use IXMmaths_geometry
  use IXMmaths_projection
  implicit none
  ! default is all variables and functions are hidden
  !	private
  ! public parameters
  integer(i4b), parameter, public :: IXCpoint=0, IXCbox=1, IXCcylinder=2, IXCsphere=3, IXCholcyl=4, &
       IXCpolygon=5
  ! public types
  public :: IXTshape
  ! public interfaces
  public :: IXFvolume

  type IXTshape
     private		! The type is public, but the contents not
     integer(i4b):: type						! type of shape (IXCbox, IXCcylinder ...)
     real(dp), pointer :: dimensions(:) => NULL()	! length and interpretation of contents of dimensions depends on value of TYPE
  end type IXTshape


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE shape
#define IXD_NO_BASE	1
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

  interface IXFvolume
     module procedure IXFvolume_shape
  end interface

  interface IXFsolid_angle
     module procedure IXFsolid_angle_shape
  end interface

  !----------------------------------------------------------------------------------------------------------------------
contains

#define IXD_DESCRIPTION	"IXTshape class"
#define IXD_TYPE shape
#define IXD_SQTYPE 'shape'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_shape(arg, status)
    implicit none
    type(IXTshape) :: arg
    type(IXTstatus) :: status
  end subroutine IXFdestroy_shape

  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_shape(op, field, arg, status)
    implicit none
    type(IXTshape) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    call IXFoperationStart(op, 'IXTshape', field, status)
    call IXFoperation_run(op,'type', arg%type, status)
    call IXFoperation_run_ptr(op,'dimensions', arg%dimensions, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_shape

  !----------------------------------------------------------------------------------------------------------------------
  !! Set shape properties by
  !!    - copying another shpae object
  !!    - the dimensions of a shape
  !!    - the type and dimensions of a shape of a shape
  !!
  !! [A copy is made of the input; the output does *not* point to the input.]
  !    subroutine IXFset_shape(self, status,  type, dimensions,ref)
  !		implicit none
  !		type(IXTshape), intent(INOUT) :: self			!! the shape object we are going to set
  !		type(IXTshape), intent(IN), optional :: ref	!! shape from which to make a copy
  !	    integer(i4b), intent(IN), optional :: type		!! set type of shape
  !	    real(dp), intent(IN), optional :: dimensions(:)		!! set size of shape
  !        type(IXTstatus):: status
  !        
  !		if (present(ref) .OR. (present(type) .AND. present(dimensions)) .OR. present(dimensions)) then	!! valid options
  !			if (present(ref)) then
  !				call IXFrealloc (self%dimensions, size(ref%dimensions), .FALSE., status)
  !				self = ref
  !			elseif (present(type) .AND. present(dimensions)) then
  !				call IXFrealloc (self%dimensions, size(dimensions), .FALSE., status)
  !				self%type = type
  !				self%dimensions = dimensions
  !			elseif (present(dimensions)) then
  !				call IXFrealloc (self%dimensions, size(dimensions), .FALSE., status)
  !				self%dimensions = dimensions
  !			endif
  !			call IXFcheck_shape(self, status)
  !		elseif (count((/present(ref),present(type),present(dimensions)/))==0) then
  !			continue
  !		else
  !			call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
  !			 & 'Check number of arguments to IXFset_ref')
  !		endif
  !
  !	end subroutine 



  recursive subroutine IXFset_shape(self, status,  type, dimensions,ref)
    implicit none
    type(IXTshape), intent(INOUT) :: self			!! the shape object we are going to set
    type(IXTshape), intent(IN), optional :: ref	!! shape from which to make a copy
    integer(i4b), intent(IN), optional :: type		!! set type of shape
    real(dp), intent(IN), optional :: dimensions(:)		!! set size of shape
    type(IXTstatus):: status

    ! this does the copy
    if (present(ref))call IXFset_shape(self,status,ref%type,ref%dimensions)

    if (present(type))self%type=type

    if ( present(dimensions))then
       call IXFrealloc(self%dimensions,size(dimensions),.false.,status)
       self%dimensions=dimensions
    else
       if (.not. associated(self%dimensions))then
          call IXFalloc(self%dimensions,1,status)
          self%dimensions(1)=0.0_dp
       endif
    endif

    call IXFcheck_shape(self, status)

    if (count((/present(ref),present(type),present(dimensions)/))==0) then
       continue
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Check number of arguments to IXFset_ref')
    endif

  end subroutine IXFset_shape


!!! Get components of a shape. 
  !    subroutine IXFget_shape(self, status, shape, type, dimensions)
  !		implicit none
  !		type(IXTshape), intent(IN) :: self					!! the shape object from which attributes will be extracted
  !		type(IXTshape), intent(OUT), optional :: shape		!! get shape
  !		integer(i4b), intent(OUT), optional :: type			!! get type of object
  !	    real(dp), intent(OUT), optional :: dimensions(:)	!! get dimensions of object
  !		type(IXTstatus), intent(INOUT) :: status
  !		if (present(shape)) shape = self
  !		if (present(type)) type = self%type
  !		if (present(dimensions)) dimensions = self%dimensions
  !	end subroutine 



  subroutine IXFget_shape(self, status, type, dimensions,wout)
    implicit none
    type(IXTshape), intent(IN) :: self					!! the shape object from which attributes will be extracted
    type(IXTshape), intent(OUT), optional :: wout		!! get shape
    integer(i4b), intent(OUT), optional :: type			!! get type of object
    real(dp), intent(OUT), optional :: dimensions(:)	!! get dimensions of object
    type(IXTstatus), intent(INOUT) :: status

    if (present(wout))call IXFset_shape(wout,status,ref=self)

    if (present(type))type=self%type

    if (present(dimensions))then
       if(size(dimensions) == size(self%dimensions))then
          dimensions=self%dimensions
       else
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'supplied array must be correct size to receive dimensions array (IXFget_shape)')
       endif
    endif
  end subroutine IXFget_shape


  subroutine IXFget_ptr_shape(shape,dims_ptr)
    real(dp),pointer::dims_ptr(:) !!output: pulse_pars array pointer
    type(IXTshape),intent(in)::shape !!input shape 

    dims_ptr=>shape%dimensions

  end subroutine IXFget_ptr_shape

  !!  subroutine which returns dimensions array from a shape object into an allocatable array.
  !! If the array passed has been allocated, it checks the array length is correct, otherwise
  !! it deallocates the passed array and reallocates it to an appropriate length.
  subroutine IXFget_alloc_shape(shape,dims_alloc)!,status)
    real(dp),allocatable::dims_alloc(:) !!dimensions array pointer
    type(IXTshape),intent(in)::shape !!input dataset_1d 

    if (allocated(dims_alloc))then
       if (size(dims_alloc) /=  size(shape%dimensions) )then
          deallocate(dims_alloc)
          allocate(dims_alloc(size(shape%dimensions)))
       endif
    else
       allocate(dims_alloc(size(shape%dimensions)))
    endif
    dims_alloc=shape%dimensions  

  end subroutine IXFget_alloc_shape











  !! Create a shape from components. The 'create' routine differs from the 'set' routine in that
  !! the 'set' routine allows an existing shape to have only part of its attributes altered. The
  !! 'create' routine requires all of the attributes to be set. The 'create' routine can be mimiced
  !! using the 'set' routine - indeed, that is precisely what the code of the 'create' routine does.
  subroutine IXFcreate_shape(self, status,  type, dimensions,shape)
    implicit none
    type(IXTshape), intent(OUT) :: self					!! the shape object we are going to set
    type(IXTshape), intent(IN), optional :: shape		!! the shape object to copy
    integer(i4b), intent(IN), optional :: type			!! type of shape
    real(dp), intent(IN), optional :: dimensions(:)		!! dimensions of shape
    type(IXTstatus):: status

    if (count((/present(shape),present(type),present(dimensions)/))==0) then	! default shape (point)
       self%type = IXCpoint
       call IXFrealloc (self%dimensions, 0, .FALSE., status)
    else
       call IXFset_shape(self, status,  type, dimensions)
       if (status/=0) call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Check number of arguments to IXFcreate_shape')
    endif

  end subroutine IXFcreate_shape

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
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
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
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
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
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid box parameters - failed in IXFcheck_box')
    endif
    return
  end subroutine IXFcheck_box

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_box (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    volume = product(dimensions)
    return
  end function IXFvolume_box

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_box (dims, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    ! local arguments
    integer(i4b) :: k(1), cyclic(2:5)=(/2,3,1,2/)
    real(dp) :: distsqr
    save cyclic

    ! we treat as a the case of a polygon, determining the dimension of the the 'thickness' as the minimum thickness
    distsqr = vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3)
    k = minloc(dims)
    omega = (dims(cyclic(k(1)+1))*dims(cyclic(k(1)+2)))*(abs(vp(k(1)))/sqrt(distsqr))/distsqr
    return
  end function IXFsolid_angle_box

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_box (dims, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b) :: nvert, ivert, i, j, k(1), cyclic(2:5)=(/2,3,1,2/)
    type(IXTstatus) :: status
    save cyclic

    nvert = 4
    k = minloc(dims)
    i = cyclic(k(1)+1)
    j = cyclic(k(1)+2)
    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(i,:) = 0.5_dp*(/-dims(i),dims(i),dims(i),-dims(i)/) - vector(i)
    area_vertices(j,:) = 0.5_dp*(/-dims(j),-dims(j),dims(j),dims(j)/) - vector(j)
    area_vertices(k(1),:) = -vector(k(1))
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_box

  !======================================================================================================================
  ! Cylinder functions
  !   Parameters: dimensions(1:2) = (radius, height) ; cylinder axis along z axis
  !   Centred: at centre of cylinder
  !======================================================================================================================
  subroutine IXFcheck_cylinder (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    if (size(dimensions)/=2 .OR. minval(dimensions) < 0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid cylinder parameters - failed in IXFcheck_cylinder')
    endif
    return
  end subroutine IXFcheck_cylinder

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_cylinder (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    volume = pi_dp*(dimensions(1)**2)*dimensions(2)		! radius is first parameter, height the second
    return
  end function IXFvolume_cylinder

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_cylinder (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    ! local arguments
    real(dp) :: distsqr

    distsqr = vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3)
    omega = (2.0_dp*dimensions(1)*dimensions(2)) * sqrt((vp(1)*vp(1) + vp(2)*vp(2))/distsqr) / distsqr

    return
  end function IXFsolid_angle_cylinder

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_cylinder (dims, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b) :: nvert, ivert
    type(IXTstatus) :: status

    nvert = 4
    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(1,:) = (/-dims(1),dims(1),dims(1),-dims(1)/) - vector(1)            ! x coordinates
    area_vertices(2,:) = 0.5_dp*(/-dims(2),-dims(2),dims(2),dims(2)/) - vector(2)     ! y coordinates
    area_vertices(3,:) = -vector(3)
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_cylinder

  !======================================================================================================================
  ! Sphere functions
  !   Parameters: dimensions(1) = radius
  !   Centred: at centre of sphere
  !======================================================================================================================
  subroutine IXFcheck_sphere (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    if (size(dimensions)/=1 .OR. minval(dimensions) < 0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid sphere parameters - failed in IXFcheck_sphere')
    endif
    return
  end subroutine IXFcheck_sphere

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_sphere (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    volume = (4.0_dp/3.0_dp)*pi_dp*(dimensions(1)**3)
    return
  end function IXFvolume_sphere

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_sphere (dimensions, vp) result(omega)
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    omega = (pi_dp*dimensions(1)**2) / (vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3))
    return
  end function IXFsolid_angle_sphere

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_sphere (dims, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b), parameter :: nvert=24     !24 sided polygon
    integer(i4b) :: i,ivert
    real(dp) :: arr(nvert)
    type(IXTstatus) :: status
    logical :: first_time = .TRUE.
    save first_time, arr

    if (first_time) then
       forall (i=1:nvert) arr(i) = twopi_dp*real(i-1,kind(1.0_dp))/real(nvert,kind(1.0_dp))
       first_time = .FALSE.
    endif

    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(1,:) = dims(1)*cos(arr) - vector(1)               ! x coordinates
    area_vertices(2,:) = dims(1)*sin(arr) - vector(2)               ! y coordinates
    area_vertices(3,:) = -vector(3)
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_sphere
  !======================================================================================================================
  ! Hollow cylinder functions
  !   Parameters: dimensions(1:3) = (inner_radius, outer_radius, height) ; cylinder axis along z axis
  !   Centred: at centre of cylinder
  !======================================================================================================================
  subroutine IXFcheck_holcyl (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    if (size(dimensions)/=3 .OR. minval(dimensions) < 0.0_dp .OR. dimensions(2)<dimensions(1)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid hollow cylinder parameters - failed in IXFcheck_holcyl')
    endif
    return
  end subroutine IXFcheck_holcyl

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_holcyl (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    volume = pi_dp*(dimensions(2)-dimensions(1))*(dimensions(2)+dimensions(1))*dimensions(3)	! dimensions contains r_out, r_in, height
    return
  end function IXFvolume_holcyl

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_holcyl (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    ! local arguments
    real(dp) :: distsqr

    distsqr = vp(1)*vp(1)+vp(2)*vp(2)+vp(3)*vp(3)
    omega = (2.0_dp*dimensions(2)*dimensions(3)) * sqrt((vp(1)*vp(1) + vp(2)*vp(2))/distsqr) / distsqr

    return
  end function IXFsolid_angle_holcyl

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_holcyl (dims, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dims(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b) :: nvert, ivert
    type(IXTstatus) :: status

    nvert = 4
    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(1,:) = (/-dims(1),dims(1),dims(1),-dims(1)/) - vector(1)            ! x coordinates
    area_vertices(2,:) = 0.5_dp*(/-dims(2),-dims(2),dims(2),dims(2)/) - vector(2)     ! y coordinates
    area_vertices(3,:) = -vector(3)
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_holcyl

  !======================================================================================================================
  ! Polygon functions
  !   A polygon shape has verticies in the x-y plane, with a thickness along the z axis
  !   Parameters: dimensions(1:nvert) = x coordinates
  !               dimensions(nvert+1:2*nvert) = y coordinates
  !               dimensions(2*nvert+1) = full thickness, t.  If missing, then thickness assumed to be zero.
  !   Centred: at (0,0) in x-y plane, and the z axis is centred in middle of the volume, i.e. at t/2
  !======================================================================================================================
  subroutine IXFcheck_polygon (dimensions, status)
    real(dp) dimensions(:)
    type(IXTstatus) status
    integer(i4b) :: n, nvert
    ! Check is incomplete - we should check that the boundary of the polygon does not cross itself.
    n = size(dimensions)
    nvert = size(dimensions)/2
    if (nvert < 3) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'No. verticies must be 3 or greater - failed in IXFcheck_polygon')
       return
    endif
    if (mod(n,2)==1 .AND. dimensions(n)<0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Thickness must >= 0.0 - failed in IXFcheck_polygon')
       return
    endif
    return
  end subroutine IXFcheck_polygon

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_polygon (dimensions) result(volume)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:)
    real(dp) :: volume
    ! local arguments
    real(dp) :: area
    integer(i4b) :: nvert
    type(IXTstatus) :: status
    if (mod(size(dimensions),2)==1) then    ! odd number of elements, so last one is the thickness
       nvert = size(dimensions)/2
       call IXFpolygon_moments (dimensions(1:nvert), dimensions(nvert+1:2*nvert), status, area)
       if (status==IXCseverity_error) return
       volume = abs(area)*dimensions(2*nvert+1)    ! recall area is directed, so get modulus
    else
       volume = 0.0_dp ! no last element, so zero thickness
    endif
    return
  end function IXFvolume_polygon

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_polygon (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates
    real(dp) :: omega
    ! local arguments
    real(dp) :: area, distsqr, cent(2)
    integer(i4b) :: nvert
    type(IXTstatus) :: status

    nvert = size(dimensions)/2
    call IXFpolygon_moments (dimensions(1:nvert), dimensions(nvert+1:2*nvert), status, area, cent)
    distsqr = (vp(1)-cent(1))**2+(vp(2)-cent(2))**2+vp(3)*vp(3)
    omega = abs(area)*(abs(vp(3))/sqrt(distsqr))/distsqr   ! recall area is directed, so get modulus
    return
  end function IXFsolid_angle_polygon

  !----------------------------------------------------------------------------------------------------------------------
  function IXFarea_vertices_polygon (dimensions, rotmat, vector) result(area_vertices)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:)
    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
    ! local arguments
    integer(i4b) :: nvert, ivert
    type(IXTstatus) :: status

    nvert = size(dimensions)/2
    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
    area_vertices(1,:) = dimensions(1:nvert) - vector(1)            ! x coordinates
    area_vertices(2,:) = dimensions(nvert+1:2*nvert) - vector(2)    ! y coordinates
    area_vertices(3,:) = -vector(3)                                 ! use the centre plane of the polygon
    forall (ivert=1:nvert)
       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
    end forall

  end function IXFarea_vertices_polygon

  !======================================================================================================================
end module IXMshape

