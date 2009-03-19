!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMshape
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMshape object.
!!
!! Nearest equivalent NeXus class: NXshape
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!

module IXMshape
  use IXMbase
  use IXMmaths_geometry
  use IXMmaths_projection
  implicit none
  ! default is all variables and functions are hidden
  ! public parameters
  integer(i4b), parameter, public :: IXCpoint=0, IXCbox=1, IXCcylinder=2, IXCsphere=3, IXCholcyl=4, IXCpolygon=5
  ! public types
  public :: IXTshape
  ! public interfaces
  type IXTshape
     private		! The type is public, but the contents not
     type(IXTbase):: base
     integer(i4b):: shape_type = -25  !! type of shape (IXCbox, IXCcylinder ...)
     real(dp), pointer :: dimensions(:,:) => NULL() !! dimensions(n_vals,n_objects), where n_vals and contents depend on shape_type
  end type IXTshape

  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE shape
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
        call IXFdealloc(arg%dimensions,status)
    arg%shape_type=-50    
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_shape

  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_shape(op, field, arg, status)
    implicit none
    type(IXTshape) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTshape', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    call IXFoperation_run(op, 'base', arg%base, status)    
    call IXFoperation_run(op,'shape_type', arg%shape_type, status)
    call IXFoperation_run_ptr(op,'dimensions', arg%dimensions, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_shape

  !----------------------------------------------------------------------------------------------------------------------
  ! Create a shape from components. The 'create' routine differs from the 'set' routine in that
  ! the 'set' routine allows an existing shape to have only part of its attributes altered. The
  ! 'create' routine requires all of the attributes to be set. The 'create' routine can be mimiced
  ! using the 'set' routine - indeed, that is precisely what the code of the 'create' routine does.
  subroutine IXFcreate_shape(shape, status, shape_type, dimensions)
    implicit none
    type(IXTshape), intent(OUT) :: shape					!! the shape object we are going to set
    integer(i4b), intent(IN) :: shape_type	!! type of shape
    real(dp), intent(IN) :: dimensions(:,:)	!! dimensions of shape
    type(IXTstatus):: status

    call IXFmark_valid(shape)
    call IXFset_shape(shape, status, shape_type, dimensions)

  end subroutine IXFcreate_shape

  subroutine IXFcreate_fromtype_shape(shape, status, shape_type, dimensions)
    implicit none
    type(IXTshape), intent(OUT) :: shape					!! the shape object we are going to set
    integer(i4b), intent(IN) :: shape_type	!! type of shape
    real(dp), intent(IN) :: dimensions(:,:)	!! dimensions of shape
    type(IXTstatus):: status
    integer(i4b)::dim_2
    
    shape%shape_type=shape_type
    dim_2=size(dimensions,2)
    if(size(dimensions,1)<1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input dimension array is of zero length(IXFcreate_fromtype_shape)')
               return
    endif
    if (shape%shape_type == IXCpoint) then
       call IXFreallocdims (shape%dimensions, (/ 1, dim_2 /), .false.,status)
       !there should be at least one dimension
       shape%dimensions=dimensions(1:1,:)
    elseif (shape%shape_type == IXCbox) then
       call IXFreallocdims (shape%dimensions, (/ 3, dim_2 /), .false.,status)
       if(size(dimensions,1)<3)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input dimension array is not long enough for shape type (IXFcreate_fromtype_shape)')
               return
       endif       
       shape%dimensions=dimensions(1:3,:)
    else if (shape%shape_type == IXCcylinder) then
       call IXFreallocdims (shape%dimensions, (/ 2, dim_2 /), .false.,status)
       if(size(dimensions,1)<2)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input dimension array is not long enough for shape type (IXFcreate_fromtype_shape)')
               return
       endif
       shape%dimensions=dimensions(1:2,:)
    else if (shape%shape_type == IXCsphere) then
       call IXFreallocdims (shape%dimensions, (/ 1, dim_2 /), .false.,status)
       shape%dimensions=dimensions(1:1,:)
    else if (shape%shape_type == IXCholcyl) then
       call IXFreallocdims (shape%dimensions, (/ 3, dim_2 /), .false.,status)
       if(size(dimensions,1)<3)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input dimension array is not long enough for shape type (IXFcreate_fromtype_shape)')
          return
       endif
       shape%dimensions=dimensions(1:3,:)       
    else if (shape%shape_type == IXCpolygon) then
       call IXFreallocdims (shape%dimensions, (/ size(dimensions,1), dim_2 /), .false.,status)
       shape%dimensions=dimensions
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid shape type - failed in IXFcheck_shape')
       return
    endif

    call IXFmark_valid(shape)
  end subroutine IXFcreate_fromtype_shape


  recursive subroutine IXFset_shape(shape, status, shape_type, dimensions,ref)
    implicit none
    type(IXTshape), intent(INOUT) :: shape			    !! the shape object we are going to set
    type(IXTshape), intent(IN), optional :: ref	        !! shape from which to make a copy
    integer(i4b), intent(IN), optional :: shape_type	!! set type of shape
    real(dp), intent(IN), optional :: dimensions(:,:)		!! set size of shape
    type(IXTstatus):: status

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_shape)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(shape)
    else    
       if(IXFvalid(shape) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_shape)')
       endif
       if(status == IXCseverity_error)return
    endif
    
    if (present(ref)) call IXFset_shape(shape, status, ref%shape_type, ref%dimensions)
    if (present(shape_type)) shape%shape_type=shape_type
    call IXFset_real_array(shape%dimensions,status,dimensions)
    call IXFcheck_shape(shape, status)

  end subroutine IXFset_shape 



  subroutine IXFget_shape(shape, status, shape_type, dimensions,wout)
    implicit none
    type(IXTshape), intent(IN) :: shape					!! the shape object from which attributes will be extracted
    type(IXTshape), intent(OUT), optional :: wout		!! get shape
    integer(i4b), intent(OUT), optional :: shape_type			!! get type of object
    real(dp), intent(OUT), optional :: dimensions(:,:)	!! get dimensions of object
    type(IXTstatus), intent(INOUT) :: status

    if (present(wout))call IXFcopy(shape,wout,status)
    if (present(shape_type))shape_type=shape%shape_type
    
    call IXFget_real_array(shape%dimensions,status,dimensions)

  end subroutine IXFget_shape


  subroutine IXFget_ptr_shape(shape,dimensions)
    real(dp),pointer,optional::dimensions(:,:) !!out pointer
    type(IXTshape),intent(in),target::shape !!input shape 

    if(present(dimensions))dimensions=>shape%dimensions

  end subroutine IXFget_ptr_shape

  !!  subroutine which returns dimensions array from a shape object into an allocatable array.
  !! If the array passed has been allocated, it checks the array length is correct, otherwise
  !! it deallocates the passed array and reallocates it to an appropriate length.
  subroutine IXFget_alloc_shape(arg,status,shape_type,dimensions)
    real(dp),allocatable,optional::dimensions(:,:) !!dimensions array pointer
    integer(i4b),intent(out),optional::shape_type
    type(IXTshape),intent(in)::arg
    type(IXTstatus),intent(inout)::status    

    if (present(dimensions))then       
       call IXFreallocdimsFortran(dimensions,shape(arg%dimensions),.false.,status)
    endif
    
    call IXFget_shape(arg,status,shape_type,dimensions)
  end subroutine IXFget_alloc_shape

  integer(i4b) function IXFsize_shape(shape) result(size_s)
    implicit none
    type(IXTshape)::shape
    
    if (associated(shape%dimensions))then
      size_s=size(shape%dimensions,2)
    else
      size_s=0
    endif
  end function IXFsize_shape
  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments

  subroutine IXFcheck_shape(shape, status)
    type(IXTshape) :: shape
    type(IXTstatus) :: status

    ! Must hand-craft in the various valid shape types at this point
    if (shape%shape_type == IXCpoint) then
       call IXFcheck_point (shape%dimensions, status)
    elseif (shape%shape_type == IXCbox) then
       call IXFcheck_box (shape%dimensions, status)
    else if (shape%shape_type == IXCcylinder) then
       call IXFcheck_cylinder (shape%dimensions, status)
    else if (shape%shape_type == IXCsphere) then
       call IXFcheck_sphere (shape%dimensions, status)
    else if (shape%shape_type == IXCholcyl) then
       call IXFcheck_holcyl (shape%dimensions, status)
    else if (shape%shape_type == IXCpolygon) then
       call IXFcheck_polygon (shape%dimensions, status)
    else
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid shape type - failed in IXFcheck_shape')
    endif

  end subroutine IXFcheck_shape
  
  subroutine IXFpopulate_reference_shape(shape_new,ref_shape,list,status)
    implicit none
    type(IXTshape),intent(in)::ref_shape
    type(IXTshape),intent(out)::shape_new
    integer(i4b)::list(:),i
    type(IXTstatus)::status
    
    shape_new%shape_type=ref_shape%shape_type    
    call IXFallocdims(shape_new%dimensions,(/ size(ref_shape%dimensions,1),size(list) /),status)
    
    do i=1,size(list)
      shape_new%dimensions(:,i)=ref_shape%dimensions(:,list(i))
    enddo
    
    call IXFmark_valid(shape_new)
    
  end subroutine IXFpopulate_reference_shape
  
  subroutine IXFcreate_wxwywz_shape(shape,shape_type,w_x,w_y,w_z,fill,status)
    implicit none
    type(IXTshape)::shape
    integer(i4b),intent(in)::shape_type
    real(dp),intent(in)::w_x(:),w_y(:),w_z(:)
    logical,intent(in)::fill(:)
    integer(i4b)::len
    type(IXTstatus)::status
    shape%shape_type=shape_type
    len=count(fill)
    
    ! we now have to allocate and fill the dimensions array according to the shape_type
    ! which has been supplied, we will currently only set for cylinder and box
    ! as these are the only appropriate for w_x,w_y,w_z input
    if(shape_type == IXCbox)then
      call IXFreallocdims(shape%dimensions,(/ 3,len /),.false.,status)
      shape%dimensions(1,:)=pack(w_x,fill)      
      shape%dimensions(2,:)=pack(w_y,fill)
      shape%dimensions(3,:)=pack(w_z,fill)
    endif

    if(shape_type == IXCcylinder)then
      call IXFreallocdims(shape%dimensions,(/ 2,len /),.false.,status)
      shape%dimensions(1,:)=pack(w_x,fill)            
      shape%dimensions(2,:)=pack(w_z,fill)
    endif

! this is coming in from a dummy detector
    if(shape_type == IXCpoint)then
      call IXFreallocdims(shape%dimensions,(/ 1,len /),.false.,status)
      shape%dimensions(1,:)=0.0 
    endif
          
    call IXFmark_valid(shape)
  end subroutine  IXFcreate_wxwywz_shape
  
  subroutine IXFset_index_shape(shape,dim_fragment,index,status)
    implicit none
    type(IXTshape)::shape
    type(IXTstatus)::status
    real(dp)::dim_fragment(:)
    integer(i4b)::index
    
    if(index > size(shape%dimensions,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'shape dimension length is too short for supplied index (IXFset_index_shape)')              
       return
    endif    
    if(size(dim_fragment)/=size(shape%dimensions,1))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'input dimension fragment is incorrect length for shape type(IXFset_index_shape)')              
       return
    endif      
    
    shape%dimensions(:,index)=dim_fragment
    
  end subroutine  IXFset_index_shape

  subroutine IXFget_index_shape(shape,index,dim_fragment,status)
    implicit none
    type(IXTshape),intent(in)::shape
    type(IXTstatus)::status
    real(dp),intent(out)::dim_fragment(:)
    integer(i4b),intent(in)::index
    
    if(index > size(shape%dimensions,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'shape dimension length is too short for supplied index (IXFget_index_shape)')              
       return
    endif    
    ! if the length of the output fragment is longer than necessary then it is ok
    if(size(dim_fragment) < size(shape%dimensions,1))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'output dimension fragment is incorrect length for shape type(IXFget_index_shape)')              
       return
    endif  
    dim_fragment(1:size(shape%dimensions,1))=shape%dimensions(:,index)
    
  end subroutine  IXFget_index_shape
  
  subroutine IXFcopy_index_from_shape(shape_ref,index_ref,index,shape,status)
    implicit none
    type(IXTshape),intent(in)::shape_ref
    type(IXTshape)::shape
    type(IXTstatus)::status
    integer(i4b),intent(in)::index_ref,index
    
    if(index > size(shape%dimensions,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'shape rotmat length is too short for supplied index (IXFcopy_index_from_shape)')              
       return
    endif
    if(index_ref > size(shape_ref%dimensions,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'shape_ref rotmat length is too short for supplied index (IXFcopy_index_from_shape)')              
       return
    endif      
    if(shape_ref%shape_type /= shape%shape_type)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'shape_ref and shape types incompatible(IXFcopy_index_from_shape)')              
       return
    endif
    shape%dimensions(:,index)=shape_ref%dimensions(:,index_ref)
    
  end subroutine  IXFcopy_index_from_shape
  
!> allocates memory to array in object but does not fill it with any data
subroutine IXFmake_shape(shape,len,shape_type,status)
implicit none
type(IXTshape)::shape
integer(i4b),intent(in)::len,shape_type
type(IXTstatus)::status
    shape%shape_type=shape_type
    if(shape_type == IXCbox)then
      call IXFreallocdims(shape%dimensions,(/ 3,len /),.false.,status)
      return
    endif

    if(shape_type == IXCcylinder)then
      call IXFreallocdims(shape%dimensions,(/ 2,len /),.false.,status)
      return
    endif

! this is coming in from a dummy detector, should not be called in this routine
    if(shape_type == IXCpoint)then
      call IXFreallocdims(shape%dimensions,(/ 1,len /),.false.,status)
    endif
end subroutine IXFmake_shape  

!> allocates memory to array in object but does not fill it with any data
subroutine IXFfinish_effective_shape(shape,new_len,status)
implicit none
type(IXTshape)::shape
integer(i4b),intent(in)::new_len
type(IXTstatus)::status
integer(i4b)::type_length

type_length=size(shape%dimensions,1)
call IXFreallocdims(shape%dimensions,(/ type_length,new_len /),.true.,status)
call IXFmark_valid(shape)

end subroutine IXFfinish_effective_shape 

  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:

  ! Rather annoyingly, the various valid types must be hand-crafted in for each parent method.
  function IXFvolume_shape(shape) result(volume)
    type(IXTshape), intent(in):: shape
    real(dp) :: volume(size(shape%dimensions,2))
!    if (shape%shape_type==IXCpoint) volume = IXFvolume_point(shape%dimensions)
    if (shape%shape_type==IXCbox) volume = IXFvolume_box(shape%dimensions)
    if (shape%shape_type==IXCcylinder) volume = IXFvolume_cylinder(shape%dimensions)
!    if (shape%shape_type==IXCsphere) volume = IXFvolume_sphere(shape%dimensions)
!    if (shape%shape_type==IXCholcyl) volume = IXFvolume_holcyl(shape%dimensions)
!    if (shape%shape_type==IXCpolygon) volume = IXFvolume_polygon(shape%dimensions)
  end function IXFvolume_shape

  function IXFsolid_angle_shape(shape, viewpoint) result(omega)
    type(IXTshape), intent(in):: shape
    real(dp), intent(in):: viewpoint(:,:)  !  3xn array of the viewpoint in local coords of each shape
    real(dp):: omega(size(shape%dimensions,2))
!    if (shape%shape_type==IXCpoint) omega = IXFsolid_angle_point(shape%dimensions, viewpoint)
    if (shape%shape_type==IXCbox) omega = IXFsolid_angle_box(shape%dimensions, viewpoint)
    if (shape%shape_type==IXCcylinder) omega = IXFsolid_angle_cylinder(shape%dimensions, viewpoint)
!    if (shape%shape_type==IXCsphere) omega = IXFsolid_angle_sphere(shape%dimensions, viewpoint)
!    if (shape%shape_type==IXCholcyl) omega = IXFsolid_angle_holcyl(shape%dimensions, viewpoint)
!    if (shape%shape_type==IXCpolygon) omega = IXFsolid_angle_polygon(shape%dimensions, viewpoint)
  end function IXFsolid_angle_shape

!  function IXFarea_vertices_shape(shape, rotmat, vector) result(area_vertices)
!    type(IXTshape), intent(in):: shape
!    real(dp), intent(in):: rotmat(3,3), vector(3)   ! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)
!    if (shape%shape_type==IXCpoint) area_vertices => IXFarea_vertices_point(shape%dimensions, rotmat, vector)
!    if (shape%shape_type==IXCbox) area_vertices => IXFarea_vertices_box(shape%dimensions, rotmat, vector)
!    if (shape%shape_type==IXCcylinder) area_vertices => IXFarea_vertices_cylinder(shape%dimensions, rotmat, vector)
!    if (shape%shape_type==IXCsphere) area_vertices => IXFarea_vertices_sphere(shape%dimensions, rotmat, vector)
!    if (shape%shape_type==IXCholcyl) area_vertices => IXFarea_vertices_holcyl(shape%dimensions, rotmat, vector)
!    if (shape%shape_type==IXCpolygon) area_vertices => IXFarea_vertices_polygon(shape%dimensions, rotmat, vector)
!  end function IXFarea_vertices_shape
!
!  subroutine IXFprojarea_vertices_shape (shape, rotmat, vector, projection, px, py, status, radius, axes)
!    type(IXTshape), intent(in):: shape
!    real(dp), intent(in):: rotmat(3,3), vector(3)   ! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: px(:), py(:)
!    integer(i4b), intent(in) :: projection
!    real(dp), intent(in), optional :: radius
!    integer(i4b), intent(in), optional :: axes(2)
!    type(IXTstatus) :: status
!    ! internal declarations
!    real(dp), pointer :: area_vertices(:,:)
!
!    area_vertices => IXFarea_vertices_shape (shape, rotmat, vector)
!    call IXFrealloc (px, size(area_vertices,2), .FALSE., status)
!    call IXFrealloc (py, size(area_vertices,2), .FALSE., status)
!    call IXFproj_projection (area_vertices, projection, px, py, status, radius, axes)
!
!    return
!  end subroutine IXFprojarea_vertices_shape
!
  !======================================================================================================================
  ! Point functions
  !
  ! These are essentially dummy - they allow for default initialisation of a shape to be a point
  ! The dimensions field is ignored, except that it is initialised to an array length zero.
  !======================================================================================================================
  subroutine IXFcheck_point (dimensions, status)
    real(dp) :: dimensions(:,:)
    type(IXTstatus) :: status
    if (size(dimensions,1)/=1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid point parameters - failed in IXFcheck_point')
    endif
    return
  end subroutine IXFcheck_point

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_point (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    volume = 0.0_dp
    return
  end function IXFvolume_point

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_point (dimensions, vp) result(omega)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per point
    real(dp) :: omega(size(dimensions,2))
    omega = 0.0_dp
    return
  end function IXFsolid_angle_point
  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_point (dims, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dims(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b) :: nvert, ivert
!    type(IXTstatus) :: status
!
!    nvert = 1
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(1,:) =  -vector(1)            ! x coordinates
!    area_vertices(2,:) =  -vector(2)            ! y coordinates
!    area_vertices(3,:) =  -vector(3)
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_point
!
  !======================================================================================================================
  ! Box functions
  !   Parameters: dimensions(1:3) = full widths along x, y, z axes
  !   Centred: at centre of box
  !======================================================================================================================
  subroutine IXFcheck_box (dimensions, status)
    real(dp) :: dimensions(:,:)
    type(IXTstatus) status
    if (size(dimensions,1)/=3 .OR. minval(dimensions) < 0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid box parameters - failed in IXFcheck_box')
    endif
    return
  end subroutine IXFcheck_box

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_box (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    volume = product(dimensions,dim=1)
    return
  end function IXFvolume_box

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_box (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per box
    real(dp) :: omega(size(dimensions,2))
    ! local arguments
    integer(i4b) :: k(size(dimensions,2)), cyclic(2:5)=(/2,3,1,2/), i
    real(dp) :: distsqr(size(dimensions,2))
    save cyclic

    ! we treat as a the case of a polygon, determining the dimension of the the 'thickness' as the minimum thickness
    distsqr = vp(1,:)*vp(1,:)+vp(2,:)*vp(2,:)+vp(3,:)*vp(3,:)
    k = minloc(dimensions,1)
    do i=1,size(dimensions,2)   ! must be a vectorised way to do this loop
        omega(i) = (dimensions(cyclic(k(i)+1),i)*dimensions(cyclic(k(i)+2),i))*(abs(vp(k(i),i))/sqrt(distsqr(i)))/distsqr(i)
    end do
    
    return
  end function IXFsolid_angle_box

  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_box (dims, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dims(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b) :: nvert, ivert, i, j, k(1), cyclic(2:5)=(/2,3,1,2/)
!    type(IXTstatus) :: status
!    save cyclic
!
!    nvert = 4
!    k = minloc(dims)
!    i = cyclic(k(1)+1)
!    j = cyclic(k(1)+2)
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(i,:) = 0.5_dp*(/-dims(i),dims(i),dims(i),-dims(i)/) - vector(i)
!    area_vertices(j,:) = 0.5_dp*(/-dims(j),-dims(j),dims(j),dims(j)/) - vector(j)
!    area_vertices(k(1),:) = -vector(k(1))
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_box
!
  !======================================================================================================================
  ! Cylinder functions
  !   Parameters: dimensions(1:2,:) = (radius, height) for each cylinder; cylinder axis along z axis
  !   Centred: at centre of cylinder
  !======================================================================================================================
  subroutine IXFcheck_cylinder (dimensions, status)
    real(dp):: dimensions(:,:)
    type(IXTstatus):: status
    if (size(dimensions,1)/=2 .OR. minval(dimensions) < 0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid cylinder parameters - failed in IXFcheck_cylinder')
    endif
    return
  end subroutine IXFcheck_cylinder

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_cylinder (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    volume = pi_dp*(dimensions(1,:)**2)*dimensions(2,:)		! radius is first parameter, height the second
    return
  end function IXFvolume_cylinder

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_cylinder (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per cylinder
    real(dp) :: omega(size(dimensions,2))
    ! local arguments
    real(dp) :: distsqr(size(dimensions,2))

    distsqr = vp(1,:)*vp(1,:)+vp(2,:)*vp(2,:)+vp(3,:)*vp(3,:)
    omega = (2.0_dp*dimensions(1,:)*dimensions(2,:)) * sqrt((vp(1,:)*vp(1,:) + vp(2,:)*vp(2,:))/distsqr) / distsqr

    return
  end function IXFsolid_angle_cylinder

!  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_cylinder (dims, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dims(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b) :: nvert, ivert
!    type(IXTstatus) :: status
!
!    nvert = 4
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(1,:) = (/-dims(1),dims(1),dims(1),-dims(1)/) - vector(1)            ! x coordinates
!    area_vertices(2,:) = 0.5_dp*(/-dims(2),-dims(2),dims(2),dims(2)/) - vector(2)     ! y coordinates
!    area_vertices(3,:) = -vector(3)
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_cylinder
!
  !======================================================================================================================
  ! Sphere functions
  !   Parameters: dimensions(1) = radius
  !   Centred: at centre of sphere
  !======================================================================================================================
  subroutine IXFcheck_sphere (dimensions, status)
    real(dp) :: dimensions(:,:)
    type(IXTstatus):: status
    if (size(dimensions,1)/=1 .OR. minval(dimensions) < 0.0_dp) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid sphere parameters - failed in IXFcheck_sphere')
    endif
    return
  end subroutine IXFcheck_sphere

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_sphere (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    volume = (4.0_dp/3.0_dp)*pi_dp*(dimensions(1,:)**3)
    return
  end function IXFvolume_sphere

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_sphere (dimensions, vp) result(omega)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per sphere
    real(dp) :: omega(size(dimensions,2))
    omega = (pi_dp*dimensions(1,:)**2) / (vp(1,:)*vp(1,:)+vp(2,:)*vp(2,:)+vp(3,:)*vp(3,:))
    return
  end function IXFsolid_angle_sphere

  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_sphere (dims, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dims(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b), parameter :: nvert=24     !24 sided polygon
!    integer(i4b) :: i,ivert
!    real(dp) :: arr(nvert)
!    type(IXTstatus) :: status
!    logical :: first_time = .TRUE.
!    save first_time, arr
!
!    if (first_time) then
!       forall (i=1:nvert) arr(i) = twopi_dp*real(i-1,kind(1.0_dp))/real(nvert,kind(1.0_dp))
!       first_time = .FALSE.
!    endif
!
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(1,:) = dims(1)*cos(arr) - vector(1)               ! x coordinates
!    area_vertices(2,:) = dims(1)*sin(arr) - vector(2)               ! y coordinates
!    area_vertices(3,:) = -vector(3)
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_sphere
!
  !======================================================================================================================
  ! Hollow cylinder functions
  !   Parameters: dimensions(1:3) = (inner_radius, outer_radius, height) ; cylinder axis along z axis
  !   Centred: at centre of cylinder
  !======================================================================================================================
  subroutine IXFcheck_holcyl (dimensions, status)
    real(dp) :: dimensions(:,:)
    type(IXTstatus):: status
    if (size(dimensions,1)/=3 .OR. minval(dimensions) < 0.0_dp .OR. any(dimensions(2,:)<dimensions(1,:))) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Invalid hollow cylinder parameters - failed in IXFcheck_holcyl')
    endif
    return
  end subroutine IXFcheck_holcyl

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_holcyl (dimensions) result(volume)
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    volume = pi_dp*(dimensions(2,:)-dimensions(1,:))*(dimensions(2,:)+dimensions(1,:))*dimensions(3,:)	! dimensions contains r_out, r_in, height
    return
  end function IXFvolume_holcyl

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_holcyl (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per holcyl
    real(dp) :: omega(size(dimensions,2))
    ! local arguments
    real(dp) :: distsqr(size(dimensions,2))

    distsqr = vp(1,:)*vp(1,:)+vp(2,:)*vp(2,:)+vp(3,:)*vp(3,:)
    omega = (2.0_dp*dimensions(2,:)*dimensions(3,:)) * sqrt((vp(1,:)*vp(1,:) + vp(2,:)*vp(2,:))/distsqr) / distsqr

    return
  end function IXFsolid_angle_holcyl

  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_holcyl (dims, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dims(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b) :: nvert, ivert
!    type(IXTstatus) :: status
!
!    nvert = 4
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(1,:) = (/-dims(1),dims(1),dims(1),-dims(1)/) - vector(1)            ! x coordinates
!    area_vertices(2,:) = 0.5_dp*(/-dims(2),-dims(2),dims(2),dims(2)/) - vector(2)     ! y coordinates
!    area_vertices(3,:) = -vector(3)
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_holcyl
!
  !======================================================================================================================
  ! Polygon functions
  !   A polygon shape has verticies in the x-y plane, with a thickness along the z axis
  !   Parameters: dimensions(1:nvert) = x coordinates
  !               dimensions(nvert+1:2*nvert) = y coordinates
  !               dimensions(2*nvert+1) = full thickness, t.  If missing, then thickness assumed to be zero.
  !   Centred: at (0,0) in x-y plane, and the z axis is centred in middle of the volume, i.e. at t/2
  !======================================================================================================================
  subroutine IXFcheck_polygon (dimensions, status)
    real(dp):: dimensions(:,:)
    type(IXTstatus):: status
    integer(i4b) :: n, nvert
    ! Check is incomplete - we should check that the boundary of the polygon does not cross itself.
    n = size(dimensions,1)
    nvert = size(dimensions,1)/2
    if (nvert < 3) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'No. verticies must be 3 or greater - failed in IXFcheck_polygon')
       return
    endif
    if (mod(n,2)==1 .AND. any(dimensions(n,:)<0.0_dp)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Thickness must >= 0.0 - failed in IXFcheck_polygon')
       return
    endif
    return
  end subroutine IXFcheck_polygon

  !----------------------------------------------------------------------------------------------------------------------
  function IXFvolume_polygon (dimensions) result(volume)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:,:)
    real(dp) :: volume(size(dimensions,2))
    ! local arguments
    real(dp) :: area(size(dimensions,2))
    integer(i4b) :: nvert, i
    type(IXTstatus) :: status
    if (mod(size(dimensions),2)==1) then    ! odd number of elements, so last one is the thickness
       nvert = size(dimensions,1)/2
       do i=1,size(dimensions,2)    ! Should IXFpolygon_moments be parallelised?
          call IXFpolygon_moments (dimensions(1:nvert,i), dimensions(nvert+1:2*nvert,i), status, area(i))
          if (status==IXCseverity_error) return
       end do
       volume = abs(area)*dimensions(2*nvert+1,:)    ! recall area is directed, so get modulus
    else
       volume = 0.0_dp ! no last element, so zero thickness
    endif
    return
  end function IXFvolume_polygon

  !----------------------------------------------------------------------------------------------------------------------
  function IXFsolid_angle_polygon (dimensions, vp) result(omega)
    ! i/o arguments:
    real(dp), intent(in) :: dimensions(:,:)
    real(dp), intent(in) :: vp(:,:)           ! 3xn array of the viewpoint in local coordinates, one 3-vector per polygon
    real(dp) :: omega(size(dimensions,2))
    ! local arguments
    real(dp) :: area(size(dimensions,2)), distsqr(size(dimensions,2)), cent(2,size(dimensions,2))
    integer(i4b) :: nvert, i
    type(IXTstatus) :: status

    nvert = size(dimensions)/2
    do i=1,size(dimensions,2)   ! Should IXFpolygon_moments be parallelised?
        call IXFpolygon_moments (dimensions(1:nvert,i), dimensions(nvert+1:2*nvert,i), status, area(i), cent(:,i))
        if (status==IXCseverity_error) return
    end do
    distsqr = (vp(1,:)-cent(1,:))**2+(vp(2,:)-cent(2,:))**2+vp(3,:)*vp(3,:)
    omega = abs(area)*(abs(vp(3,:))/sqrt(distsqr))/distsqr   ! recall area is directed, so get modulus
    return
  end function IXFsolid_angle_polygon

  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_polygon (dimensions, rotmat, vector) result(area_vertices)
!    ! i/o arguments:
!    real(dp), intent(in) :: dimensions(:)
!    real(dp), intent(in) :: rotmat(3,3), vector(3)          !! rotation matrix and origin of view coordinates in local frame
!    real(dp), pointer :: area_vertices(:,:)                 !! coords of area vertices in view coordinate frame
!    ! local arguments
!    integer(i4b) :: nvert, ivert
!    type(IXTstatus) :: status
!
!    nvert = size(dimensions)/2
!    call IXFrealloc (area_vertices, 3, nvert, .FALSE., status)
!    area_vertices(1,:) = dimensions(1:nvert) - vector(1)            ! x coordinates
!    area_vertices(2,:) = dimensions(nvert+1:2*nvert) - vector(2)    ! y coordinates
!    area_vertices(3,:) = -vector(3)                                 ! use the centre plane of the polygon
!    forall (ivert=1:nvert)
!       area_vertices(:,ivert) = matmul(rotmat,area_vertices(:,ivert))
!    end forall
!
!  end function IXFarea_vertices_polygon

  !======================================================================================================================
end module IXMshape

