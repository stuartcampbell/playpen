!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMgeometry
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMgeometry object.
!!
!! Nearest equivalent NeXus class: NXgeometry
!!
!! Differs from the NeXus class in that the shape description allows for a selection of different shapes: the
!! shapes are stored in an array of IXTshape, one element for each different shape. We expect that in a typical
!! instrument description there will not be many different shapes, and so the length of the array of type(IXTshape)
!! will not be very long.
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!

module IXMgeometry
  use IXMtranslation
  use IXMorientation
  use IXMshape
  implicit none
  ! public types
  public :: IXTgeometry
  type IXTgeometry
  ! default is all variables and functions are hidden  
     private		! The type is public, but the contents not
     type(IXTbase)::base
     type(IXTtranslation) :: translation                    !! positions of each of the geometrical items
     type(IXTorientation) :: orientation                    !! orientations of each of the geometrical items
     integer(i4b), pointer :: elmt_to_shape(:) => NULL()    !! element of shape for a given geometrical item
     integer(i4b), pointer :: elmt_to_index(:) => NULL()    !! index within that element of shape for a given geometrical item
     type(IXTshape), allocatable :: shape(:)
  end type IXTgeometry


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE geometry
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

!  interface IXFvolume
!     module procedure IXFvolume_geometry
!  end interface
!
!  interface IXFsolid_angle
!     module procedure IXFsolid_angle_geometry
!  end interface

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

    if(IXFvalid(arg%translation))call IXFdestroy(arg%translation,status)
    if(IXFvalid(arg%orientation))call IXFdestroy(arg%orientation,status)    
    if(IXFvalid(arg%shape))then
       if(allocated(arg%shape))call IXFdealloc_shape(arg%shape,status)
    endif
    call IXFdealloc(arg%elmt_to_shape,status)    
    call IXFdealloc(arg%elmt_to_index,status)    
    call IXFclear_valid(arg)
  end subroutine IXFdestroy_geometry

  !!generic create routine, there are some specialized routines which are interfaced to 'IXFcreate'
  subroutine IXFcreate_geometry(geometry, translation, orientation,elmt_to_shape,elmt_to_index,shape,status)
    implicit none
    type(IXTgeometry),intent(out) :: geometry
    type(IXTtranslation),intent(in)::translation
    type(IXTorientation),intent(in)::orientation
    type(IXTshape),intent(in)::shape(:)
    integer(i4b),intent(in)::elmt_to_shape(:),elmt_to_index(:)
    type(IXTstatus) :: status   

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(translation) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTtranslation failure, all nested objects MUST be initialised (IXFcreate_geometry)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(orientation) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTorientation failure, all nested objects MUST be initialised (IXFcreate_geometry)')
    endif

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(shape) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTshape failure, all nested objects MUST be initialised (IXFcreate_geometry)')
    endif
    if(status == IXCseverity_error)return

    call IXFmark_valid(geometry)
    call IXFset_geometry(geometry,status,translation,orientation,elmt_to_shape,elmt_to_index,shape)


  end subroutine IXFcreate_geometry

  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_geometry(op, field, arg, status)
    implicit none
    type(IXTgeometry) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTgeometry', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    call IXFoperation_run(op, 'base', arg%base, status)    
    call IXFoperation_run(op,'translation', arg%translation, status)
    call IXFoperation_run(op,'orientation', arg%orientation, status)
    call IXFoperation_run_ptr(op,'elmt_to_shape', arg%elmt_to_shape,status)
    call IXFoperation_run_ptr(op,'elmt_to_index', arg%elmt_to_index,status)    
    call IXFoperation_run_alloc(op,'shape', arg%shape, status)
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
  recursive subroutine IXFset_geometry (geometry, status, translation, orientation,elmt_to_shape,elmt_to_index, shape,ref)
    implicit none
    type(IXTgeometry), intent(INOUT) :: geometry					    !! the geometry object we are going to set
    type(IXTgeometry), optional, intent(IN) :: ref					        !! the reference geometry object which is copied
    type(IXTtranslation), intent(IN), optional :: translation		!! translation object
    type(IXTorientation), intent(IN), optional :: orientation		!! orientation object
    integer(i4b),intent(in),optional::elmt_to_shape(:),elmt_to_index(:)
    type(IXTshape), intent(IN), optional :: shape(:)		        	!! shape object
    type(IXTstatus):: status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_geometry)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(geometry)
    else    
       if(IXFvalid(geometry) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_geometry)')
       endif
       if(status == IXCseverity_error)return
    endif

    
    ! So long as the objects are valid (as they must be if already created), then any combination is valid
    if (present(ref))call IXFset_geometry(geometry,status,ref%translation,ref%orientation,ref%elmt_to_shape,ref%elmt_to_index,ref%shape)

    if (present(translation)) call IXFcopy(translation,geometry%translation,status)
    if (present(orientation)) call IXFcopy(orientation,geometry%orientation,status)
    if(present(shape))then   
       call IXFrealloc_shape(geometry%shape,size(shape),.false.,status)
       call IXFcopy(shape,geometry%shape,status)  
    endif
    call IXFset_integer_array(geometry%elmt_to_shape,status,elmt_to_shape)
    call IXFset_integer_array(geometry%elmt_to_index,status,elmt_to_index)

    call IXFcheck_geometry(geometry,status)
  end subroutine IXFset_geometry


  !! Get components of a geometry object. 
  !!
  !! Syntax:
  !!   - call IXFget_geometry (geometry, wout=geometry_out)        to get a copy of the geometry object
  !!   - call IXFget_geometry (geometry, status, [translation=tr,] [orientation=or,] [shape=sh]) to get one or more attributes       
  !!
  subroutine IXFget_geometry(geometry, status, translation, orientation,elmt_to_shape,elmt_to_index, shape,wout)
    implicit none
    type(IXTgeometry), intent(IN) :: geometry			    !! the geometry object from which attributes will be extracted
    type(IXTgeometry),optional, intent(out) :: wout			    !! the geometry object which attributes will be copied to
    type(IXTtranslation), intent(OUT), optional :: translation		!! translation object
    type(IXTorientation), intent(OUT), optional :: orientation		!! orientation object
    type(IXTshape), intent(OUT), optional :: shape(:)		            !! shape object
    integer(i4b),intent(out),optional::elmt_to_shape(:),elmt_to_index(:)
    type(IXTstatus):: status

    if (present(wout))call IXFcopy(geometry,wout,status)
    ! So long as the objects are valid (as they must be if already created), then any combination is valid
    if (present(translation)) call IXFcopy(geometry%translation,translation,status)
    if (present(orientation)) call IXFcopy(geometry%orientation,orientation,status)
    call IXFget_integer_array(geometry%elmt_to_shape,status,elmt_to_shape)
    call IXFget_integer_array(geometry%elmt_to_index,status,elmt_to_index)
    if (present(shape)) call IXFcopy(geometry%shape,shape,status)
  end subroutine IXFget_geometry

  subroutine IXFget_alloc_geometry(geometry, status, translation, orientation,elmt_to_shape,elmt_to_index, shape,wout)
    implicit none
    type(IXTgeometry), intent(IN) :: geometry			    !! the geometry object from which attributes will be extracted
    type(IXTgeometry),optional, intent(out) :: wout			    !! the geometry object which attributes will be copied to
    type(IXTtranslation), intent(OUT), optional :: translation		!! translation object
    type(IXTorientation), intent(OUT), optional :: orientation		!! orientation object
    type(IXTshape), optional,allocatable :: shape(:)		            !! shape object
    integer(i4b),allocatable,optional::elmt_to_shape(:),elmt_to_index(:)
    type(IXTstatus):: status

    if (present(elmt_to_shape))call IXFreallocFortran(elmt_to_shape,size(elmt_to_shape),.false.,status)
    if (present(elmt_to_index))call IXFreallocFortran(elmt_to_index,size(elmt_to_index),.false.,status)
    if(present(shape))then
        call IXFrealloc(shape,size(geometry%shape),.false.,status)
    endif

    call IXFget_geometry(geometry,status,translation,orientation,elmt_to_shape,elmt_to_index,shape,wout)

  end subroutine IXFget_alloc_geometry

  !----------------------------------------------------------------------------------------------------------------------
  !! Subroutine to check consistency of arguments.

  ! dummy routine, as attributes are already valid, as their own check routines guarantee that. 
  subroutine IXFcheck_geometry(geometry, status)
    type(IXTgeometry) :: geometry
    type(IXTstatus) :: status
    integer(i4b)::i

    call IXFcheck_translation(geometry%translation,status)
    call IXFcheck_orientation(geometry%orientation,status)
    do i=1,size(geometry%shape)
      if(IXFvalid(geometry%shape(i)))call IXFcheck(geometry%shape(i),status)
    enddo
  end subroutine IXFcheck_geometry

  subroutine IXFdummy_geometry(geometry,list,status)
    implicit none
    type(IXTgeometry),intent(inout)::geometry
    integer(i4b),intent(in)::list(:)
    real(dp),allocatable::dims(:,:),trans(:,:),orient(:,:,:)
    type(IXTstatus)::status
    integer(i4b)::len,i
    len=size(list)
    allocate(dims(1,len),trans(3,len),orient(3,3,len))
    ! list is the list of indices to detectors which will be averaged over to create the effective geometry object for the effective detector
    ! we have no information about the shape of the detectors and therefore create a point shape to the length of detectors used
    if(.not.allocated(geometry%shape))call IXFalloc_shape(geometry%shape,1,status)
    dims=1.0d0
    trans=1.0d0
    orient=1.0d0
    call IXFcreate_shape(geometry%shape(1),status,0_i4b,dims)
    call IXFcreate_translation(geometry%translation,status,trans)
    call IXFcreate_orientation(geometry%orientation,status,orient)
    call IXFalloc(geometry%elmt_to_index,len,status)
    call IXFalloc(geometry%elmt_to_shape,len,status)
    geometry%elmt_to_shape=1
    geometry%elmt_to_index=(/ (i,i=1,len) /)
    call IXFmark_valid(geometry)
    deallocate(dims,trans,orient)
  end subroutine IXFdummy_geometry

  subroutine IXFpopulate_reference_geometry(geometry,ref_geometry,lookup,raw_ranks,det_list,status)
    use IXMsort
    implicit none
    type(IXTgeometry),intent(out)::geometry
    type(IXTgeometry),intent(in)::ref_geometry
    integer(i4b),intent(in)::det_list(:),lookup(:),raw_ranks(:)
    type(IXTstatus)::status    
    integer(i4b)::i,j,shape_index,len,nindex,nshape_count,index_count,indices(size(det_list)),shapes(size(det_list))
    integer(i4b),allocatable::s_type(:),list(:)
    logical,allocatable ::fill(:,:)
    len=size(det_list)
       
    call IXFpopulate_reference_translation(geometry%translation,ref_geometry%translation,lookup,raw_ranks,det_list,status)
    call IXFpopulate_reference_orientation(geometry%orientation,ref_geometry%orientation,lookup,raw_ranks,det_list,status)

    call IXFalloc(geometry%elmt_to_index,len,status)
    call IXFalloc(geometry%elmt_to_shape,len,status)

    do i=1,len
      indices(lookup(raw_ranks(i)))=ref_geometry%elmt_to_index(det_list(i))
      shapes(lookup(raw_ranks(i)))=ref_geometry%elmt_to_shape(det_list(i))
    enddo
    
    !highest index of shapes used
    nindex=maxval(shapes)    
         
    allocate(list(nindex),s_type(nindex),fill(nindex,len))    
    fill=.false.    
    s_type=-1
    nshape_count=0
    
    do i=1,nindex
    ! if shape is of type i then determine which 
      where(shapes == i) fill(i,:)=.true.
      !list is the number of shapes of index i
      list(i)=count(fill(i,:))
      if(list(i)> 0 )then
      !shape of index i is used
        nshape_count=nshape_count+1        
        call IXFget_shape(ref_geometry%shape(i) ,status,shape_type=s_type(i))        
      endif
    end do      
        
     call IXFrealloc_shape(geometry%shape,nshape_count,.false.,status)
   
    ! this mechanism populates the shape object in increasing shape_type order
    ! i=0 point, i=1 box, i=2 cylinder, etc...
    
    shape_index=1
    do i=1,nindex
      if (list(i) > 0)then        
        call IXFpopulate_reference_shape(geometry%shape(shape_index),ref_geometry%shape(i),pack(indices,fill(i,:)) ,status)
        where(fill(i,:) .EQV. .true.)geometry%elmt_to_shape=shape_index
        shape_index=shape_index+1        
        index_count=1        
        do j=1,len
          if(fill(i,j).eqv. .true.)then
            geometry%elmt_to_index(j)=index_count
            index_count=index_count+1
          endif
        enddo
      endif
    enddo
    call IXFmark_valid(geometry)    
    
  end subroutine IXFpopulate_reference_geometry  
  subroutine IXFreference_file_geometry(geometry,w_x,w_y,w_z,alpha_x,alpha_y,alpha_z,phi,theta,L2,ndet,det_type,status)
    implicit none
    type(IXTgeometry)::geometry
    type(IXTstatus)::status
    integer(i4b),intent(in)::ndet,det_type(:)
    real(dp),pointer::L2(:),theta(:),phi(:),delta(:),alpha_x(:),alpha_y(:),alpha_z(:),w_x(:),w_y(:),w_z(:)
    integer(i4b)::nshape_count,maxshape,i,j,shape_index,index_count
    integer(i4b),allocatable::list(:),s_type(:)
    logical,allocatable::fill(:,:)
    
    nshape_count=0
    call IXFcreate_l2thetaphi_translation(geometry%translation,L2,theta,phi,status)    
    call IXFcreate_alphaxyz_phi_theta_orientation(geometry%orientation,alpha_x,alpha_y,alpha_z,phi,theta,status)
    
    call IXFalloc(geometry%elmt_to_shape,ndet,status)
    call IXFalloc(geometry%elmt_to_index,ndet,status)       
    
!    where(det_type == 0)geometry%elmt_to_shape=0    
    ! dummy detectors will be created as points

    maxshape=maxval(det_type)
 
! if we have dummy detectors    
    maxshape=maxshape+1
    
    allocate(list(maxshape),s_type(maxshape))
    allocate(fill(maxshape,ndet))
    fill=.false.    
    s_type=-1

    do i=0,maxshape-1
      where(det_type == i) fill(i+1,:)=.true.
      list(i+1)=count(fill(i+1,:))
      if(list(i+1)> 0 )then
        nshape_count=nshape_count+1
        s_type(i+1)=i
      endif
    end do      
    
    call IXFrealloc_shape(geometry%shape,nshape_count,.false.,status)
    
    ! this mechanism populates the shape object in increasing shape_type order
    ! i=0 point, i=1 box, i=2 cylinder, etc...
    
    shape_index=1
    do i=1,maxshape
      if (list(i) > 0)then
        call IXFcreate_wxwywz_shape(geometry%shape(shape_index),s_type(i),w_x,w_y,w_z,fill(i,:),status)
        where(fill(i,:) .EQV. .true.)geometry%elmt_to_shape=shape_index
        shape_index=shape_index+1
        index_count=1
        do j=1,ndet
          if(fill(i,j).eqv. .true.)then
            geometry%elmt_to_index(j)=index_count
            index_count=index_count+1
          endif
        enddo
      endif
    enddo
    call IXFmark_valid(geometry)
    
    deallocate(fill,list,s_type)    
    
  end subroutine IXFreference_file_geometry

!> allocates memory in most of geometry, except the IXTshape objects
! which can only be filled after inspection of populated effective detector arrays (det_type/type_index)
subroutine IXFsetup_effective_geometry(geometry,len,status)
  implicit none
  type(IXTgeometry)::geometry
  integer(i4b),intent(in)::len
  type(IXTstatus)::status

    call IXFrealloc(geometry%elmt_to_shape,len,.false.,status)
    call IXFrealloc(geometry%elmt_to_index,len,.false.,status)

    call IXFmake_orientation(geometry%orientation,len,status)
    call IXFmake_translation(geometry%translation,len,status)
    
end subroutine IXFsetup_effective_geometry

subroutine IXFcombine_geometry(geom_full,posn_index,list_in,try_combine,w_type,w_dimension,geom_eff,combine_ok,status,trans_out)
  implicit none
  type(IXTgeometry),intent(in)::geom_full
  type(IXTgeometry)::geom_eff
  integer(i4b),intent(in)::list_in(:),posn_index
  type(IXTstatus)::status
  type(IXTtranslation),optional,intent(out)::trans_out
  logical,intent(in)::try_combine
  logical,intent(out)::combine_ok
  integer(i4b)::shape_index,w_type(:)
  real(dp)::w_dimension(:,:)
  real(dp)::vector(3,1),rotmat(3,3)
  logical::same_shape
  

!  if(try_combine)then
   !all the same shape type, and which shape index are they
    call IXFfind_if_same_value(geom_full%elmt_to_shape(list_in),same_shape,shape_index)

! w_dimension is of size (3 * Leff) and is filled up by the dimensions of combined shapes
! w_type is of size (Leff), w_type(posn_index)=IXCbox/IXCcylinder etc, whatever the combined shape becomes for posn_index
! in the current combine geometry we will produce either an IXCbox or an IXCcylinder only

! this might be extended in other ways to produce a polygon shape instead
! we would perhaps make an allocatable array of shapes in which to store data
! type(IXTshape),allocatable::w_shapearray(:)
! w_shapearray(1) would be an IXCbox which can hold information for IXCbox & IXCcylinder, this would be of
! the same size as the current w_dimension
! if a polygon shape was returned we would reallocate w_shapearray to hold a new shape object which had arbitrarily 
! longer dimensions say (2*nvert*nbuff) [holds nvert vertices], w_shapearray(2) where nbuff=1000 nvert=8 (arbitrary numbers)
! as well as w_type recording the shape type, we would have to record the w_shapeindex for each 
! element w_shapearray(w_shapeindex) and also the w_type_index, an index into the array contained in each IXTshape object
! a w_population counter for each shape in w_shapearray, since more memory is allocated than necessarily used.
! one would also need to record the number of vertices defined 

! subsequently if a polygon was returned which had more than 8 vertices w_shapearray could be extended further in the 
! same way as before
! or
! if the polygon array was filled with nbuff objects w_shapearray could be extended further with another 
! shape of dimensions (16*2*nbuff) the process would continue until all geometry elements have been combined

! finish effective geometry would become more involved deconvolving the population of the shapes in w_shapearray, 
! with their indices and shape types. in this process it is imperative not to get confused between index to shape 
! and the actual shape type as they are NOT congruent


! we are not doing the above, so just use first detector from det_list as the shape information as if there was only one detector in the list
! geom_full%shape and geom_eff%shape have the same length, so an index into either of them is equivalent


if(try_combine .and. same_shape)then
! this is where the full combine shape will be called from

! temporarily filling w_dimension and w_type with an appropriate value
call IXFget_index_shape(geom_full%shape(shape_index),geom_full%elmt_to_index(list_in(1)),w_dimension(:,posn_index),status)
call IXFget_shape(geom_full%shape(shape_index),status,shape_type=w_type(posn_index))

! fill information from the first geometry element in list_in
call IXFcopy_index_from_translation(geom_full%translation,list_in(1),posn_index,geom_eff%translation,status)
call IXFcopy_index_from_orientation(geom_full%orientation,list_in(1),posn_index,geom_eff%orientation,status)

call IXFget_index_translation(geom_eff%translation,posn_index,vector(:,1),status)
  if(present(trans_out))call IXFcreate_translation(trans_out,status, vector )  
  combine_ok=.true.
else

! this will catch all other possibilities and do a generic box over all types
! but for now we will use the method of taking the first in the list

! temporarily filling w_dimension and w_type with an appropriate value
call IXFget_index_shape(geom_full%shape(shape_index),geom_full%elmt_to_index(list_in(1)),w_dimension(:,posn_index),status)
call IXFget_shape(geom_full%shape(shape_index),status,shape_type=w_type(posn_index))

! fill information from the first geometry element in list_in
call IXFcopy_index_from_translation(geom_full%translation,list_in(1),posn_index,geom_eff%translation,status)
call IXFcopy_index_from_orientation(geom_full%orientation,list_in(1),posn_index,geom_eff%orientation,status)

call IXFget_index_translation(geom_eff%translation,posn_index,vector(:,1),status)

  combine_ok=.false.
  
endif
end subroutine IXFcombine_geometry

subroutine IXFfinish_effective_geometry(geometry,w_type,w_dimension,status)
  implicit none
  type(IXTgeometry)::geometry
  type(IXTstatus)::status
  integer(i4b),intent(in)::w_type(:)
  real(dp),intent(in)::w_dimension(:,:)
  integer(i4b)::i,j,maxshape,totshape,sub_counter,shape_count
  logical,allocatable::type_mask(:,:)
  integer(i4b),allocatable::ntype(:)
  real(dp),allocatable::sub_dimension(:,:)

!don't have to do anything with translation/orientation arrays since they 
! were allocated to appropriate lengths to begin with, just mark them as valid

call IXFfinish_effective_translation(geometry%translation)
call IXFfinish_effective_orientation(geometry%orientation)
totshape=0
maxshape=maxval(w_type)+1
allocate(type_mask(maxshape,size(w_type)),ntype(maxshape))
type_mask=.false.
! i takes the values of the shape types
! i+1 is the appropriate index value
do i=0,maxshape-1
  where (w_type == i)type_mask(i+1,:)=.true.
  ntype(i+1)=count(type_mask(i+1,:))
  if(ntype(i+1)>0)totshape=totshape+1
enddo
  
call IXFrealloc_shape(geometry%shape,totshape,.false.,status)

!i is now an index NOT a shape type
! i-1 is the shape type
shape_count=1
!shape_count goes from 1->totshape
do i=1,maxshape
  if(ntype(i)>0)then
    allocate(sub_dimension(size(w_dimension,1),ntype(i)))
    ! we need to do a 2-D pack operation manually
    ! sub_dimension=pack(w_dimension,type_mask)  
    
    sub_counter=1
    !su_counter will change from 1->ntype(i)
    do j=1,size(w_type)
      if(type_mask(i,j))then
        sub_dimension(:,sub_counter)=w_dimension(:,j)
        !index goes from 1-> ntype(i)
        geometry%elmt_to_index(j)=sub_counter
        sub_counter=sub_counter+1
      endif
    enddo  
            
    call IXFcreate_fromtype_shape(geometry%shape(shape_count),status,i-1,sub_dimension)
    where(type_mask(i,:) .eqv. .true.)geometry%elmt_to_shape=shape_count    
    shape_count=shape_count+1
    deallocate(sub_dimension)    
  endif
enddo

call IXFmark_valid(geometry)

end subroutine IXFfinish_effective_geometry
!  !----------------------------------------------------------------------------------------------------------------------
!  ! Other methods:
!  !----------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------
! Other methods:
!----------------------------------------------------------------------------------------------------------------------
! Volume of a set of geometric objects
subroutine IXFvolume_geometry (geometry, volume, status)
    use IXMmaths_utils
    type(IXTgeometry), intent(in) :: geometry
    real(dp), intent(out) :: volume(:)
    type(IXTstatus) :: status
    ! local variables
    integer(i4b) :: i, ibeg, iend
    integer(i4b) :: ns(size(geometry%shape)), position(size(geometry%elmt_to_shape))
    
    ! Get indexing of geometric objects into the staggered array of shapes
    do i=1,size(geometry%shape)
        !call IXFget_shape(geometry%shape(i), status, n_shapes=ns(i))   ! I assume that IXFget_shape will have an optional argument added to it
        ns(i)=IXFsize_shape(geometry%shape(i))
    end do 
    call IXFstaggered_position (geometry%elmt_to_index, ns, geometry%elmt_to_shape, position=position)
    
    ! Loop over the different shape types
    iend=0
    do i=1,size(geometry%shape)
    !    call IXFget_shape(geometry%shape(i), status, n_shapes=ns(i))   ! I assume that IXFget_shape will have an optional argument added to it
        ns(i)=IXFsize_shape(geometry%shape(i))
        ibeg = iend + 1
        iend = ibeg + ns(i) - 1
        volume(ibeg:iend) = IXFvolume_shape (geometry%shape(i))
    end do
    
    ! Re-order the output to match the order of the objects as given by index into translation, elmt_to_shape etc.
    volume=volume(position)
    
    return
end subroutine IXFvolume_geometry

!----------------------------------------------------------------------------------------------------------------------
! Solid angle of a geometric object from a viewpoint other than the origin
subroutine IXFsolid_angle_geometry (geometry, vp, omega, status)
    type(IXTgeometry), intent(in) :: geometry
    real(dp), intent(in) :: vp(3)           !! viewpoint in local coordinates i.e. the coordinate frame in which the geometric objects are expressed
    real(dp), intent(out) :: omega(:)
    type(IXTstatus) :: status
    ! local variables:
    integer(i4b) :: i, ibeg, iend
    !integer(i4b) :: ns(size(geometry%shape)), position(size(geometry%elmt_to_shape)), inv_position(size(geometry%elmt_to_shape))
    integer(i4b),allocatable::ns(:), position(:), inv_position(:)
    real(dp),allocatable :: vp_prime(:,:)

allocate( vp_prime(3,size(geometry%elmt_to_shape)))
allocate(ns(size(geometry%shape)), position(size(geometry%elmt_to_shape)), inv_position(size(geometry%elmt_to_shape)))

    ! Get indexing of geometric objects into the staggered array of shapes, and the inverse mapping
    do i=1,size(geometry%shape)
        !call IXFget_shape(geometry%shape(i), status, n_shapes=ns(i))
        ns(i)=IXFsize_shape(geometry%shape(i))
    end do 
    call IXFstaggered_position (geometry%elmt_to_index, ns, geometry%elmt_to_shape, position=position, inv_position=inv_position)
        
    ! Get viewpoint in local coordinate frames of each object
    call IXFs2sprime_orientation (geometry%orientation, geometry%translation, vp,vp_prime,status) ! get viewpoint in shape coordinates
    vp_prime = vp_prime(:,inv_position) ! reorder so now match the order of the objects in shape arrays

        
    ! Loop over the different shape types to calculate solid angle for each one
    iend=0
    do i=1,size(geometry%shape)
        ibeg = iend + 1
        iend = ibeg + ns(i) - 1
        omega(ibeg:iend) = IXFsolid_angle_shape (geometry%shape(i), vp_prime(:,ibeg:iend))
    end do
    
    ! Re-order the output to match the order of the objects as given by index into translation, elmt_to_shape etc.
    omega=omega(position)
deallocate(ns, position,inv_position,vp_prime)

end subroutine IXFsolid_angle_geometry
!
!  !----------------------------------------------------------------------------------------------------------------------
!  function IXFarea_vertices_geometry (geometry, or, t) result(area_vertices)
!    ! i/o arguments:
!    type(IXTgeometry), intent(in) :: geometry
!    type(IXTorientation), intent(in) :: or          !! orientation matrix of view coord frame
!    type(IXTtranslation), intent(in) :: t           !! origin of view coord frame
!    real(dp), pointer :: area_vertices(:,:)         !! coords of vertices in view coord frame
!    ! local arguments
!    type(IXTorientation) :: odiff
!    type(IXTtranslation) :: tdiff
!    real(dp) :: rotmat(3,3), vector(3)
!    type(IXTstatus) status
!
!    ! Get orientation and translation of view frame w.r.t. shape frame
!    call IXFdifference_orientation(geometry%orientation, geometry%translation, or, t, odiff, tdiff)
!    !*** must tidy here, as should use get_absolute_translation, get_absolute_orientation or soemthing:
!    call IXFget_orientation(odiff, status, rotmat=rotmat)
!    call IXFget_translation(tdiff, status, vector=vector)
!    area_vertices => IXFarea_vertices_shape (geometry%shape, rotmat, vector)
!
!  end function IXFarea_vertices_geometry
!
!  !----------------------------------------------------------------------------------------------------------------------
!  subroutine IXFprojarea_vertices_geometry (geometry, or, t, projection, px, py, status, radius, axes)
!    type(IXTgeometry), intent(in):: geometry
!    type(IXTorientation), intent(in) :: or          !! orientation matrix of view coord frame
!    type(IXTtranslation), intent(in) :: t           !! origin of view coord frame
!    integer(i4b), intent(in) :: projection
!    real(dp), pointer :: px(:), py(:)
!    real(dp), intent(in), optional :: radius
!    integer(i4b), intent(in), optional :: axes(2)
!    type(IXTstatus) :: status
!    ! internal declarations
!    type(IXTorientation) :: odiff
!    type(IXTtranslation) :: tdiff
!    real(dp) :: rotmat(3,3), vector(3)
!
!    ! Get orientation and translation of view frame w.r.t. shape frame
!    call IXFdifference_orientation(geometry%orientation, geometry%translation, or, t, odiff, tdiff)
!    !*** must tidy here, as should use get_absolute_translation, get_absolute_orientation or soemthing:
!    call IXFget_orientation(odiff, status, rotmat=rotmat)
!    call IXFget_translation(tdiff, status, vector=vector)
!    call IXFprojarea_vertices_shape (geometry%shape, rotmat, vector, projection, px, py, status, radius, axes)
!
!    return
!  end subroutine IXFprojarea_vertices_geometry

!----------------------------------------------------------------------------------------------------------------------
! Combine geometric objects together to make a single contiguous geometric object. If unable to do so, then
! combine in a generic sense, and indicate as such.

!subroutine IXFcombine_geometry (geometry, combine_if_can, symmetry, geometry_out, combine_ok)
!    type(IXTgeometry), intent(in) :: geometry
!    logical, intent(in) :: combine_if_can
!    type(IXTsymdetectors), intent(in) :: symmetry(:)
!    type(IXTgeometry), intent(out) :: geometry_out
!    logical, intent(out) :: combine_ok
!    
!    ! Return something that compiles until algorithm sorted
!    geometry_out=
!    combine_ok=.true.
!    
!end subroutine IXFcombine_geometry
!----------------------------------------------------------------------------------------------------------------------
end module IXMgeometry