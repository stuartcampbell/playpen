!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMorientation
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMorientation object.
!!
!! Nearest equivalent NeXus class: NXorientation
!!
!! Differs from the NXorientation class in that we will hold the full 3-by-3 matrix. It is defined so that if the
!! local coordinate frame is S', and the reference coordinate frame is S, then the components of a vector v as
!! expressed in the two coordinate frames are related by:
!!
!!        v'(i) = R(i,j) v(j)
!!
!! Because the basis vectors of S and S' are both orthonormal, the inverse matrix is given by the transpose,
!! R^-1 = R', therefore:
!!
!!        v(i) = R(j,i) v'(j)
!!
!! Obtaining the components in the reference frame therefore multiplies elements in the most efficient order
!! in Fortran.
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1414 $ ($Date: 2008-07-03 12:27:21 -0400 (Thu, 03 Jul 2008) $)
!!

module IXMorientation
  use IXMtype_definitions
  use IXMbase
  use IXMmaths_basis
  use IXMmaths_utils
  use IXMtranslation
  implicit none
  ! default is all variables and functions are hidden
  ! public types
  public :: IXTorientation
  ! public interfaces
  !    public :: 
  type IXTorientation
     private		! The type is public, but the contents not
     type(IXTbase):: base
     real(dp), pointer :: rotmat(:,:,:)=> NULL()
     !! rotmat will have dimensions (3,3,n), containing the rotation matricies for a group of n objects
  end type IXTorientation


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE orientation
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

  interface IXFtimes
     module procedure IXFtimes_orientation
  end interface IXFtimes

!
!  interface IXFsetgen_orientation
!     module procedure IXFset_rotvec_orientation
!  end interface


 
  !----------------------------------------------------------------------------------------------------------------------
contains

#define IXD_DESCRIPTION	"IXTorientation class"
#define IXD_TYPE orientation
#define IXD_SQTYPE 'orientation'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_orientation(arg, status)
    implicit none
    type(IXTorientation) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
  end subroutine IXFdestroy_orientation

  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_orientation(op, field, arg, status)
    implicit none
    type(IXTorientation) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTorientation', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run_ptr(op,'rotmat', arg%rotmat, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_orientation


  recursive subroutine IXFset_orientation(orientation,status,rotmat,ref)
    implicit none
    type(IXTorientation), intent(INOUT) :: orientation					!! the orientation object we are going to set
    type(IXTorientation), intent(IN),optional :: ref					!! the orientation object we are going to set
    real(dp), intent(IN), optional :: rotmat(:,:,:)				!! 3-by-3 matrix of the orientation
    
    type(IXTstatus),intent(inout):: status

    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_orientation)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(orientation)
    else    
       if(IXFvalid(orientation) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_orientation)')
       endif
       if(status == IXCseverity_error)return
    endif
    

    if (present(ref))call IXFset_orientation(orientation,status,ref%rotmat)
    call IXFset_real_array(orientation%rotmat,status,rotmat)
    
    call IXFcheck_orientation(orientation, status)

  end subroutine IXFset_orientation

  subroutine IXFget_orientation(orientation, status, rotmat,wout)
    implicit none
    type(IXTorientation), intent(IN) :: orientation			!! the orientation object from which attributes will be extracted
    type(IXTorientation), intent(OUT), optional :: wout	!! copy of the orientation   
    real(dp), intent(OUT), optional :: rotmat(:,:,:)		!! get three rotmat orientation
    type(IXTstatus), intent(INOUT) :: status

    if (present(wout)) call IXFcopy(orientation,wout,status)    
    call IXFget_real_array(orientation%rotmat,status,rotmat)

  end subroutine IXFget_orientation

  subroutine IXFget_alloc_orientation(orientation, status, rotmat,wout)
    implicit none
    type(IXTorientation), intent(IN) :: orientation			!! the orientation object from which attributes will be extracted
    type(IXTorientation), intent(OUT), optional :: wout	!! copy of the orientation   
    real(dp), allocatable, optional :: rotmat(:,:,:)		!! get three rotmat orientation
    type(IXTstatus), intent(INOUT) :: status

    if(present(rotmat))call IXFreallocdimsFortran(rotmat,shape(rotmat),.false.,status)
    call IXFget_orientation(orientation,status,rotmat,wout)

  end subroutine IXFget_alloc_orientation

!
!  subroutine IXFget_rotvec_orientation(self, status, rotvec)
!    implicit none
!    type(IXTorientation), intent(IN) :: self			!! the orientation object from which attributes will be extracted
!    real(dp), intent(OUT), optional :: rotvec(3)		!! get rotation vector
!    type(IXTstatus), intent(INOUT) :: status
!    rotvec = IXFrotmat_to_rotvec(self%rotmat)
!  end subroutine IXFget_rotvec_orientation
!
!
  !! Create an orientation from a rotation matrix. In the case of an orientation object, the 'create' and 'set'
  !! routines are almost identical. Generally a 'create' routine differs from the 'set' routine in that
  !! the 'set' routine allows an existing object to have just a selection of its attributes altered. The
  !! 'create' routine creates a new object from its components. The 'create' routine can be mimiced
  !! using the 'set' routine - indeed, that is precisely what the code of the 'create' routine does.
  !!
  !! Syntax:
  !!   - call IXFcreate_orientation (orient_out, orient_in)	copy orient_in
  !!   - call IXFcreate_orientation (orient_out, status, r)	create using rotation matrix r(3,3)
  !!   - call IXFcreate_orientation (orient_out, status)		default orientation (identity rotation)


!  subroutine IXFcreate_orientation(self, status, rotmat)
!    implicit none
!    type(IXTorientation), intent(OUT) :: self			!! the orientation object we are going to create
!    real(dp), intent(IN), optional :: rotmat(:,:,:)		!! rotation matrix defining the orientation
!    type(IXTstatus):: status
!
!    ! Create default output:
!    call IXFunit_matrix(self%rotmat)	! no rotation
!    ! Now call set routine:
!    if (present(rotmat)) then
!       call IXFset_attributes_orientation(self, status, rotmat)
!       if (status /= IXCseverity_ok) then
!          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
!               & 'Error in IXFcreate_attributes_orientation')
!       endif
!    endif
!
!  end subroutine IXFcreate_orientation
  subroutine IXFcreate_orientation(self, status, rotmat)
    implicit none
    type(IXTorientation), intent(OUT) :: self			!! the orientation object we are going to create
    real(dp), intent(IN) :: rotmat(:,:,:)		!! rotation matrix defining the orientation
    type(IXTstatus):: status

    call IXFmark_valid(self)    
    call IXFset_orientation(self,status,rotmat)
        
  end subroutine IXFcreate_orientation
  
  !----------------------------------------------------------------------------------------------------------------------
  !! Subroutine to check consistency of arguments.
  !! Use the fact that the transpose of the rotation matrix relating orthonormal bases is the inverse of the
  !! rotation matrix.

  subroutine IXFcheck_orientation(orientation, status)
    type(IXTorientation) :: orientation
    type(IXTstatus) :: status
!    if (.not. IXFrotmat_orthogonal(orientation%rotmat)) then
!       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
!            & 'Rotation matrix not orthogonal - failure in IXFcheck_orientation')
!    endif

    if(size(orientation%rotmat,1) /= 3) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
          & 'Rotation matrix is wrong shape must be 3*3*n array - failure in IXFcheck_orientation')
    endif
    
    if(size(orientation%rotmat,2) /= 3) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
          & 'Rotation matrix is wrong shape must be 3*3*n array - failure in IXFcheck_orientation')
    endif

    
  end subroutine IXFcheck_orientation
  
  subroutine IXFpopulate_reference_orientation(orientation_new,ref_orientation,lookup,raw_ranks,list,status)
    implicit none
    type(IXTorientation),intent(in)::ref_orientation
    type(IXTorientation),intent(out)::orientation_new
    integer(i4b),intent(in)::list(:),lookup(:),raw_ranks(:)
    integer(i4b)::i
    type(IXTstatus)::status
    
    call IXFallocdims(orientation_new%rotmat,(/3, 3, size(list) /),status)
    
    do i=1,size(list)
      orientation_new%rotmat(:,:,lookup(raw_ranks(i)))=ref_orientation%rotmat(:,:,list(i))
    enddo
    
    call IXFmark_valid(orientation_new)
    
  end subroutine IXFpopulate_reference_orientation

  subroutine IXFcreate_alphaxyz_phi_theta_orientation(orientation,alpha_x,alpha_y,alpha_z,phi,theta,status)
    implicit none
    real(dp),intent(in)::alpha_x(:),alpha_y(:),alpha_z(:),phi(:),theta(:)
    type(IXTorientation),intent(out)::orientation
    integer(i4b)::i,len1,len2,len3
    type(IXTstatus)::status
    real(dp) :: r_ki_to_kf(3,3), r_kf_to_det(3,3)
    len1=size(alpha_x)
    len2=size(alpha_y)
    len3=size(alpha_z)
    
    if(len1 /= len2)then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input arrays must be of same length(IXFcreate_alphaxyz_orientation)')
    else
      if(len2/=len3)then      
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'input arrays must be of same length(IXFcreate_alphaxyz_orientation)')      
      endif
    endif
    
    call IXFreallocdims(orientation%rotmat,(/3, 3, len1 /),.false.,status)
    
    do i=1,len1
      r_kf_to_det = IXFrotvec_to_rotmat((/ alpha_x(i)*deg_to_rad_dp,alpha_y(i)*deg_to_rad_dp,alpha_z(i)*deg_to_rad_dp /))
      call IXFtrans_spherical_polars (phi(i)*deg_to_rad_dp, theta(i)*deg_to_rad_dp, r_ki_to_kf)
      orientation%rotmat(:,:,i) = matmul(r_kf_to_det,r_ki_to_kf)
    enddo
    !some other operation will have to be done on the rot matrix to put it back into global axis frame
    
    call IXFmark_valid(orientation)
    
  end subroutine IXFcreate_alphaxyz_phi_theta_orientation

!> allocates memory to array in object but does not fill it with any data
subroutine IXFmake_orientation(orientation,len,status)
implicit none
type(IXTorientation)::orientation
integer(i4b),intent(in)::len
type(IXTstatus)::status

    call IXFreallocdims(orientation%rotmat,(/ 3, 3, len /),.false.,status)

end subroutine IXFmake_orientation

  subroutine IXFset_index_orientation(orientation,rotmat,index,status)
    implicit none
    type(IXTorientation)::orientation
    type(IXTstatus)::status
    real(dp)::rotmat(3,3)
    integer(i4b)::index
    
    if(index > size(orientation%rotmat,3))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'orientation rotmat length is too short for supplied index (IXFset_i_orientation)')              
       return
    endif    
    orientation%rotmat(:,:,index)=rotmat
    
  end subroutine  IXFset_index_orientation
  subroutine IXFget_index_orientation(orientation,index,rotmat,status)
    implicit none
    type(IXTorientation),intent(in)::orientation
    type(IXTstatus)::status
    real(dp),intent(out)::rotmat(3,3)
    integer(i4b),intent(in)::index
    
    if(index > size(orientation%rotmat,3))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'orientation rotmat length is too short for supplied index (IXFget_index_orientation)')              
       return
    endif    
    rotmat=orientation%rotmat(:,:,index)
    
  end subroutine  IXFget_index_orientation
  subroutine IXFcopy_index_from_orientation(orientation_ref,index_ref,index,orientation,status)
    implicit none
    type(IXTorientation),intent(in)::orientation_ref
    type(IXTorientation)::orientation
    type(IXTstatus)::status
    integer(i4b),intent(in)::index_ref,index
    
    if(index > size(orientation%rotmat,3))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'orientation rotmat length is too short for supplied index (IXFcopy_index_from_orientation)')              
       return
    endif
    if(index_ref > size(orientation_ref%rotmat,3))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'orientation_ref rotmat length is too short for supplied index (IXFcopy_index_from_orientation)')              
       return
    endif      
    orientation%rotmat(:,:,index)=orientation_ref%rotmat(:,:,index_ref)
    
  end subroutine  IXFcopy_index_from_orientation
subroutine IXFfinish_effective_orientation(orientation)
  implicit none
  type(IXTorientation)::orientation
     call IXFmark_valid(orientation)
    
end subroutine
  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:

  !! Multiply two orientations. If R is the orientation of S' with respect to S, and R' is the orientation
  !! of S'' with respect to S', then the orientation of S'' with respect to S is given by the matrix
  !! multiplication Rtot = R'*R.
  !!
  !!>  i.e.   S ------>  S' ----------> S''
  !!              R,T          R',T'
  !!
  !!   then   S ----------------------> S''
  !!<                    Rtot, Ttot

  subroutine IXFtimes_orientation(or1,or2,ores,status)
    type(IXTorientation), intent(IN) :: or1, or2
    type(IXTorientation) :: ores
    type(IXTstatus)::status
    integer(i4b)::len1,len2,i
    logical::o1_unity,o2_unity
    o1_unity=.false.
    o2_unity=.false.
    len1=size(or1%rotmat,3)
    len2=size(or2%rotmat,3)    
    if(len1 == 1)o1_unity=.true.
    if(len2 == 1)o2_unity=.true.
        
    if((len1 /= len2) .and. (.not. o1_unity .or. .not. o2_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'orientation inputs are wrong length, must be (3,3,n)&(3,3,n) or (3,3,n)&(3,3,1)(IXFtimes_translation)')
       return
    endif
            
    if(o1_unity .and. (.not. o2_unity))then
      call IXFallocdims(ores%rotmat,(/ 3,3,len2 /),status)    
      do i=1,len2
        ores%rotmat(:,:,i) = matmul(or1%rotmat(:,:,1),or2%rotmat(:,:,i))
      enddo
      call IXFmark_valid(ores)
      return
    endif
    
    if(o2_unity .and. (.not. o1_unity))then
      call IXFallocdims(ores%rotmat,(/ 3,3,len1 /),status)
      do i=1,len1
        ores%rotmat(:,:,i) =  matmul(or1%rotmat(:,:,i),or2%rotmat(:,:,1)) 
      enddo
      call IXFmark_valid(ores)
      return
    endif
    ! otherwise both the same length
    call IXFallocdims(ores%rotmat,(/ 3,3,len1 /),status)
    do i=1,len1
      ores%rotmat(:,:,i) = matmul(or1%rotmat(:,:,i), or2%rotmat(:,:,i))
    enddo
    call IXFmark_valid(ores)
    
  end subroutine IXFtimes_orientation
!
!  subroutine IXFcombine_orientation(or1, t1, or2, t2, ores, tres)
!    type(IXTorientation), intent(IN) :: or1, or2
!    type(IXTtranslation), intent(IN) :: t1, t2
!    type(IXTorientation), intent(OUT) :: ores
!    type(IXTtranslation), intent(OUT) :: tres
!    ores%rotmat = matmul(or2%rotmat, or1%rotmat)
!    tres = IXFmatmul_translation(transpose(or1%rotmat),t2) + t1
!  end subroutine IXFcombine_orientation
!

  subroutine IXFcombine_orientation(or1, t1, or2, t2, ores, tres,status)
    type(IXTorientation), intent(IN) :: or1, or2
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTorientation), intent(OUT) :: ores
    type(IXTtranslation), intent(OUT) :: tres
    type(IXTstatus)::status
    integer(i4b)::i

    call IXFtimes(or2, or1,ores,status)
    if(status == IXCseverity_error)return
    do i=1,size(or1%rotmat,3)
!      call IXFplus(IXFmatmul_translation(transpose(or1%rotmat(:,:,i)),t2), t1,tres,status)
      if(status == IXCseverity_error)return
    enddo
  end subroutine IXFcombine_orientation



!  !! Difference between two coord frames:
!  !!>  if   S ------> S1    &   S ------> S2
!  !!           O1,T1               O2,T2
!  !!
!  !!   returns   S1 --------------> S2
!  !!<                Odiff, Tdiff
!  subroutine IXFdifference_orientation(or1, t1, or2, t2, odiff, tdiff)
!    type(IXTorientation), intent(IN) :: or1, or2
!    type(IXTtranslation), intent(IN) :: t1, t2
!    type(IXTorientation), intent(OUT) :: odiff
!    type(IXTtranslation), intent(OUT) :: tdiff
!    odiff%rotmat = matmul(or2%rotmat, transpose(or1%rotmat))
!    tdiff = IXFmatmul_translation(or1%rotmat,t2-t1)
!  end subroutine IXFdifference_orientation
!
!  subroutine IXFdifference_orientation(or1, t1, or2, t2, odiff, tdiff,status)
!    type(IXTorientation), intent(IN) :: or1, or2
!    type(IXTtranslation), intent(IN) :: t1, t2
!    type(IXTorientation), intent(OUT) :: odiff
!    type(IXTtranslation), intent(OUT) :: tdiff
!    type(IXTtranslation)::t_temp
!    type(IXTstatus)::status
!    odiff%rotmat = matmul(or2%rotmat, transpose(or1%rotmat))
!    !t_temp = t2-t1
!    call IXFminus_translation(t2,t1,t_temp,status)
!    call IXFmatmul_translation(or1%rotmat,t_temp,tdiff,status)
!    call IXFdestroy(t_temp,status)
!  end subroutine IXFdifference_orientation


  !! If vector has components v in S, return its components v' in S', where S -(R,T)-> S'
  subroutine IXFs2sprime_orientation(or, t, v,vprime,status)
    type(IXTorientation), intent(IN) :: or
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)    
    type(IXTstatus)::status
    real(dp) :: vprime(:,:)    
    call IXFs2sprime_translation (or%rotmat, t, v,vprime,status)
  end subroutine IXFs2sprime_orientation

  !! If vector has components v' in S', return its components v in S, where S -(R,T)-> S'
  subroutine IXFsprime2s_orientation(or, t, vprime,v,status)
    type(IXTorientation), intent(IN) :: or
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: vprime(3)
    type(IXTstatus)::status
    real(dp) :: v(:,:)
    call IXFsprime2s_translation (or%rotmat, t, vprime,v,status)
  end subroutine IXFsprime2s_orientation

end module IXMorientation

