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
!! @version $Revision: 1344 $ ($Date: 2008-04-22 05:53:04 -0400 (Tue, 22 Apr 2008) $)
!!

module IXMorientation
  use IXMtype_definitions
  use IXMbase
  use IXMmaths_basis
  use IXMmaths_utils
  use IXMtranslation
  implicit none
  ! default is all variables and functions are hidden
  !	private
  ! public types
  public :: IXTorientation
  ! public interfaces
  !    public :: 

  type IXTorientation
     private		! The type is public, but the contents not
     type(IXTbase):: base
     real(dp) :: rotmat(3,3)=0.0_dp	! rotation matrix
  end type IXTorientation


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE orientation
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

  interface operator (*)
     module procedure IXFtimes_op_orientation
  end interface

!  interface IXFset
!     module procedure IXFset_class_orientation, IXFset_attributes_orientation, &
!          IXFset_rotvec_orientation
!  end interface

  interface IXFsetgen_orientation
     module procedure IXFset_rotvec_orientation
  end interface

!  interface IXFget_orientation
!     module procedure IXFget_class_orientation, IXFget_attributes_orientation
!  end interface

  interface IXFcreate
     module procedure IXFcreate_class_orientation
  end interface

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
    call IXFoperation_run(op,'rotmat', arg%rotmat, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_orientation

  !----------------------------------------------------------------------------------------------------------------------
  !! Set orientation from one of
  !!   - another orientation object
  !!   - the components of an orientation (i.e. a 3-by-3 orthogonal matrix)
  !!   - a rotation vector (a 3-vector giving rotation axis with magnitude equal to angle of rotation)
  !!
  !! Syntax:
  !!   - call IXFset_orientation (orient_out, orient_in)     if orient_in is another orientation object
  !!   - call IXFset_orientation (orient_out, status, r)     if r=3-by-3 matrix
  !!   - call IXFset_orientation (orient_out, status, v)     if v=3-vector
  !!
  !! A copy is made of the input; the output does *not* point to the input.

  ! Set from another object od same class:
  subroutine IXFset_class_orientation (self, orientation)
    implicit none
    type(IXTorientation), intent(INOUT) :: self					!! the orientation object we are going to set
    type(IXTorientation), intent(IN) :: orientation	!! orientation from which to make a copy
    self = orientation              ! no checking required, as existing object must already be valid
  end subroutine IXFset_class_orientation

  ! Set from attributes:
  subroutine IXFset_attributes_orientation (self, status, rotmat)
    implicit none
    type(IXTorientation), intent(INOUT) :: self					!! the orientation object we are going to set
    real(dp), intent(IN), optional :: rotmat(3,3)				!! 3-by-3 matrix of the orientation
    type(IXTstatus):: status
    if (present(rotmat)) then
       self%rotmat = rotmat
       call IXFcheck_orientation(self, status)
    endif
  end subroutine IXFset_attributes_orientation

  recursive subroutine IXFset_orientation(orientation,status,rotmat,rotvec,ref)
    implicit none
    type(IXTorientation), intent(INOUT) :: orientation					!! the orientation object we are going to set
    type(IXTorientation), intent(IN),optional :: ref					!! the orientation object we are going to set
    real(dp), intent(IN), optional :: rotmat(3,3)				!! 3-by-3 matrix of the orientation
    real(dp), intent(IN), optional :: rotvec(3)				!! rotation vector to be used to set orientation
    type(IXTstatus),intent(inout):: status


    if (present(rotmat) .and. present(rotvec))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'You can set IXTorientation using either *rotmat* or *rotvec*, not both (IXFset_orientation)')
    endif

    if(status == IXCseverity_error)return

    if (present(ref))call IXFset_orientation(orientation,status,ref%rotmat)
    if (present(rotmat))orientation%rotmat=rotmat
    ! Set from other data: implicit call to conversion
    if (present(rotvec))orientation%rotmat = IXFrotvec_to_rotmat(rotvec)
    call IXFcheck_orientation(orientation, status)

  end subroutine IXFset_orientation
  !-----------------------------------------------------------------------------------------------------------------------
  !! subroutine which returns elements of a dataset_1d object. For "array" elements it assumes that the
  !! lengths of supplied arrays are the correct size to copy the object arrays into. If this is not the 
  !! case then an error is generated, for this circumstance the IXFget_alloc_dataset_1d subroutine must
  !! then be used.
!! in this particular case "rotvec", while not an element of orientation, can also be retrieved
  subroutine IXFget_orientation(orientation,status,rotmat,rotvec,wout)
    implicit none
    type(IXTorientation), intent(INOUT) :: orientation					!! the orientation object we are going to set
    type(IXTorientation), intent(out),optional :: wout					!! the orientation object we are going to set
    real(dp), intent(out), optional :: rotmat(3,3)				!! 3-by-3 matrix of the orientation
    real(dp), intent(out), optional :: rotvec(3)				!! rotation vector to be used to set orientation
    type(IXTstatus),intent(inout):: status


    if (present(rotmat) .and. present(rotvec))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'You can set IXTorientation using either *rotmat* or *rotvec*, not both (IXFset_orientation)')
    endif

    if(status == IXCseverity_error)return

    if (present(wout))call IXFcopy(orientation,wout,status)
    if (present(rotmat))rotmat=orientation%rotmat
    ! Set from other data: implicit call to conversion
    if (present(rotvec))rotvec = IXFrotmat_to_rotvec(orientation%rotmat)
    call IXFcheck_orientation(orientation, status)

  end subroutine IXFget_orientation

  ! Set from other data:
  subroutine IXFset_rotvec_orientation (self, status, rotvec)
    implicit none
    type(IXTorientation), intent(INOUT) :: self					!! the orientation object we are going to set
    real(dp), intent(IN) :: rotvec(3)					        !! rotation vector to be used to set orientation
    type(IXTstatus):: status
    self%rotmat = IXFrotvec_to_rotmat(rotvec)
    call IXFcheck_orientation(self, status)
  end subroutine IXFset_rotvec_orientation

  !! Get components of an orientation. 
  !!
  !! Syntax:
  !!   - call IXFget_orientation (orientation, orient_out)        to get a copy of the orientation object
  !!   - call IXFget_orientation (orientation, status, r)         to get 3-by-3 matrix
  !!   - call IXFget_rotvec_orientation (orientation, status, v)  to get equivalent rotation vector
  !!
  subroutine IXFget_class_orientation(self, orientation)
    implicit none
    type(IXTorientation), intent(IN) :: self			!! the orientation object from which attributes will be extracted
    type(IXTorientation), intent(OUT):: orientation	    !! get orientation object
    orientation = self
  end subroutine IXFget_class_orientation

  subroutine IXFget_attributes_orientation(self, status, rotmat)
    implicit none
    type(IXTorientation), intent(IN) :: self			!! the orientation object from which attributes will be extracted
    real(dp), intent(OUT), optional :: rotmat(3,3)		!! get rotation matrix
    type(IXTstatus), intent(INOUT) :: status
    rotmat = self%rotmat
  end subroutine IXFget_attributes_orientation

  subroutine IXFget_rotvec_orientation(self, status, rotvec)
    implicit none
    type(IXTorientation), intent(IN) :: self			!! the orientation object from which attributes will be extracted
    real(dp), intent(OUT), optional :: rotvec(3)		!! get rotation vector
    type(IXTstatus), intent(INOUT) :: status
    rotvec = IXFrotmat_to_rotvec(self%rotmat)
  end subroutine IXFget_rotvec_orientation


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

  subroutine IXFcreate_class_orientation(self, orientation)
    implicit none
    type(IXTorientation), intent(OUT) :: self			!! the orientation object we are going to create
    type(IXTorientation), intent(IN)  :: orientation	!! the orientation object we are going to copy
    self = orientation
  end subroutine IXFcreate_class_orientation

  subroutine IXFcreate_orientation(self, status, rotmat)
    implicit none
    type(IXTorientation), intent(OUT) :: self			!! the orientation object we are going to create
    real(dp), intent(IN), optional :: rotmat(3,3)		!! rotation matrix defining the orientation
    type(IXTstatus):: status

    ! Create default output:
    call IXFunit_matrix(self%rotmat)	! no rotation
    ! Now call set routine:
    if (present(rotmat)) then
       call IXFset_attributes_orientation(self, status, rotmat)
       if (status /= IXCseverity_ok) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
               & 'Error in IXFcreate_attributes_orientation')
       endif
    endif

  end subroutine IXFcreate_orientation

  !----------------------------------------------------------------------------------------------------------------------
  !! Subroutine to check consistency of arguments.
  !! Use the fact that the transpose of the rotation matrix relating orthonormal bases is the inverse of the
  !! rotation matrix.

  subroutine IXFcheck_orientation(orientation, status)
    type(IXTorientation) :: orientation
    type(IXTstatus) :: status
    if (.not. IXFrotmat_orthogonal(orientation%rotmat)) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, IXCerr_invparam, &
            & 'Rotation matrix not orthogonal - failure in IXFcheck_orientation')
    endif
  end subroutine IXFcheck_orientation

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

  function IXFtimes_op_orientation(or1,or2) result(ores)
    type(IXTorientation), intent(IN) :: or1, or2
    type(IXTorientation) :: ores
    ores%rotmat = matmul(or2%rotmat, or1%rotmat)
  end function IXFtimes_op_orientation

  subroutine IXFcombine_orientation(or1, t1, or2, t2, ores, tres)
    type(IXTorientation), intent(IN) :: or1, or2
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTorientation), intent(OUT) :: ores
    type(IXTtranslation), intent(OUT) :: tres
    ores%rotmat = matmul(or2%rotmat, or1%rotmat)
    tres = IXFmatmul_translation(transpose(or1%rotmat),t2) + t1
  end subroutine IXFcombine_orientation

  !! Difference between two coord frames:
  !!>  if   S ------> S1    &   S ------> S2
  !!           R1,T1               R2,T2
  !!
  !!   returns   S1 --------------> S2
  !!<                Rdiff, Tdiff
  subroutine IXFdifference_orientation(or1, t1, or2, t2, odiff, tdiff)
    type(IXTorientation), intent(IN) :: or1, or2
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTorientation), intent(OUT) :: odiff
    type(IXTtranslation), intent(OUT) :: tdiff
    odiff%rotmat = matmul(or2%rotmat, transpose(or1%rotmat))
    tdiff = IXFmatmul_translation(or1%rotmat,t2-t1)
  end subroutine IXFdifference_orientation

  !! If vector has components v in S, return its components v' in S', where S -(R,T)-> S'
  function IXFs2sprime_orientation(or, t, v) result(vprime)
    type(IXTorientation), intent(IN) :: or
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)
    real(dp) :: vprime(3)
    vprime = IXFs2sprime_translation (or%rotmat, t, v)
  end function IXFs2sprime_orientation

  !! If vector has components v' in S', return its components v in S, where S -(R,T)-> S'
  function IXFsprime2s_orientation(or, t, vprime) result(v)
    type(IXTorientation), intent(IN) :: or
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: vprime(3)
    real(dp) :: v(3)
    v = IXFsprime2s_translation (or%rotmat, t, v)
  end function IXFsprime2s_orientation

end module IXMorientation

