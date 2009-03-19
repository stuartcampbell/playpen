!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMtranslation
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMtranslation object.
!!
!! Nearest equivalent NeXus class: NXtranslation
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1344 $ ($Date: 2008-04-22 05:53:04 -0400 (Tue, 22 Apr 2008) $)
!!

module IXMtranslation
  use IXMtype_definitions
  use IXMbase
  use IXMmaths_basis
  implicit none
  ! default is all variables and functions are hidden
  !	private
  ! public types
  public :: IXTtranslation
  ! public interfaces
  !    public :: 

  type IXTtranslation
     private		! The type is public, but the contents not
     type(IXTbase):: base
     real(dp) :: vector(3)=0.0_dp	! cartesian 3-vector
  end type IXTtranslation


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE translation
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors

  interface operator (+)
     module procedure IXFtt_plus_op_translation, IXFtv_plus_op_translation, IXFvt_plus_op_translation
  end interface

  interface operator (-)
     module procedure IXFtt_minus_op_translation, IXFtv_minus_op_translation, IXFvt_minus_op_translation
  end interface

  interface IXFdot
     module procedure IXFdot_translation
  end interface

  interface IXFnorm
     module procedure IXFnorm_translation
  end interface

  interface IXFcross
     module procedure IXFcross_translation
  end interface

  !----------------------------------------------------------------------------------------------------------------------
contains

#define IXD_DESCRIPTION	"IXTtranslation class"
#define IXD_TYPE translation
#define IXD_SQTYPE 'translation'
#include "class_base.f90"

  !!generic destroy routine which does nothing but helps compilation
  subroutine IXFdestroy_translation(arg, status)
    implicit none
    type(IXTtranslation) :: arg
    type(IXTstatus) :: status
    call IXFdestroy(arg%base,status)
  end subroutine IXFdestroy_translation
  !----------------------------------------------------------------------------------------------------------------------
  recursive subroutine IXFoperation_run_translation(op, field, arg, status)
    implicit none
    type(IXTtranslation) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTtranslation', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op,'vector', arg%vector, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_translation

  !----------------------------------------------------------------------------------------------------------------------
  ! Constructor routine to assemble the object from the individual components.

  !! Set translation from
  !!   - another translation
  !!   - the components of a translation (i.e. a three-vector)
  !!
  !! Syntax:
  !!   - call IXFset_translation (translation_out, status, ref=translation_in)    if translation_in is another translation object
  !!   - call IXFset_translation (translation_out, status, vector=v)			if v=3-vector
  !!
  !! [A copy is made of the input; the output does *not* point to the input]

  recursive subroutine IXFset_translation(self, status, vector,ref)
    implicit none
    type(IXTtranslation), intent(INOUT) :: self					!! the translation object we are going to set
    type(IXTtranslation), intent(IN), optional :: ref	!! translation from which to make a copy
    real(dp), intent(IN), optional :: vector(3)					!! set three vector of the translation
    type(IXTstatus):: status
   ! check that either the reference object is initialised
   ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_translation)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(self)
    else    
       if(IXFvalid(self) .neqv. .true.) then
           call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
                IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_translation)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_translation(self,status,ref%vector)
    if (present(vector))self%vector=vector

    call IXFcheck_translation(self,status)

  end subroutine IXFset_translation


  !! Get components of a translation.  
  !!
  !! Syntax:
  !!   - call IXFget_translation (translation, status, wout=translation_out)     copy of the translation
  !!   - call IXFget_translation (translation, status, vector=v)            v will contain the 3-vector translation

  subroutine IXFget_translation(self, status, vector,wout)
    implicit none
    type(IXTtranslation), intent(IN) :: self			!! the translation object from which attributes will be extracted
    type(IXTtranslation), intent(OUT), optional :: wout	!! copy of the translation
    real(dp), intent(OUT), optional :: vector(3)		!! get three vector translation
    type(IXTstatus), intent(INOUT) :: status

    if (present(wout)) call IXFcopy(self,wout,status)
    if (present(vector)) vector = self%vector

  end subroutine IXFget_translation


  !! Create a translation from a three-vector. In the case of a translation object, the 'create' and 'set'
  !! routines are almost identical. Generally a 'create' routine differs from the 'set' routine in that
  !! the 'set' routine allows an existing object to have just a selection of its attributes altered. The
  !! 'create' routine creates a new object from its components. 
  !!   - call IXFcreate_translation (translation_out, status, vector=v)			set translation to v(3)
  !!   - call IXFcreate_translation (translation_out, status)						set to default translation (zero)

  subroutine IXFcreate_translation(self, status, vector)
    implicit none
    type(IXTtranslation), intent(OUT) :: self					!! the translation object we are going to set
    real(dp), intent(IN) :: vector(3)					!! three-vector defining the translation
    type(IXTstatus):: status

    call IXFset_translation(self,status,vector)
    call IXFmark_valid(self)

  end subroutine IXFcreate_translation

  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments

  subroutine IXFcheck_translation(translation, status)
    type(IXTtranslation) :: translation
    type(IXTstatus) :: status
    ! Dummy argument, as any three-vector is valid
    call IXFcheck_base(translation%base,status)
  end subroutine IXFcheck_translation

  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:

  ! methods just on translations:

  ! addition:
  function IXFtt_plus_op_translation(t1,t2) result(tres)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation) :: tres
    tres%vector = t1%vector + t2%vector
  end function IXFtt_plus_op_translation

  function IXFtv_plus_op_translation(t,v) result(tres)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation) :: tres
    tres%vector = t%vector + v
  end function IXFtv_plus_op_translation

  function IXFvt_plus_op_translation(v,t) result(tres)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation) :: tres
    tres%vector = v + t%vector
  end function IXFvt_plus_op_translation

  ! subtraction:
  function IXFtt_minus_op_translation(t1,t2) result(tres)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation) :: tres
    tres%vector = t1%vector - t2%vector
  end function IXFtt_minus_op_translation

  function IXFtv_minus_op_translation(t,v) result(tres)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation) :: tres
    tres%vector = t%vector - v
  end function IXFtv_minus_op_translation

  function IXFvt_minus_op_translation(v,t) result(tres)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation) :: tres
    tres%vector = v - t%vector
  end function IXFvt_minus_op_translation

  ! Other binary, unary operators:
  function IXFnorm_translation(t) result(length)
    type(IXTtranslation), intent(IN) :: t
    real(dp) length
    length = IXFnorm(t%vector)
  end function IXFnorm_translation

  function IXFdot_translation(t1,t2) result(dot)
    type(IXTtranslation), intent(IN) :: t1, t2
    real(dp) dot
    dot = IXFdot(t1%vector, t2%vector)
  end function IXFdot_translation

  function IXFcross_translation(t1,t2) result(tres)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation) :: tres
    tres%vector = IXFcross(t1%vector,t2%vector)
  end function IXFcross_translation

  ! methods with orientations too:

  !! If t in S, return t' in S', where S -(R)-> S'
  !! If a translation has components t in the reference frame, get the components in a frame rotated by matrix R
  function IXFmatmul_translation(rotmat,t) result(tres)
    real(dp), intent(IN) :: rotmat(3,3)     !! orientation matrix
    type(IXTtranslation), intent(IN) :: t
    type(IXTtranslation) :: tres
    tres%vector = matmul(rotmat, t%vector)
  end function IXFmatmul_translation

  !! If vector has components v in S, return its components v' in S', where S -(R,T)-> S'
  function IXFs2sprime_translation(rotmat, t, v) result(vprime)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: rotmat(3,3), v(3)
    real(dp) :: vprime(3)
    vprime = matmul(rotmat,v-t%vector)
  end function IXFs2sprime_translation

  !! If vector has components v' in S', return its components v in S, where S -(R,T)-> S'
  function IXFsprime2s_translation(rotmat, t, vprime) result(v)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: rotmat(3,3), vprime(3)
    real(dp) :: v(3)
    v = matmul(transpose(rotmat),vprime) + t%vector
  end function IXFsprime2s_translation

end module IXMtranslation
