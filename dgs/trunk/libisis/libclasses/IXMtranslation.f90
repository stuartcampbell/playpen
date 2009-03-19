!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMtranslation
!-----------------------------------------------------------------------------------------------------------------------------------
!! Fortran definition of IXMtranslation object.
!!
!! Nearest equivalent NeXus class: NXtranslation
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 1418 $ ($Date: 2008-07-08 08:20:55 -0400 (Tue, 08 Jul 2008) $)
!!

module IXMtranslation
  use IXMtype_definitions
  use IXMbase
  use IXMmaths_basis
  implicit none
  ! default is all variables and functions are hidden
  ! public types
  public :: IXTtranslation
  ! public interfaces
  !    public :: 
  type IXTtranslation
     private		! The type is public, but the contents not
     type(IXTbase):: base
     real(dp), pointer :: vector(:,:)=> NULL()
     !! vector will have dimensions (3,n) containing the Cartesian 3-vectors for a group of n objects
  end type IXTtranslation


  !-----------------------------------------------------------------------
  ! The following creates interfaces for generic functions defined by LIBISISEXC intrastructure
#define IXD_TYPE translation
#include "class_header.f90"

  !-----------------------------------------------------------------------
  ! Interfaces to generic functions defined by class authors
!
  interface IXFplus_translation
     module procedure IXFtt_plus_translation, IXFtv_plus_translation , IXFvt_plus_translation
  end interface IXFplus_translation
  interface IXFplus
     module procedure IXFtt_plus_translation, IXFtv_plus_translation, IXFvt_plus_translation
  end interface IXFplus
  interface IXFminus_translation
     module procedure IXFtt_minus_translation, IXFtv_minus_translation , IXFvt_minus_translation
  end interface IXFminus_translation
  interface IXFminus
     module procedure IXFtt_minus_translation, IXFtv_minus_translation, IXFvt_minus_translation
  end interface IXFminus  
!
!  interface operator (-)
!     module procedure IXFtt_minus_op_translation, IXFtv_minus_op_translation, IXFvt_minus_op_translation
!  end interface
!

! FAA: this doen't work with g95 as IXFdot_translation and IXFdot have
!      different signatures
!  interface IXFdot
!     module procedure IXFdot_translation
!  end interface

  interface IXFnorm
     module procedure IXFnorm_translation
  end interface

! FAA: this doen't work with g95 as IXFcross_translation and IXFcross have
!      different signatures
!  interface IXFcross
!     module procedure IXFcross_translation
!  end interface
  
  interface IXFfind_L2thetaphi_translation
     module procedure find_L2thetaphi_array, find_L2thetaphi_single
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
    call IXFdealloc(arg%vector,status)
    call IXFclear_valid(arg)
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
    call IXFoperation_run_ptr(op,'vector', arg%vector, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_translation


  recursive subroutine IXFset_translation(translation, status, vector,ref)
    implicit none
    type(IXTtranslation), intent(INOUT) :: translation				!! the translation object we are going to set
    type(IXTtranslation), intent(IN), optional :: ref	!! translation from which to make a copy    
    real(dp), intent(IN), optional :: vector(:,:)					!! set three vector of the translation
    type(IXTstatus)::status
    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_translation)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(translation)
    else    
       if(IXFvalid(translation) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_translation)')
       endif
       if(status == IXCseverity_error)return
    endif
    
    if (present(ref))call IXFset_translation(translation,status,ref%vector)
    call IXFset_real_array(translation%vector,status,vector)
    call IXFcheck_translation(translation,status)

  end subroutine IXFset_translation



  subroutine IXFget_translation(translation, status, vector,wout)
    implicit none
    type(IXTtranslation), intent(IN) :: translation			!! the translation object from which attributes will be extracted
    type(IXTtranslation), intent(OUT), optional :: wout	!! copy of the translation   
    real(dp), intent(OUT), optional :: vector(:,:)		!! get three vector translation
    type(IXTstatus), intent(INOUT) :: status

    if (present(wout)) call IXFcopy(translation,wout,status)    
    call IXFget_real_array(translation%vector,status,vector)

  end subroutine IXFget_translation

  subroutine IXFget_alloc_translation(translation, status, vector,wout)
    implicit none
    type(IXTtranslation), intent(IN) :: translation			!! the translation object from which attributes will be extracted
    type(IXTtranslation), intent(OUT), optional :: wout	!! copy of the translation   
    real(dp), allocatable, optional :: vector(:,:)		!! get three vector translation
    type(IXTstatus), intent(INOUT) :: status

    if(present(vector))call IXFreallocdimsFortran(vector,shape(vector),.false.,status)
    call IXFget_translation(translation,status,vector,wout)

  end subroutine IXFget_alloc_translation

  subroutine IXFcreate_translation(translation, status, vector)
    implicit none
    type(IXTtranslation), intent(OUT) :: translation			!! the translation object we are going to set    
    real(dp), intent(IN) :: vector(:,:)					!! three-vector defining the translation
    type(IXTstatus):: status
    
    call IXFmark_valid(translation)
    call IXFset_translation(translation,status,vector)

  end subroutine IXFcreate_translation

  subroutine IXFcreate_L2thetaphi_translation(translation,L2,theta,phi,status)
    implicit none
    type(IXTtranslation), intent(OUT) :: translation			!! the translation object we are going to set    
    integer(i4b)::len1,len2,len3
    real(dp), intent(IN) :: L2(:),theta(:),phi(:)					!! three-vector defining the translation
    type(IXTstatus):: status
    
    len1=size(L2)
    len2=size(phi)
    len3=size(theta)
    
    if(len1 /= len2)then
      call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input arrays must be of same length(IXFcreate_l2thetaphi_translation)')
    else
      if(len2/=len3)then      
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'input arrays must be of same length(IXFcreate_l2thetaphi_translation)')      
      endif
    endif
    
    call IXFreallocdims(translation%vector,(/ 3,len1 /),.false.,status)
    
    translation%vector(1,:)=L2*sin(theta)*cos(phi)
    translation%vector(2,:)=L2*sin(theta)*sin(phi)
    translation%vector(3,:)=L2*cos(theta)
    
    call IXFmark_valid(translation)       

  end subroutine IXFcreate_L2thetaphi_translation
  
  subroutine find_L2thetaphi_array(translation,L2,phi,theta,status)
    implicit none
    type(IXTtranslation),intent(in)::translation
    real(dp),intent(out)::L2(:),theta(:),phi(:)
    integer(i4b)::i
    type(IXTstatus)::status
    
    if(size(L2) /= size(theta))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input arrays must be of same length(IXFfind_L2thetaphi_translation)')      
          return
    else
      if(size(theta)/=size(phi))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input arrays must be of same length(IXFfind_L2thetaphi_translation)')      
          return
      else
        if(size(phi) /= size(translation%vector,2))then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_invparam, 'input arrays must be of same length(IXFfind_L2thetaphi_translation)')              
          return
        endif
      endif  
    endif

  do i=1,size(L2)
    call find_l2thetaphi(translation%vector(:,i),L2(i),theta(i),phi(i))
  enddo
    
  end subroutine find_L2thetaphi_array

  subroutine find_L2thetaphi_single(translation,L2,phi,theta,status)
    implicit none
    type(IXTtranslation),intent(in)::translation
    real(dp),intent(out)::L2,theta,phi
    type(IXTstatus)::status
    
    if(size(translation%vector,2)/=1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
               IXCerr_invparam, 'translation vector length is greater than 1, returning L2 theta phi of first vector &
               &(IXFfind_L2thetaphi_translation)')              
    endif
    call find_L2thetaphi(translation%vector(:,1),L2,theta,phi)
    
  end subroutine find_L2thetaphi_single

  subroutine find_L2thetaphi(vector,L2,theta,phi)
    implicit none
    real(dp),intent(in)::vector(3)
    real(dp),intent(out)::L2,theta,phi
    
    L2=IXFnorm(vector)
    theta=acos(vector(3)/L2)
    
    if(vector(2) > 0)then
      phi=acos(vector(1)/(L2*sin(theta)))
      return
    endif
    
    if(vector(1) < 0 .and. vector(2) < 0)then
      phi=pi_dp+abs(asin(vector(1)/(L2*sin(theta))))
      return
    endif
    
    if(vector(1) > 0 .and. vector(2) < 0)then
      phi=twopi_dp+asin(vector(1)/(L2*sin(theta)))
      return
    endif
  end subroutine find_L2thetaphi
    
  subroutine IXFset_index_translation(translation,vector,index,status)
    implicit none
    type(IXTtranslation)::translation
    type(IXTstatus)::status
    real(dp)::vector(3)
    integer(i4b)::index
    
    if(index > size(translation%vector,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'translation vector length is too short for supplied index (IXFset_index_translation)')              
       return
    endif    
    translation%vector(:,index)=vector
    
  end subroutine  IXFset_index_translation

  subroutine IXFget_index_translation(translation,index,vector,status)
    implicit none
    type(IXTtranslation),intent(in)::translation
    type(IXTstatus)::status
    real(dp),intent(out)::vector(3)
    integer(i4b),intent(in)::index
    
    if(index > size(translation%vector,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'translation vector length is too short for supplied index (IXFget_index_translation)')              
       return
    endif    
    vector=translation%vector(:,index)
    
  end subroutine  IXFget_index_translation

  
  subroutine IXFcopy_index_from_translation(translation_ref,index_ref,index,translation,status)
    implicit none
    type(IXTtranslation),intent(in)::translation_ref
    type(IXTtranslation)::translation
    type(IXTstatus)::status
    integer(i4b),intent(in)::index_ref,index
    
    if(index > size(translation%vector,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'translation rotmat length is too short for supplied index (IXFcopy_index_from_translation)')              
       return
    endif
    if(index_ref > size(translation_ref%vector,2))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_warning, &
           IXCerr_invparam, 'translation_ref rotmat length is too short for supplied index (IXFcopy_index_from_translation)')              
       return
    endif      
    translation%vector(:,index)=translation_ref%vector(:,index_ref)
    
  end subroutine  IXFcopy_index_from_translation

  !----------------------------------------------------------------------------------------------------------------------
  ! Subroutine to check consistency of arguments

  subroutine IXFcheck_translation(translation, status)
    type(IXTtranslation) :: translation
    type(IXTstatus) :: status
    ! Dummy argument, as any three-vector is valid
    if(size(translation%vector,1) /= 3)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_invparam, 'translation%vector is wrong shape must be 3*n array (IXFcheck_translation)')
    endif
    
  end subroutine IXFcheck_translation
  
  subroutine IXFpopulate_reference_translation(translation_new,ref_translation,lookup,raw_ranks,list,status)
    implicit none
    type(IXTtranslation),intent(in)::ref_translation
    type(IXTtranslation),intent(out)::translation_new
    integer(i4b),intent(in)::list(:),lookup(:),raw_ranks(:)
    integer(i4b)::i
    type(IXTstatus)::status
    
    call IXFallocdims(translation_new%vector,(/ 3, size(list) /),status)
    
    do i=1,size(list)
      translation_new%vector(:,lookup(raw_ranks(i)))=ref_translation%vector(:,list(i))
    enddo
    
    call IXFmark_valid(translation_new)
    
  end subroutine IXFpopulate_reference_translation

!> allocates memory to array in object but does not fill it with any data
subroutine IXFmake_translation(translation,len,status)
implicit none
type(IXTtranslation)::translation
integer(i4b),intent(in)::len
type(IXTstatus)::status

    call IXFreallocdims(translation%vector,(/ 3,  len /),.false.,status)

end subroutine IXFmake_translation
subroutine IXFfinish_effective_translation(translation)
  implicit none
  type(IXTtranslation)::translation
     call IXFmark_valid(translation)
    
end subroutine
  !----------------------------------------------------------------------------------------------------------------------
  ! Other methods:

  ! methods just on translations:

  ! addition:
  subroutine IXFtt_plus_translation(t1,t2,tres,status)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::len1,len2,i
    logical::t1_unity,t2_unity
    t1_unity=.false.
    t2_unity=.false.
    len1=size(t1%vector,2)
    len2=size(t2%vector,2)    
    if(len1 == 1)t1_unity=.true.
    if(len2 == 1)t2_unity=.true.
        
    if((len1 /= len2) .and. (.not. t1_unity .or. .not. t2_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'translation inputs are wrong length, must be (3,n)&(3,n) or (3,n)&(3,1)(IXFplus_translation)')
       return
    endif
        
    if(t1_unity .and. (.not. t2_unity))then
      call IXFallocdims(tres%vector,(/ 3,len2 /),status)    
      do i=1,len2
        tres%vector(:,i) = t1%vector(:,1) + t2%vector(:,i)
      enddo
      call IXFmark_valid(tres)
      return
    endif
    
    if(t2_unity .and. (.not. t1_unity))then
      call IXFallocdims(tres%vector,(/ 3,len1 /),status)
      do i=1,len1
        tres%vector(:,i) =  t1%vector(:,i) +t2%vector(:,1) 
      enddo
      call IXFmark_valid(tres)
      return
    endif
    ! otherwise both the same length
    call IXFallocdims(tres%vector,(/ 3,len1 /),status)
    tres%vector = t1%vector + t2%vector
    call IXFmark_valid(tres)
    
  end subroutine IXFtt_plus_translation

  subroutine IXFtv_plus_translation(t1,v,tres,status)
    type(IXTtranslation), intent(IN) :: t1
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::i,len
    len=size(t1%vector,2)
    call IXFallocdims(tres%vector,(/ 3,len /),status)
    do i=1,len 
      tres%vector(:,i) = t1%vector(:,i) + v
    enddo   
    call IXFmark_valid(tres)     
  end subroutine IXFtv_plus_translation

  subroutine IXFvt_plus_translation(v,t2,tres,status)
    type(IXTtranslation), intent(IN) :: t2
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::i,len
    len=size(t2%vector,2)
    call IXFallocdims(tres%vector,(/ 3,len /),status)    
    do i=1,len 
      tres%vector(:,i) = v + t2%vector(:,i)
    enddo
    call IXFmark_valid(tres)
  end subroutine IXFvt_plus_translation
  
  subroutine IXFtt_minus_translation(t1,t2,tres,status)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::len1,len2,i
    logical::t1_unity,t2_unity
    t1_unity=.false.
    t2_unity=.false.
    len1=size(t1%vector,2)
    len2=size(t2%vector,2)    
    if(len1 == 1)t1_unity=.true.
    if(len2 == 1)t2_unity=.true.
        
    if((len1 /= len2) .and. (.not. t1_unity .or. .not. t2_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'translation inputs are wrong length, must be (3,n)&(3,n) or (3,n)&(3,1)(IXFminus_translation)')
       return
    endif
        
    if(t1_unity .and. (.not. t2_unity))then
      call IXFallocdims(tres%vector,(/ 3,len2 /),status)    
      do i=1,len2
        tres%vector(:,i) = t1%vector(:,1) - t2%vector(:,i)
      enddo
      call IXFmark_valid(tres)
      return
    endif
    
    if(t2_unity .and. (.not. t1_unity))then
      call IXFallocdims(tres%vector,(/ 3,len1 /),status)
      do i=1,len1
        tres%vector(:,i) =  t1%vector(:,i) - t2%vector(:,1) 
      enddo
      call IXFmark_valid(tres)
      return
    endif
    ! otherwise both the same length
    call IXFallocdims(tres%vector,(/ 3,len1 /),status)
    tres%vector = t1%vector - t2%vector
    call IXFmark_valid(tres)
    
  end subroutine IXFtt_minus_translation

  subroutine IXFtv_minus_translation(t1,v,tres,status)
    type(IXTtranslation), intent(IN) :: t1
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::i,len
    len=size(t1%vector,2)
    call IXFallocdims(tres%vector,(/ 3,len /),status)
    do i=1,len 
      tres%vector(:,i) = t1%vector(:,i) - v
    enddo   
    call IXFmark_valid(tres)     
  end subroutine IXFtv_minus_translation

  subroutine IXFvt_minus_translation(v,t2,tres,status)
    type(IXTtranslation), intent(IN) :: t2
    real(dp), intent(IN) :: v(3)
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::i,len
    len=size(t2%vector,2)
    call IXFallocdims(tres%vector,(/ 3,len /),status)    
    do i=1,len 
      tres%vector(:,i) = v - t2%vector(:,i)
    enddo
    call IXFmark_valid(tres)
  end subroutine IXFvt_minus_translation


!  ! Other binary, unary operators:
  function IXFnorm_translation(t) result(length)
    type(IXTtranslation), intent(IN) :: t
    real(dp)::length(size(t%vector,2))
    integer(i4b)::i,len
 ! length is expected to be an array of appropriate length
    len=size(t%vector,2)    
    do i=1,len
      length(i) = IXFnorm(t%vector(:,i))
    enddo
  end function IXFnorm_translation
!
!  ! Other binary, unary operators:

  subroutine IXFdot_translation(t1,t2,dot,status)
    type(IXTtranslation), intent(IN) :: t1,t2
    real(dp)::dot(max(size(t1%vector,2),size(t2%vector)))
    integer(i4b)::i,len1,len2
    logical::t1_unity,t2_unity
    type(IXTstatus)::status
! dot is expected to be an array of appropriate length
    t1_unity=.false.
    t2_unity=.false.
    len1=size(t1%vector,2)
    len2=size(t2%vector,2)    
    if(len1 == 1)t1_unity=.true.
    if(len2 == 1)t2_unity=.true.
        
    if((len1 /= len2) .and. (.not. t1_unity .or. .not. t2_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'translation inputs are wrong length, must be (3,n)&(3,n) or (3,n)&(3,1)(IXFdot_translation)')
       return
    endif            
    
    if(t1_unity .and. (.not. t2_unity))then
      do i=1,len2
        dot(i) = IXFdot(t1%vector(:,1),t2%vector(:,i))
      enddo
      return
    endif
    
    if(t2_unity .and. (.not. t1_unity))then
      do i=1,len1
        dot(i) =  IXFdot(t1%vector(:,i),t2%vector(:,1)) 
      enddo
      return
    endif

    ! otherwise both the same length
    do i=1,len1
      dot(i) = IXFdot(t1%vector(:,i),t2%vector(:,i))
    enddo             
  end subroutine IXFdot_translation

  subroutine IXFcross_translation(t1,t2,tres,status)
    type(IXTtranslation), intent(IN) :: t1, t2
    type(IXTtranslation),intent(out) :: tres
    type(IXTstatus)::status
    integer(i4b)::len1,len2,i
    logical::t1_unity,t2_unity
    t1_unity=.false.
    t2_unity=.false.
    len1=size(t1%vector,2)
    len2=size(t2%vector,2)    
    if(len1 == 1)t1_unity=.true.
    if(len2 == 1)t2_unity=.true.
        
    if((len1 /= len2) .and. (.not. t1_unity .or. .not. t2_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'translation inputs are wrong length, must be (3,n)&(3,n) or (3,n)&(3,1)(IXFcross_translation)')
       return
    endif        
    if(t1_unity .and. (.not. t2_unity))then
      call IXFallocdims(tres%vector,(/ 3,len2 /),status)    
      do i=1,len2
        tres%vector(:,i) = IXFcross( t1%vector(:,1), t2%vector(:,i))
      enddo
      call IXFmark_valid(tres)
      return
    endif    
    if(t2_unity .and. (.not. t1_unity))then
      call IXFallocdims(tres%vector,(/ 3,len1 /),status)
      do i=1,len1
        tres%vector(:,i) =  IXFcross(t1%vector(:,i) , t2%vector(:,1) )
      enddo
      call IXFmark_valid(tres)
      return
    endif
    ! otherwise both the same length
    call IXFallocdims(tres%vector,(/ 3,len1 /),status)
    do i=1,len1
      tres%vector(:,i) = IXFcross(t1%vector(:,i), t2%vector(:,i))
    enddo  
    call IXFmark_valid(tres)
    
  end subroutine IXFcross_translation
!
!  ! methods with orientations too:
!
!  !! If t in S, return t' in S', where S -(R)-> S'
!  !! If a translation has components t in the reference frame, get the components in a frame rotated by matrix R
!  function IXFmatmul_translation(rotmat,t) result(tres)
!    real(dp), intent(IN) :: rotmat(3,3)     !! orientation matrix
!    type(IXTtranslation), intent(IN) :: t
!    type(IXTtranslation) :: tres
!    integer(i4b)::i
!    do i=1,size(t%vector,2)
!      tres%vector(:,i) = matmul(rotmat, t%vector(:,i))
!    enddo
!    call IXFmark_valid(tres)
!  end function IXFmatmul_translation
!!
!  !! If vector has components v in S, return its components v' in S', where S -(R,T)-> S'
!  function IXFs2sprime_translation(rotmat, t, v) result(vprime)
!    type(IXTtranslation), intent(IN) :: t
!    real(dp), intent(IN) :: rotmat(3,3), v(3)
!    real(dp) :: vprime(3,size(t%vector,2))
!    integer(i4b)::i
!    do i=1,size(t%vector,2)
!      vprime(:,i) = matmul(rotmat,v-t%vector(:,i))
!    enddo
!  end function IXFs2sprime_translation
!
!!
!!  !! If vector has components v' in S', return its components v in S, where S -(R,T)-> S'
!  function IXFsprime2s_translation(rotmat, t, vprime) result(v)
!    type(IXTtranslation), intent(IN) :: t
!    real(dp), intent(IN) :: rotmat(3,3), vprime(3)
!    real(dp) :: v(3,size(t%vector,2))
!    integer(i4b)::i
!    do i=1,size(t%vector,2)
!      v(:,i) = matmul(transpose(rotmat),vprime) + t%vector(:,i)
!    enddo
!  end function IXFsprime2s_translation

!
!  ! methods with orientations too:
!
!  !! If t in S, return t' in S', where S -(R)-> S'
!  !! If a translation has components t in the reference frame, get the components in a frame rotated by matrix R
  subroutine IXFmatmul_translation(rotmat,t,tres,status)
    real(dp), intent(IN) :: rotmat(:,:,:)     !! orientation matrix
    type(IXTtranslation), intent(IN) :: t
    type(IXTtranslation) :: tres
    integer(i4b)::i,len_r,len_t
    logical::r_unity,t_unity
    type(IXTstatus)::status
    r_unity=.false.
    t_unity=.false.    
    
    len_r=size(rotmat,3)
    len_t=size(t%vector,2)
    if (len_r==1)r_unity=.true.
    if (len_t==1)t_unity=.true.

    if((len_r /= len_t) .and. (.not. r_unity .or. .not. t_unity))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'translation and rotmat inputs wrong length, must be (3,3,n)&(3,n),(3,3,1)&(3,N) or (3,3,N)&(3,1)(IXFmatmul_translation)')
       return
    endif

    if(r_unity .and. (.not. t_unity))then
      call IXFallocdims(tres%vector,(/ 3,len_t /),status)
      do i=1,len_t
        tres%vector(:,i)=matmul(rotmat(:,:,1),t%vector(:,i))
      enddo
      call IXFmark_valid(tres)
      return
    endif
    if(t_unity .and. (.not. r_unity))then
      call IXFallocdims(tres%vector,(/ 3,len_r /),status)
      do i=1,len_r
        tres%vector(:,i)=matmul(rotmat(:,:,i),t%vector(:,1))
      enddo
      call IXFmark_valid(tres)
      return
    endif
    
    call IXFallocdims(tres%vector,(/ 3,len_t /),status)
    do i=1,len_t
      tres%vector(:,i) = matmul(rotmat(:,:,i), t%vector(:,i))
    enddo
    
  end subroutine IXFmatmul_translation
!
  !! If vector has components v in S, return its components v' in S', where S -(R,T)-> S'
  subroutine IXFs2sprime_translation(rotmat, t, v,vprime,status)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: rotmat(:,:,:), v(3)
    type(IXTstatus)::status
    real(dp) :: vprime(:,:)
    integer(i4b)::i,len_r,len_t    

    len_r=size(rotmat,3)
    len_t=size(t%vector,2)

    if((len_r /= len_t) .or. (size(vprime,2) /=len_t) .or. (size(vprime,1) /=3))then
      vprime=0.0
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'rotmat and translation of incommensurate length, or output is of wrong shape(IXFs2sprime_translation)')
      return
    endif
    
    do i=1,len_t
      vprime(:,i) = matmul(rotmat(:,:,i),v-t%vector(:,i))
    enddo

  end subroutine IXFs2sprime_translation

!
!  !! If vector has components v' in S', return its components v in S, where S -(R,T)-> S'
  subroutine IXFsprime2s_translation(rotmat, t, vprime,v,status)
    type(IXTtranslation), intent(IN) :: t
    real(dp), intent(IN) :: rotmat(:,:,:), vprime(3)
!    real(dp) :: v(3,size(t%vector,2))
    type(IXTstatus)::status
    real(dp)::v(:,:)
    integer(i4b)::i,len_r,len_t

    len_r=size(rotmat,3)
    len_t=size(t%vector,2)

    if((len_r /= len_t) .or. (size(v,2) /=len_t) .or. (size(v,1) /=3))then
      v=0.0
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
         IXCerr_invparam, 'rotmat and translation of incommensurate length, or output is of wrong shape(IXFsprime2s_translation)')
      return
    endif

    do i=1,len_t
      v(:,i) = matmul(transpose(rotmat(:,:,i)),vprime) + t%vector(:,i)
    enddo
  end subroutine IXFsprime2s_translation

end module IXMtranslation
