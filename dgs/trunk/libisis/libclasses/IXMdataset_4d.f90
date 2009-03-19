!------------------------------
! MODULE: IXMdataset_4d
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1299 $ ($Date: 2008-01-16 09:41:22 -0500 (Wed, 16 Jan 2008) $)
!!
!! FORTRAN definition of IXMdataset_4d object 
!------------------------------
! MODULE: IXMdataset_4d
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1299 $ ($Date: 2008-01-16 09:41:22 -0500 (Wed, 16 Jan 2008) $)
!!
!! FORTRAN definition of IXMdataset_4d object 
module IXMdataset_4d
  use IXMdataset_common
  use IXMdataset_1d  
  implicit none
  public :: IXTdataset_4d
  type IXTdataset_4d
     private
     type(IXTbase) :: base
     character(len=long_len) ::title='title' !! Title of dataset for plotting purposes
     real(dp), pointer :: signal(:,:,:,:)=>NULL()
     real(dp), pointer:: error(:,:,:,:)=>NULL()
     type(IXTaxis):: s_axis !! Signal axis title (e.g. 'Counts')
     real(dp), pointer :: x1(:)=>NULL()  !! x-axis (either histogram boundaries or point data)
     type(IXTaxis):: x1_axis !! axis along x-axis e.g. 'meV' or 'microseconds' [NeXus: axis attribute]
     !! x-distribution
     !! - If X_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If X_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x1_distribution=.false.
     real(dp), pointer :: x2(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: x2_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x2_distribution=.false.
     real(dp), pointer :: x3(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: x3_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x3_distribution=.false.
     real(dp), pointer :: x4(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: x4_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x4_distribution=.false.
  end type IXTdataset_4d

#define IXD_TYPE	dataset_4d
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTdataset_4d class"
#define IXD_TYPE dataset_4d
#define IXD_SQTYPE 'dataset_4d'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_dataset_4d(op, field, arg, status)
    implicit none
    type(IXTdataset_4d) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdataset_4d', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run(op,'title', arg%title, status)
    call IXFoperation_run_ptr(op,'signal', arg%signal, status)
    call IXFoperation_run_ptr(op,'error',arg%error,status)
    call IXFoperation_run(op,'s_axis', arg%s_axis, status)
    call IXFoperation_run_ptr(op,'x1', arg%x1, status)
    call IXFoperation_run(op,'x1_axis', arg%x1_axis, status)
    call IXFoperation_run(op,'x1_distribution', arg%x1_distribution, status)
    call IXFoperation_run_ptr(op,'x2', arg%x2, status)
    call IXFoperation_run(op,'x2_axis', arg%x2_axis, status)
    call IXFoperation_run(op,'x2_distribution', arg%x2_distribution, status)
    call IXFoperation_run_ptr(op,'x3', arg%x3, status)
    call IXFoperation_run(op,'x3_axis', arg%x3_axis, status)
    call IXFoperation_run(op,'x3_distribution', arg%x3_distribution, status)
    call IXFoperation_run_ptr(op,'x4', arg%x4, status)
    call IXFoperation_run(op,'x4_axis', arg%x4_axis, status)
    call IXFoperation_run(op,'x4_distribution', arg%x4_distribution, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_dataset_4d

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_dataset_4d(w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status !! error status object
    type(IXTdataset_4d),intent(in) :: w1 !! dataset_4d to be checked

  end subroutine IXFcheck_dataset_4d

  subroutine IXFdestroy_dataset_4d(w3d,status)
    type(IXTdataset_4d)::w3d
    type(IXTstatus)::status

    call IXFdestroy(w3d%base,status)
    call IXFdealloc(w3d%x1,status)
    call IXFdealloc(w3d%x2,status)
    call IXFdealloc(w3d%x3,status)
    call IXFdealloc(w3d%x4,status)
    call IXFdealloc(w3d%signal,status)
    call IXFdealloc(w3d%error,status)
    w3d%title='title'
    call IXFdestroy(w3d%s_axis,status)
    call IXFdestroy(w3d%x1_axis,status)
    call IXFdestroy(w3d%x2_axis,status)
    call IXFdestroy(w3d%x3_axis,status)
    call IXFdestroy(w3d%x4_axis,status)
    w3d%x1_distribution=.false.
    w3d%x2_distribution=.false.
    w3d%x3_distribution=.false.
    w3d%x4_distribution=.false.
    call IXFclear_valid(w3d)

  end subroutine IXFdestroy_dataset_4d
  
  subroutine IXFcreate_dataset_4d(dataset_4d,title,signal,error,s_axis,x1,x1_axis,  &
       x1_distribution,x2,x2_axis,x2_distribution,x3,x3_axis,x3_distribution,x4,x4_axis,x4_distribution,status)
    implicit none
    type(IXTdataset_4d),intent(out)::dataset_4d  !! the dataset_1d object to be created
    character(len=*),intent(in) :: title !!input: title passed
    real(dp),intent(in) :: signal(:,:,:,:)!!input: signal ARRAY passed
    real(dp),intent(in):: error(:,:,:,:) !!input: error ARRAY passed
    real(dp),intent(in) :: x1(:),x2(:),x3(:),x4(:) !!input: x/y ARRAY passed
    type(IXTaxis),intent(in) :: s_axis,x1_axis,x2_axis,x3_axis,x4_axis	 !!input: x/y_axis passed
    logical,intent(in) :: x1_distribution,x2_distribution,x3_distribution,x4_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status !! error status object

    ! nested objects should be tested for initialisation, this shows they have been created properly

    if( IXFvalid(s_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_4d)')
    endif

    if( IXFvalid(x1_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_4d)')
    endif

    if( IXFvalid(x2_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_4d)')
    endif

    if( IXFvalid(x3_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_4d)')
    endif
    
    if( IXFvalid(x4_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_4d)')
    endif

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(dataset_4d)

    call IXFset_dataset_4d(dataset_4d,status,title,signal,error,s_axis,x1,x1_axis,  &
         x1_distribution,x2,x2_axis,x2_distribution,x3,x3_axis,x3_distribution,x4,x4_axis,x4_distribution)

  end subroutine IXFcreate_dataset_4d
  
  recursive subroutine IXFset_dataset_4d(dataset_4d,status,title,signal,error,s_axis,  &
       x1,x1_axis,x1_distribution,x2,x2_axis,x2_distribution,x3,x3_axis,x3_distribution,x4,x4_axis,x4_distribution,ref)
    implicit none
    type(IXTdataset_4d),intent(inout)::dataset_4d
    type(IXTdataset_4d),intent(in),optional::ref  !! reference dataset_4d
    character(len=*),optional,intent(in) :: title	
    real(dp),optional,intent(in) :: signal(:,:,:,:), error(:,:,:,:) 

    real(dp), optional,intent(in) :: x1(:),x2(:),x3(:),x4(:)
    type(IXTaxis),optional,intent(in) :: s_axis,x1_axis,x2_axis,x3_axis,x4_axis	

    logical,optional,intent(in) :: x1_distribution, x2_distribution,x3_distribution,x4_distribution
    type(IXTstatus),intent(inout)::status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_dataset_4d)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(dataset_4d)
    else    
       if(IXFvalid(dataset_4d) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_dataset_4d)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_dataset_4d(dataset_4d,status,ref%title,ref%signal,ref%error, &
         ref%s_axis,ref%x1,ref%x1_axis,ref%x1_distribution,          &
         ref%x2,ref%x2_axis,ref%x2_distribution,ref%x3,ref%x3_axis,ref%x3_distribution,ref%x4,ref%x4_axis,ref%x4_distribution)


    if (present(title))dataset_4d%title=title

    if (present(x1_distribution))dataset_4d%x1_distribution=x1_distribution

    if (present(x2_distribution))dataset_4d%x2_distribution=x2_distribution
    if (present(x3_distribution))dataset_4d%x3_distribution=x3_distribution
    if (present(x4_distribution))dataset_4d%x4_distribution=x4_distribution

    if (present(s_axis))call IXFcopy(s_axis,dataset_4d%s_axis,status)
    if (present(x1_axis))call IXFcopy(x1_axis,dataset_4d%x1_axis,status)
    if (present(x2_axis))call IXFcopy(x2_axis,dataset_4d%x2_axis,status)
    if (present(x3_axis))call IXFcopy(x3_axis,dataset_4d%x3_axis,status)
    if (present(x4_axis))call IXFcopy(x4_axis,dataset_4d%x4_axis,status)

    call IXFset_real_array(dataset_4d%x1,status,x1)
    call IXFset_real_array(dataset_4d%x2,status,x2)
    call IXFset_real_array(dataset_4d%x3,status,x3)
    call IXFset_real_array(dataset_4d%x4,status,x4)

    if (present(signal))then
       call IXFreallocdims(dataset_4d%signal,shape(signal),.false.,status)
       dataset_4d%signal=signal
    endif

    if (present(error))then       
       call IXFreallocdims(dataset_4d%error,shape(error),.false.,status)
       dataset_4d%error=error
    endif

    call IXFcheck_dataset_4d(dataset_4d,status)

  end subroutine IXFset_dataset_4d

  subroutine IXFget_dataset_4d(w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status !! error status object
    type(IXTdataset_4d),intent(in) :: w1 !! dataset_3d to be checked

  end subroutine IXFget_dataset_4d

end module IXMdataset_4d

