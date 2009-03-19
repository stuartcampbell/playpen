!------------------------------
! MODULE: IXMdataset_3d
!------------------------------
!! @author Dickon Champion, ISIS
!! @version $Revision: 1344 $ ($Date: 2008-04-22 05:53:04 -0400 (Tue, 22 Apr 2008) $)
!!
!! FORTRAN definition of IXMdataset_3d object 
module IXMdataset_3d
  use IXMdataset_common
  use IXMdataset_2d  
  implicit none
  public :: IXTdataset_3d
  type IXTdataset_3d
     private
     type(IXTbase) :: base
     character(len=long_len),allocatable ::  title(:) !! Title of dataset for plotting purposes
     real(dp), pointer :: signal(:,:,:)=>NULL()
     real(dp), pointer:: error(:,:,:)=>NULL()
     type(IXTaxis):: s_axis !! Signal axis title (e.g. 'Counts')
     real(dp), pointer :: x(:)=>NULL()  !! x-axis (either histogram boundaries or point data)
     type(IXTaxis):: x_axis !! axis along x-axis e.g. 'meV' or 'microseconds' [NeXus: axis attribute]
     !! x-distribution
     !! - If X_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If X_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: x_distribution=.false.
     real(dp), pointer :: y(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: y_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: y_distribution=.false.
     real(dp), pointer :: z(:)=>NULL() !! y-axis (either histogram boundaries or point data)
     type(IXTaxis):: z_axis
     !! y-distribution
     !! - If Y_DISTRIBUTION=.TRUE. then the signal S is a distribution along the x-axis
     !! - If Y_DISTRIBUTION=.FALSE.then the signal is not a distribution
     logical :: z_distribution=.false.
  end type IXTdataset_3d

#define IXD_TYPE	dataset_3d
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTdataset_3d class"
#define IXD_TYPE dataset_3d
#define IXD_SQTYPE 'dataset_3d'
#include "class_base.f90"

  recursive subroutine IXFoperation_run_dataset_3d(op, field, arg, status)
    implicit none
    type(IXTdataset_3d) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTdataset_3d', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op,'base', arg%base, status)
    call IXFoperation_run_alloc(op,'title', arg%title, status)
    call IXFoperation_run_ptr(op,'signal', arg%signal, status)
    call IXFoperation_run_ptr(op,'error',arg%error,status)
    call IXFoperation_run(op,'s_axis', arg%s_axis, status)
    call IXFoperation_run_ptr(op,'x', arg%x, status)
    call IXFoperation_run(op,'x_axis', arg%x_axis, status)
    call IXFoperation_run(op,'x_distribution', arg%x_distribution, status)
    call IXFoperation_run_ptr(op,'y', arg%y, status)
    call IXFoperation_run(op,'y_axis', arg%y_axis, status)
    call IXFoperation_run(op,'y_distribution', arg%y_distribution, status)
    call IXFoperation_run_ptr(op,'z', arg%z, status)
    call IXFoperation_run(op,'z_axis', arg%z_axis, status)
    call IXFoperation_run(op,'z_distribution', arg%z_distribution, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_dataset_3d

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_dataset_3d(w3, status)
    implicit none
    type(IXTstatus),intent(inout) :: status !! error status object
    type(IXTdataset_3d),intent(in) :: w3 !! dataset_3d to be checked
    integer(i4b)::sxflag,syflag,szflag,xflag,yflag,zflag
    call IXFcheck(w3%x_axis,status)
    call IXFcheck(w3%s_axis,status)
    call IXFcheck(w3%y_axis,status)

    if( .not. associated(w3%y))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_3d%y must be associated with an array (IXFcheck_dataset_3d)')
    endif
    if( .not. associated(w3%x))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_3d%x must be associated with an array (IXFcheck_dataset_3d)')
    endif
    if( .not. associated(w3%z))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_3d%z must be associated with an array (IXFcheck_dataset_3d)')
    endif
 
    if( .not. associated(w3%signal))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_2d%signal must be associated with an array (IXFcheck_dataset_3d)')
    endif
    if( .not. associated(w3%error))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_3d%error must be associated with an array (IXFcheck_dataset_3d)')
    endif

    if( .not. allocated(w3%title))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'dataset_3d%title array must be allocated (IXFcheck_dataset_3d)')
    endif


    if(status == IXCseverity_error)return
    sxflag=size(w3%signal,1)
    syflag=size(w3%signal,2)
    szflag=size(w3%signal,3)        
    xflag=size(w3%x)
    yflag=size(w3%y)
    zflag=size(w3%z)

    !check for empty array
    if(size(w3%signal) < 1) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'must have at least one element in signal array  (IXFcheck_dataset_3d)')
    endif

    !check no. of error == no. of signal in 2D respect
    if (sum(abs(shape(w3%signal) - shape(w3%error)))/=0  )then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'signal and error data incompatible  (IXFcheck_dataset_3d) ')
    endif

    ! check it is either point or histogram data
    if(xflag /= sxflag)then
       if(xflag /= sxflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined along x-axis (IXFcheck_dataset_3d)')
       endif
    endif

    if(yflag /= syflag)then
       if(yflag /= syflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined along y-axis (IXFcheck_dataset_3d)')
       endif
    endif
    if(zflag /= szflag)then
       if(zflag /= szflag+1)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'neither histogram or point data defined along z-axis (IXFcheck_dataset_3d)')
       endif
    endif
    
  end subroutine IXFcheck_dataset_3d

  subroutine IXFdestroy_dataset_3d(w3d,status)
    type(IXTdataset_3d)::w3d
    type(IXTstatus)::status

    call IXFdestroy(w3d%base,status)
    call IXFdealloc(w3d%x,status)
    call IXFdealloc(w3d%y,status)
    call IXFdealloc(w3d%z,status)
    call IXFdealloc(w3d%signal,status)
    call IXFdealloc(w3d%error,status)
    !w3d%title='title'
    deallocate(w3d%title)
    call IXFdestroy(w3d%s_axis,status)
    call IXFdestroy(w3d%x_axis,status)
    call IXFdestroy(w3d%y_axis,status)
    call IXFdestroy(w3d%z_axis,status)
    w3d%x_distribution=.false.
    w3d%y_distribution=.false.
    w3d%z_distribution=.false.
    call IXFclear_valid(w3d)

  end subroutine IXFdestroy_dataset_3d
  
  subroutine IXFcreate_dataset_3d(dataset_3d,title,signal,error,s_axis,x,x_axis,  &
       x_distribution,y,y_axis,y_distribution,z,z_axis,z_distribution,status)
    implicit none
    type(IXTdataset_3d),intent(out)::dataset_3d  !! the dataset_1d object to be created
    character(len=*),intent(in) :: title(:) !!input: title passed
    real(dp),intent(in) :: signal(:,:,:)!!input: signal ARRAY passed
    real(dp),intent(in):: error(:,:,:) !!input: error ARRAY passed
    real(dp),intent(in) :: x(:),y(:),z(:) !!input: x/y ARRAY passed
    type(IXTaxis),intent(in) :: s_axis,x_axis,y_axis,z_axis	 !!input: x/y_axis passed
    logical,intent(in) :: x_distribution,y_distribution,z_distribution    !!input: distribution flag 
    type(IXTstatus),intent(inout)::status !! error status object

    ! nested objects should be tested for initialisation, this shows they have been created properly

    if( IXFvalid(s_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_3d)')
    endif

    if( IXFvalid(x_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_3d)')
    endif

    if( IXFvalid(y_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_3d)')
    endif

    if( IXFvalid(z_axis) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTaxis failure, all nested objects MUST be initialised (IXFset_dataset_3d)')
    endif

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(dataset_3d)

    call IXFset_dataset_3d(dataset_3d,status,title,signal,error,s_axis,x,x_axis,  &
            x_distribution,y,y_axis,y_distribution,z,z_axis,z_distribution)

  end subroutine IXFcreate_dataset_3d
  
  recursive subroutine IXFset_dataset_3d(dataset_3d,status,title,signal,error,s_axis,  &
       x,x_axis,x_distribution,y,y_axis,y_distribution,z,z_axis,z_distribution,ref)
    implicit none
    type(IXTdataset_3d),intent(inout)::dataset_3d
    type(IXTdataset_3d),intent(in),optional::ref  !! reference dataset_3d
    character(len=*),optional,intent(in) :: title(:)	
    real(dp),optional,intent(in) :: signal(:,:,:), error(:,:,:) 

    real(dp), optional,intent(in) :: x(:),y(:),z(:)
    type(IXTaxis),optional,intent(in) :: s_axis,x_axis,y_axis,z_axis	

    logical,optional,intent(in) :: x_distribution, y_distribution,z_distribution
    type(IXTstatus),intent(inout)::status


    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_dataset_3d)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(dataset_3d)
    else    
       if(IXFvalid(dataset_3d) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_dataset_3d)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))call IXFset_dataset_3d(dataset_3d,status,ref%title,ref%signal,ref%error, &
         ref%s_axis,ref%x,ref%x_axis,ref%x_distribution,          &
         ref%y,ref%y_axis,ref%y_distribution,ref%z,ref%z_axis,ref%z_distribution)


    if (present(title))then
       call IXFreallocFortran(dataset_3d%title,size(title),.false.,status)       
       dataset_3d%title=title       
    endif

    if (present(x_distribution))dataset_3d%x_distribution=x_distribution

    if (present(y_distribution))dataset_3d%y_distribution=y_distribution
    if (present(z_distribution))dataset_3d%z_distribution=z_distribution

    if (present(s_axis))call IXFcopy(s_axis,dataset_3d%s_axis,status)
    if (present(x_axis))call IXFcopy(x_axis,dataset_3d%x_axis,status)
    if (present(y_axis))call IXFcopy(y_axis,dataset_3d%y_axis,status)
    if (present(z_axis))call IXFcopy(z_axis,dataset_3d%z_axis,status)


    call IXFset_real_array(dataset_3d%x,status,x)
    call IXFset_real_array(dataset_3d%y,status,y)   
    call IXFset_real_array(dataset_3d%z,status,z)   

    call IXFset_real_array(dataset_3d%signal,status,signal)   
    call IXFset_real_array(dataset_3d%error,status,error)

    call IXFcheck_dataset_3d(dataset_3d,status)

  end subroutine IXFset_dataset_3d

  subroutine IXFget_dataset_3d(w1, status)
    implicit none
    type(IXTstatus),intent(inout) :: status !! error status object
    type(IXTdataset_3d),intent(in) :: w1 !! dataset_3d to be checked

  end subroutine IXFget_dataset_3d


end module IXMdataset_3d
