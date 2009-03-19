module IXMm_path

use IXMoperation

#define IXD_TYPE path
#include "bindings_header.f90"

contains
!***
#define IXD_NO_BASE 1
!***

#define IXD_TYPE path
#include "bindings_base.f90"

end module IXMm_path

#define IXD_TYPE path
#include "bindings_extra.f90"

subroutine IXBaddpath_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTpath),allocatable :: paths(:)
  character(len=long_len),allocatable :: directory(:)
  character(len=long_len)::name
  integer(i4b)::i
  
  call IXBgetFromBinding(prhs(2),' ', 1, 0, name, status)
  allocate(directory(nrhs-2))
  do i=3, nrhs
    call IXBgetFromBinding(prhs(i),' ', 1, 0, directory(i-2), status)
  enddo    
  if (status == IXCseverity_error) return
  call IXFaddpath(name,directory,status)
  deallocate(directory)
  call IXFcopygpath(paths,status)
!  if (status == IXCseverity_error) return  
  call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, paths, status)
end subroutine IXBaddpath_path

subroutine IXBaddtoend_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTpath),allocatable :: paths(:)
  character(len=long_len) ,allocatable :: directory(:)
  character(len=long_len)::name
  integer(i4b)::i
  call IXBgetFromBinding(prhs(2),' ', 1, 0, name, status)
  allocate(directory(nrhs-2))
  do i=3, nrhs
    call IXBgetFromBinding(prhs(i),' ', 1, 0, directory(i-2), status)
  enddo    
  if (status == IXCseverity_error) return
  call IXFaddtoend(name,directory,status)
  deallocate(directory)
  call IXFcopygpath(paths,status)
!  if (status == IXCseverity_error) return  
  call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, paths, status)
end subroutine IXBaddtoend_path

subroutine IXBaddtobeg_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTpath),allocatable :: paths(:)
  character(len=long_len) ,allocatable :: directory(:)
  character(len=long_len)::name
  integer(i4b)::i
  call IXBgetFromBinding(prhs(2),' ', 1, 0, name, status)
  allocate(directory(nrhs-2))
  do i=3, nrhs
    call IXBgetFromBinding(prhs(i),' ', 1, 0, directory(i-2), status)
  enddo    
  if (status == IXCseverity_error) return
  call IXFaddtobeg(name,directory,status)
  deallocate(directory)
  call IXFcopygpath(paths,status) 
!  if (status == IXCseverity_error) return  
  call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, paths, status)
end subroutine IXBaddtobeg_path

subroutine IXBdelpath_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  character(len=long_len)::name
  type(IXTpath),allocatable :: paths(:)
  
  call IXBgetFromBinding(prhs(2),' ', 1, 0, name, status)
  if (status == IXCseverity_error) return
  call IXFdelpath(name,status)
  call IXFcopygpath(paths,status)
!  if (status == IXCseverity_error) return  
  call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, paths, status)
end subroutine IXBdelpath_path

subroutine IXBshowpath_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  character(len=long_len)::name
  if(nrhs == 1)then
    call IXBgetFromBinding(prhs(1),' ', 1, 0, name, status)
  else
    name=''
  endif
  if (status == IXCseverity_error) return
  call IXFshowpath(name,status)
end subroutine IXBshowpath_path


subroutine IXBdeldir_path(nlhs, plhs, nrhs, prhs, status)
  use IXMm_path
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTstatus) :: status
  type(IXTpath),allocatable :: paths(:)
  character(len=long_len),allocatable :: directory(:)
  character(len=long_len)::name
  integer(i4b)::i

  call IXBgetFromBinding(prhs(2),' ', 1, 0, name, status)
  allocate(directory(nrhs-2))
  do i=3, nrhs
    call IXBgetFromBinding(prhs(i),' ', 1, 0, directory(i-2), status)
  enddo    
  if (status == IXCseverity_error) return
  call IXFdeldir(name,directory,status)
  call IXFcopygpath(paths,status)
!  if (status == IXCseverity_error) return  
  call IXBsendToBinding(plhs(1),prhs(1), ' ', 1, 0, paths, status)
end subroutine IXBdeldir_path