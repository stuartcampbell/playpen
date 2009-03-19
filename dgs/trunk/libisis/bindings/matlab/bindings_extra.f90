#if defined(IXD_TYPE)

subroutine IXBdisplay_&/**/
                      &IXD_TYPE (nlhs,plhs,nrhs,prhs,s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs, nrhs
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  integer:: n
  n = ixGetNumberOfElements(prhs(1))
  if (n < 1) then
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, '0 size array (IXBdisplay)')
      return
  endif
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s /= IXCseverity_error) then
      call IXFdisplay(w1,s)
  endif
  deallocate(w1)
end subroutine

!! return 0 if all OK, 1 if an error

subroutine IXBcheck_&/**/
                    &IXD_TYPE (nlhs,plhs,nrhs,prhs,s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs, nrhs, return_stat
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  integer :: n
  n = ixGetNumberOfElements(prhs(1))
  if (n < 1) then
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, '0 size array (IXBcheck)')
      return
  endif
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s /= IXCseverity_error) then
      call IXFcheck(w1,s)
  endif
  if (s == IXCseverity_error) then
      return_stat = 1
  else
      return_stat = 0
  endif
  deallocate(w1)
  if (nlhs > 0) call IXBsendToBinding(plhs(1), ' ', 1, 0, return_stat, s)
end subroutine

subroutine IXBcreate_&/**/
                     &IXD_TYPE (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  integer :: n
  n = ixGetNumberOfElements(prhs(1))
  if (n < 1) then
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, '0 size array (IXBcreate)')
      return
  endif
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s == IXCseverity_error) then
      deallocate(w1)
      return
  endif
! potential solution to synchronise matlab and fortran defaults
!  call IXFinitialise(w1,s)
!  if (s == IXCseverity_error) return
  call IXBgetFromBinding(prhs(2),' ',1,0,w1,s)
  if (s == IXCseverity_error) then
      deallocate(w1)
      return
  endif
  call IXFcheck_and_valid(w1,s)
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, s)
  endif
  deallocate(w1)
end subroutine

subroutine IXBread_&/**/
                   &IXD_TYPE (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  use IXMm_fileio
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  type(IXTfileio) :: fio
  integer:: n
  character(len=256) :: path
  n = ixGetNumberOfElements(prhs(1))
  if (n < 1) then
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, '0 size array (IXBread)')
      return
  endif
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s == IXCseverity_error) then
      deallocate(w1)
      return
  endif
  path = ' '
  call IXBgetFromBinding(prhs(2),' ',1,0,fio,s)
  call IXBgetFromBinding(prhs(3),' ',1,0,path,s)
  call IXFfile_read(w1(1),fio,path,s)
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, s)
  endif
  deallocate(w1)
end subroutine

subroutine IXBwrite_&/**/
                    &IXD_TYPE (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  use IXMm_fileio
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: w1(:)
  type(IXTstatus) :: s
  type(IXTfileio) :: fio
  integer:: n
  character(len=256) :: path
  n = ixGetNumberOfElements(prhs(1))
  if (n < 1) then
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, '0 size array (IXBwrite)')
      return
  endif
  allocate(w1(n))
  call IXBgetFromBinding(prhs(1),' ',1,0,w1,s)
  if (s == IXCseverity_error) then
      deallocate(w1) 
      return
  endif
  path = ' '
  call IXBgetFromBinding(prhs(2),' ',1,0,fio,s)
  call IXBgetFromBinding(prhs(3),' ',1,0,path,s)
  call IXFfile_write(w1(1),fio,path,s)
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w1, s)
  endif
  deallocate(w1)
end subroutine

#undef IXD_TYPE

#endif /* IXD_TYPE */
