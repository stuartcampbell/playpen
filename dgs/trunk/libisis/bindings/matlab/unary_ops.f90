
! IXD_TYPE
! IXD_NAME

#if defined(IXD_TYPE) && defined(IXD_NAME)

subroutine IXB&/**/
              &IXD_NAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE ), allocatable :: wres(:), w1(:)
  type(IXTstatus) :: s
  integer n, i
  n = ixGetNumberOfElements(prhs(2))
  allocate(w1(n),wres(n))
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, s)
  if (s == IXCseverity_error) return
  do i = 1,n
      call IXF&/**/
              &IXD_NAME (wres(i),w1(i),s)
  enddo
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
  deallocate(w1,wres)
end subroutine

#undef IXD_NAME
#undef IXD_TYPE

#endif /* defined(IXD_TYPE) && defined(IXD_NAME) */

