#if defined(IXD_F1NAME) 

subroutine IXBdataset_1d&/**/
              &IXD_F1NAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_dataset_2d
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d), allocatable :: w2d(:),wres(:)
  type(IXTdataset_1d),allocatable:: w1d(:)
  type(IXTstatus) :: s
  integer :: i, n

    n = ixGetNumberOfElements(prhs(2))
    i = ixGetNumberOfElements(prhs(3))
    if (n /= i) then
        call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
                IXCerr_outofmem, &
            'binary_ops: arrays must be same length') 
        return
    endif
    allocate(w1d(n),w2d(n),wres(n))
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w2d, s)
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w1d, s)
    
    if (s == IXCseverity_error) return
    do i=1,n
        call IXFdataset_1d&/**/
                &IXD_F1NAME (wres(i),w2d(i),w1d(i),s)
    enddo
  
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
  deallocate(w1d,w2d,wres)
end subroutine
#undef IXD_F1NAME
#endif /* defined(IXD_F1NAME) */

#if defined(IXD_F2NAME)

subroutine IXBarraydataset_1d_&/**/
              &IXD_F2NAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_dataset_2d
  use IXMm_dataset_1d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTdataset_2d) :: w2d
  type(IXTdataset_1d),allocatable:: w1d(:)
  type(IXTstatus) :: s
  integer :: i
  i = ixGetNumberOfElements(prhs(3))
  allocate(w1d(i))
  if(ixIsClass(prhs(3), 'IXTdataset_1d') .ne. 0)then    
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w2d, s)
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w1d, s)     
    if (s == IXCseverity_error) return
      call IXF&/**/
              &IXD_F2NAME (w2d,w1d,s)
  elseif(ixIsClass(prhs(2), 'IXTdataset_1d') .ne. 0)then
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2d, s)
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1d, s)     
    if (s == IXCseverity_error) return
      call IXF&/**/
              &IXD_F2NAME (w1d,w2d,s)
  endif  
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, w2d, s)
  endif
  deallocate(w1d)
end subroutine

#undef IXD_F2NAME
#endif /* defined(IXD_F2NAME) */
