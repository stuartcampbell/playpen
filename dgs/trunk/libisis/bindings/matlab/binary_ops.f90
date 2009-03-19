! IXD_NAME
! IXD_TYPE
#if defined(IXD_NAME) && defined(IXD_TYPE)

subroutine IXB&/**/
              &IXD_NAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE), allocatable :: wres(:),w1(:),w2(:)
  type(IXTstatus) :: s
  integer :: i, n
  real(dp),pointer :: scalar(:)
  if (ixIsClass(prhs(2), 'double') .ne. 0) then
    call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, scalar, s)
    n = ixGetNumberOfElements(prhs(3))
    i=size(scalar)
    if (n /= i) then
        call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
                IXCerr_invparam, &
            'binary_ops: arrays must be same length') 
        return
    endif
        
    allocate(w2(n),wres(n))
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, s)
    if (s == IXCseverity_error) return
    do i = 1,n
        call IXF&/**/
                &IXD_NAME (wres(i),scalar(i),w2(i),s)
    enddo
    deallocate(w2)
  elseif (ixIsClass(prhs(3), 'double') .ne. 0) then
    call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, scalar, s)  
    n = ixGetNumberOfElements(prhs(2))
    i=size(scalar)
    if (n /= i) then
        call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
                IXCerr_invparam, &
            'binary_ops: arrays must be same length') 
        return
    endif    
    allocate(w1(n),wres(n))
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, s)
    if (s == IXCseverity_error) return
    do i = 1,n
        call IXF&/**/
                &IXD_NAME (wres(i),w1(i),scalar(i),s)
    enddo
    deallocate(w1)
  else
    n = ixGetNumberOfElements(prhs(2))
    i = ixGetNumberOfElements(prhs(3))
    if (n /= i) then
        call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
                IXCerr_invparam, &
            'binary_ops: arrays must be same length') 
        return
    endif
    allocate(w1(n),w2(n),wres(n))
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, s)
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w2, s)
    if (s == IXCseverity_error) return
    do i=1,n
        call IXF&/**/
                &IXD_NAME (wres(i),w1(i),w2(i),s)
    enddo
    deallocate(w1,w2)
  endif
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
  deallocate(wres)
end subroutine

#undef IXD_NAME
#undef IXD_TYPE

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) */

#if defined(IXD_ANAME) && defined(IXD_TYPE)

subroutine IXBarray_X_&/**/
              &IXD_ANAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE) :: w2d,wres
  type(IXTstatus) :: s
  integer :: i, n
  real(dp),pointer :: scalar(:)
  
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, scalar, s)  
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w2d, s)
  if (s == IXCseverity_error) return
  call IXFarray_X_&/**/
                &IXD_ANAME (wres,w2d,scalar,s)
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
end subroutine

subroutine IXBarray_Y_&/**/
              &IXD_ANAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE):: w2d,wres
  type(IXTstatus) :: s
  integer :: i, n
  real(dp),pointer :: scalar(:)
  
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, scalar, s)  
  call IXBgetFromBinding(prhs(2), ' ', 1, 0, w2d, s)
  if (s == IXCseverity_error) return
  call IXFarray_Y_&/**/
                &IXD_ANAME (wres,w2d,scalar,s)
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
end subroutine

#undef IXD_ANAME
#undef IXD_TYPE

#endif /* defined(IXD_ANAME) && defined(IXD_TYPE)  */

#if defined(IXD_AANAME) && defined(IXD_TYPE) && defined(IXD_DIM)
subroutine IXBarray_&/**/
              &IXD_AANAME (nlhs, plhs, nrhs, prhs, s)
  use IXMm_&/**/
           &IXD_TYPE
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXT&/**/
          &IXD_TYPE) :: w1,wres
  type(IXTstatus) :: s
  integer :: i, n
  real(dp),pointer :: scalar( IXD_DIM )
  
  if (ixIsClass(prhs(2), 'double') .ne. 0) then
    call IXBgetFromBindingPtr(prhs(2), ' ', 1, 0, scalar, s)        
    call IXBgetFromBinding(prhs(3), ' ', 1, 0, w1, s)
    if (s == IXCseverity_error) return
    call IXF&/**/
                &IXD_AANAME (wres,scalar,w1,s)
  elseif (ixIsClass(prhs(3), 'double') .ne. 0) then    
    call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0, scalar, s)  
    call IXBgetFromBinding(prhs(2), ' ', 1, 0, w1, s)
    if (s == IXCseverity_error) return
    call IXF&/**/
                &IXD_AANAME (wres,w1,scalar,s)
  endif
  if (s == IXCseverity_error) then
    plhs(1)=ixDuplicateArray(prhs(1))
  else
    call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, wres, s)
  endif
end subroutine

#undef IXD_AANAME
#undef IXD_DIM
#undef IXD_TYPE
#endif /* defined(IXD_AANAME) && defined(IXD_TYPE) && defined(IXD_DIM) */
