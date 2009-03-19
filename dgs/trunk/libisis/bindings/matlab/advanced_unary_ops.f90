#if defined (REBIN_OPERATION)
subroutine IXB&/**/
              &REBIN_OPERATION(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  use IXMm_dataset_2d
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTdataset_2d) :: wref
  type(IXTstatus) :: status
  real(dp),pointer :: rebin_params(:)
  integer(i4b)::n

  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  allocate(rf(n))
  !this function checks for the types of argument it has been passed
  ! and calls the appropriate function accordingly
  if(ixIsClass(prhs(3), 'double') .ne. 0)then
     ! scalar is second element
     call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,rebin_params, status)
     ! read runfile class
     call IXBgetFromBinding(prhs(2),' ', 1, 0, rf(1), status)
     if (status == IXCseverity_error) return
     call IXF&/**/
              &REBIN_OPERATION(rf(1),status,Xdesc=rebin_params)     
  else
     !there are no scalars so make the usual read function and the reference value
     call IXBgetFromBinding(prhs(2),' ', 1, 0, rf(1), status)
     call IXBgetFromBinding(prhs(3), ' ', 1, 0,wref, status)
     if (status == IXCseverity_error) return     
     call IXF&/**/
              &REBIN_OPERATION(rf(1),status,Xref=wref)     
  endif

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif
end subroutine 
#undef REBIN_OPERATION
#endif /* defined (REBIN_OPERATION) */


#if defined (INT_OPERATION)
subroutine IXB&/**/
              &INT_OPERATION(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTrunfile)::rf_out
  type(IXTstatus) :: status
  real(dp):: limits(2)
  integer(i4b)::n

  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXBpopulate_runfile)')
  endif
  if (status == IXCseverity_error) return
  allocate(rf(n))
  !this function checks for the types of argument it has been passed
  ! and calls the appropriate function accordingly
  
  call IXBgetFromBinding(prhs(2),' ', 1, 0, rf(1), status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0,limits, status)
     ! read runfile class

  if (status == IXCseverity_error) return

  call IXF&/**/
              &INT_OPERATION(rf_out,rf(1),limits,status)     

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf_out, status)
  endif  
end subroutine 
#undef INT_OPERATION
#endif /* defined (INT_OPERATION) */

#if defined (SINGLEVALUE_OPERATION) && defined(VALUE_TYPE)
subroutine IXB&/**/
              &SINGLEVALUE_OPERATION(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTstatus) :: status  
  integer(i4b)::n
  VALUE_TYPE :: value

  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXB_SINGLEVALUE_OPERATION )')
  endif
  if (status == IXCseverity_error) return
  allocate(rf(n))
  !this function checks for the types of argument it has been passed
  ! and calls the appropriate function accordingly
  
  call IXBgetFromBinding(prhs(2),' ', 1, 0, rf(1), status)
  call IXBgetFromBinding(prhs(3), ' ', 1, 0,value, status)
     ! read runfile class

  if (status == IXCseverity_error) return

  call IXF&/**/
              &SINGLEVALUE_OPERATION(rf(1),value,status)     

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif  
end subroutine 
#undef SINGLEVALUE_OPERATION
#undef VALUE_TYPE
#endif /*defined (SINGLEVALUE_OPERATION) && defined(VALUE_TYPE) */

#if defined (ARRAYVALUE_OPERATION) && defined(VALUE_TYPE)
subroutine IXB&/**/
              &ARRAYVALUE_OPERATION(nlhs, plhs, nrhs, prhs, status)
  use IXMm_runfile
  implicit none
  integer :: nlhs,nrhs
  integer(cpointer_t) :: plhs(nlhs),prhs(nrhs)
  type(IXTrunfile), allocatable :: rf(:)
  type(IXTstatus) :: status  
  integer(i4b)::n
  VALUE_TYPE :: value(:)

  n = ixGetNumberOfElements(prhs(2))
  if(n > 1)then
    call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
    IXCerr_invparam, 'no multiple runfiles allowed yet (IXB_ARRAYVALUE_OPERATION )')
  endif
  if (status == IXCseverity_error) return
  allocate(rf(n))
  !this function checks for the types of argument it has been passed
  ! and calls the appropriate function accordingly
  
  call IXBgetFromBinding(prhs(2),' ', 1, 0, rf(1), status)
  call IXBgetFromBindingPtr(prhs(3), ' ', 1, 0,value, status)
     ! read runfile class

  if (status == IXCseverity_error) return

  call IXF&/**/
              &ARRAYVALUE_OPERATION(rf(1),value,status)     

  if (status == IXCseverity_error) then
     plhs(1)=ixDuplicateArray(prhs(1))
  else
     call IXBsendToBinding(plhs(1), prhs(1), ' ', 1, 0, rf(1), status)
  endif  
end subroutine 
#undef ARRAYVALUE_OPERATION
#undef VALUE_TYPE
#endif /*defined (ARRAYVALUE_OPERATION) && defined(VALUE_TYPE) */
