! IXD_NAME IXD_TYPE IXD_DIMS IXD_MTYPE

#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) && defined(IXD_MTYPE)

subroutine IXBgetFromBindingPtr&/**/
                               &IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingPtr&/**/
                                                              &IXD_NAME
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray, array_data
  integer, allocatable :: array_dims(:)
  integer array_index, op_count, ndims, ndims_matlab
  character(len=*), intent(in) :: field
  logical :: read_only
  IXD_TYPE , pointer :: value( IXD_DIMS )
  read_only = .false.
  if (matlab_ptr == 0) then
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'getFromBindingPtr: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  ndims = size(shape(value))
  if (marray .ne. 0) then
     if (ixIsClass(marray, IXD_MTYPE ) /= 0) then
        if (.not. read_only) marray = ixDuplicateArray(marray)
        array_data = ixGetData(marray)
        ndims_matlab = ixGetNumberOfDimensions(marray)
        allocate(array_dims(ndims_matlab))
        call ixGetDimensions(marray,array_dims,ndims_matlab)
        if (ndims == 1 .and. ndims_matlab==2 ) then ! matlab arrays are always 2d (matrices)
           call associate_array(value, array_data, array_dims(ndims_matlab:ndims_matlab), 1)
        else
          if(ndims==ndims_matlab)then
            call associate_array(value, array_data, array_dims, ndims_matlab)
          else
            call IXFadd_status(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, 'matlab/fortran array shape mismatch for '//field)      
            deallocate(array_dims)
            return
          endif
        endif
        deallocate(array_dims)
        call push_memory(value, marray, .not. read_only)
     endif
  else if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
  endif
end subroutine

! We do not call push memory as the ares are not associated
subroutine IXBgetFromBindingAlloc&/**/
                                 &IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingAlloc&/**/
                                                                &IXD_NAME
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray, array_data
  integer, allocatable :: array_dims(:)
  integer array_index, op_count, ndims, ndims_matlab
  character(len=*), intent(in) :: field
  IXD_TYPE , allocatable :: value( IXD_DIMS )
  if (matlab_ptr == 0) then
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'getFromBindingAlloc: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  ndims = size(shape(value))
  if (marray .ne. 0) then
     if (ixIsClass(marray, IXD_MTYPE ) /= 0) then
         array_data = ixGetData(marray)
         ndims_matlab = ixGetNumberOfDimensions(marray)
         allocate(array_dims(ndims_matlab))
         call ixGetDimensions(marray,array_dims,ndims_matlab)
         if (ndims == 1) then
            call IXFAllocdimsFortran(value, array_dims(ndims_matlab:ndims_matlab), status)
         else
            call IXFAllocdimsFortran(value, array_dims, status)
         endif
         call copyFromVectorC&/**/
                             &IXD_NAME (array_data, value, size(value), status)
         deallocate(array_dims)
     endif
  else  if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
  endif
end subroutine

! We do not call push memory as the ares are not associated
subroutine IXBgetFromBinding&/**/
                            &IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBinding&/**/
                                                           &IXD_NAME
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray, array_data
  integer, allocatable :: array_dims(:)
  integer array_index, op_count, ndims, ndims_matlab
  character(len=*), intent(in) :: field
  IXD_TYPE :: value( IXD_DIMS )
  if (matlab_ptr == 0) then 
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, & 
!                        IXCerr_outofmem, 'getFromBinding: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  ndims = size(shape(value))
  if (marray .ne. 0) then
     if (ixIsClass(marray, IXD_MTYPE ) /= 0) then
        array_data = ixGetData(marray)
        ndims_matlab = ixGetNumberOfDimensions(marray)
        allocate(array_dims(ndims_matlab))
        call ixGetDimensions(marray,array_dims,ndims_matlab)
        call copyFromVectorC&/**/
                            &IXD_NAME (array_data, value, size(value), status)
        deallocate(array_dims)
     endif
  else  if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
  endif
end subroutine

subroutine IXBsendToBinding&/**/
                           &IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBinding&/**/
                                                          &IXD_NAME
  implicit none
  integer(cpointer_t), intent(in) :: matlab_ptr
  type(IXTstatus) :: status
  integer(cpointer_t) :: marray, array_data
  type(IXTmemory_info) :: memory_info
  integer(cpointer_t) :: value_ptr
  integer,allocatable :: array_dims(:)
  integer ndims, array_index, op_count
  character(len=*),intent(in) :: field
  IXD_TYPE :: value( IXD_DIMS )
  if (size(value) == 0) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, &
                        IXCerr_outofmem, 'sendToBinding: zero size field')
     return
  endif
  memory_info = find_memory(value,.true.,.false.)
  value_ptr = memory_info%external_ptr
  if (value_ptr .eq. 0) then
     ndims = size(shape(value))
     if (ndims == 1) then
        ndims = 2
        allocate(array_dims(ndims))
        array_dims(1) = 1
        array_dims(2) = size(value)
     else
        allocate(array_dims(ndims))
        array_dims = shape(value)
     endif
     value_ptr = ixCreateNumericArray(ndims,array_dims,ixClassIDFromClassName( IXD_MTYPE ),0)
     array_data = ixGetData(value_ptr)
     call copyToVectorC&/**/
                       &IXD_NAME (value, array_data, size(value), status)
     deallocate(array_dims)
  endif
  call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
end subroutine

#undef IXD_NAME 
#undef IXD_TYPE 
#undef IXD_DIMS
#undef IXD_MTYPE

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) && defined(IXD_MTYPE) */

!
! may want to add to above as debug later
!
!            if (size(value) > 1) then; \
!                call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field created') ; \
!            endif; \
!
!
!
!        if (associated(value)) then; \
!        else; \
!            ndims = 2; \
!            allocate(array_dims(ndims)); \
!            array_dims(1) = 1; \
!            array_dims(2) = 1; \
!            value_ptr = mxCreateNumericArray(ndims,array_dims,mxClassIDFromClassName(__mtype),0); \
!            array_data = mxGetPr(value_ptr); \
!            call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'undefined array field created') ; \
!        endif; \
!
