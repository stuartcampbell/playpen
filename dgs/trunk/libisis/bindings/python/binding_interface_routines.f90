! IXD_NAME IXD_TYPE IXD_DIMS IXD_MTYPE

#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) && defined(IXD_MTYPE)

subroutine IXBgetFromBindingPtr&
                                IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr, marray, array_data
	integer, allocatable :: array_dims(:)
	integer array_index, op_count, ndims, ndims_matlab
    character(len=*), intent(in) :: field
    IXD_TYPE , pointer :: value( IXD_DIMS )
	if (matlab_ptr == 0) return
 	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    ndims = size(shape(value))
    if (marray .ne. 0) then
      array_data = IXBGetPr(marray)
	  ndims_matlab = IXBGetNumberOfDimensions(marray)
	  allocate(array_dims(ndims_matlab))
	  call IXBCopyPtrToInteger4(IXBGetDimensions(marray),array_dims,ndims_matlab)
	  if (ndims == 1) then
          call associate_array(value, array_data, array_dims(ndims_matlab:ndims_matlab), 1)
	  else
          call associate_array(value, array_data, array_dims, ndims_matlab)
	  endif
	  deallocate(array_dims)
      call push_memory(value, marray, .false.)
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
    endif
end subroutine

! We do not call push memory as the ares are not associated
subroutine IXBgetFromBindingAlloc&
                                  IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr, marray, array_data
	integer, allocatable :: array_dims(:)
	integer array_index, op_count, ndims, ndims_matlab
    character(len=*), intent(in) :: field
    IXD_TYPE , allocatable :: value( IXD_DIMS )
	if (matlab_ptr == 0) return
 	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    ndims = size(shape(value))
    if (marray .ne. 0) then
      array_data = IXBGetPr(marray)
	  ndims_matlab = IXBGetNumberOfDimensions(marray)
	  allocate(array_dims(ndims_matlab))
	  call IXBCopyPtrToInteger4(IXBGetDimensions(marray),array_dims,ndims_matlab)
	  if (ndims == 1) then
		  call IXFAllocdimsFortran(value, array_dims(ndims_matlab:ndims_matlab), status)
	  else
		  call IXFAllocdimsFortran(value, array_dims, status)
	  endif
	  call copyFromVector&
	                      IXD_NAME (%val(array_data), value, size(value), status)
	  deallocate(array_dims)
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
    endif
end subroutine

! We do not call push memory as the ares are not associated
subroutine IXBgetFromBinding&
                             IXD_NAME (matlab_ptr, field, array_index, op_count, value, status);
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr, marray, array_data
	integer, allocatable :: array_dims(:)
	integer array_index, op_count, ndims, ndims_matlab
    character(len=*), intent(in) :: field
    IXD_TYPE :: value( IXD_DIMS )
	if (matlab_ptr == 0) return
 	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    ndims = size(shape(value))
    if (marray .ne. 0) then
      array_data = IXBGetPr(marray)
	  ndims_matlab = IXBGetNumberOfDimensions(marray)
	  allocate(array_dims(ndims_matlab))
	  call IXBCopyPtrToInteger4(IXBGetDimensions(marray),array_dims,ndims_matlab)
	  call copyFromVector&
	                      IXD_NAME (%val(array_data), value, size(value), status)
	  deallocate(array_dims)
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
    endif
end subroutine

subroutine IXBsendToBinding&
                            IXD_NAME (matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
    integer(cpointer_t), intent(in) :: matlab_ptr
	type(IXTstatus) :: status
	integer(cpointer_t) :: marray, array_data
	integer(cpointer_t) :: value_ptr
	integer,allocatable :: array_dims(:)
	integer ndims, array_index, op_count
    character(len=*),intent(in) :: field
    IXD_TYPE :: value( IXD_DIMS )
	if (size(value) == 0) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'zero size field')
      return
	endif
	value_ptr = find_memory(value,.true.,.false.)
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
		value_ptr = IXBCreateNumericArray(ndims,array_dims,IXBClassIDFromClassName( IXD_MTYPE ),0)
		array_data = IXBGetPr(value_ptr)
		call copyToVector&
		                  IXD_NAME (value, %val(array_data), size(value), status)
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
!			if (size(value) > 1) then; \
!			    call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field created') ; \
!			endif; \
!
!
!
!		if (associated(value)) then; \
!		else; \
!			ndims = 2; \
!			allocate(array_dims(ndims)); \
!			array_dims(1) = 1; \
!			array_dims(2) = 1; \
!			value_ptr = mxCreateNumericArray(ndims,array_dims,mxClassIDFromClassName(__mtype),0); \
!			array_data = mxGetPr(value_ptr); \
!			call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'undefined array field created') ; \
!		endif; \
!
