!
! This module defines the getFromBinding, getFromBindingPtr, getFromBindingAlloc and sendToBinding
! interfaces used during the operationRun interface
!

! required: IXD_NAME IXD_TYPE 
! optional: IXD_DIMS

#if defined(IXD_NAME) && defined(IXD_TYPE)

subroutine IXBsendToBinding&/**/
                           &IXD_NAME (external_ptr, field, array_index, op_count, value, status)
    use IXMtype_definitions
    use IXMstatus
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: external_ptr
    character(len=*) :: field
#ifdef IXD_DIMS
      IXD_TYPE :: value( IXD_DIMS )
#else
      IXD_TYPE :: value
#endif
	integer :: array_index, op_count
end subroutine

#undef IXD_NAME
#undef IXD_TYPE
#undef IXD_DIMS 

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) */
