#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS)

	subroutine wrap_&/**/
	                 &IXD_NAME (var, wrapped_var, status)
		implicit none
		IXD_TYPE , target, intent(in) :: var( IXD_DIMS )
		type(IXTwrapped_var) :: wrapped_var
		type(IXTstatus) :: status
		wrapped_var%vartype = IXCvartype_&/**/
		                                 &IXD_NAME
		wrapped_var%&/**/
		            &IXD_NAME => var
	end subroutine
   
	function f_wrap_&/**/
	                     &IXD_NAME (var) result(wrapped_var)
		implicit none
		IXD_TYPE , target, intent(in) :: var( IXD_DIMS )
		type(IXTwrapped_var) :: wrapped_var
		wrapped_var%vartype = IXCvartype_&/**/
		                                 &IXD_NAME
		wrapped_var%&/**/
		            &IXD_NAME => var
	end function

	subroutine unwrap_&/**/
	                  &IXD_NAME (wrapped_var, var, status)
		implicit none
		IXD_TYPE :: var( IXD_DIMS )
		type(IXTwrapped_var) :: wrapped_var
		type(IXTstatus) :: status
		if (wrapped_var%vartype == IXCvartype_&/**/
		                                      &IXD_NAME ) then
			var = wrapped_var%&/**/
			                  &IXD_NAME
		else
		    call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
		                       IXCerr_invparam, 'unwrap array from invalid type and/or shape')
		endif
	end subroutine

	subroutine unwrap_alloc_&/**/
	                  &IXD_NAME (wrapped_var, var, status)
		implicit none
		IXD_TYPE , allocatable :: var( IXD_DIMS )
		type(IXTwrapped_var) :: wrapped_var
		type(IXTstatus) :: status
		if (wrapped_var%vartype == IXCvartype_&/**/
		                                      &IXD_NAME ) then
			if (allocated(var)) deallocate(var)
			call IXFallocdimsFortran(var, shape(wrapped_var%&
			                                            &IXD_NAME ), status)
			var = wrapped_var%&/**/
			                  &IXD_NAME
		else
		    call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
		                       IXCerr_invparam, 'unwrap array from invalid type and/or shape')
		endif
	end subroutine

	subroutine unwrap_ptr_&/**/
	                  &IXD_NAME (wrapped_var, var, status)
		implicit none
		IXD_TYPE , pointer:: var( IXD_DIMS )
		type(IXTwrapped_var) :: wrapped_var
		type(IXTstatus) :: status
		if (wrapped_var%vartype == IXCvartype_&/**/
		                                      &IXD_NAME ) then
			var => wrapped_var%&/**/
			                  &IXD_NAME
		else
		    call IXFadd_status(status, IXCfacility_wrapvar,	IXCseverity_error, &
		                       IXCerr_invparam, 'unwrap array from invalid type and/or shape')
		endif
	end subroutine

#undef IXD_NAME
#undef IXD_TYPE
#undef IXD_DIMS

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) */
