
! IXD_NAME IXD_TYPE IXD_DIMS IXD_PREFIX IXD_QUALIFIER IXD_UNDEF

#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) && defined(IXD_PREFIX) && defined(IXD_QUALIFIER) && defined(IXD_CHECK) && defined(IXD_UNDEF)

subroutine runOperation&/**/
                       &IXD_NAME (op, name, value, status)
   implicit none
   type(IXToperation) :: op
   character(len=*) :: name
   IXD_TYPE , IXD_QUALIFIER :: value ( IXD_DIMS )
   type(IXTstatus) :: status
!   character(len=1320) :: buffer
   op%count(op%level) = op%count(op%level) + 1
   if ( associated(op%display) .and. (op%level <= 1)) then
       if ( IXD_CHECK(value) ) then
	       call IXFoperationPrint(name,value,op%level,status)
       else
	       call IXFwrite_line_indent(' '//name//' = <undefined>', op%level, &
                                     '-', status)
       endif
   endif
   if (associated(op%matlabread)) then
       call IXBgetFromBinding&/**/
                             &IXD_PREFIX (op%matlabread%prhs(op%level), name, op%array_index(op%level), &
                                          op%count(op%level), value, status)
   endif
   if (associated(op%matlabwrite)) then
     if (IXD_CHECK(value)) then
       call IXBcreateBindingPLHS(op%matlabwrite%plhs(op%level), op%matlabwrite%prhs, status)
       call IXBsendToBinding(op%matlabwrite%plhs(op%level), name, op%array_index(op%level), &
                             op%count(op%level), value, status)
     else
       call IXFadd_status(IXCfacility_libisis, IXCseverity_info, &
        IXCerr_unknown, 'matlabwrite: Field '//trim(name)//' is NULL; ignoring')
     endif
   endif
   if (associated(op%get) .and. IXD_CHECK(value)) then
		if (name == op%get%field) then
		    call IXFwrap_var(value, op%get%var, status)
        endif
   endif
   if (associated(op%set)) then
		if (name == op%set%field) then
		    call IXFunwrap_var&/**/
		                      &IXD_PREFIX (op%set%var, value, status)
        endif
   endif
   if (associated(op%filewrite)) then
       if (IXD_CHECK(value)) then
           call IXBfileWrite(op%filewrite%fio, name, value, status)
       else
           call IXFadd_status(IXCfacility_libisis, IXCseverity_info, &
         IXCerr_unknown, 'Filewrite: Field '//trim(name)//' is NULL; ignoring')
       endif
    endif
    if (associated(op%fileread)) then
       call IXBfileRead&/**/
                       &IXD_PREFIX (op%fileread%fio, name, value, status)
    endif
    if (associated(op%init)) then
        IXD_INITIALISE(value)
    endif
end subroutine 

#undef IXD_NAME
#undef IXD_TYPE
#undef IXD_DIMS
#undef IXD_PREFIX
#undef IXD_QUALIFIER
#undef IXD_CHECK
#undef IXD_UNDEF
#undef IXD_INITIALISE

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) && defined(IXD_PREFIX) && defined(IXD_QUALIFIER) */
