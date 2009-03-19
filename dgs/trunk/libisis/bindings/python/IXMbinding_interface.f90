!-------------------------------------------------------------------------------
! MODULE: isisexc_matlab_interface
!-------------------------------------------------------------------------------
!! A set of routines to proved a mapping between MATLAB and FOTRAN data types
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 430 $ ($Date: 2005-09-08 12:23:18 -0400 (Thu, 08 Sep 2005) $)


module IXMbinding_interface
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory
  use IXMoperation_interfaces
  implicit none
!-------------------------------------------------------------------------------
! List Matlab mx* and mex* functions used
!-------------------------------------------------------------------------------
  integer(cpointer_t), external :: IXBGetField, IXBGetData, IXBGetFieldByNumber, &
         IXBCreateString, IXBCreateScalarDouble, IXBGetPr, &
		 IXBCreateNumericArray, IXBDuplicateObject, mexPrintf, IXBGetCell, &
		 IXBGetNumberOfDimensions, IXBGetDimensions
  integer*4, external :: IXBGetString, IXBAddField, IXBGetFieldNumber, &
		 IXBGetNumberOfElements, IXBClassIDFromClassName, IXBIsStruct, IXBIsCell, &
		 IXBIsDouble
  real*8, external :: IXBGetScalar
  external IXBSetFieldByNumber, IXBSetN !, mxDestroyArray
! DO NOT use mxCopyInteger4ToPtr etc. - they only work with sparse matrices
!-------------------------------------------------------------------------------

contains

  subroutine IXBbinding_init()
    implicit none
  end subroutine  

  subroutine IXBbinding_cleanup()
    implicit none
  end subroutine  
   
  subroutine setMatlabField(matlab_ptr, field, field_num, array_index, op_count, value_ptr, status)
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
	integer(cpointer_t) :: value_ptr
	integer(cpointer_t) :: field_ptr
	integer array_index, op_count
	integer :: field_num, f_num

! simple variable set
    if ((len_trim(field) == 0) .and. (field_num == 0) .and. (array_index == 1) .and. (op_count == 0) ) then
	    matlab_ptr = value_ptr
		return
	endif
! No LHS
	if (matlab_ptr == 0) then
	    matlab_ptr = value_ptr
	    return
	endif
    if (IXBIsStruct(matlab_ptr) == 1) then
	    if (len_trim(field) > 0) then
		    f_num = IXBGetFieldNumber(matlab_ptr, field)
		else
			f_num = field_num
		endif
	    if (f_num <= 0) then
		    f_num = IXBAddField(matlab_ptr, field)
		endif
		field_ptr = IXBGetFieldByNumber(matlab_ptr, array_index, f_num)
!		field_ptr = mxGetFieldByNumber(matlab_ptr, 1, f_num)
		if (field_ptr .ne. 0) then
!			call mxDestroyArray(field_ptr)
		endif
!	    call mxSetFieldByNumber(matlab_ptr, 1, f_num, value_ptr)
	    call IXBSetFieldByNumber(matlab_ptr, array_index, f_num, value_ptr)
	endif
    if (IXBIsCell(matlab_ptr) == 1) then
	endif
  end subroutine

  subroutine getFieldIndexFromMatlab(matlab_ptr, index, marray, status)
	implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    integer :: index	!! Structure field to extract
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
    integer :: stat
	marray = 0
    if (matlab_ptr .ne. 0) then
	    marray = IXBGetFieldByNumber(matlab_ptr, 1, index)
	else
		marray = 0
	endif
    if (marray .eq. 0) then
!	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, & 
 !                      'Field '''//field// ''' missing') 
    endif
  end subroutine

  subroutine getCellFromMatlab(matlab_ptr, index, marray, status)
	implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    integer :: index	!! Structure field to extract
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
    integer :: stat
	marray = 0
    if (matlab_ptr .ne. 0) then
!	    marray = mxGetCell(matlab_ptr, index)
!	else
!		marray = 0
	endif
    if (marray .eq. 0) then
!	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, & 
 !                      'Cell number '''//field// ''' missing') 
    endif
  end subroutine

  subroutine getFieldFromMatlab(matlab_ptr, field, field_num, array_index, op_count, marray, status)
	implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
    integer :: stat, nelem
	integer field_num, array_index, op_count
	marray = matlab_ptr ! if not cell or structure, then return object
    if (matlab_ptr == 0) return
	if (IXBIsStruct(matlab_ptr) == 1) then
        if (len_trim(field) > 0) then
!	        marray = mxGetField(matlab_ptr, 1, field)
	        marray = IXBGetField(matlab_ptr, array_index, field)
		else if (field_num > 0) then
!			marray = mxGetFieldByNumber(matlab_ptr, 1, field_num)
			marray = IXBGetFieldByNumber(matlab_ptr, array_index, field_num)
		endif
	endif
	if (IXBIsCell(matlab_ptr) == 1) then
	    nelem = IXBGetNumberOfElements(matlab_ptr)
		if ( (nelem > 0) .and. (op_count > 0) .and. (op_count <= nelem) ) then
	        marray = IXBDuplicateObject(IXBGetCell(matlab_ptr, op_count))
		else
			marray = 0
		endif
	endif
  end subroutine

  subroutine createMatlabPLHS(plhs, prhs, status)
   implicit none
   character(len=9) :: field_names(1) = (/ '__dummy__' /)
   integer(cpointer_t) :: plhs, prhs
   type(IXTstatus) :: status
   if (plhs /= 0) return	! (plhs /= 0) so nothing needs doing
   if (prhs /= 0) then
       plhs = IXBDuplicateObject(prhs)
   else
! this causes a link error on simons computer
!	   plhs = mxCreateStructArray(1,1,size(field_names),field_names)
       plhs = 0
   endif
   if (plhs == 0) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, ' ') 
   endif
  end subroutine

end module IXMbinding_interface

  subroutine IXBgetFromBindingChar(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	character(len=*) field
	integer :: field_num, array_index, op_count
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: value	!! FORTRAN variable to update
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
    integer :: stat
    value = ' '
	if (matlab_ptr == 0) return
	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    if (marray /= 0) then
        stat = IXBGetString(marray, value, len(value))
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
                       'Character field '''//field// ''' missing') 
    endif
  end subroutine

  subroutine IXBsendToBindingChar(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    character(len=*) :: value	!! FORTRAN variable to send
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
	integer(cpointer_t) :: value_ptr
	integer :: array_index, op_count, l_value
	l_value = len_trim(value)
	if (l_value == 0) l_value = 1
	value_ptr = IXBCreateString(value(1:l_value))
	call IXBSetN(value_ptr, l_value) ! for some reason l_value above is not honoured
    call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
  end subroutine

  subroutine IXBgetFromBindingLogical(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	character(len=*) field
	integer :: field_num, array_index, op_count
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    logical :: value	!! FORTRAN variable to update
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
    integer :: stat
    integer, external :: IXBgetMatlabLogical
	if (matlab_ptr == 0) return
	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    if (marray /= 0) then
		if (IXBgetMatlabLogical(marray) /= 0) then
			value = .true.
		else
			value = .false.
		endif
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
                       'Logical field '''//field// ''' missing') 
    endif
  end subroutine

  subroutine IXBsendToBindingLogical(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    logical :: value	!! FORTRAN variable to send
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
	integer(cpointer_t) :: value_ptr
	integer :: array_index, op_count
    integer(cpointer_t), external :: IXBcreateMatlabLogical
	if (value == .true.) then
		value_ptr = IXBcreateMatlabLogical(1)
	else
		value_ptr = IXBcreateMatlabLogical(0)
	endif
    call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
  end subroutine


  subroutine IXBgetFromBindingDp(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr, marray
    character(len=*) :: field
    real*8 :: value
	integer array_index, op_count
	if (matlab_ptr == 0) return
 	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    if (marray .ne. 0) then
      value = IXBGetScalar(marray)
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
                       'Real*8 field '''//field// ''' missing') 
    endif
  end subroutine

  subroutine IXBsendToBindingDp(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    real*8 :: value	!! FORTRAN variable to send
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
	integer(cpointer_t) :: value_ptr
	integer array_index, op_count
	value_ptr = IXBCreateScalarDouble(value)
    call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
  end subroutine

  subroutine IXBgetFromBindingI4b(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
 	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr, marray, array_data
    character(len=*), intent(in) :: field
    integer(i4b) :: value
	real*8 :: temp;
	integer array_index, op_count
	if (matlab_ptr == 0) return
 	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
    if (marray .ne. 0) then
	    if (IXBIsDouble(marray)) then
		    temp = IXBGetScalar(marray)
			value = nint(temp)
		else
			array_data = IXBGetData(marray)
			call associate_integer4(array_data, value)
		endif
	else if (IXBIsStruct(matlab_ptr) == 1) then
	  call IXFstatus_add(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
                       'Integer*4 field '''//field// ''' missing') 
    endif
  end subroutine

  subroutine IXBsendToBindingI4b(matlab_ptr, field, array_index, op_count, value, status)
    use IXMbinding_interface
	implicit none
	type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    integer :: value	!! FORTRAN variable to send
	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
	integer(cpointer_t) :: value_ptr
    integer, target :: value2(1)
	integer, pointer :: value2p(:)
	integer array_index, op_count
	value2(1) = value
	value2p => value2
    call IXBsendToBinding(matlab_ptr, field, array_index, op_count, value2p, status)
  end subroutine

!  subroutine IXBgetFromBindingWrapped(matlab_ptr, field, array_index, value, status)
!    use IXMbinding_interface
!   use IXMwrapped_var
! 	implicit none
!	type(IXTstatus) :: status
!    integer(cpointer_t) :: matlab_ptr
!    character(len=*), intent(in) :: field
!    type(IXTwrapped_var) :: value
!	integer array_index
!	if (matlab_ptr == 0) return
!	call getFieldFromMatlab(matlab_ptr, field, 0, array_index, marray, status)
!    if (marray == 0) return
!  end subroutine
  
! subroutine IXBsendToBindingWrapped(matlab_ptr, field, array_index, value, status)
!    use IXMbinding_interface
!    use IXMwrapped_var
!	implicit none
!	type(IXTstatus) :: status
!	type(IXTwrapped_var) :: value
!    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
!    character(len=*) :: field	!! Structure field to extract
!	integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
!	integer(cpointer_t) :: value_ptr
!    integer, target :: value2(1)
!	integer, pointer :: value2p(:)
!	integer array_index
!	integer(i4b) :: i
!	integer(i4b), pointer :: i1(:), i2(:,:), i3(:,:,:), i4(:,:,:,:)
!	real(dp) :: dp
!	real(dp), pointer :: dp1(:), dp2(:,:), dp3(:,:,:), dp4(:,:,:,:)
	
!	select case(IXFwrap_type(value))
!	  case(IXCvartype_i)
!         call IXBsendToBinding(matlab_ptr, field, array_index, i, status)
!	  case(IXCvartype_i1)
!	      call IXFunwrap_var_ptr(value, i1, status)
!          call IXBsendToBinding(matlab_ptr, field, array_index, i1, status) 
!	end select
!  end subroutine


!  check_types

!mxGetCell(pm, index)
!mxGetClassName



subroutine IXBdofortrancall(func, nlhs, plhs, nrhs, prhs, errcode)
	use IXMtype_definitions
    use IXMstatus
	use IXMlibcore
    use IXMbinding_interface
   	implicit none
	integer :: nlhs, nrhs, errcode
	integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
!	external func
    interface 
	  subroutine func(nlhs, plhs, nrhs, prhs, status)
       use IXMstatus
	   use IXMtype_definitions
	   implicit none
	   integer :: nlhs, nrhs
	   integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
	   type(IXTstatus) :: status
	  end subroutine
	end interface
	type(IXTstatus) :: status
	! initialse status and set the current subroutine name
	call IXFlibrary_init()
!	call IXFstatus_init('domatlabcall', status)
	call IXBbinding_init()
	! the work
	call func(nlhs, plhs, nrhs, prhs, status)
    ! flush out any informational messages and clean up status
	if (status == IXCseverity_error) then
	    errcode = 1
	else
		errcode = 0
	endif 
	call IXBbinding_cleanup()
	call IXFlibrary_finish(status)
end subroutine



! interface getFromMatlab
! interface getFromMatlabPtr
! interface getFromMatlabAlloc
! interface sendToMatlab
#define IXD_NAME	dp1
#define IXD_TYPE	real(dp)
#define IXD_DIMS	:
#define IXD_MTYPE	"double"
#include "binding_interface_routines.f90"


#define IXD_NAME	dp2
#define IXD_TYPE	real(dp)
#define IXD_DIMS	:,:
#define IXD_MTYPE	"double"
#include "binding_interface_routines.f90"

#define IXD_NAME	dp3
#define IXD_TYPE	real(dp)
#define IXD_DIMS	:,:,:
#define IXD_MTYPE	"double"
#include "binding_interface_routines.f90"

#define IXD_NAME	dp4
#define IXD_TYPE	real(dp)
#define IXD_DIMS	:,:,:,:
#define IXD_MTYPE	"double"
#include "binding_interface_routines.f90"

#define IXD_NAME	i1
#define IXD_TYPE	integer(i4b)
#define IXD_DIMS	:
#define IXD_MTYPE	"int32"
#include "binding_interface_routines.f90"

#define IXD_NAME	i2
#define IXD_TYPE	integer(i4b)
#define IXD_DIMS	:,:
#define IXD_MTYPE	"int32"
#include "binding_interface_routines.f90"

#define IXD_NAME	i3
#define IXD_TYPE	integer(i4b)
#define IXD_DIMS	:,:,:
#define IXD_MTYPE	"int32"
#include "binding_interface_routines.f90"

#define IXD_NAME	i4
#define IXD_TYPE	integer(i4b)
#define IXD_DIMS	:,:,:,:
#define IXD_MTYPE	"int32"
#include "binding_interface_routines.f90"
