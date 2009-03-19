#ifndef BINDING_FUNCS_H 
#define BINDING_FUNCS_H
#if 0
//void IXBCwriteLine(const char* line, int len, binding_status_t* status);

/* It returns a pointer to MATLAB memory, the dimensions of which
 * are given by dims_array and the data type given by the string array_type. 
 arrays_type is a valid IXCTYPE_* constant */
void* IXBCallocArrayDescriptor(int ndims, int dims_array[], int array_type);

/* deallocate array descriptor */
void IXBCdeallocArrayDescriptor(void* ptr);

/* It return a pointer to array data given an external pointer to the array descriptor */
void* IXBCGetArrayData(void* ptr);

/* return try is external pointer is type specified */
int IXBCisType(void* ptr, const char* type, int ntype);

/* return a full (deep) copy of an object */
void* IXBCduplicateObject(void* ptr);

/* return number of elements in an array */
int IXBCgetNumberOfElement(void* ptr);

!! It creates a result version of an external object that has been held on the memory
!! stack. With some bindings you cannot return an object that was passed into fortran by the
!! bindings layer back to the original program as a result.
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 389 $ ($Date: 2005-03-21 08:41:19 -0500 (Mon, 21 Mar 2005) $)
!! @see IXMmemory
function IXBexternalMakeResult(external_ptr, fortran_alloc) result(p)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface
  implicit none
  integer(cpointer_t) :: external_ptr !! pointer to mxArray
  logical fortran_alloc !! .true. if mxArray allocated in fortran, .false. if allocated in matlab
  integer :: p
  type(IXTstatus) :: status
  if (fortran_alloc == .false.) then
      p = mxDuplicateArray(external_ptr)
  else
      p = external_ptr
  endif
end function

subroutine IXBcreateBindingPLHS(plhs,prhs,s)
  use IXMtype_definitions
  use IXMstatus 
  use IXMmatlab_interface, createPLHS => createMatlabPLHS
  implicit none
  integer(cpointer_t)::prhs;
  integer(cpointer_t),target::plhs
  type(IXTstatus)::s
  call createPLHS(plhs,prhs,s)
  return
end subroutine

function IXBcreateBindingFieldIfNeeded(plhs, field) result(marray)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface
  implicit none
  integer(cpointer_t) :: plhs, marray
  integer fnum
  character(len=*) :: field
  type(IXTstatus)::s
  character(len=256) :: field_names(1)

	   	   fnum = mxGetFieldNumber(plhs, field)
		   if (fnum <= 0) then
	           field_names(1) = field
! this causes a link error on simons computer
!		       marray = mxCreateStructArray(1,1,size(field_names),field_names)
               marray = 0
		   else
		       marray = mxGetFieldByNumber(plhs, 1, fnum)
		   endif
end function

subroutine IXBgetFieldFromBinding(prhs, field, field_num, array_index, op_count, marray, status)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface, getField => getFieldFromMatlab
  implicit none
  integer(cpointer_t) :: prhs, marray
  character(len=*) :: field
  integer field_num, array_index, op_count
  type(IXTstatus)::status
  call getField(prhs, field, field_num, array_index, op_count, marray, status)
end subroutine

  integer(cpointer_t), external :: mxGetField, mxGetData, mxGetFieldByNumber, &
         mxCreateString, mxCreateScalarDouble, mxGetPr, &
		 mxCreateNumericArray, mxDuplicateArray, mexPrintf, mxGetCell, &
		 mxGetNumberOfDimensions, mxGetDimensions
  integer*4, external :: mxGetString, mxAddField, mxGetFieldNumber, &
		 mxGetNumberOfElements, mxClassIDFromClassName, mxIsStruct, mxIsCell, &
		 mxIsDouble
#endif


#ifdef __cplusplus
extern }
#endif /* __cplusplus */

#endif /* BINDING_FUNCS_H */ 