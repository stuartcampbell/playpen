! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-----------------------------------------------------------------------------------------------------------------------------------
! bindings placeholders/stubs for MATLAB
! DO NOT use mxCopyInteger4ToPtr etc. here - they claim only to work with sparse matrices
!-----------------------------------------------------------------------------------------------------------------------------------
! This file contains the low level IXB* routines that will to be substituted
! for those provided in LIBCORE / LIBCLASSES
!

!! It writes a line of text to the default standard output device
!!
!! If external (e.g. MATLAB) bindings are being used then this
!! function will be replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)
!! @see IXMlibcore
subroutine IXBwrite_line(line,status)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface
  implicit none
  integer n, i
  character(len=*) line
  character(len=512) :: buffer
  type(IXTstatus) :: status
  ! % has special meaning in printf, so must trap
  buffer = line
  do i = 1, len(buffer)
     if (buffer(i:i) == '%') then
        buffer(i:i) = '+'
     endif
  enddo
  i = min(len(buffer), len(line))
  n = iexPrintf(buffer(:i))
!  n = iexPrintf(char(13)//char(10))
  n = iexPrintf(char(10))
end subroutine IXBwrite_line

!! It returns a pointer to MATLAB memory, the dimensions of which
!! are given by dims_array and the data type given by the string array_type.
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)
!! @see IXMmemory
function IXBallocArrayDescriptor(ndims, dims_array, array_type) result(mxarray_ptr)
  use IXMtype_definitions
  use IXMmatlab_interface, ignore_this => IXBallocArrayDescriptor
  implicit none
  integer ndims, array_size, i, dims_array(ndims), temp_dims(2)
  integer(cpointer_t) :: mxarray_ptr
  character(len=*) :: array_type
  ! matlab arrays are always at least 2 dimensional (1 x nx)
  !	dims(1) = 1
  !	dims(2) = nx
  if (ndims == 1) then
     temp_dims(1) = 1
     temp_dims(2) = dims_array(1)
     mxarray_ptr = ixCreateNumericArray(2, temp_dims, ixClassIDFromClassName(array_type), 0)
  else
     mxarray_ptr = ixCreateNumericArray(ndims, dims_array, ixClassIDFromClassName(array_type), 0)
  endif
end function IXBallocArrayDescriptor

!! This routine is only used by the IXMmemory module
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)
!! @see IXMmemory
!!
subroutine IXBdeallocArrayDescriptor(mxarray_ptr)
  use IXMtype_definitions
  use IXMmatlab_interface
  implicit none
  integer(cpointer_t) :: mxarray_ptr !! pointer to external array descriptor
  if (mxarray_ptr /= 0) then 
      call ixDestroyArray(mxarray_ptr)
      mxarray_ptr = 0
  endif
end subroutine IXBdeallocArrayDescriptor

!! It return a pointer to array data given an external pointer to the array descriptor
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)
!! @see IXMmemory
function IXBGetArrayData(mxarray_ptr) result(p)
  use IXMtype_definitions
  use IXMmatlab_interface, ignore_this => IXBGetArrayData
  implicit none
  integer(cpointer_t) :: mxarray_ptr
  integer(cpointer_t) :: p
  p = ixGetData(mxarray_ptr)
end function IXBGetArrayData

!! It creates a result version of an external object that has been held on the memory
!! stack. With some bindings you cannot return an object that was passed into fortran by the
!! bindings layer back to the original program as a result.
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)
!! @see IXMmemory
function IXBexternalMakeResult(external_ptr, fortran_alloc) result(p)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface, ignore_this => IXBexternalMakeResult
  implicit none
  integer(cpointer_t) :: external_ptr !! pointer to mxArray
  logical fortran_alloc !! .true. if mxArray allocated in fortran, .false. if allocated in matlab
  integer(cpointer_t) :: p
  type(IXTstatus) :: status
  if (fortran_alloc .eqv. .false.) then
      p = ixDuplicateArray(external_ptr)
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

function IXBcreateBindingFieldIfNeeded(plhs, field, array_index, s) result(marray)
  use IXMmatlab_interface
  implicit none
  integer(cpointer_t) :: plhs, marray
  integer :: fnum, array_index
  character(len=*) :: field
  type(IXTstatus)::s
  character(len=long_len) :: field_names(1)

  marray = 0
  if (ixIsStruct(plhs) == 0) then
     call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
         IXCerr_outofmem, 'createBindingFieldIfNeeded: not struct so cannot test for field '''//field//'''') 
     return
  endif
  fnum = ixGetFieldNumber(plhs, field)
  if (fnum <= 0) then
     field_names(1) = field
     ! this causes a link error on simons computer
     !               marray = ixCreateStructArray(1,1,size(field_names),field_names)
     call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
         IXCerr_outofmem, 'createBindingFieldIfNeeded: field ''' &
         //field//''' not found')
  else
     marray = ixGetFieldByNumber(plhs, array_index, fnum)
  endif
end function IXBcreateBindingFieldIfNeeded

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

subroutine IXBsendFieldToBinding(plhs, field, field_num, array_index, op_count, marray, status)
  use IXMtype_definitions
  use IXMstatus
  use IXMmatlab_interface, setField => setMatlabField
  implicit none
  integer(cpointer_t) :: plhs, marray
  character(len=*) :: field
  integer field_num, array_index, op_count
  type(IXTstatus)::status
  call setField(plhs, field, field_num, array_index, op_count, marray, status)
end subroutine

function IXBcreateClassObject(class_name, s) result(marray)
  use IXMmatlab_interface
  implicit none
  integer(cpointer_t) :: marray, plhs(1), zero_pointer
  character(len=*) :: class_name
  type(IXTstatus)::s
  marray = 0
  zero_pointer = 0
  plhs(1) = 0
  if (iexCallMATLAB(1, plhs, 0, (/ zero_pointer /), class_name) /= 0) then
      marray = 0
      call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
         IXCerr_outofmem, 'IXBcreateClassObject: error in iexCallMATLAB') 
  else
      marray = plhs(1)
  endif
end function

function IXBcreateClassArray(class_name, n, s) result(marray)
  use IXMmatlab_interface, ignore_this => IXBcreateClassArray
  implicit none
  integer(cpointer_t) :: prhs(2), plhs(1), marray, IXBcreateClassObject
  character(len=*) :: class_name
  type(IXTstatus)::s
  integer :: n
  marray = 0
  plhs(1) = 0
  prhs(1) = IXBcreateClassObject(class_name, s)
  if (prhs(1) == 0) return
  prhs(2) = ixCreateDoubleScalar(dble(n))
  if (iexCallMATLAB(1, plhs, 2, prhs, 'extend') /= 0) then
      marray = 0
      call IXFadd_status(s, IXCfacility_bindings, IXCseverity_error, &
         IXCerr_outofmem, 'IXBcreateClassArray: error in iexCallMATLAB') 
  else
      marray = plhs(1)
  endif
end function
