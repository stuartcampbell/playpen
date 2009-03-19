!-------------------------------------------------------------------------------
! MODULE: isisexc_matlab_interface
!-------------------------------------------------------------------------------
!! A set of routines to proved a mapping between MATLAB and FOTRAN data types
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 1356 $ ($Date: 2008-05-09 06:26:56 -0400 (Fri, 09 May 2008) $)


module IXMmatlab_interface
  use IXMtype_definitions
  use IXMstatus
  use IXMmemory
  use IXMoperation_interfaces
  implicit none
  !-------------------------------------------------------------------------------
  ! List Matlab mx* and mex* functions used
  !-------------------------------------------------------------------------------
  integer(cpointer_t), external ::  ixGetData, ixGetFieldByNumber, &
       ixCreateDoubleScalar, &
       ixCreateNumericArray, ixDuplicateArray, ixGetCell, ixCreateCellArray
  integer(i4b), external :: &
       ixGetNumberOfElements, ixIsStruct, ixIsCell, &
       ixIsDouble, ixIsChar, ixIsInt32, ixGetM, ixGetN, &
       ixGetNumberOfDimensions
  real(dp), external :: ixGetScalar
  external ixSetFieldByNumber, ixSetN, ixGetDimensions, &
      ixCopyPtrToInteger2, ixSetCell !, ixDestroyArray
  !-------------------------------------------------------------------------------

contains

   function ixIsClass(marray, class_name) result(r)
      implicit none
      integer(cpointer_t) :: marray
      integer :: r
      character(len=*) :: class_name
      integer, external :: ixIsClassStub
      r = ixIsClassStub(marray, ixCString(class_name))
   end function

   function ixGetString(marray, string) result(r)
      implicit none
      integer(cpointer_t) :: marray
      integer :: r
      character(len=*) :: string
      integer(i1b) :: buffer(len(string)+1) ! +1 to allow for returned NULL
      integer(cpointer_t), external :: ixGetStringStub
      r = ixGetStringStub(marray, buffer, size(buffer))
      buffer(len(string)+1) = 0 ! just in case; shouldn't be needed really
      call ixFString(string, buffer)
   end function
      
   function ixGetField(marray, i, field_name) result(r)
      implicit none
      integer(cpointer_t) :: r, marray
      character(len=*) :: field_name
      integer i
      integer(cpointer_t), external :: ixGetFieldStub
      r = ixGetFieldStub(marray, i, ixCString(field_name))
   end function

   function ixGetFieldNumber(marray, field_name) result(r)
      implicit none
      integer(cpointer_t) :: r, marray
      character(len=*) :: field_name
      integer(cpointer_t), external :: ixGetFieldNumberStub
      r = ixGetFieldNumberStub(marray, ixCString(field_name))
   end function

   function ixClassIDFromClassName(name) result(r)
      implicit none
      integer :: r
      character(len=*) :: name
      integer, external :: ixClassIDFromClassNameStub
      r = ixClassIDFromClassNameStub(ixCString(name))
   end function

   subroutine ixGetClassName(marray, name)
      implicit none
      integer(cpointer_t) :: marray
      character(len=*) :: name
      integer(i1b) :: buffer(len(name))
      external :: ixGetClassNameStub
      call ixGetClassNameStub(marray, buffer, size(buffer))
      call ixFString(name, buffer)
   end subroutine

   function ixCreateString(string) result(r)
      implicit none
      integer(cpointer_t) :: r
      character(len=*) :: string
      integer(cpointer_t), external :: ixCreateStringStub
      r = ixCreateStringStub(ixCString(string))
   end function

   function ixAddField(marray, field) result(r)
      implicit none
      integer(cpointer_t) :: marray
      integer :: r
      character(len=*) :: field
      integer, external :: ixAddFieldStub
      r = ixAddFieldStub(marray, ixCString(field))
   end function
   
   function iexPrintf(string) result(r)
      implicit none
      integer r
      CHARACTER(len=*), INTENT(in) :: string
      integer, external :: iexPrintfStub
      r = iexPrintfStub(ixCString(string))
   end function

   function iexCallMATLAB(nlhs, plhs, nrhs, prhs, fcn_name) result(stat)
      implicit none
      integer :: nlhs, nrhs, stat
      integer(cpointer_t) :: plhs(*), prhs(*)
      CHARACTER(len=*), INTENT(in) :: fcn_name
      integer, external :: iexCallMATLABStub
      stat = iexCallMATLABStub(nlhs, plhs, nrhs, prhs, ixCString(fcn_name))
   end function

   function ixCreateCharMatrixFromStrings(strings) result(r)
      implicit none
      character(len=*) :: strings(:)
      integer i, j, n, m
      integer(i1b), allocatable :: buffer(:,:)
      integer(cpointer_t) :: r
      integer(cpointer_t), external :: ixCreateCharMatrixFromStringsStub
      m = size(strings)
      n = len(strings(1))
      allocate(buffer(n+1,m))
      do i = 1,m
         do j = 1,n
            buffer(j,i) = ichar(strings(i)(j:j))
         enddo
         buffer(n+1,i) = 0
      enddo
      r = ixCreateCharMatrixFromStringsStub(m, buffer)
      deallocate(buffer)
   end function

! converts a Fortran string into a C string
   FUNCTION ixCString(string) RESULT (array)

      CHARACTER(len=*), INTENT(in) :: string
      INTEGER(kind=i1b) :: array(len(string)+1)
      INTEGER :: i, slen
! always write at least 1 character even if string is full of blanks      
      slen = min(len_trim(string), (size(array)-1))
      if (slen == 0) slen = 1
      DO i = 1,slen
         array(i) = ichar(string(i:i))
      END DO
      array(slen+1) = 0

  END FUNCTION ixCString

  subroutine ixFString(name, buffer)
      implicit none
      character(len=*) :: name
      integer(i1b) buffer(:) ! NULL terminated C string
      integer i
      name  = ' '
      i = 0
      do while( (i < len(name)) .and. (i < size(buffer)) )
         i = i + 1
         if (buffer(i) == 0) return
         name(i:i) = char(buffer(i))
      enddo
   end subroutine

  subroutine IXBmatlab_init()
    implicit none
  end subroutine IXBmatlab_init

  subroutine IXBmatlab_cleanup()
    implicit none
  end subroutine IXBmatlab_cleanup

!! Set item FIELD of structure MATLAB_PTR to VALUE_PTR if struct
!! set ARRAY_INDEX of cell MATLAB_PRT to VALUE_PTR
!! else if not field set, then set matlab_ptr = value_ptr
!! if matlab_ptr == 0, set matlab_ptr = value_ptr
  subroutine setMatlabField(matlab_ptr, field, field_num, array_index, op_count, value_ptr, status)
    implicit none
    type(IXTstatus) :: status
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	        !! Structure field to extract
    integer(cpointer_t) :: value_ptr
    integer(cpointer_t) :: field_ptr
    integer array_index, op_count
    integer :: field_num, f_num
    ! No LHS
    if (matlab_ptr == 0) then
       matlab_ptr = value_ptr
       return
    endif
    if (ixIsStruct(matlab_ptr) == 1) then
       if (len_trim(field) > 0) then
          f_num = ixGetFieldNumber(matlab_ptr, field)
       else
          f_num = field_num
       endif
       if (f_num <= 0) then
          f_num = ixAddField(matlab_ptr, field)
       endif
       field_ptr = ixGetFieldByNumber(matlab_ptr, array_index, f_num)
       !		field_ptr = ixGetFieldByNumber(matlab_ptr, 1, f_num)
       if (field_ptr .ne. 0) then
          !			call ixDestroyArray(field_ptr)
       endif
       !	    call ixSetFieldByNumber(matlab_ptr, 1, f_num, value_ptr)
       call ixSetFieldByNumber(matlab_ptr, array_index, f_num, value_ptr)
       return
    endif
    if (ixIsCell(matlab_ptr) == 1) then
       call ixSetCell(matlab_ptr, array_index, value_ptr)
       return
    endif
    ! simple variable set
    if ((len_trim(field) == 0) .and. (field_num == 0) .and. (array_index == 1) .and. (op_count == 0) ) then
       matlab_ptr = value_ptr
       return
    endif
  end subroutine setMatlabField

!! Return matlab pointer MARRAY corresponding to field INDEX of structure MATLAB_PTR
!! Sets MARRAY = 0 on error/missing 
  subroutine getFieldIndexFromMatlab(matlab_ptr, field_index, array_index, marray, status)
    implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    integer :: field_index, array_index
    integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
 !   integer :: stat
    marray = 0
    if (matlab_ptr .ne. 0) then
       marray = ixGetFieldByNumber(matlab_ptr, array_index, field_index)
    else
       marray = 0
    endif
    if (marray .eq. 0) then
       !	  call IXFadd_status(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, & 
       !                      'Field '''//field// ''' missing') 
    endif
  end subroutine getFieldIndexFromMatlab

!! Return matlab pointer MARRAY corresponding to field INDEX of cell array MATLAB_PTR
!! Sets MARRAY = 0 on error/missing 
  subroutine getCellFromMatlab(matlab_ptr, index, marray, status)
    implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    integer :: index	!! Structure field to extract
    integer(cpointer_t) :: marray			!! temporary for MATLAB mxArray pointer
    type(IXTstatus) :: status
!    integer :: stat
    marray = 0
    if (matlab_ptr .ne. 0) then
   	    marray = ixDuplicateArray(ixGetCell(matlab_ptr, index))
   	else
   		marray = 0
    endif
    if (marray .eq. 0) then
       !	  call IXFadd_status(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, & 
       !                      'Cell number '''//field// ''' missing') 
    endif
  end subroutine getCellFromMatlab

!! Return matlab pointer MARRAY corresponding to field FIELD of structure array element ARRAY_INDEX
!! of MATLAB_PTR
!! if FIELD == ' ' will use FIELD_NUM instead if > 0
!! If cell array, returns element OP_COUNT of cell array or 0
!! Sets MARRAY = MATLAB_PTR on error/missing 
  subroutine getFieldFromMatlab(matlab_ptr, field, field_num, array_index, op_count, marray, status)
    implicit none
    integer(cpointer_t) :: matlab_ptr	!! Pointer to MATLAB structure
    character(len=*) :: field	!! Structure field to extract
    integer(cpointer_t) :: marray			!! temporary for MATLAB ixArray pointer
    type(IXTstatus) :: status
    integer ::  nelem
    integer field_num, array_index, op_count
    marray = matlab_ptr ! if not cell or structure, then return object
    if (matlab_ptr == 0) return
    if (ixIsStruct(matlab_ptr) == 1) then
       if (len_trim(field) > 0) then
          !	        marray = ixGetField(matlab_ptr, 1, field)
          marray = ixGetField(matlab_ptr, array_index, field)
       else if (field_num > 0) then
          !			marray = ixGetFieldByNumber(matlab_ptr, 1, field_num)
          marray = ixGetFieldByNumber(matlab_ptr, array_index, field_num)
       endif
    endif
    if (ixIsCell(matlab_ptr) == 1) then
       nelem = ixGetNumberOfElements(matlab_ptr)
       if ( (nelem > 0) .and. (op_count > 0) .and. (op_count <= nelem) ) then
          marray = ixDuplicateArray(ixGetCell(matlab_ptr, op_count))
       else
          marray = 0
       endif
    endif
  end subroutine getFieldFromMatlab

!! If PLHS == 0 then create one from PRHS else do nothing
  subroutine createMatlabPLHS(plhs, prhs, status)
    implicit none
    character(len=9) :: field_names(1) = (/ '__dummy__' /)
    integer(cpointer_t) :: plhs, prhs
    type(IXTstatus) :: status
    if (plhs /= 0) return    ! (plhs /= 0) so nothing needs doing
    if (prhs /= 0) then
       plhs = ixDuplicateArray(prhs)
    else
       ! this means that a field is missing in the LHS and indicates an error
       ! as we should be passed a template object to fill up and thus all the
       ! fields should exist and not need creating.
       !       plhs = ixCreateStructArray(1,1,size(field_names),field_names)
       plhs = 0
    endif
    if (plhs == 0) then
       call IXFadd_status(status, IXCfacility_bindings, IXCseverity_error, IXCerr_outofmem, &
            'createMatlabPLHS: PLHS==0 (this could mean the Matlab .m file class definition and the fortran do not agree)') 
    endif
  end subroutine createMatlabPLHS

end module IXMmatlab_interface

!! Read a character string from MATLAB_PTR - if FIELD /= ' ' then from field
!! else direct from MATLAB_PTR
subroutine IXBgetFromBindingChar(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingChar
  implicit none
  character(len=*):: field
  integer :: field_num, array_index, op_count
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  character(len=*) :: value    !! FORTRAN variable to update
  integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
  type(IXTstatus) :: status
  integer :: stat
!  value = ' '
  if (matlab_ptr == 0) then 
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'getFromBindingChar: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  if (marray /= 0) then
     if (ixIsChar(marray) == 1) then 
         stat = ixGetString(marray, value)
     endif
  else if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
          'Character field '''//field// ''' missing') 
  endif
end subroutine IXBgetFromBindingChar

subroutine IXBsendToBindingChar(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBindingChar
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  character(len=*) :: field    !! Structure field to extract
  character(len=*) :: value    !! FORTRAN variable to send
  integer(cpointer_t) :: marray            !! temporary for MATLAB ixArray pointer
  integer(cpointer_t) :: value_ptr
  integer :: array_index, op_count, l_value
  l_value = len_trim(value)
  if (l_value == 0) l_value = 1
  value_ptr = ixCreateString(value(1:l_value))
  call ixSetN(value_ptr, l_value) ! for some reason l_value above is not honoured
  call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
end subroutine IXBsendToBindingChar

subroutine IXBgetFromBindingLogical(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingLogical
  implicit none
  character(len=*) field
  integer :: i, field_num, array_index, op_count
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  logical :: value    !! FORTRAN variable to update
  integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
  type(IXTstatus) :: status
  integer :: stat
  integer, external :: IXBgetMatlabLogical
  if (matlab_ptr == 0) return
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  if (marray /= 0) then
     i = IXBgetMatlabLogical(marray)
     if (i == 1) then
        value = .true.
     else if (i == 0) then
        value = .false.
     endif
  else if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
          'Logical field '''//field// ''' missing') 
  endif
end subroutine IXBgetFromBindingLogical

subroutine IXBsendToBindingLogical(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBindingLogical
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  character(len=*) :: field    !! Structure field to extract
  logical :: value    !! FORTRAN variable to send
  integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
  integer(cpointer_t) :: value_ptr
  integer :: array_index, op_count
  integer(cpointer_t), external :: IXBcreateMatlabLogical
  if (value .eqv. .true.) then
     value_ptr = IXBcreateMatlabLogical(1)
  else
     value_ptr = IXBcreateMatlabLogical(0)
  endif
  call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
end subroutine IXBsendToBindingLogical

subroutine IXBgetFromBindingDp(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingDp
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray
  character(len=*) :: field
  real*8 :: value
  integer array_index, op_count
  if (matlab_ptr == 0) return
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  if (marray /= 0) then
     if (ixIsDouble(marray) /= 0) then
         value = ixGetScalar(marray)
     endif
  else if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
          'Real*8 field '''//field// ''' missing') 
  endif
end subroutine IXBgetFromBindingDp

subroutine IXBsendToBindingDp(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBindingDp
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  character(len=*) :: field    !! Structure field to extract
  real*8 :: value    !! FORTRAN variable to send
  integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
  integer(cpointer_t) :: value_ptr
  integer array_index, op_count
  value_ptr = ixCreateDoubleScalar(value)
  call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
end subroutine IXBsendToBindingDp

subroutine IXBgetFromBindingI4b(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingI4b
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
     if (ixIsDouble(marray) /= 0) then
        temp = ixGetScalar(marray)
        value = nint(temp)
     else if (ixIsInt32(marray) /= 0) then
        array_data = ixGetData(marray)
        call associate_integer4(array_data, value)
     endif
  else if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, & 
          'Integer*4 field '''//field// ''' missing') 
  endif
end subroutine IXBgetFromBindingI4b

subroutine IXBsendToBindingI4b(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBindingI4b
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
  character(len=*) :: field    !! Structure field to extract
  integer :: value    !! FORTRAN variable to send
  integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
  integer(cpointer_t) :: value_ptr
  integer, target :: value2(1)
  integer, pointer :: value2p(:)
  integer array_index, op_count
  value2(1) = value
  value2p => value2
  call IXBsendToBinding(matlab_ptr, field, array_index, op_count, value2p, status)
end subroutine IXBsendToBindingI4b

function IXBgetNumberOfElements(external_ptr, status) result(n)
  use IXMmatlab_interface, inore_this => IXBgetNumberOfElements
  implicit none
  integer(cpointer_t) :: external_ptr
  type(IXTstatus)::status
  integer :: n
  if (external_ptr /= 0) then
     n = ixGetNumberOfElements(external_ptr)
  else
     n = 0
  endif
end function IXBgetNumberOfElements


!  subroutine IXBgetFromBindingWrapped(matlab_ptr, field, array_index, value, status)
!    use IXMmatlab_interface
!   use IXMwrapped_var
!     implicit none
!    type(IXTstatus) :: status
!    integer(cpointer_t) :: matlab_ptr
!    character(len=*), intent(in) :: field
!    type(IXTwrapped_var) :: value
!    integer array_index
!    if (matlab_ptr == 0) return
!    call getFieldFromMatlab(matlab_ptr, field, 0, array_index, marray, status)
!    if (marray == 0) return
!  end subroutine

! subroutine IXBsendToBindingWrapped(matlab_ptr, field, array_index, value, status)
!    use IXMmatlab_interface
!    use IXMwrapped_var
!    implicit none
!    type(IXTstatus) :: status
!    type(IXTwrapped_var) :: value
!    integer(cpointer_t) :: matlab_ptr    !! Pointer to MATLAB structure
!    character(len=*) :: field    !! Structure field to extract
!    integer(cpointer_t) :: marray            !! temporary for MATLAB mxArray pointer
!    integer(cpointer_t) :: value_ptr
!    integer, target :: value2(1)
!    integer, pointer :: value2p(:)
!    integer array_index
!    integer(i4b) :: i
!    integer(i4b), pointer :: i1(:), i2(:,:), i3(:,:,:), i4(:,:,:,:)
!    real(dp) :: dp
!    real(dp), pointer :: dp1(:), dp2(:,:), dp3(:,:,:), dp4(:,:,:,:)

!    select case(IXFwrap_type(value))
!      case(IXCvartype_i)
!         call IXBsendToBinding(matlab_ptr, field, array_index, i, status)
!      case(IXCvartype_i1)
!          call IXFunwrap_var_ptr(value, i1, status)
!          call IXBsendToBinding(matlab_ptr, field, array_index, i1, status) 
!    end select
!  end subroutine


!  check_types

!mxGetCell(pm, index)
!mxGetClassName



subroutine IXBdomatlabcall(func, nlhs, plhs, nrhs, prhs, errcode)
  use IXMtype_definitions
  use IXMstatus
  use IXMlibcore
  use IXMmatlab_interface
  implicit none
  integer :: nlhs, nrhs, errcode
  integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
  !    external func
  interface 
     subroutine func(nlhs, plhs, nrhs, prhs, status)
       use IXMstatus
       use IXMtype_definitions
       implicit none
       integer :: nlhs, nrhs
       integer(cpointer_t) :: plhs(nlhs), prhs(nrhs)
       type(IXTstatus) :: status
     end subroutine func
  end interface
  type(IXTstatus) :: status
  ! initialse status and set the current subroutine name
  errcode = IXFlibrary_init()
  call IXFinit_status('domatlabcall', status)
  call IXBmatlab_init()
  ! the work
  call func(nlhs, plhs, nrhs, prhs, status)
  ! flush out any informational messages and clean up status
  if (status == IXCseverity_error) then
     errcode = 1
  else
     errcode = 0
  endif
  call IXBmatlab_cleanup()
  call IXFlibrary_finish(status)
end subroutine IXBdomatlabcall

! interface getFromMatlab
! interface getFromMatlabPtr
! interface getFromMatlabAlloc
! interface sendToMatlab
#define IXD_NAME    dp1
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :
#define IXD_MTYPE    "double"
#include "matlab_interface_routines.f90"


#define IXD_NAME    dp2
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:
#define IXD_MTYPE    "double"
#include "matlab_interface_routines.f90"

#define IXD_NAME    dp3
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:
#define IXD_MTYPE    "double"
#include "matlab_interface_routines.f90"

#define IXD_NAME    dp4
#define IXD_TYPE    real(dp)
#define IXD_DIMS    :,:,:,:
#define IXD_MTYPE    "double"
#include "matlab_interface_routines.f90"

#define IXD_NAME    i1
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :
#define IXD_MTYPE    "int32"
#include "matlab_interface_routines.f90"

#define IXD_NAME    i2
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:
#define IXD_MTYPE    "int32"
#include "matlab_interface_routines.f90"

#define IXD_NAME    i3
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:
#define IXD_MTYPE    "int32"
#include "matlab_interface_routines.f90"

#define IXD_NAME    i4
#define IXD_TYPE    integer(i4b)
#define IXD_DIMS    :,:,:,:
#define IXD_MTYPE    "int32"
#include "matlab_interface_routines.f90"

! we need to do character array manually rather than using 
! matlab_interface_routines.f90
!
!#define IXD_NAME    c1
!#define IXD_TYPE    character(len=*)
!#define IXD_DIMS    :
!#define IXD_MTYPE    "char"
!#include "matlab_interface_routines.f90"
!

subroutine IXBgetFromBindingAllocc1(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingAllocc1
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray, array_data, mtmp
  integer, allocatable :: array_dims(:)
  integer(i2b), allocatable :: value_i2b(:)
  integer array_index, loop_step, op_count, ndims, ndims_matlab, i, j, k, n
  character(len=*), intent(in) :: field
  character(len=*) , allocatable :: value(:)
  if (matlab_ptr == 0) then
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'getFromBindingAlloc: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  ndims = size(shape(value))
  if (marray .ne. 0) then
     if (ixIsCell(marray) /= 0) then
         n = ixGetNumberOfElements(marray)
         call IXFAllocDimsFortran(value, (/ n /), status)
         do i = 1, n
             mtmp = ixGetCell(marray, i)
             call IXBgetFromBindingChar(mtmp, ' ', 1, 0, value(i), status)
         enddo
     else if (ixIsClass(marray, 'char') /= 0) then
         array_data = ixGetData(marray)
         ndims_matlab = ixGetNumberOfDimensions(marray)
         allocate(array_dims(ndims_matlab))
         call ixGetDimensions(marray,array_dims,ndims_matlab)
         n = product(array_dims)
         allocate(value_i2b(n)) ! strings are 16bit
         ! final dimension is string lenght (row) so allocate for rest
         call IXFAllocdimsFortran(value, array_dims(1:ndims_matlab-1), status)
         call ixCopyPtrToInteger2(array_data, value_i2b, n)
!         call copyFromVectorC&/**/
!                             &IXD_NAME (array_data, value_i2b, n, status)
! i loops down columns, but strings are in rows
         loop_step = n / array_dims(ndims_matlab) ! size without final dimension
         do j = 1, loop_step
             value(j) = ' '
         enddo
         do i = 1, n
           j = 1 + mod(i - 1, loop_step) ! column index
           k = 1 + (i - 1) / loop_step   ! row index
           if (k <= len(value(1))) then
               value(j)(k:k) = char(value_i2b(i))
           endif
         enddo
         deallocate(array_dims,value_i2b)
     endif
  else  if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
  endif
end subroutine

! We do not call push memory as the ares are not associated
subroutine IXBgetFromBindingc1(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBgetFromBindingc1
  implicit none
  type(IXTstatus) :: status
  integer(cpointer_t) :: matlab_ptr, marray, array_data, mtmp
  integer, allocatable :: array_dims(:)
  integer array_index, loop_step, op_count, ndims, ndims_matlab, i, j, k, n
  character(len=*), intent(in) :: field
  character(len=*) :: value(:)
  integer(i2b), allocatable :: value_i2b(:)
  if (matlab_ptr == 0) then 
!     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, & 
!                        IXCerr_outofmem, 'getFromBinding: ptr=0')
     return
  endif
  call getFieldFromMatlab(matlab_ptr, field, 0, array_index, op_count, marray, status)
  ndims = size(shape(value))
  if (marray .ne. 0) then
     if (ixIsCell(marray) /= 0) then
         n = ixGetNumberOfElements(marray)
         do i = 1, min(n, size(value))
             mtmp = ixGetCell(marray, i)
             call IXBgetFromBindingChar(mtmp, ' ', 1, 0, value(i), status)
         enddo
     else if (ixIsClass(marray, 'char') /= 0) then
        array_data = ixGetData(marray)
        ndims_matlab = ixGetNumberOfDimensions(marray)
        allocate(array_dims(ndims_matlab))
        call ixGetDimensions(marray,array_dims,ndims_matlab)
        n = product(array_dims)
        allocate(value_i2b(n)) ! strings are 16bit
        call ixCopyPtrToInteger2(array_data, value_i2b, n)
!        call copyFromVectorC&/**/
!                            &IXD_NAME (array_data, value_i2b, n, status)
! i loops down columns, but strings are in rows
        loop_step = n / array_dims(ndims_matlab) ! size without final dimension
        do j = 1, loop_step
             value(j) = ' '
        enddo
        do i = 1, n
           j = 1 + mod(i - 1, loop_step) ! column index
           k = 1 + (i - 1) / loop_step   ! row index
           if ( (j <= size(value)) .and. (k <= len(value(1))) ) then
               value(j)(k:k) = char(value_i2b(i))
           endif
        enddo
        deallocate(array_dims,value_i2b)
     endif
  else  if (ixIsStruct(matlab_ptr) == 1) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, IXCerr_outofmem, 'array field missing')
  endif
end subroutine

subroutine IXBsendToBindingc1(matlab_ptr, field, array_index, op_count, value, status)
  use IXMmatlab_interface, ignore_this => IXBsendToBindingc1
  implicit none
  integer(cpointer_t), intent(in) :: matlab_ptr
  type(IXTstatus) :: status
  integer(cpointer_t) :: marray, array_data
  integer(cpointer_t) :: value_ptr
  integer,allocatable :: array_dims(:)
  integer ndims, array_index, op_count, i
  character(len=*),intent(in) :: field
  character(len=*) :: value(:)
  logical :: as_cell
  if (size(value) == 0) then
     call IXFadd_status(status, IXCfacility_bindings, IXCseverity_info, &
                        IXCerr_outofmem, 'sendToBinding: zero size field')
     return
  endif
  as_cell = .false.
  if (as_cell) then
      value_ptr = ixCreateCellArray(1, shape(value))
      do i = 1, size(value)
          marray = ixCreateString(value(i))
          call ixSetCell(value_ptr, i, marray)
      enddo
  else
      value_ptr = ixCreateCharMatrixFromStrings(value)
  endif
  call setMatlabField(matlab_ptr, field, 0, array_index, op_count, value_ptr, status)
end subroutine 
