! $Id: IXMmemory_routines.f90 1390 2008-05-20 17:26:55Z Freddie Akeroyd $
! requires IXD_NAME IXD_FTYPE IXD_MTYPE IXD_DIMS IXD_STACK

#if defined(IXD_NAME) && defined(IXD_FTYPE) && defined(IXD_MTYPE) && defined(IXD_DIMS) && defined(IXD_STACK)

subroutine push_memory&/**/
                      &IXD_NAME (array, external_ptr, fortran_alloc)
    implicit none
    IXD_FTYPE , target :: array( IXD_DIMS )
    integer(cpointer_t), intent(in) :: external_ptr
    logical, intent(in) :: fortran_alloc
    integer hash, c_hashmemory
    external c_hashmemory
    type(memory_item&/**/
                    &IXD_NAME ), pointer :: old_p, new_p
    hash = c_hashmemory(array, IXCmemory_buckets)
    IXD_STACK (hash)%count = IXD_STACK (hash)%count + 1
    allocate(new_p)
    old_p => IXD_STACK (hash)%head
    IXD_STACK (hash)%head => new_p
    new_p%info%fortran_alloc = fortran_alloc
    new_p%info%external_ptr = external_ptr
    new_p%array => array
    new_p%next => old_p
end subroutine

! as_result is true is the we want a pointer that will ultimately be returned 
! to the external program

function find_memory&/**/
	                &IXD_NAME (array, as_result, remove) result(memory_info)
    implicit none
    IXD_FTYPE , target :: array( IXD_DIMS )
    integer :: i, hash, c_hashmemory
    logical :: as_result, remove
       external c_hashmemory
	type(memory_item&/**/
                    &IXD_NAME ), pointer :: p, pp
    type(IXTmemory_info) :: memory_info
    memory_info%external_ptr = 0 ! return value if we find nothing
    memory_info%fortran_alloc = .false.
	hash = c_hashmemory(array, IXCmemory_buckets)
    p => IXD_STACK (hash)%head
    pp => NULL()    ! pointer to previous
    do while(associated(p))
        if (associated(p%array, array)) then
		    if (as_result) then
	            p%info%external_ptr = IXBexternalMakeResult(p%info%external_ptr, p%info%fortran_alloc)
                p%info%fortran_alloc = .true.
		    endif
! update pointer if we had to allocate some memory
            memory_info = p%info
	        if (remove) then 
	            if (associated(pp)) then
	                pp%next => p%next
	            else
	                IXD_STACK (hash)%head => p%next
	            endif
	            ! invalidate and remove item p
	            p%info%external_ptr = 0
	            p%array => NULL()
	            p%next => NULL()
                    deallocate(p)
                    IXD_STACK (hash)%count = IXD_STACK (hash)%count - 1
                endif
                return
        endif	                
	pp => p
	p => p%next
    enddo
end function

subroutine cleanup_&/**/
	               &IXD_NAME (status)
	implicit none
	character(len=128) :: message
	type(memory_item&/**/
                        &IXD_NAME ), pointer :: p, pn
	integer :: i, count, max_count
	type(IXTstatus) :: status
	max_count = 0
    do i=1,size( IXD_STACK )
	    count = 0
        p => IXD_STACK (i)%head
        if (associated(p)) then
            do while(associated(p))
                pn => p%next
                p%next => NULL()
! check if p%external_ptr still allocated? cannot just free though as it may be going back
! to matlab as a plhs so would have to mark all plhs somehow in the stack. Hmm.
                deallocate(p)
                p => pn             
				count = count + 1
            end do            
        endif
		if (count /= IXD_STACK (i)%count) then
		    call IXFadd_status(status, IXCfacility_memory, IXCseverity_error, &
		            IXCerr_outofmem, "memory stack cleanup error")
        endif
		max_count = max(count, max_count)
        IXD_STACK (i)%head => NULL()
        IXD_STACK (i)%count = 0
    end do
	if (max_count > 0) then
  		write(message,'(A,I9)') 'Max bucket size used = ', max_count
!		call IXFadd_status(status, IXCfacility_memory, IXCseverity_info, &
!		            IXCerr_outofmem, message)
	endif
end subroutine

! matlab arrays are always at least 2 dimensional (1 x nx)
!	dims(1) = 1
!	dims(2) = nx

subroutine allocDims&/**/
	                &IXD_NAME (array, dims_array, status)
    implicit none
	IXD_FTYPE , pointer :: array( IXD_DIMS )
	type(IXTstatus) :: status
    integer(cpointer_t) :: array_ptr
    integer(cpointer_t) :: mxarray_ptr
	integer :: ndims, dims_array(:)
    character(len=256) :: message
	ndims = size(dims_array)
    mxarray_ptr = IXBallocArrayDescriptor(ndims, dims_array, IXD_MTYPE )
    if (mxarray_ptr == 0) then
        array => NULL()
        write(message, '(A,(10I5))') 'IXFallocDims: out of memory when allocating array dimensions ', dims_array
	    call IXFadd_status(status, IXCfacility_memory, IXCseverity_error, &
		            IXCerr_outofmem, message)
        return
    endif
    array_ptr = IXBgetArrayData(mxarray_ptr)
    if (array_ptr == 0) then
        array => NULL()
	    call IXFadd_status(status, IXCfacility_memory, IXCseverity_error, &
		            IXCerr_outofmem, 'Invalid array descriptor in IXFallocDims')
        return
    endif
    call associate_array(array, array_ptr, dims_array, ndims)
    call push_memory(array, mxarray_ptr, .true.)
end subroutine

subroutine reallocDims&/**/
	                  &IXD_NAME (array, dims_array, preserve_contents, status)
    implicit none
	IXD_FTYPE , pointer :: array( IXD_DIMS )
	IXD_FTYPE_TEMP , pointer :: new_array( IXD_DIMS )
	IXD_FTYPE_TEMP :: pad_array(1)
	type(IXTstatus) :: status
	logical preserve_contents
	integer :: ndims, dims_array(:)
	new_array => NULL()
    pad_array = IXD_NULL
	if (.not. associated(array)) then
	    call IXFallocDims(array, dims_array, status)
	    return
	endif
	if (sum(abs(shape(array) - dims_array)) == 0) return ! nothing to do
    if (preserve_contents) then
	    call IXFallocDims(new_array, dims_array, status)
! need to copy old contents across
            if (associated(new_array)) new_array = reshape(array, shape(new_array), pad_array)
	    call IXFdealloc(array, status)
	    array => new_array
	else
        call IXFdealloc(array, status)
	    call IXFallocDims(array, dims_array, status)
	endif
end subroutine

subroutine copyToVector&/**/
	                   &IXD_NAME (source, dest, n, status)
    implicit none
	integer, intent(in) :: n
	IXD_FTYPE :: source( IXD_DIMS )
	IXD_FTYPE :: dest(n)
	type(IXTstatus) :: status
	if (size(source) == n) then
		dest = pack(source, .true.)
	else
		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, &
		            IXCerr_outofmem, "copyToVector array size invalid")
	endif
end subroutine

subroutine copyToVectorC&/**/
	                   &IXD_NAME (array, ptr, n, status)
    implicit none
	integer, intent(in) :: n
	IXD_FTYPE :: array( IXD_DIMS )
	integer(cpointer_t) :: ptr 
    IXD_FTYPE_TEMP , pointer :: dest(:)
	type(IXTstatus) :: status
        call associate_array(dest, ptr, (/ n /), 1)
        call IXFcopyToVector(array, dest, n, status)
end subroutine

subroutine copyFromVector&/**/
	                     &IXD_NAME (source, dest, n, status)
    implicit none
	integer, intent(in) :: n
	IXD_FTYPE :: dest ( IXD_DIMS )
	IXD_FTYPE :: source(n)
	type(IXTstatus) :: status
	if (size(dest) == n) then
	    dest = reshape(source, shape(dest))
	else
		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, &
		            IXCerr_outofmem, "copyFromVector array size invalid")
	endif
end subroutine

subroutine copyFromVectorC&/**/
	                     &IXD_NAME (ptr, array, n, status)
    implicit none
	integer, intent(in) :: n
	IXD_FTYPE :: array ( IXD_DIMS )
	integer(cpointer_t) :: ptr
    IXD_FTYPE_TEMP , pointer :: source(:)
	type(IXTstatus) :: status
    call associate_array(source, ptr, (/ n /), 1)
	call IXFcopyFromVector(source, array, n, status)
end subroutine

subroutine free_array_&/**/
                      &IXD_NAME (array, status)
	implicit none
	type(IXTmemory_info) :: memory_info
	IXD_FTYPE , pointer :: array ( IXD_DIMS )
	type(IXTstatus) :: status
	if (associated(array)) then
		memory_info = find_memory(array,.false.,.true.)
#ifdef LIBISIS_DEBUG
        array = IXD_NULL ! overwrite array contents as extra check
#endif /* LIBISIS_DEBUG */
        if (memory_info%fortran_alloc) call IXBdeallocArrayDescriptor(memory_info%external_ptr)
    endif
    array => NULL()
end subroutine

subroutine free_fortran_array_&/**/
                              &IXD_NAME (array, status)
	implicit none
	integer :: istat
	IXD_FTYPE , allocatable :: array ( IXD_DIMS )
	type(IXTstatus) :: status
	if (allocated(array)) then
#ifdef LIBISIS_DEBUG
	    array = IXD_NULL ! overwrite array contents as extra check
#endif /* LIBISIS_DEBUG */
	    deallocate(array,stat=istat)
	    if (istat /= 0) then
		    call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, &
		        IXCerr_outofmem, 'IXMmemory(free_fortran_array): Failed to deallocate array')
	    endif
	endif
end subroutine

! These implement the IXFAllocdimsFortran interface

  subroutine allocdims_fortran_array_&/**/
                                 &IXD_NAME (array, dims_array, status)
    implicit none
	integer :: istat
	type(IXTstatus) :: status
	IXD_FTYPE , allocatable :: array ( IXD_DIMS )
    integer :: dims_array(:)
    if (allocated(array)) then
#ifdef LIBISIS_DEBUG
	    array = IXD_NULL
#endif /* LIBISIS_DEBUG */
        deallocate(array,stat=istat)
!		call IXFadd_status(status, IXCfacility_memory, IXCseverity_error, &
!		       IXCerr_outofmem, 'IXMmemory(alloc_fortran_array): cannot alloc() - array already allocated')
!       return
    endif
#if IXD_NDIMS == 1
	allocate(array(dims_array(1)),stat=istat)
#elif IXD_NDIMS == 2
	allocate(array(dims_array(1),dims_array(2)),stat=istat)
#elif IXD_NDIMS == 3
	allocate(array(dims_array(1),dims_array(2),dims_array(3)),stat=istat)
#elif IXD_NDIMS == 4
	allocate(array(dims_array(1),dims_array(2),dims_array(3),dims_array(4)),stat=istat)
#endif
	    if (istat /= 0) then
		    call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, &
		       IXCerr_outofmem, 'IXMmemory(alloc_fortran_array): Failed to allocate array')
	    endif
  end subroutine

! These implement the IXFAllocdimsFortran interface

  subroutine alloc_fortran_array_&/**/
                                 &IXD_NAME (array, &
#if IXD_NDIMS == 1
 &n1, &
#elif IXD_NDIMS == 2
 &n1, n2, &
#elif IXD_NDIMS == 3
 &n1, n2, n3, &
#elif IXD_NDIMS == 4
 &n1, n2, n3, n4, &
#endif
 &status)
    implicit none
	type(IXTstatus) :: status
	IXD_FTYPE , allocatable :: array ( IXD_DIMS )
    integer :: n1, n2, n3, n4,dims_array( IXD_NDIMS )

	dims_array(1) = n1
#if IXD_NDIMS > 1
	dims_array(2) = n2
#endif
#if IXD_NDIMS > 2
	dims_array(3) = n3
#endif
#if IXD_NDIMS > 3
	dims_array(4) = n4
#endif
    call IXFAllocdimsFortran(array,dims_array,status)

  end subroutine

  subroutine realloc_fortran_array_&/**/
                                 &IXD_NAME (array, &
#if IXD_NDIMS == 1
      &n1, &
#elif IXD_NDIMS == 2
      &n1, n2, &
#elif IXD_NDIMS == 3
      &n1, n2, n3, &
#elif IXD_NDIMS == 4
      &n1, n2, n3, n4, &
#endif
      &preserve_contents, status)
    implicit none
	type(IXTstatus) :: status
	IXD_FTYPE , allocatable :: array ( IXD_DIMS )
    integer :: n1,n2,n3,n4,dims_array( IXD_NDIMS )
	logical:: preserve_contents

	dims_array(1) = n1
#if IXD_NDIMS > 1
	dims_array(2) = n2
#endif
#if IXD_NDIMS > 2
	dims_array(3) = n3
#endif
#if IXD_NDIMS > 3
	dims_array(4) = n4
#endif
    call IXFReallocdimsFortran(array,dims_array,preserve_contents,status)

  end subroutine

!  subroutine alloc_fortran_dp_array1(array, dims_array, status)
!   use IXMstatus
!    implicit none
!	type(IXTstatus) :: status
!    real(dp), allocatable :: array(:)
!    character(len=256) :: message
!    integer :: i, dims_array(:), istat
!	if (size(dims_array) /= 1) then
!		write(message,'(a,5i9)') 'Failed to allocate 1D real array as dimensionality = ', size(dims_array)
!		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, message)
!		return
!    endif
!	if (allocated(array)) then
!		write(message,'(a,5i9)') '1D real array already allocated shape = ', shape(array)
!		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, message)
!		return
!	endif
!	allocate(array(dims_array(1)),stat=istat)
!	if (istat /= 0) then
!		write(message,'(a,5i9)') 'Failed to allocate 1D real array dimensions: ', dims_array
!		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, message)
!		return
!	endif
!  end subroutine

! These implement the IXFreallocdimsFortran interface

  subroutine reallocdims_fortran_array_&/**/
                                   &IXD_NAME (array, dims_array, preserve_contents, status)
    implicit none
	type(IXTstatus) :: status
	IXD_FTYPE , allocatable :: array ( IXD_DIMS )
	IXD_FTYPE_TEMP , allocatable :: old_array( IXD_DIMS )
    IXD_FTYPE_TEMP :: pad_array(1)
    logical preserve_contents
    character(len=256) :: message
    integer :: i, dims_array(:), istat
    pad_array = IXD_NULL
	if ( size(dims_array) /= IXD_NDIMS ) then
		write(message,'(a,5i9)') 'IXMmemory: realloc_fortran_array failed as dimensionality = ', size(dims_array)
		call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, IXCerr_outofmem, message)
		return
    endif
	if (allocated(array)) then
          if (sum(abs(shape(array) - dims_array)) == 0) return
          if (preserve_contents) then
            call IXFallocdimsFortran(old_array, shape(array), status)
            old_array = array
            call IXFdeallocFortran(array, status)
            call IXFallocdimsFortran(array, dims_array, status)
            array = reshape(old_array, shape(array), pad_array)
            call IXFdeallocFortran(old_array, status)
          else
            call IXFdeallocFortran(array, status)
            call IXFallocdimsFortran(array, dims_array, status)
          endif
        else
          call IXFallocdimsFortran(array, dims_array, status)
	endif
  end subroutine

! These implement the IXFAlloc interface

  subroutine alloc_array_&/**/
                         &IXD_NAME (array, &
#if IXD_NDIMS == 1
     &n1, &
#elif IXD_NDIMS == 2
     &n1, n2, &
#elif IXD_NDIMS == 3
     &n1, n2, n3, &
#elif IXD_NDIMS == 4
     &n1, n2, n3, n4, &
#endif
     &status)
    implicit none
	type(IXTstatus) :: status
	IXD_FTYPE , pointer :: array ( IXD_DIMS )
    integer :: n1, n2, n3, n4, dims_array( IXD_NDIMS )
	dims_array(1) = n1
#if IXD_NDIMS > 1
	dims_array(2) = n2
#endif
#if IXD_NDIMS > 2
	dims_array(3) = n3
#endif
#if IXD_NDIMS > 3
	dims_array(4) = n4
#endif
    call IXFAllocDims(array, dims_array, status)
  end subroutine

! These implement the IXFrealloc interface

  subroutine realloc_array_&/**/
                           &IXD_NAME (array, &
#if IXD_NDIMS == 1
      &n1, &
#elif IXD_NDIMS == 2
      &n1, n2, &
#elif IXD_NDIMS == 3
      &n1, n2, n3, &
#elif IXD_NDIMS == 4
      &n1, n2, n3, n4, &
#endif
      &preserve_contents, status)
    implicit none
	logical:: preserve_contents
	type(IXTstatus) :: status
	IXD_FTYPE , pointer :: array ( IXD_DIMS )
    integer :: n1, n2, n3, n4, dims_array( IXD_NDIMS )
	dims_array(1) = n1
#if IXD_NDIMS > 1
	dims_array(2) = n2
#endif
#if IXD_NDIMS > 2
	dims_array(3) = n3
#endif
#if IXD_NDIMS > 3
	dims_array(4) = n4
#endif
    call IXFreallocDims(array, dims_array, preserve_contents, status)
  end subroutine

! These implement the associate_array interface

#if IXD_NDIMS == 1
#define ASSOCIATE_N     n1
#define ASSOCIATE_DIMS  dims_array(1)
#endif
#if IXD_NDIMS == 2
#define ASSOCIATE_N     n1, n2
#define ASSOCIATE_DIMS  dims_array(1), dims_array(2)
#endif
#if IXD_NDIMS == 3
#define ASSOCIATE_N     n1, n2, n3
#define ASSOCIATE_DIMS  dims_array(1), dims_array(2), dims_array(3)
#endif
#if IXD_NDIMS == 4
#define ASSOCIATE_N     n1, n2, n3, n4
#define ASSOCIATE_DIMS  dims_array(1), dims_array(2), dims_array(3), dims_array(4)
#endif

  subroutine associate_array_&/**/
                             &IXD_NAME (value, external_ptr, dims_array, ndims)
     use IXMtype_definitions
	implicit none
	IXD_FTYPE , pointer :: value ( IXD_DIMS )
    integer(cpointer_t) :: external_ptr
    integer :: ndims, dims_array(ndims)
    interface
      subroutine associate_x_array_&/**/
                           &IXD_NAME (value, value_ptr, ASSOCIATE_N )
         use IXMtype_definitions
         implicit none
         integer  :: ASSOCIATE_N
	     IXD_FTYPE , pointer :: value ( IXD_DIMS )
         integer(cpointer_t) :: value_ptr
      end subroutine
    end interface
 
    call associate_x_array_&/**/
                           &IXD_NAME (value, %val(external_ptr), ASSOCIATE_DIMS )
  end subroutine

#undef ASSOCIATE_N
#undef ASSOCIATE_DIMS

#undef IXD_STACK
#undef IXD_NAME
#undef IXD_FTYPE
#undef IXD_FTYPE_TEMP
#undef IXD_DIMS
#undef IXD_NDIMS
#undef IXD_MTYPE
#undef IXD_NULL

#endif /* defined(IXD_NAME) && defined(IXD_FTYPE) && defined(IXD_MTYPE) && defined(IXD_DIMS) && defined(IXD_STACK) */
