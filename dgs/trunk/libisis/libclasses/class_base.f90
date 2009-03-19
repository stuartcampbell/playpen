! $Id: class_base.f90 1386 2008-05-20 08:26:17Z Dickon Champion $
! IXD_TYPE
! IXD_SQTYPE

#if defined(IXD_TYPE) && defined(IXD_SQTYPE)

#ifndef IXD_NO_BASE

!> function to determine validity of an object
pure function IXFvalid_&/**/
                     &IXD_TYPE (w1) result(valid)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(in) ::w1 !< input object
	logical :: valid !< result
	valid = IXFvalid_base(w1%base)
end function
!> function to determine validity of an array of objects, if one object is invalid,
! then whole array is deemed invalid
pure function IXFvalid_array_&/**/
                              &IXD_TYPE (dt) result(valid)
    implicit none
    integer :: i
    logical :: valid !< result
    type(IXT&/**/
            &IXD_TYPE),intent(in) :: dt(:)
    valid = .false.
    do i =1,size(dt)
        valid=IXFvalid_base(dt(i)%base)
        if(valid .eqv. .false.)return
    enddo
  end function

!> private subroutine which sets an object as valid  
  subroutine IXFmark_valid_&/**/
                     &IXD_TYPE (w1)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(inout) ::w1 !< input object
	call IXFmark_valid_base(w1%base)
  end subroutine

!> private subroutine which sets an object as invalid  
  subroutine IXFclear_valid_&/**/
                     &IXD_TYPE (w1)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(inout) ::w1 !< input object
	call IXFclear_valid_base(w1%base) 
  end subroutine
  
!> private subroutine which sets an array of objects as valid    
  subroutine IXFmark_valid_array_&/**/
                     &IXD_TYPE (w1)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(inout) ::w1(:) !< input array of objects
    integer::i    	        
    do i=1,size(w1)
      call IXFmark_valid_base(w1(i)%base)
    enddo
  end subroutine

!> private subroutine which sets an array of objects as invalid    
  subroutine IXFclear_valid_array_&/**/
                     &IXD_TYPE (w1)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(inout) ::w1(:)  !< input array of objects
    integer::i
    do i=1,size(w1)
      call IXFclear_valid_base(w1(i)%base)
    enddo
  end subroutine

pure function IXFinitialised_&/**/
                     &IXD_TYPE (w1) result(init)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE ), intent(in) ::w1
	logical :: init
	init = IXFinitialised_base(w1%base)
end function

pure function IXFinitialised_array_&/**/
                              &IXD_TYPE (dt) result(init)
    implicit none
    integer :: i
    logical :: init
    type(IXT&/**/
            &IXD_TYPE),intent(in) :: dt(:)
    init = .false.
    do i =1,size(dt)
        init=IXFinitialised_base(dt(i)%base)
        if(init .eqv. .false.)return
    enddo
  end function
  
  subroutine IXFinitialise_&/**/
                           &IXD_TYPE (dt, s)
    implicit none
    type(IXT&/**/
            &IXD_TYPE) :: dt
    type(IXTstatus) :: s
    type(IXToperation)::op
!    if(IXFinitialised(dt))call IXFdestroy(dt,status)
    call IXFoperationMake(op,IXTop_init(0),s)
    call IXFoperation_run(op,' ',dt,s)
    call IXFoperationCleanup(op, s)
! The operationRun on IXTbase will set the initialised flag on 
! that member, which is equivalent to doing a IXFmark_init on the object 
  end subroutine

  subroutine IXFinitialise_array_&/**/
                                 &IXD_TYPE (dt, s)
    implicit none
    integer :: i
    type(IXT&/**/
            &IXD_TYPE) :: dt(:)
    type(IXTstatus) :: s
    do i = 1, size(dt)
       call IXFinitialise(dt(i), s)
    enddo
  end subroutine

!> subroutine which is called to allocate an array of objects
  subroutine IXFalloc_&/**/
                      &IXD_TYPE (dt, n, status)
    implicit none
    integer :: n !< length of array to be allocated
    integer :: istat
    character(len=256) :: buffer
    type(IXT&/**/
            &IXD_TYPE), allocatable :: dt(:) !< array of objects
    type(IXTstatus) :: status !< error status object
    allocate(dt(n), stat=istat)
    if (istat /= 0) then
       write(buffer,'(A,I8,A,I3,A)') &
            'IXFalloc error ('//IXD_SQTYPE//', size = ', n, &
            ', allocate stat = ', istat, ')'
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, buffer)
    endif
    call IXFclear_valid(dt)
  end subroutine
  
!> subroutine which is called to reallocate an array of objects  
  subroutine IXFrealloc_&/**/
                      &IXD_TYPE (dt, n, preserve, status)
    implicit none
    integer :: n !< length of array to be allocated
    integer:: istat,i
    logical :: preserve !< argument defining if exsting data in object is to be kept
    type(IXT&/**/
            &IXD_TYPE), allocatable :: dt(:) !< array of objects to be reallocated
    type(IXT&/**/
            &IXD_TYPE), allocatable :: existing(:)            
    type(IXTstatus) :: status !< error status object
    
    if (allocated(dt))then
      if(size(dt) .eq. n ) then
        if (.not. preserve)call IXFdestroy(dt,status)
        return
      endif
      
      if (preserve) then
        allocate(existing(size(dt)),stat=istat)
        if (istat /= 0) then
           call IXFadd_status(status, IXCfacility_memory, IXCseverity_fatal, &
                IXCerr_outofmem, 'Failed to allocate array(IXFrealloc)')
           return
        endif

          do i=1,size(dt)
            call IXFcopy(dt(i),existing(i),status)
          enddo
        call IXFdealloc(dt,status)
        call IXFalloc(dt,n,status)

        do i=1,min(n,size(existing))
          call IXFcopy(existing(i),dt(i),status)
        enddo
          
        call IXFdestroy(existing,status)
        deallocate(existing)
      else
        call IXFdealloc(dt,status)
        call IXFalloc(dt,n,status)
      endif
    else
      call IXFalloc(dt,n,status)
    endif      
  end subroutine  
    
#endif /* ndef IXD_NO_BASE */

!> subroutine to copy the contents of one object into another object
subroutine IXFcopy_&/**/
                        &IXD_TYPE (orig, copy ,status)
    implicit none
    type(IXT&/**/
            &IXD_TYPE):: orig !> object to be copied (original)
    type(IXT&/**/
            &IXD_TYPE):: copy !> copy of object (resultant)
    type(IXTstatus) :: status !< error status object
#ifndef IXD_NO_BASE
    if(IXFvalid(orig))call IXFset_&/**/
                       &IXD_TYPE(copy,status,ref=orig)
#else
    call IXFset_&/**/
                       &IXD_TYPE(copy,status,ref=orig)
#endif                                              
! IXFset does not copy base over anymore, so it needs to be copied separately                       
#ifndef IXD_NO_BASE
    call IXFset_base(copy%base,status,ref=orig%base)
#endif                       
end subroutine

!> subroutine which will call standard object check on an object and if it passes will
! then mark the object as valid
subroutine IXFcheck_and_valid_&/**/
                        &IXD_TYPE (arg,s)
  implicit none
  type(IXT&/**/
            &IXD_TYPE):: arg !< object to be checked
  type(IXTstatus)::s !< error status object
  call IXFcheck(arg,s)
  if (s == IXCseverity_error) return
#ifndef IXD_NO_BASE
  call IXFmark_valid(arg) 
#endif
end subroutine

!> subroutine which will call standard object check on an array of object and if it passes will
! then mark the object array as valid
subroutine IXFcheckarray_and_valid_&/**/
                        &IXD_TYPE (arg,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::arg(:) !< object array to be checked
	type(IXTstatus)::s !< error status object
	integer :: i
	do i = 1, size(arg)
	    call IXFcheck(arg(i), s)
        if (s == IXCseverity_error) return
#ifndef IXD_NO_BASE
        call IXFmark_valid(arg(i)) 
#endif	    
	end do
end subroutine



#ifndef IXD_NO_DSO

  subroutine IXFpopulate_file_dso_&/**/
                       &IXD_TYPE (w1,dso,s)
    use IXMdata_source
    implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1
	type(IXTstatus)::s
    type(IXTdata_source)::dso
    character(len=long_len)::fpath,dumpath
	type(IXTfileio) :: fio
    logical::found
    found=.false.
!item in dso was path to 'source.xml' or 'source.nxs' and datatype is defined as IXTsource
! IXD_SQTYPE is 'source'
    call IXFfindpath_data_source (dso,IXD_SQTYPE,fpath,dumpath,found,s)
    if(found)then
      call IXFfile_open(fio,fpath,IXC_READ,s)
!in the future named source, at the moment source MUST have a name in xml/nexus format
!call IXFfile_read(source,fio,'*',status)  
!IXD_SQTYPE is source but could be dso%detail
      call IXFfile_read(w1,fio,IXD_SQTYPE,s)
      call IXFfile_close(fio,s)  
      call IXFcheck(w1,s)
    else
      call IXFadd_status(s, IXCfacility_libisis, IXCseverity_warning, &
       IXCerr_filenotfound,'IXT'//IXD_SQTYPE//' file not defined in data_source (IXFpopulate_file_'//IXD_SQTYPE//')')
    endif  
  end subroutine 
#endif /* ndef IXD_NO_DSO */

#ifndef IXD_NO_DSO
  subroutine IXFpopulate_file_&/**/
                       &IXD_TYPE (w1,fpath,s,object_name)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1
    character(len=*),intent(in)::fpath
	type(IXTstatus)::s
	character(len=*),optional,intent(in)::object_name
	type(IXTfileio) :: fio

    call IXFfile_open(fio,fpath,IXC_READ,s)
!in the future named source, at the moment source MUST have a name in xml/nexus format
!call IXFfile_read(source,fio,'*',status)
    if(present(object_name))then
      call IXFfile_read(w1,fio,object_name,s)
    else
      call IXFfile_read(w1,fio,IXD_SQTYPE,s)
    endif
    call IXFfile_close(fio,s)  
    call IXFcheck(w1,s)
  end subroutine 
#endif /* ndef IXD_NO_DSO */


subroutine IXFdisplay&/**/
                     &IXD_TYPE (w1,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1
	type(IXTstatus)::s
	type(IXToperation)::op
	call IXFoperationMake(op,IXTop_display('IXT'//IXD_SQTYPE//' class' ),s)
	call IXFoperation_run(op,' ',w1,s)
    call IXFoperationCleanup(op, s)
end subroutine

subroutine IXFdisplayArray&/**/
                          &IXD_TYPE (w1,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1(:)
	type(IXTstatus)::s
	integer :: i
	if (size(w1) > 5) then
       call IXFadd_status(s, IXCfacility_libisis, IXCseverity_info, &
            IXCerr_outofmem, 'Array size > 5; only displaying first 5 entries')
	endif
	do i = 1, min(size(w1), 5)
	    call IXFdisplay(w1(i), s)
	end do
end subroutine

subroutine IXFcheckArray&/**/
                        &IXD_TYPE (w1,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1(:)
	type(IXTstatus)::s
	integer :: i
	do i = 1, size(w1)
	    call IXFcheck(w1(i), s)
	end do
end subroutine


subroutine wrap_&/**/
                &IXD_TYPE (var, wrapped_var, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ), target :: var
	type(IXTwrapped_var) :: wrapped_var
	type(IXTstatus) :: status
!	type(IXTwrapped_object) :: wrapped_object
	type(IXPT&/**/
	         &IXD_TYPE ) :: pvar
	pvar%ptr => var
! comment out - breaks if structure contains allocatable arrays
!    wrapped_object = transfer(pvar, wrapped_object)
!    call IXFwrap_var(wrapped_object, wrapped_var, status)
end subroutine

subroutine unwrap_&/**/
                  &IXD_TYPE (wrapped_var, var, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ) :: var
	type(IXTwrapped_var) :: wrapped_var
	type(IXTstatus) :: status
	type(IXTwrapped_object) :: wrapped_object
!	type(IXPT&/**/
!	         &IXD_TYPE ) :: pvar
    call IXFunwrap_var(wrapped_var, wrapped_object, status)
! comment out - breaks if structure contains allocatable arrays
!	pvar = transfer(wrapped_object, pvar)
!	var = pvar%ptr
end subroutine

subroutine unwrap_ptr_&/**/
                      &IXD_TYPE (wrapped_var, var, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ), pointer :: var
	type(IXTwrapped_var) :: wrapped_var
	type(IXTstatus) :: status
	type(IXTwrapped_object) :: wrapped_object
	type(IXPT&/**/
	         &IXD_TYPE ) :: pvar
    call IXFunwrap_var(wrapped_var, wrapped_object, status)
! comment out - breaks if structure contains allocatable arrays
!    pvar = transfer(wrapped_object, pvar)
	var => pvar%ptr
end subroutine

subroutine gget_&/**/
                &IXD_TYPE (var, status, field1, value1, field2, value2, &
                           field3, value3, field4, value4)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ) :: var
    character(len=*) :: field1
	type(IXTwrapped_var), target :: value1
    character(len=*), optional :: field2
	type(IXTwrapped_var), optional, target :: value2
    character(len=*), optional :: field3
	type(IXTwrapped_var), optional, target :: value3
    character(len=*), optional :: field4
	type(IXTwrapped_var), optional, target :: value4
	type(IXTstatus) :: status
	type(IXToperation) :: op
	call IXFoperationMake(op, IXTop_get(value1, field1), status)
	call IXFoperation_run(op, ' ', var, status)
    call IXFoperationCleanup(op, status)
    if (present(field2) .and. present(value2)) then
	    call IXFoperationMake(op, IXTop_get(value2, field2), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
    if (present(field3) .and. present(value3)) then
	    call IXFoperationMake(op, IXTop_get(value3, field3), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
    if (present(field4) .and. present(value4)) then
	    call IXFoperationMake(op, IXTop_get(value4, field4), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
end subroutine

subroutine gset_&/**/
                &IXD_TYPE (var, status, field1, value1, field2, value2, &
                           field3, value3, field4, value4)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ) :: var
    character(len=*) :: field1
	type(IXTwrapped_var) :: value1
	type(IXTstatus) :: status
    character(len=*), optional :: field2
	type(IXTwrapped_var), optional :: value2
    character(len=*), optional :: field3
	type(IXTwrapped_var), optional :: value3
    character(len=*), optional :: field4
	type(IXTwrapped_var), optional :: value4
	type(IXToperation) :: op
	call IXFoperationMake(op, IXTop_set(value1, field1), status)
	call IXFoperation_run(op, ' ', var, status)
        call IXFoperationCleanup(op, status)
    if (present(field2) .and. present(value2)) then
	    call IXFoperationMake(op, IXTop_set(value2, field2), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
    if (present(field3) .and. present(value3)) then
	    call IXFoperationMake(op, IXTop_set(value3, field3), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
    if (present(field4) .and. present(value4)) then
	    call IXFoperationMake(op, IXTop_set(value4, field4), status)
	    call IXFoperation_run(op, ' ', var, status)
            call IXFoperationCleanup(op, status)
    endif
	call IXFcheck(var, status)
end subroutine

recursive subroutine IXFoperation_run_array_&/**/
                                         &IXD_TYPE (op, field, arg, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ) :: arg(:)
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    character(len=256) :: t_field
	integer :: i, n
	n = size(arg)
    call IXFoperationArrayInit(op, 'IXT'//IXD_SQTYPE, field, n, status)
!    call IXFoperationStart(op, field, status)
    if (IXFfile_op(op)) then
        do i = 1, min(n,size(arg))
            op%array_index(op%level) = i
            t_field = ' '
            write(t_field,fieldnameformat) field(1:len_trim(field)), i
	    call IXFoperation_run(op, t_field(1:len_trim(t_field)), arg(i), status)
	enddo
    else
        call IXFoperationStart(op, IXD_SQTYPE, field, status)
        do i = 1, min(n,size(arg))
            op%array_index(op%level) = i
	    call IXFoperation_run(op, ' ', arg(i), status)
        enddo
        call IXFoperationFinish(op, field, status)
    endif
!    call IXFoperationFinish(op, field, status)
end subroutine

recursive subroutine IXFoperation_run_array_alloc_&/**/
                                         &IXD_TYPE (op, field, arg, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ), allocatable :: arg(:)
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    character(len=256) :: t_field
    integer :: i, n
    if (associated(op%init)) then
!        call destroy if allocated?
        return
    endif
    if (allocated(arg)) then
        n = size(arg)
    else
        n = 0
    endif
    call IXFoperationArrayInit(op, 'IXT'//IXD_SQTYPE, field, n, status)
    if (n == 0) return
    if (.not. allocated(arg)) then
        allocate(arg(n))
    else if (n /= size(arg)) then
        deallocate(arg)
        allocate(arg(n))
    endif
    if (IXFfile_op(op)) then
        do i = 1, n
            op%array_index(op%level) = i
            t_field = ' '
            write(t_field,fieldnameformat) field(1:len_trim(field)), i
	    call IXFoperation_run(op, t_field(1:len_trim(t_field)), arg(i), status)
	enddo
    else
        call IXFoperationStart(op, IXD_SQTYPE, field, status)
        do i = 1, n
            op%array_index(op%level) = i
	    call IXFoperation_run(op, ' ', arg(i), status)
        enddo
        call IXFoperationFinish(op, field, status)
    endif
end subroutine

recursive subroutine IXFoperation_run_array_ptr_&/**/
                                            &IXD_TYPE (op, field, arg, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ), pointer :: arg(:)
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    character(len=256) :: t_field
    integer :: i, n
    if (associated(op%init)) then
!        call destroy if allocated?
        arg => NULL()
        return
    endif
    if (associated(arg)) then
        n = size(arg)
    else
        n = 0
    endif
    call IXFoperationArrayInit(op, 'IXT'//IXD_SQTYPE, field, n, status)
    if (n == 0) return
!    call IXFoperationStart(op, field, status)
    if (associated(arg)) then
        if (n /= size(arg)) then
!             deallocate(arg)
            allocate(arg(n))
        endif
    else
        allocate(arg(n))
    endif
    if (IXFfile_op(op)) then
        do i = 1, n
            op%array_index(op%level) = i
            t_field = ' '
            write(t_field,fieldnameformat) field(1:len_trim(field)), i
	    call IXFoperation_run(op, t_field(1:len_trim(t_field)), arg(i), status)
	enddo
    else
        call IXFoperationStart(op, IXD_SQTYPE, field, status)
        do i = 1, n
            op%array_index(op%level) = i
	    call IXFoperation_run(op, ' ', arg(i), status)
        enddo
        call IXFoperationFinish(op, field, status)
    endif
end subroutine

recursive subroutine IXFoperation_run_ptr_&/**/
                                       &IXD_TYPE (op, field, arg, status)
    implicit none
	type(IXT&/**/
	        &IXD_TYPE ), pointer :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    if (associated(op%init)) then
!        call destroy if allocated?
        arg => NULL()
        return
    endif
    if (associated(op%fileread) .and. (.not. associated(arg))) then
        allocate(arg)
    endif
    if (associated(op%matlabread) .and. (.not. associated(arg))) then
        allocate(arg)
    endif
    if (associated(arg)) then
        call IXFoperation_run(op, field, arg, status)
    endif
end subroutine

  subroutine IXFdealloc_&/**/
                        &IXD_TYPE (dt, status)
    implicit none
    integer :: istat
    type(IXT&/**/
            &IXD_TYPE), allocatable :: dt(:)
    type(IXTstatus) :: status
    istat = 0
    if (allocated(dt)) then
        call IXFdestroy(dt,status)
        deallocate(dt, stat=istat)
    endif
    if (istat /= 0) then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXFdealloc error ('//IXD_SQTYPE//')')
    endif
  end subroutine
  
  subroutine IXFdestroy_array_&/**/
                              &IXD_TYPE (dt, status)
    implicit none
    integer :: i
    type(IXT&/**/
            &IXD_TYPE) :: dt(:)
    type(IXTstatus) :: status
    do i =1,size(dt)
        call IXFdestroy(dt(i),status)
    enddo
  end subroutine

subroutine IXFcopyarray_&/**/
                        &IXD_TYPE (orig, copy ,status)
    implicit none
    integer:: i
    type(IXT&/**/
            &IXD_TYPE):: orig(:) ,copy(:)
    type(IXTstatus) :: status
    if(size(orig) /= size(copy))then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'original and copy array of structures must be of same length (IXFcopy)')
       return
    endif
       
    do i=1,size(orig)
       call IXFcopy(orig(i),copy(i),status)
    enddo
end subroutine
    
subroutine IXFfile_read_&/**/
                       &IXD_TYPE (w1,fio,path,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1
	type(IXTfileio) :: fio
	type(IXTstatus)::s
	type(IXToperation)::op
	character(len=*) :: path
	if (path .eq. ' ') path = IXD_SQTYPE
	call IXFoperationMake(op,IXTop_fileread(fio,path),s)
    call IXFoperationStart(op, 'IXT'//IXD_SQTYPE , path, s)
	call IXFoperation_run(op,' ',w1,s)
    call IXFoperationFinish(op, path, s)
    call IXFoperationCleanup(op, s)
end subroutine

subroutine IXFfile_write_&/**/
                       &IXD_TYPE (w1,fio,path,s)
	implicit none
	type(IXT&/**/
	        &IXD_TYPE )::w1
	type(IXTfileio) :: fio
	type(IXTstatus)::s
	type(IXToperation)::op
	character(len=*) :: path
	if (path .eq. ' ') path = IXD_SQTYPE
	call IXFoperationMake(op,IXTop_filewrite(fio,path),s)
    call IXFoperationStart(op, 'IXT'//IXD_SQTYPE , path, s)
	call IXFoperation_run(op,' ',w1,s)
    call IXFoperationFinish(op, path, s)
    call IXFoperationCleanup(op, s)
end subroutine

  subroutine get_&/**/
                 &IXD_TYPE (handle, item_name, value, status)
    implicit none
    type(IXTfileio) :: handle
    type(IXTstatus) :: status
    character(len=*) :: item_name
	type(IXT&/**/
        	&IXD_TYPE ) :: value
!    call IXFopen_raw(handle%sources(1), rf, status)
!    call IXFget_raw(rf, item_name, value, status)
  end subroutine
  
  subroutine get_array_&/**/
                 &IXD_TYPE (handle, item_name, value, status)
    implicit none
    type(IXTfileio) :: handle
    type(IXTstatus) :: status
    character(len=*) :: item_name
	type(IXT&/**/
        	&IXD_TYPE ) :: value(:)
!    call IXFopen_raw(handle%sources(1), rf, status)
!    call IXFget_raw(rf, item_name, value, status)
  end subroutine

  subroutine add_&/**/
                   &IXD_TYPE (list, value)
	implicit none
	type(IXTL&/**/
        	 &IXD_TYPE ) :: list
	type(IXT&/**/
        	&IXD_TYPE ) :: value
  end subroutine


#undef IXD_TYPE
#undef IXD_SQTYPE

#endif /* defined(IXD_TYPE) && defined(IXD_SQTYPE) */
