
#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_TYPE_TEMP)&& defined(IXD_DIMS) && defined(IXD_NXTYPE)

    subroutine IXBfileRead&/**/
                          &IXD_NAME (fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    IXD_TYPE :: value ( IXD_DIMS )
    IXD_TYPE_TEMP :: pad_value
    IXD_TYPE_TEMP , pointer :: nx_value ( IXD_DIMS )
    integer ::  stat
    type(NXhandle) :: handle
!    call IXFwrite_line(' IXBfileRead array field '//name, status)
    handle = transfer(fio%file_id, handle)
    nx_value => NULL()
    pad_value = IXCundef_char
    stat = NXUreaddata(handle, name, nx_value) ! ignore units
    call nexus_error(stat, 'Error in IXBfileRead of '''//trim(name)//'''', status)
    value = reshape(nx_value, shape(value), (/ pad_value /) )
    deallocate(nx_value)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
    end subroutine

    subroutine IXBfileReadAlloc&/**/
                               &IXD_NAME (fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    IXD_TYPE , allocatable :: value ( IXD_DIMS )
    IXD_TYPE_TEMP , pointer :: nx_value ( IXD_DIMS )
    integer :: stat
    type(NXhandle) :: handle
!    call IXFwrite_line(' IXBfileReadAlloc array field '//name, status)
    handle = transfer(fio%file_id, handle)
    nx_value => NULL()
    stat = NXUreaddata(handle, name, nx_value) ! ignore units
    call nexus_error(stat, 'Error in IXBfileReadAlloc of '''//trim(name)//'''', status)
    call IXFreallocdimsFortran(value, shape(nx_value), .false., status)
    value = nx_value
    deallocate(nx_value)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
    end subroutine

    subroutine IXBfileReadPtr&/**/
                             &IXD_NAME (fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    IXD_TYPE , pointer :: value ( IXD_DIMS )
    integer :: stat
    type(NXhandle) :: handle
!    call IXFwrite_line(' IXBfileReadPtr array field '//name, status)
    handle = transfer(fio%file_id, handle)
    stat = NXUreaddata(handle, name, value) ! ignore units
    call nexus_error(stat, 'Error in IXBfileReadPtr of '''//trim(name)//'''', status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
    end subroutine

    subroutine IXBfileWrite&/**/
                           &IXD_NAME (fio, name, value, status)
    implicit none
    type(IXTfileio) :: fio
    character(len=*) :: name
    type(IXTstatus) :: status
    integer :: stat
    type(NXhandle) :: handle
    IXD_TYPE :: value ( IXD_DIMS )
!    call IXFwrite_line(' IXBfileWrite array field '//name, status)
    handle = transfer(fio%file_id, handle)
    stat = NXUwritedata(handle, name, value)
    call nexus_error(stat, 'Error in IXBfileWrite of '''//trim(name)//'''', status)
    fio%file_id = transfer(handle, fio%file_id, size(fio%file_id))
    end subroutine

#undef IXD_NAME
#undef IXD_TYPE
#undef IXD_TYPE_TEMP
#undef IXD_DIMS
#undef IXD_NXTYPE

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_TYPE_TEMP)&& defined(IXD_DIMS) && defined(IXD_NXTYPE) */
