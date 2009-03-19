! IXD_NAME IXD_TYPE IXD_FORMAT 
 
#if defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_FORMAT)

 subroutine runOperationPrint1&/**/
                              &IXD_NAME (name,value,level,status)
    implicit none
    character(len=2640) :: buffer  ! need to handle at least 10*long_len strings
	character(len=*) :: name
	IXD_TYPE :: value(:)
	integer :: lb, level, i, j, istat
    type(IXTstatus) :: status
    buffer = ' '
	if (name /= ' ') then
		call makeArrayNameString(buffer, name, shape(value))
	    lb = len_trim(buffer)
        call IXFwrite_line_indent(buffer(:lb),level,'-',status)
    endif
    if (size(value) > 1) return
	j=0
    do while(j < min(size(value), 10))
         buffer = ' '
	     write(buffer, IXD_FORMAT, iostat=istat) (value(j+i),i=1,min(IXD_NUMPRINT,size(value)-j))
         if (istat /= 0) buffer = 'Overflow in runOperationPrint1'
		 lb = len_trim(buffer)
		 if (lb == 0) lb = 1
         call IXFwrite_line_indent(buffer(:lb),level,' ',status)
         j = j + IXD_NUMPRINT
	enddo
end subroutine

subroutine runOperationPrint2&/**/
                             &IXD_NAME (name,value,level,status)
    implicit none
    character(len=1320) :: buffer
	character(len=*) :: name
	IXD_TYPE :: value(:,:)
	integer :: lb, level, i
    type(IXTstatus) :: status
	if (name /= ' ') then
		call makeArrayNameString(buffer, name, shape(value))
		lb = len_trim(buffer)
        call IXFwrite_line_indent(buffer(:lb),level,'-',status)
    endif
    if (size(value) > 1) return
    do i=1,min(size(value,1),10)
		call IXFoperationPrint(' ',value(i,:),level,status)
	enddo
end subroutine

subroutine runOperationPrint3&/**/
                              &IXD_NAME (name,value,level,status)
    implicit none
    character(len=1320) :: buffer
	character(len=*) :: name
	IXD_TYPE :: value(:,:,:)
	integer :: lb, level, i
    type(IXTstatus) :: status
	if (name /= ' ') then
		call makeArrayNameString(buffer, name, shape(value))
		lb = len_trim(buffer)
        call IXFwrite_line_indent(buffer(:lb),level,'-',status)
    endif
    if (size(value) > 1) return
    do i=1,min(size(value,3),10)
		call IXFoperationPrint(' ',value(:,:,i),level,status)
	enddo
end subroutine

subroutine runOperationPrint4&/**/
                              &IXD_NAME (name,value,level,status)
    implicit none
    character(len=1320) :: buffer
	character(len=*) :: name
	IXD_TYPE :: value(:,:,:,:)
	integer :: lb, level, i
    type(IXTstatus) :: status
	if (name /= ' ') then
		call makeArrayNameString(buffer, name, shape(value))
		lb = len_trim(buffer)
        call IXFwrite_line_indent(buffer(:lb),level,'-',status)
    endif
    if (size(value) > 1) return
    do i=1,min(size(value,4),10)
		call IXFoperationPrint(' ',value(:,:,:,i),level,status)
	enddo
end subroutine

#undef IXD_NAME 
#undef IXD_TYPE
#undef IXD_FORMAT
#undef IXD_NUMPRINT

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_FORMAT) */
