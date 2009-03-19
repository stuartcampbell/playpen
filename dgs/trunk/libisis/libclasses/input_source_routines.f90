
!#define IXD_NAME		i4b1
!#define IXD_TYPE		integer(i4b)
!#define IXD_DIMS		:   (optional)
                       
#if defined(IXD_NAME) && defined(IXD_TYPE)

  subroutine get_&/**/
                 &IXD_NAME (handle, item_name, value, status)
    implicit none
    type(IXTinput_source) :: handle
    type(IXTisis_raw_file) :: rf
    type(IXTstatus), target :: status
    character(len=*) :: item_name
#ifdef IXD_DIMS
    IXD_TYPE :: value ( IXD_DIMS )
#else
    IXD_TYPE :: value
#endif
    call IXFopen_raw(handle%sources(1), rf, status)
    call IXFget_raw(rf, item_name, value, status)
  end subroutine
  
#undef IXD_NAME
#undef IXD_TYPE
#undef IXD_DIMS

#endif /* defined(IXD_NAME) && defined(IXD_TYPE) && defined(IXD_DIMS) */
