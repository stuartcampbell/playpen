#if defined(IXD_NAME) && defined(IXD_CNAME)
    call IXFfile_open(fio,'IXT'//IXD_CNAME//'.xml',IXC_CREATEXML,status)
    call IXFfile_write(IXD_NAME,fio,IXD_CNAME,status)
    call IXFfile_close(fio,status)
    call IXFfile_open(fio,'IXT'//IXD_CNAME//'.nxs',IXC_CREATE,status)
    call IXFfile_write(IXD_NAME,fio,IXD_CNAME,status)
    call IXFfile_close(fio,status)
#undef IXD_NAME
#undef IXD_CNAME
#endif
