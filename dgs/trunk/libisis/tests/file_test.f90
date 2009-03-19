program file_test
use IXMtype_definitions
use IXMlibcore
use IXMdataset_1d
use IXMdataset_2d
use IXMdatum
use IXMraw_file
use IXMrunfile
use IXMgroups
type(IXTstatus) :: status
type(IXTdataset_1d) :: w1a, w1b,w1d, wres, d1d_array(4)
type(IXTdataset_2d) :: w2d,wr2d,wrb2d,wrg2d
    type(IXToperation)::op
logical::xhist,xdist,ydist,yhist
type(IXTraw_file) :: raw_file
type(IXTfileio) :: fio
type(IXTrunfile) :: rf
type(IXTgroups) :: ca
type(IXTmoderator) :: mod
integer :: vali(1), ntimechan, nspec

call IXFfile_open(fio,'test.nxs',IXC_CREATE,status)
call IXFfile_write(mod,fio,'moderator',status)
call IXFfile_close(fio,status)
pause
call IXFfile_open(fio,'test.nxs',IXC_READ,status)
call IXFfile_read(mod,fio,'moderator',status)
call IXFfile_close(fio,status)
pause
end
