program dataset1d_test
use IXMlibcore
use IXMdataset_1d
use IXMraw_file
use IXMunits
implicit none

type(IXTdataset_1d)::d1d
type(IXTraw_file)::rawfile
type(IXTstatus)::status
integer(i4b)::emode
real(dp)::theta,delay,efixed,L1,L2
type(IXTunits)::units_test


write(6,*) 'Starting dataset_1d units change test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif
emode=0
theta=45.5d0
efixed=50.0d0
delay=0.008d0
L1=11.0d0
L2=3.0d0
call IXFcreate(units_test,IXCenergy,IXCwnumber,status)

call IXFopen_raw('mar09948.raw', rawfile, status)
call IXFget_raw(rawfile, 25 , d1d, status)
call IXFunits_dataset_1d(d1d,status,emode,efixed,d1d%x_distribution,L1,L2,theta,delay,units_test)

!call IXFunits_x_dataset_2d(d2d,arrayd2d,status,emode,efixed,L1,L2,theta,delay,units_test)

call IXFlibrary_finish(status)
pause 'press any key to finish'

end

