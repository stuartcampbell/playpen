program bindings_test
use IXMtype_definitions
use IXMlibcore
use IXMdataset_1d
use IXMdataset_2d
use IXMdatum
use IXMisis_raw_file
use IXMrunfile
use IXMgroups
implicit none
type(IXTstatus) :: status
type(IXTrunfile) :: rf
write(6,*) 'Starting bindings test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

call IXFlibrary_finish(status)
pause 'press any key to finish'

end
