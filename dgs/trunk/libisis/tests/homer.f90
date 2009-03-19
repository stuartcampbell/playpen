program homer_test
use IXMlibcore
use IXMraw_file
use IXMaxis
use IXMrunfile
use IXMmask
use IXMdata_source
use IXMbase
use IXMoptions
 !this should include all components for the time being
implicit none
type(IXTrunfile)::rf_wb
type(IXTstatus)::status
type(IXTaxis)::d_axis
type(IXTmask)::mask
real(dp)::d_int(2),eival
type(IXTdata_source)::dso_wb
type(IXTraw_file)::raw_file
integer::period,monei_info(3)=(/ 2,3,0 /)


write(6,*) 'Starting HOMER test  program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

period=1

call IXFadditem_data_source(dso_wb,'./mari_files/MAR11060.RAW',IXCrawfile,'undefined',status)
call IXFadditem_data_source(dso_wb,'DEFINITION',IXCdiff_inst,'undefined',status)
call IXFadditem_data_source(dso_wb,'./mari_files/source.nxs','source','undefined',status)
call IXFadditem_data_source(dso_wb,'./mari_files/moderator.nxs','moderator','undefined',status)
call IXFadditem_data_source(dso_wb,'./mari_files/fermi_chopper.nxs','fermi_chopper','undefined',status)
call IXFadditem_data_source(dso_wb,'./mari_files/mari_res.map','detmapfile','undefined',status)




!call IXFgetei_runfile(rf1,eival,status)
!can use eival with populate if you wnat to change units

call IXFcreate_code_axis(d_axis,'e   ',status)
d_int(1)=20
d_int(2)=40

!moni/detec multi
call IXFpopulate_runfile(rf_wb,status,dso_wb,period,(/ 100, 500 /),monei_info,d_axis=d_axis,i_lim=d_int)





call IXFlibrary_finish(status)
write(6,*)'Ending HOMER test Program'
!pause 'press any key to finish'
!*********************************************

end 

