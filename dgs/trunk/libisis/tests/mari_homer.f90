program homer_test
use IXMlibcore
use IXMrunfile
use IXMmask
use IXMmap
 !this should include all components for the time being
implicit none
type(IXTrunfile)::rf_wb,rf_sam,rf_ZC,rf_run,rf_v2
type(IXTstatus)::status
type(IXTaxis)::d_axis,m_axis
type(IXTmask)::mask,hmask
real(dp)::d_int(2),energy,rebin_array(3)
type(IXTdata_source)::dso
integer::period,monei_info(3)=(/ 2,3,0 /)
character(len=long_len)::dirs(1)
real(dp),allocatable::o_value(:),o_error(:)
integer(i4b),allocatable::o_cause(:),o_file(:)
real(dp),pointer::bgrd(:),m_rebin(:),dp_rebin(:),dp_int(:)
type(IXTfileio)::fio
type(IXToptions)::opt
type(IXTmap)::bank
character(21)::sample_number,white_beam_number
character(21)::detmap

rebin_array=(/-11.0, 0.05, 11.0 /)
energy=12.0
white_beam_number='mydata:::MAR11060.RAW'
sample_number='mydata:::MAR11001.RAW'
detmap='mymaps:::mari_res.map'
dirs(1)='./mari_files/'

bgrd=>NULL()
m_rebin=>NULL()
dp_rebin=>NULL()
dp_int=>NULL()

write(6,*) 'Starting Mari HOMER test  program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

period=1

call IXFaddpath('mydata',dirs,status)
call IXFaddpath('mymaps',dirs,status)
call IXFaddpath('myfiles',dirs,status)

call IXFadditem_data_source(dso,white_beam_number,IXCrawfile,'undefined',status)
call IXFadditem_data_source(dso,'DEFINITION',IXCdiff_inst,'undefined',status)
call IXFadditem_data_source(dso,'myfiles:::source.nxs','source','undefined',status)
call IXFadditem_data_source(dso,'myfiles:::moderator.nxs','moderator','undefined',status)
call IXFcreate_code_axis(d_axis,'$e',status)
d_int(1)=20
d_int(2)=40

! m_axis is a null  an empty object

call IXFcreate_options(opt,.false.,.false.,.false.,.true.,.true.,.false.,.false.,status)
!moni/detec multi
call IXFpopulate_runfile(rf_wb,status,dso,period,(/ 100, 500 /),(/ 0 , 0, 0 /),d_axis=d_axis,i_lim=d_int, m_axis=m_axis,opt=opt,bgrd=bgrd,m_rebin=m_rebin,d_rebin=dp_rebin)
call IXFmon_norm_runfile(rf_wb,1_i4b,(/ 1000.0_dp, 2000.0_dp /),1000.0_dp,status)
call IXFdelitem_data_source(dso,IXCrawfile,status)

 

call IXFadditem_data_source(dso,sample_number,IXCrawfile,'undefined',status)
d_int(1)=5
d_int(2)=20000
call IXFcreate_options(opt,.false.,.false.,.false.,.true.,.false.,.false.,.false.,status)

call IXFpopulate_runfile(rf_ZC,status,dso,period,(/ 100, 500 /),(/ 2 , 3, 1 /),i_lim=d_int, m_axis=m_axis,opt=opt,bgrd=bgrd,m_rebin=m_rebin,d_rebin=dp_rebin)




d_int(1)=18000
d_int(2)=19500
!call IXFpopulate_runfile(rf_run,status,dso,period,(/ 100, 500 /),monei_info,i_lim=d_int)
call IXFpopulate_runfile(rf_run,status,dso,period,(/ 100, 500 /),(/ 2 , 3, 1 /),i_lim=d_int, m_axis=m_axis,opt=opt,bgrd=bgrd,m_rebin=m_rebin,d_rebin=dp_rebin)
call IXFdelitem_data_source(dso,IXCdiff_inst,status)

call IXFdiag_runfile(rf_run,rf_wb,1e-10_dp,1e10_dp,.false.,0.0_dp,100.0_dp,  0.0_dp,1.5_dp,3.3_dp,&
	.true., 0.01_dp,100.0_dp, 0.1_dp,2.0_dp, 0.0_dp ,&
	mask, o_cause,o_value,o_error,o_file,10.0_dp,0.0_dp,status,rf_ZC=rf_ZC,bank=bank,rf_v2=rf_v2,hmask=hmask)



      call IXFfile_open(fio,'mymaps:::mask.nxs',IXC_CREATE,status)
!in the future named source, at the moment source MUST have a name in xml/nexus format
!call IXFfile_read(source,fio,'*',status)  
!IXD_SQTYPE is source but could be dso%detail
      call IXFfile_write(mask,fio,'mask',status)
      call IXFfile_close(fio,status)

  
call IXFcreate_code_axis(d_axis,'$w',status)
 call IXFcreate_options(opt,.false.,.false.,.false.,.false.,.true.,.true.,.true.,status)
call IXFadditem_data_source(dso,detmap,'detmapfile','undefined',status)
call IXFadditem_data_source(dso,'mymaps:::mask.nxs','detmaskfile','undefined',status)
call IXFadditem_data_source(dso,'myfiles:::fermi_chopper.nxs','fermi_chopper','sloppy',status)
call IXFadditem_data_source(dso,'DEFINITION',IXCchop_inst,'undefined',status)

call IXFpopulate_runfile(rf_sam,status,dso,period,(/ 100, 500 /),(/ 2 , 3, 1 /),ei=(/ energy,0.0_dp /),d_axis=d_axis,d_rebin=rebin_array, &
     opt=opt,m_axis=m_axis,bgrd=bgrd,i_lim=dp_int,m_rebin=m_rebin)



call IXFmon_norm_runfile(rf_sam,1_i4b,(/ 1000.0_dp, 2000.0_dp /),170160000.0_dp,status)
call IXFsolid_runfile(rf_sam,dso,rf_wb,status)

!call IXFcorr_runfile(rf_sam,status)


      call IXFfile_open(fio,'mymaps:::runfile_out.nxs',IXC_CREATE,status)
!in the future named source, at the moment source MUST have a name in xml/nexus format
!call IXFfile_read(source,fio,'*',status)  
!IXD_SQTYPE is source but could be dso%detail
      call IXFfile_write(rf_sam,fio,'runfile',status)
      call IXFfile_close(fio,status)


call IXFdestroy_runfile(rf_sam,status)
call IXFdestroy_runfile(rf_wb,status)
call IXFdestroy_runfile(rf_ZC,status)
call IXFdestroy_mask(mask,status)
call IXFdestroy_axis(d_axis,status)
call IXFdestroy_data_source(dso,status)

call IXFlibrary_finish(status)
write(6,*)'Ending HOMER test Program'
pause 'press any key to finish'
!*********************************************

end 

