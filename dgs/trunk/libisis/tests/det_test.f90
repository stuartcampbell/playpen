! this program will populate a whole runfile structure from component elements
! some subobjects will take default values and some are created

program detector_test
use IXMlibcore
use IXMrunfile
use IXMisis_raw_file
use IXMfileio

implicit none

character(len=long_len):: name
type(IXTrunfile)::runfile,runfilecopy,genie_in
type(IXTdetector),allocatable::effdet(:),realdet(:)
type(IXTdet_solid)::solid
type(IXTdet_he3)::he3
type(IXTstatus)::status
type(IXTsource)::source
type(IXTdataset_1d)::atten1d
type(IXTdataset_2d)::mon_data(1),det_data(1)
type(IXTisis_raw_file) :: raw_file
type(IXTattenuator)::attenuator(2)
type(IXTaperture)::aperture(1)
type(IXTmoderator)::mod
type(IXTspectra)::spectra
type(IXTfermi_chopper)::fc
type(IXTchopper_instrument)::het
type(IXTworkspace)::workspace1,workspace2
type(IXTeffdet_index)::effectivedetectorindex
type(IXTsample)::sample
type(IXTuser)::userlist(2)
type(IXTmonitor)::monitor(1)
type(IXTbridge)::monbridge,detbridge
type(IXTws_bridge)::ws_bridge
type(IXTsw_bridge)::sw_bridge
type(IXTfileio) :: fio
integer,allocatable :: spec_nums(:)
integer::nspec
real(dp)::array3(10),array4(10),array5(10),array6(10)
integer(i4b)::array1(10),array2(10),i,array7(10),array8(10),array9(10)
real(dp)::sd(5,3),ed(5,3),sm(5,1),em(5,1),x(5),yd(3),ym(1)


write(6,*) 'Starting dataset_1d test program'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif



! create datasets from raw file

call IXFopen_raw('mar09948.raw', raw_file, status)
call IXFget_raw(raw_file, 'NSP1', nspec, status)
!allocate (spec_nums(nspec))
allocate (spec_nums(40))
!spec_nums=(/(i,i=1,nspec)/)
spec_nums=(/(i,i=5,44)/)
call IXFget_raw(raw_file, spec_nums, det_data(1), status)
deallocate(spec_nums)
allocate (spec_nums(4))
spec_nums=(/(i,i=1,4)/)
call IXFget_raw(raw_file, spec_nums, mon_data(1), status)
! edit the datasets

call IXFset_dataset_2d(mon_data(1),status,title='monitordata')
call IXFset_dataset_2d(det_data(1),status,title='detectordata')

call random_number(ed)
call random_number(sd)
call random_number(sm)
call random_number(em)
call random_number(x)
call random_number(yd)
call random_number(ym)

!call IXFset_dataset_2d(mon_data(1),status,x=x,y=ym,signal=sm,error=em)
!call IXFset_dataset_2d(det_data(1),status,x=x,y=yd,signal=sd,error=ed)

!***********

array7=7_i4b
array2=2_i4b
array1=1_i4b
array9=9_i4b
array8=8_i4b
call random_number(array3)
call random_number(array4)
call random_number(array5)
call random_number(array6)


! create components of detector
allocate(effdet(1))
allocate(realdet(1))

call IXFcreate_det_he3(he3,array1,array3,array4,status)
call IXFcreate_det_solid(solid,array1,array4,status)
call IXFcreate_detector(effdet(1),array1,array2,array3,array4, &
     array5,array6,array3,array1,array2,he3,solid,array1,status)

! create another detector for an instrument to point at
call IXFcreate_detector(realdet(1),array2,array1,array3,array4, &
     array5,array6,array3,array1,array2,he3,solid,array1,status)
     
!call IXFdisplay(det1(1),status)
!call IXFdisplay(det2(1),status)    


! source from default
call IXFcheck_source(source,status)

! aperture from default

call IXFcheck(aperture(1),status)
call IXFset_aperture(aperture(1),status,name='first',distance=9.0_dp,shape='square',width=0.04_dp,height=0.04_dp)

!create components of moderator (the rest from default)
call IXFset_moderator(mod,status,pulse_pars=array3,name='water',height=0.115_dp,width=0.12_dp)

! create components of attenuator

call IXFset_dataset_1d(atten1d,status,title='first attenuation',x=array3,signal=array4,error=array5)
call IXFset_attenuator(attenuator(1),status,attenuation=atten1d,name='ISIS annual report 1991')

call IXFset_dataset_1d(atten1d,status,title='second attenuation')
call IXFset_attenuator(attenuator(2),status,attenuation=atten1d,name='ISIS annual report 1987')

! edit/create fermi chopper 
call IXFset_fermi_chopper(fc,status,name='sloppy',energy=50.0_dp,frequency=200.0_dp,distance=10.0_dp,height=0.064_dp,radius=1.3_dp,slit_width=0.00228_dp)

! create spectra 
call IXFcreate_spectra(spectra,array7,array1,array1,array1,status)     

! edit pieces of workspace
call IXFset_effdet_index(effectivedetectorindex,status,array1,array2)
! one for monitor_wokspace, one for detector_workspace
call IXFset_workspace(workspace1,status,array1,effectivedetectorindex,effdet(1))
call IXFset_workspace(workspace2,status,array1,effectivedetectorindex,effdet(1))

! give a different detector to one of the workspaces
call IXFset_workspace(workspace1,status,eff_det=realdet(1))

!user list

call IXFset_user(userlist(1),status,name='Toby Perring',email='t.g.perring@rl.ac.uk',fax='5720',telephone='5428',address='ISIS')
call IXFset_user(userlist(2),status,name='Gabriel Aeppli',telephone='0207 679 1234',fax='123456',affiliation='UCL',address='UCL Nanotechnology')

!the sample , mostly take defaults
call IXFset_sample(sample,status,name='holmium titanate',chemical_formula='ho2ti2o7')
!build instrument from parts
name='HET'
call IXFcreate_chopper_instrument(het,name,source,mod,fc,aperture,attenuator,spectra,realdet(1),status)

! edit/create monitor, moments component will create with default values

call IXFset_monitor(monitor(1),status,monitor_no=6_i4b,integral_units='bananas')

! edit/create bridges/sub-bridge, one for monitor, one for workspace

call IXFset_ws_bridge(ws_bridge,status,array1,array7,array2,array8,array9)
call IXFset_sw_bridge(sw_bridge,status,array1,array2,array8,array9)

call IXFcreate_bridge(monbridge,ws_bridge,sw_bridge,status)
call IXFcreate_bridge(detbridge,ws_bridge,sw_bridge,status)

! build a full runfile from component parts
call IXFset_runfile(runfile,status,inst=het,title='thunderbirds are GO runfile')
call IXFset_runfile(runfile,status,monitor_workspace=workspace1,detector_workspace=workspace2)
call IXFset_runfile(runfile,status,monitor_data=mon_data,detector_data=det_data)
call IXFset_runfile(runfile,status,users=userlist,sample=sample,monitors=monitor)
call IXFset_runfile(runfile,status,monitor_bridge=monbridge,detector_bridge=detbridge)

call IXFfile_open(fio,'c:\test.nxs',IXC_CREATE,status)
call IXFfile_write(runfile,fio,'rf',status)
call IXFfile_close(fio,status)
!call IXFdisplay(runfile,status)
pause

call IXFfile_open(fio,'c:\new.nxs',IXC_READ,status)!read in from genie
call IXFfile_read(genie_in,fio,'rf',status)
call IXFfile_close(fio,status)
call IXFcheck(genie_in, status)
!call IXFdisplay(runfile,status)

call IXFfile_open(fio,'c:\newout.nxs',IXC_CREATE,status)!now write out what you read in...
call IXFfile_write(genie_in,fio,'rf',status)!
call IXFfile_close(fio,status)


pause

call IXFdestroy(het,status)
call IXFdestroy(attenuator(1),status)
call IXFdestroy(attenuator(2),status)
call IXFdestroy(aperture(1),status)
call IXFdestroy(workspace1,status)
call IXFdestroy(workspace2,status)
call IXFdestroy(effectivedetectorindex,status)
call IXFdestroy(userlist,status)

!call IXFdisplay(det_data(1),status)
!call IXFdisplay(mon_data(1),status)    


call IXFcopy(runfile,runfilecopy,status)

call IXFdestroy(effdet(1),status)
call IXFdestroy(realdet(1),status)
     
call IXFlibrary_finish(status)
pause 'press any key to finish'

end program
