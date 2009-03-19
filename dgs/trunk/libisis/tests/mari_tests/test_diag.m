%required parameters
mapdir='../mari_files';maskdir='../mari_files';datadir='../mari_files';
nexdir='../mari_files';
% sets mari specific paths and constants
setup_mari_inst(datadir,mapdir,nexdir,maskdir);
DSOc=setup_mari_data_source;
% monochromatic data source
DSOw=setup_mari_data_source;
 % whitebeam data source
%%%%%
DSOc=add_chopinst(DSOc);
%%%%%
disp('------');
disp('whitebeam vanadium');
disp('-------');
% integrate everything bw 1000,2000 musec for whitebeam
wb_int=[20,40];
wb_units='e   ';
% populate whitebeam runfile
DSOw=add_diffinst(DSOw);
ei_wb='white';
DSOw=add_rawfile(DSOw,'inst_data:::MAR11060.RAW');
wbrf1= homer(DSOw,'ei',ei_wb,'det_units',wb_units,'d_int',wb_int,'normalisation',1,'range',[1000,2000],'scale',1000,'nchunk',100,'noback');
%wbrf2= homer(DSOw,'MAR','12042','ei',ei_wb,'det_units',wb_units,'d_int',wb_int,'normalisation',1,'range',[1000,2000],'scale',1000);
rf_ZC1=homer(DSOw,'11001','ei',ei_wb,'d_int',[NaN('double'),NaN('double')]);

inst='MAR';
run_no='11001';
% background integration
d_int=[18000,19500];
%%%%
range=[1000,2000];
fixei=12.98;
%scale=1000000; %% units of 1e6 counts
%%%%
DSOc=add_item(DSOc,'inst_nxs:::fermi_chopper.nxs','fermi_chopper','sloppy');
disp('****** monitor 1 b/w 1000-2000** with no background subtraction*');
% m1b= homer(DSOc,inst,run_no,'fixei',fixei,'d_int',d_int,...
%      'normalisation',1,'range',range,'scale',1.8182e8);
monosam= homer(DSOc,run_no,'fixei',fixei,'d_int',d_int,'nchunk',100);
 
[mask,cause]=diagnose(monosam,wbrf1,'vv_lo',0.1,'vv_hi',3.0,'sv_hi',5.0,'s_zero',false,'mess','rf_ZC',rf_ZC);
 