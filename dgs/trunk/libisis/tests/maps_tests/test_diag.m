mapdir='../maps_files/';maskdir='../maps_files/';datadir='../maps_files/';
nexdir='../maps_files/';
setup_maps_diag_defaults;
setup_maps_inst(datadir,mapdir,nexdir,maskdir);
%required parameters

monosam=read_nxs('monosam.nxs',IXTrunfile,'mono_sam');

%v1=read_nxs('wb1.nxs',IXTrunfile);
%zc=read_nxs('ZC.nxs',IXTrunfile);
v1=read_nxs('wbrf.nxs',IXTrunfile,'whitebeam');
v2=read_nxs('wbrf2.nxs',IXTrunfile,'whitebeam2');
% w1=read_ascii('v1.txt');
% w2=read_ascii('v2.txt');
% v1.det_data.datasets.signal=w1.signal;
% v2.det_data.datasets.signal=w2.signal;
% [mask,cause]=diag('monosam.nxs','wbrf.nxs','rfile_entry','mono_sam','sv_hi',3.5,'bank',maps_bank,'rf_v1_entry','whitebeam',...
%     'rf_v2_entry','whitebeam2','rf_v2','wbrf2.nxs',...
% 'hmask','4to1_022.msk',...
% 's_out_lo',1e-6,'s_out_hi',1e6,'v_out_lo',1e-6,'v_out_hi',1e6,'nomess','out_asc','map10266_newdiag.msk','out_nex','mask10266.nxs');
%[mask,cause]=diagnose(monosam,v1,'bank','maps_bank.nxs','hardmask','4to1_022.msk',...
%'s_out_lo',1e-10,'s_out_hi',1e10,'v_out_lo',1e-10,'v_out_hi',1e10,'nomess');
[mask,cause]=diagnose(monosam,v1,'rf_v2',v2,'nobank','nohard','nomess');