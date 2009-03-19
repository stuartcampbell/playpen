mapdir='./';
maskdir='./';
datadir='./';
nexdir='./';
setup_merlin_inst(datadir,mapdir,nexdir,maskdir);
setup_merlin_homer_defaults;
setup_merlin_diag_defaults;
dso=setup_merlin_data_source;
%%%%

%dso=add_item(dso,'inst_nxs:::rings_detinfo.nxs','detector','fulldet');
%dso=add_item(dso,'inst_nxs:::rings_detinfo.nxs','spectra');
%d_map='inst_maps:::rings_map.dat';
d_map='inst_maps:::rings_map.map';
m_map='inst_maps:::merlin_monitors.map';

wbrf=white_beam(dso,'00579');

sam_mask=diagnose(dso,'00603',wbrf);

% you can specify a detector mask or use the output from diag
%%%
  d_rebin=[-10,0.1,35];
% 
  merlin=mono_sample(dso,'sloppy','00603','ei',450,'d_rebin',d_rebin,'det_mask',sam_mask,...
      'noback','solid',wbrf,'mon_map',m_map,'det_map',d_map);
% 
 %da(merlin.det_data.datasets)
