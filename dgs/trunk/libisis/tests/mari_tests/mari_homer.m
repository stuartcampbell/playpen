%% sets mari specific paths and constants
mapdir='../mari_files';maskdir='../mari_files';datadir='../mari_files';
nexdir='../mari_files';

setup_mari_inst(datadir,mapdir,nexdir,maskdir);
setup_mari_homer_defaults;
setup_mari_diag_defaults;
dso=setup_mari_data_source;
%% Whitebeam vanadium (sample) population

wbrf=white_beam(dso,'11060');

% wbrf is an IXTrunfile object
disp('---whitebeam vanadium---')

%% diag on monochromatic sample run
% sample mask
% ... ,'sv_hi',5.0,... 
% overrides default value of sv_hi (/FACTOR in VMS)
% ...,'bmin',18000,'bmax',19500,...
% overrides default values for bmin/bmax of 12000,18000
sam_mask=diagnose(dso,'11001',wbrf,'sv_hi',5.0,'bmin',18000,'bmax',19500,'out_asc','inst_masks:::m11001.msk'); %output diag results to a file, 
% sam_mask is an IXTmask object
disp('---sample diag---')
%% monochromatic vanadium population

mv_map='inst_maps:::mari_res.map';
mv_mask='inst_masks:::mar11001.msk'; %sam_mask input could also be used

% there is no need to run diagnose on the monochromatic vanadium run since
% the same wiring has been used for the sample run and the same detector
% should be masked

monvan=mono_van(dso,'sloppy','11015','ei',12,'noback','det_map',mv_map,'det_mask',mv_mask,...
     'solid',wbrf,'corr','abs',1,'mass',32.58,'RMM',50.9415);
% monvan is an IXTrunfile object
 disp('---monovan population---')
 
%% calculate absolute scaling factor from monochromatic Vanadium run

val_abs=monovan_abs(monvan);

%% monochromatic sample population

run_no='11001';
% multiple run numbers can be added together, in this case they are defined
% in a cell array of strings
% run_no={'run1' 'run2' 'run3' ...}

d_rebin=[-11.0, 0.05, 11.0];  % output rebinning in energy transfer
dmask='inst_masks:::m11001.msk'; %sam_mask input could also be used

%map file to be used, if it is not defined then 1:1 map will be
%used, input can also be an IXTmap object
dmap='inst_maps:::mari_res.map';

% a background subtraction can be defined using
% ...,'background',[12000,18000],...

% the instrument prefix has been defined by the setup_maps_inst.m script,
% so only the run number needs to be given
fixei=get_ei('11001',15);

% the ei can be fixed using 'fixei' or be determined from a guess value
% using 'ei'

mari_final= mono_sample(dso,'sloppy',run_no,'ei',13,'d_rebin',d_rebin,'det_map',dmap,'det_mask',dmask,...
     'solid',wbrf,'corr','abs',val_abs,'mass',10.0,'RMM',435.96,'noback');
% mari_final is an IXTrunfile object
disp('---sample population---')
%% convert runfile output to spe file

runfile2spe(mari_final,'inst_data:::mar11001.spe');
