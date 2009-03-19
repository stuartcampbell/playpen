%% sets heti specific paths and constants
mapdir='../het_files';maskdir='../het_files';datadir='../het_files';
nexdir='../het_files';

setup_het_inst(datadir,mapdir,nexdir,maskdir);
setup_het_homer_defaults;
setup_het_diag_defaults;
dso=setup_het_data_source;

%% Whitebeam vanadium (sample) population

wbrf=white_beam(dso,'15869');
% wbrf is an IXTrunfile object
disp('---whitebeam vanadium---')


%% diag on monochromatic sample run
% sample mask
% ... ,'sv_hi',5.0,... 
% overrides default value of sv_hi (/FACTOR in VMS)
% ...,'bmin',18000,'bmax',19500,...
% overrides default values for bmin/bmax of 12000,18000
sam_mask=diagnose(dso,'15958',wbrf,'out_asc','inst_masks:::h15958.msk'); %output diag results to a file, 
% sam_mask is an IXTmask object
disp('---sample diag---')
%% monochromatic vanadium population

% for examples with monochromatic vanadium population see files
% mari_tests/mari_homer.m   or   maps_tests/maps_homer.m


%% monochromatic sample population

run_no='15958';
% multiple run numbers can be added together, in this case they are defined
% in a cell array of strings
% run_no={'run1' 'run2' 'run3' ...}
d_rebin=[-10.0, 1.0, 36]; % output rebinning in energy transfer
dmask='inst_masks:::h15958.msk'; %sam_mask input could also be used

%map file to be used, if it is not defined then 1:1 map will be
%used, input can also be an IXTmap object
dmap='inst_maps:::RINGS_HET.MAP';

% define background subtraction limits
bgrd=[12000,18000];

% the instrument prefix has been defined by the setup_maps_inst.m script,
% so only the run number needs to be given
fixei=get_ei('15958',40);

% the ei can be fixed using 'fixei' or be determined from a guess value
% using 'ei'

 het_final= mono_sample(dso,'sloppy',run_no,'fixei',fixei,'d_rebin',d_rebin,'det_map',dmap,'det_mask',dmask,...
     'solid',wbrf,'corr');
 % het_final is an IXTrunfile object
 disp('---sample population---')
%% convert runfile output to spe file

 runfile2spe(het_final,'inst_data:::het15958.spe');