%% sets maps specific paths and constants
mapdir='../maps_files/';maskdir='../maps_files/';datadir='../maps_files/';
nexdir='../maps_files/';

setup_maps_inst(datadir,mapdir,nexdir,maskdir);
setup_maps_homer_defaults;
setup_maps_diag_defaults;
dso=setup_maps_data_source;
%%%%%
%% whitebeam vanadium (sample) population

wb_int=[20,300];

sam_wb= white_beam(dso,'10241','d_int',wb_int); %override default whitebeam integration limits
% sam_wb is an IXTrunfile object
disp('---whitebeam vanadium---')


%% diag on monochromatic sample run
% sample mask
sam_mask=diagnose(dso,'10266',sam_wb,'out_asc','inst_masks:::10266.msk'); %output diag results to a file, 
% sam_mask is an IXTmask object
disp('---sample diag---')

%% monochromatic vanadium population

mv_wb = white_beam(dso,'10298','d_int',wb_int); %again overriding default whitebeam integration limits, 
% mv_wb is an IXTrunfile object
disp('---mono vanadium---')

%% diag on monochromatic vanadium run
% 'nohard' and 'nobank' required to remove diag defaults for mid-tubes
% population

mv_mask=diagnose(dso,'10291',mv_wb,'nohard','nobank'); % mv_mask is an IXTmask object
disp('---mono diag---')


% the instrument prefix has been defined by the setup_maps_inst.m script,
% so only the run number needs to be given
ei_out=get_ei('10291',400);


mv_map='inst_maps:::mid_tubes.map';
% the homer command will also determine the ei implicitly if requested
monvan=mono_van(dso,'sloppy','10291','ei',400,'det_map',mv_map,'det_mask',mv_mask,...
    'solid',mv_wb,'corr',...
    'abs',1,'mass',30.1,'RMM',50.9415); %check mass vanadium
disp('---monovan population---')


% if a monochromatic vanadium has been performed with the same wiring
% configuration as the monochromatic sample, then you would reasonably
% expect to use the mask generated by diag on the mono sample run for the
% treatment of the mono van run, this is what the Homer GUI will do


%% calculate absolute scaling factor from monochromatic Vanadium run

val_abs=monovan_abs(monvan);

%% monochromatic sample population
run_no='10266'; 
% multiple run numbers can be added together, in this case they are defined
% in a cell array of strings
% run_no={'run1' 'run2' 'run3' ...}

d_rebin=[-50.0, 4.0, 380]; % output rebinning in energy transfer

% detector mask input can be from a .msk file, or from an IXTmask object
% determined using diagnose (see examples above)

%map file to be used, if it is not defined then 1:1 map will be
%used, input can also be an IXTmask object
dmap='inst_maps:::4to1.map'; 

% a background subtraction can be defined using
% ...,'background',[12000,18000],...

% the ei can be fixed using 'fixei' or be determined from a guess value
% using 'ei'
%%%
maps_final= mono_sample(dso,'sloppy',run_no,'ei',400,'d_rebin',d_rebin,'det_map',dmap,'det_mask',sam_mask,...
    'solid',sam_wb,'corr','abs',val_abs,'mass',166.0,'RMM',57.06);
%maps_final is an IXTrunfile object
disp('---sample population---')

%% convert runfile output to a spe file
runfile2spe(maps_final,'inst_data:::map10266.spe');
