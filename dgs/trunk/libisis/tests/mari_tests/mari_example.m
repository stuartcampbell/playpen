%% sets mari specific paths and constants
mapdir='../mari_files';maskdir='../mari_files';datadir={'../mari_files','../mari_tests'};
nexdir='../mari_files';
setup_mari_inst(datadir,mapdir,nexdir,maskdir);
setup_mari_homer_defaults;
setup_mari_diag_defaults;
dso=setup_mari_data_source;
%%%%%%%
%% whitebeam population
% whitebeam is ALWAYS a 1:1 mapping, NEVER use a detector map
% MAR11060.RAW is the vanadium white beam data, it must be stored in the
% 'inst_data:::' path
wbrf=white_beam(dso,11060);

%% mask creation using diagnose
% uses wbrf as input

sam_mask=diagnose(dso,'11001',wbrf,'sv_hi',5.0,'bmin',18000,'bmax',19500,'out_nex','inst_masks:::m11001.nxs');

% VMS equivalents
% 'vv_lo' is /VLOW
% 'vv_hi' is /VHIGH
% 'sv_hi' is /FACTOR
% 'r'     is /STABILITY
% a second vanadium run can be populated into a runfile and provided with
% the 'rf_V2' keyword

% sam_mask is an IXTmask object containing a list of masked spectra, it can
% also be output as a .msk or .nxs file if specified, for example:
% 'out_nex','inst_masks:::msk11001.nxs'
% path prefix CANNOT be used for ascii output
% 'out_asc','mar11001.msk'
%% normalisation
% by default normalisation is using an integral of monitor 1 between
% 1000-2000 musecs. Different normalisation parameters can be provided:
% 'norm','uahr' -> divides by total microamps for run
% 'norm', 1, 'range',[500,1500]  -> normalises by integral of monitor 1 between 500 & 1500 ms
% 'norm', 2, 'range', 'peak'           ->      normalises by area of monitor 2 peak


%% monochromatic vanadium population
% MAR11015.RAW is the monochromatic vanadium data, it must be stored in the
% 'inst_data::' path
run_no='11015';
mv_map='inst_maps:::mari_res.map';
% the 'det_map' argument can be created on the fly as an IXTmap object
%the 'det_mask' keyword will accept an IXTmask object or file specification
%mv_mask='inst_masks:::mar11001.msk';

% ei can be calculated for a run using the 'ei' keyword or fixed using the 
% 'fixei' keyword
ei_guess=11

monvan=mono_van(dso,'sloppy',run_no,'ei',ei_guess,'noback','det_map',mv_map,'det_mask',sam_mask,...
     'solid',wbrf,'corr','abs',1,'mass',32.58,'RMM',50.9415);

%% calculate absolute scaling factor from monochromatic Vanadium

val_abs=monovan_abs(monvan)

%% populate and normalize dataset

% MAR11001.RAW is the monochromatic sample data, it must be stored in the
% 'inst_data:::' path
run_no='11001';
d_rebin=[-11.0, 0.05, 11.0];
% if no output rebin is specified a default range is determined accordingly:
% d_rebin=[-ei/2.0, ei/200.0, 0.95*ei ]

%the 'det_mask' keyword will accept an IXTmask object or file specification
%dmask='inst_masks:::mar11001.msk';
dmap='inst_maps:::mari_res.map';
% ei can be calculated for a run using the 'ei' keyword or fixed using the 
% 'fixei' keyword
fixei=12.98;
ei_guess=11;

%%  a more basic output runfile

rfout=mono_sample(dso,'a',run_no,'ei',ei_guess,'det_map',dmap,'det_mask',sam_mask,...
    'solid',wbrf,'corr','noback');

%%  a fully absolute units corrected output runfile
final= mono_sample(dso,'sloppy',run_no,'fixei',fixei,'d_rebin',d_rebin,'det_map',dmap,'det_mask',sam_mask,...
     'solid',wbrf,'corr','abs',val_abs,'mass',10.0,'RMM',435.96,'noback');

%% creation of spe file  
 runfile2spe(final,'mar11001_full.spe');

 runfile2spe(rfout,'mar11001.spe');
 
