function setup_mari_inst(data_dir,map_dir,nex_dir,mask_dir)
%there also might be a function to make standard constants between
%matlab/fortran
% common_startup()
%these two paths MUST be set
% inst_maps  defines where appropriate getei_m.MAP is situated as well as
% other system maps
% inst_nxs defines nexus files for populating the runfile structure
% inst_data defines default area for raw files if you are going to use the
%'run_no' arguments to homer, other paths can be used if the rawfile
%is defined in the data_source

%CLEAR previous data_source information
ixf_global_var('data_source','remove');
ixf_global_var('homer','remove');

delgpath('inst_maps')
delgpath('inst_nxs')
delgpath('inst_data')
delgpath('inst_masks')
if iscellstr(data_dir)
    mkgpath('inst_data',data_dir{:});
else
    mkgpath('inst_data',data_dir);
end
if iscellstr(nex_dir)
    mkgpath('inst_nxs',nex_dir{:});
else
    mkgpath('inst_nxs',nex_dir);
end
if iscellstr(mask_dir)
    mkgpath('inst_masks',mask_dir{:});
else
    mkgpath('inst_masks',mask_dir);
end
if iscellstr(map_dir)
    mkgpath('inst_maps',map_dir{:});
else
    mkgpath('inst_maps',map_dir);
end

% set new data_source information
ixf_global_var('data_source','set','INST','HET');
ixf_global_var('data_source','set','ext','RAW');
ixf_global_var('data_source','set','path','inst_data:::');
ixf_global_var('data_source','set','sav_prefix','s');

