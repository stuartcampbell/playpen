function  [varargout] = get_ei(run_numbers,varargin)
% [ei_out,properties] = get_ei( 'run_number',ei_guess)
%
% or to calculate an ei from a combined set of runs
%
% ei_out = get_ei(ei_guess, 'run_no1', 'run_no2',...)
%
% this function appropriately fills a cut down IXTrunfile (using a specific
% map for the monitors, if defined in the instrument setup script), 
% which in turn is used to determine ei
%
% properties is an optional output containing all the peripheral output
% from the get_ei command ie peak position, area etc...
%
% run numbers can be defnied in the following ways:
% 1) a numerical array of numbers -> [12345  45673 34436] or a single
% numeric
% 2) an array of strings -> char('12345' '45673' '34436') or a single
% string
% 3) a cell array of strings -> {'12345' '45673' '34436'}
% 4) a cell array of numerics -> {12345  45673 34436}
% 5) or a path to a particular raw file -> 'C:\data\MAR11060.RAW' or a cell
% array of paths
%
%
% See also mget_ei
rf=IXTrunfile;
opt=IXToptions;
dso=IXTdata_source;

% options object to do nothing to data
opt.base=IXTbase('optbase',true);
opt.bgrd=false;
opt.m_axis=false;
opt.m_rebin=false;
opt.d_axis=false;
opt.d_rebin=false;
opt.ei=false;

[rawpath,pathlen]=make_path_from_par({run_numbers});
%rawfile=strcat(parameters{2},parameters{3},'.RAW');
%    add new path info to dso
if(pathlen > 1)
    for i=1:pathlen
        dso=add_item(dso,rawpath{i},'rawfile_mult');
    end
else
    dso=add_item(dso,rawpath{1},'rawfile');
end
dso=add_item(dso,'DEFINITION','diff_inst');
dso=add_item(dso,'inst_nxs:::source.nxs','source');
dso=add_item(dso,'inst_nxs:::moderator.nxs','moderator');

monitor_map=ixf_global_var('data_source','get','monitor_map'); %
if ~isempty (monitor_map)
    dso=add_item(dso,monitor_map,'monmapfile');
end
% read monitor index file into a mask object and pass values to getei
% function
mon_specs=fileread(IXTmask,'inst_maps:::getei_m_index.dat');
% null input arguments are always required
m_rebin=[];
m_axis=IXTaxis;
ei=[];
% for the moment period is defined as 1, and would conceivably be the same
% for all periods in the same raw file
rf=populate_mon(rf,dso,int32(1),opt,ei,m_axis,m_rebin);
if nargin == 2
    ei_guess = varargin{1};
else
    ei_guess = -1;
end
% give it raw indices for now
[ei_out,ei_props]=getei(rf,ei_guess,mon_specs.mask_array);
ei_extras.peak(1)=ei_props(1);
ei_extras.area(1)=ei_props(2);
ei_extras.area_norm(1)=ei_props(3);
ei_extras.peak(2)=ei_props(4);
ei_extras.area(2)=ei_props(5);
ei_extras.area_norm(2)=ei_props(6);

if nargout~=0
    varargout(1)={ei_out};
    varargout(2)={ei_extras};
end