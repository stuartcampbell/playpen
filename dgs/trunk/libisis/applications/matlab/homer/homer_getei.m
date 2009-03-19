function  ei = homer_getei (ei_guess,rawpath,mon_map)
% this function appropriately fills a cut down IXTrunfile using a specific map for the
% monitors, which in turn is used to determine ei
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

%rawfile=strcat(parameters{2},parameters{3},'.RAW');
if(size(rawpath,1) > 1)
    for i=1:size(rawpath,1)
        dso=add_item(dso,rawpath(i,:),'rawfile_mult');
    end
else
    dso=add_item(dso,rawpath,'rawfile');
end
dso=add_item(dso,'DEFINITION','diff_inst');
dso=add_item(dso,'inst_nxs:::source.nxs','source');
dso=add_item(dso,'inst_nxs:::moderator.nxs','moderator');
if ~isempty(mon_map)
    dso=add_item(dso,mon_map,'monmapfile');
end
% read monitor index file into a mask object and pass values to getei
% function
mon_specs=fileread(IXTmask,'inst_maps:::getei_m_index.dat');
% null input arguments are always required
m_rebin=[];
m_axis=IXTaxis;
ei=[];
% for the moment period is defined as 1
rf=populate_mon(rf,dso,int32(1),opt,ei,m_axis,m_rebin);
% give it raw indices for now
ei=getei(rf,ei_guess,mon_specs.mask_array)

