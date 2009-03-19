function rf=mon (dso,varargin)
% runfile = mon(dso,varargin)
% populate an IXTrunfile object with the monitor data only, it accepts
% the following keyword/argument pairs
% 'period', 'mon_map','mon_mask','m_rebin'
%%%%%%
arglist = struct(... % argument names and default values
    'period',1,...
    'mon_map','',...
    'mon_mask','',...
    'm_rebin',[]);
[par,argout,present] = parse_arguments(varargin,arglist);

[present,argout] = getdefaults(present,argout);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rf=IXTrunfile;
rf=create_population_command(rf,inputname(1),mfilename('fullpath'),mfilename,par,argout,present);

% if there are run_numbers defined in the par cell array create entries in
% the dso
dso=create_dso_from_par(dso,par);


nexmonmap=0;
nexmonmask=0;

if(present.mon_map);
    [dso,nexmonmap]=map_prep(dso,'monmapfile','monmap.nxs',argout.mon_map);
end
if(present.mon_mask)    
    [dso,nexmonmask]=mask_prep(dso,'monmaskfile','monmask.nxs',argout.mon_mask);
end
% we will define the runfile as a diffraction instrument, and you may be
% able to do diffraction related units changes which don't require L2 or ei
dso=add_diffinst(dso);
% no normalisation performed, simply populates the mon_data object
rf=mon_pop(rf,dso,argout,present);

if(nexmonmap)
    delete monmap.nxs;
    dso=del_item(dso,'monmapfile');        
end
if(nexmonmask)
    delete monmask.nxs;
    dso=del_item(dso,'monmaskfile');        
end


