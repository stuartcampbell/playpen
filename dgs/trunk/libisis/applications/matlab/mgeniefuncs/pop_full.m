function rf=pop_full (dso,varargin)
arglist = struct(... % argument names and default values
    'background',[], ...
    'det_map','', ...
    'det_mask','', ...
    'mon_map','',...
    'mon_mask','',...
    'm_rebin',[],...
    'period',1,...
    'd_rebin',[],...
    'd_int',[]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parsing of input arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[par,argout,present] = parse_arguments(varargin,arglist);
%%%%%%%%%
[present,argout] = getdefaults(present,argout);

%%%%%%%%%
%%%%
if (present.d_int && present.d_rebin)
    error('bad parameter input to pop_full, ''d_int'' and ''d_rebin'' are mutually exclusive')    
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rf=IXTrunfile;
rf=create_population_command(rf,inputname(1),mfilename('fullpath'),mfilename,par,argout,present);

% if there are run_numbers defined in the par cell array create entries in
% the dso
dso=create_dso_from_par(dso,par);


nexdetmap=0;
nexmonmap=0;
nexdetmask=0;
nexmonmask=0;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  input arguments are used to create data source object input to the
%  populatation routines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(present.mon_map)
    [dso,nexmonmap]=map_prep(dso,'monmapfile','monmap.nxs',argout.mon_map);
end
if(present.det_mask)
    [dso,nexdetmask]=mask_prep(dso,'detmaskfile','detmask.nxs',argout.det_mask);
end
if(present.det_map)
    [dso,nexdetmap]=map_prep(dso,'detmapfile','detmap.nxs',argout.det_map);
end
if(present.mon_mask)
    [dso,nexmonmask]=mask_prep(dso,'monmaskfile','monmask.nxs',argout.mon_mask);
end
% we will define the runfile as a diffraction instrument, and you may be
% able to do diffraction related units changes which don't require L2 or ei
dso=add_diffinst(dso);
% no normalisation performed
rf=full_pop(rf,dso,argout,present);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete temporary nexus files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if(nexdetmap)
    delete detmap.nxs;
    dso=del_item(dso,'detmapfile');
end
if(nexdetmask)
    delete detmask.nxs;
    dso=del_item(dso,'detmaskfile');
end
if(nexmonmap)
    delete monmap.nxs;
    dso=del_item(dso,'monmapfile');
end
if(nexmonmask)
    delete monmask.nxs;
    dso=del_item(dso,'monmaskfile');
end
