function rf=spec(dso,varargin)
% runfile = spec(dso,varargin)
% populate an IXTrunfile object with the detector data only, it accepts
% the following keyword/argument pairs
% 'period', 'det_map','det_mask','d_rebin','background',d_int'
arglist = struct(... % argument names and default values
    'period',1,...
    'det_map','',...
    'det_mask','',...
    'd_rebin',[],...
    'background',[],...
    'd_int',[]);
[par,argout,present] = parse_arguments(varargin,arglist);
%%%%
if (present.d_int && present.d_rebin)
    disp('bad parameter input to homer, {d_int} and {d_rebin} are mutually exclusive');
    return
end
%%%
rf=IXTrunfile;
rf=create_population_command(rf,inputname(1),mfilename('fullpath'),mfilename,par,argout,present);

% if there are run_numbers defined in the par cell array create entries in
% the dso
dso=create_dso_from_par(dso,par);

nexdetmap=0;
nexdetmask=0;

if(present.det_map);
    [dso,nexdetmap]=map_prep(dso,'detmapfile','detmap.nxs',argout.det_map);
end
if(present.det_mask)    
    [dso,nexdetmask]=mask_prep(dso,'detmaskfile','detmask.nxs',argout.det_mask);
end
% we will define the runfile as a diffraction instrument, and you may be
% able to do diffraction related units changes which don't require L2 or ei
dso=add_diffinst(dso);
% no normalisation performed, simply populates the det_data object
rf=det_pop(rf,dso,argout,present);

if(nexdetmap)
    delete detmap.nxs;
    dso=del_item(dso,'detmapfile');        
end
if(nexdetmask)
    delete detmask.nxs;
    dso=del_item(dso,'detmaskfile');        
end
