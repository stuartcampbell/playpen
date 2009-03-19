function rf=homer (dso,varargin)
arglist = struct(... % argument names and default values
    'period',int32(1),...
    'ei',[],...
    'fixei',[],...
    'background',[], ...
    'det_map','', ...
    'det_mask','', ...
    'mon_map','',...
    'mon_mask','',...
    'm_rebin',[],...
    'd_int',[],...
    'd_rebin',[],...
    'det_units','',...
    'mon_units','',...
    'normalisation',[],...
    'range',[],...
    'scale',[],...
    'nchunk',default_homer_nchunk,... % this seems to be the fastest of them all
    'solid',[],...
    'corr',0,...
    'abs',1.0,...
    'mass',[],...
    'RMM',[]);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parsing of input arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flags = {'corr'};


[par,argout,present] = parse_arguments(varargin,arglist,flags);


%%%%%%%%%
[present,argout,default_rebin] = getdefaults(present,argout);


% if 'nonorm' then no normalisation will be performed, otherwise defaults
% will be filled for monitor 1 normalisation with range [1000 2000], if
% range not specified

if isnumeric(argout.normalisation) && present.normalisation && ~present.range
    argout.range=ixf_global_var('homer','get','range');
    present.range=1;
end
%%%%%%%%%
%%%%%%%%%


%%%%
if (present.d_int && present.d_rebin)
    error('ERROR: bad parameter input to homer, ''d_int'' and ''d_rebin'' are mutually exclusive')
end
% set default integration if neither d_int or d_rebin present
% since d_rebin MUST really be supplied



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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% specialised call to population routines, depending on input
% arguments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% if no ei present then assume whitebeam, else an error will be called
if (~present.ei && ~present.fixei)
    argout.ei=ixf_global_var('homer','get','ei'); %
    if ~isempty (argout.ei)
        present.ei=1;
    end
end

if (present.ei && present.fixei)
    error('''ei'' and ''fixei'' are mutually exclusive keywords')
end

if (present.ei)
    if (isnumeric(argout.ei))
%  determine ei from guess homer_getei uses its own dso, since special population and fill
% and appropriate getei_m.map file in inst_maps:::
        rf=homer_pop(rf,dso,argout,present,default_rebin);
    end
    if(ischar(argout.ei))
        if(strcmp(argout.ei,'white'))
            %do diff_inst population
            % white beam vanadium etc...
            disp('white beam vanadium')%instrument type will be set
            default_rebin=false; %no ei value for a whitebeam
            rf=homer_pop(rf,dso,argout,present,default_rebin);
        else 
            error('ERROR: bad ''ei'' input parameters')
            return            
        end
    end
else
    if(present.fixei)
        if(isnumeric(argout.fixei))
            % argout.fixei will be given as ei argument to homer_pop
            rf=homer_pop(rf,dso,argout,present,default_rebin);
        else
            error('ERROR: bad ''fixei'' input parameters')
            return
        end
    else
        % simple integration - no units change allowed, no ei or fixei
        if ~present.det_units
            default_rebin=false;
            rf=homer_pop(rf,dso,argout,present,default_rebin);
        else
            error('ERROR: ei or fixei must be defined')
        end
    end
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% normalisation routines if requested
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



if (present.normalisation && present.range) %otherwise total failure
    if(isnumeric(argout.normalisation))
        if(isnumeric(argout.range))
            disp('monitor integral normalisation')
            if ~present.scale

                argout.scale=ixf_global_var('homer','get','mon_scale');
            end
            rf=homer_norm1(rf,argout);

        else
            if(ischar(argout.range))
                if(strcmp(argout.range,'peak'))
                    disp('moments/peak normalisation')
                    if(present.ei)
                        % give the ei guess as a ball park figure
                        if(isnumeric(argout.ei))
                            if ~present.scale
                                argout.scale=ixf_global_var('homer','get','peak_scale');
                            end

                            rf=homer_norm2(rf,argout,argout.ei);
                        end
                    elseif(present.fixei)
                        if(isnumeric(argout.fixei))
                            if ~present.scale
                                argout.scale=ixf_global_var('homer','get','peak_scale');
                            end

                            rf=homer_norm2(rf,argout,argout.fixei);
                        end
                    else
                        error('ERROR: no ei defined for moments normalisation');
                    end
                end
            end
        end
    else
        error('ERROR: incompatible normalisation parameters');        
    end
else
    if (present.normalisation)
        if(ischar(argout.normalisation))
            if(present.range)
                error('ERROR: incompatible normalisation parameters');
            end
            if(strcmp(argout.normalisation,'uahr'))
                disp('uahr normalisation')
                if ~present.scale
                    argout.scale=ixf_global_var('homer','get','uamp_scale');
                end
                % argout is just for argout.scale value

                rf=homer_norm3(rf,argout);
            end
        end
    end
end

if (present.solid)
    rf=homer_solid(rf,dso,argout);

end
if (present.corr)
	rf=homer_corr(rf);

end

if (present.mass && present.RMM) % if abs not present then it will take value of 1, if it is present then it will have a value
    % therefore its presence is NOT checked for

    factor=argout.abs*(argout.mass/argout.RMM);
    rf=homer_absolute(rf,factor);

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete any temporary nexus files created
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
