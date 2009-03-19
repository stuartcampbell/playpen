function  rf = homer_pop (rf,dso, values, present,default_rebin)
opt=IXToptions(IXTbase);

if(present.m_rebin)
    opt.m_rebin=true;
    m_rebin=values.m_rebin;
else
    opt.m_rebin=false;
    m_rebin=[];
end

if(present.d_rebin);
    opt.d_rebin=true;
    d_rebin=values.d_rebin;
else
    opt.d_rebin=false;
    d_rebin=[];
end

if(present.d_int)
        opt.d_int=true;
        i_lim=values.d_int;
else
        opt.d_int=false;
        i_lim=[];
end

whitebeam=false;
if present.ei
    if ischar(values.ei)
        % then is a diffraction instrument values.ei='white'
        opt.ei=false;
        ei=[];
        whitebeam=true;
    end
    if isnumeric(values.ei)
        ei=values.ei;
        opt.ei=true;
        if (~present.d_rebin && ~present.d_int && default_rebin)
            % make default rebinning values according to Ei value
            opt.d_rebin=true;
            disp('default rebinning used, to remove define ''nod_rebin''');
            d_rebin=[-ei/2.0, ei/200.0, 0.95*ei ]
        end
    end
elseif present.fixei
    ei=values.fixei;
    opt.ei=true;
    if (~present.d_rebin && ~present.d_int && default_rebin)
        % make default rebinning values according to Ei value
        opt.d_rebin=true;
        disp('default rebinning used, to remove define ''nod_rebin''');
        d_rebin=[-ei/2.0, ei/200.0, 0.95*ei ]
    end
else
    ei=[];
end

if whitebeam
    monei_info=[0 0 0];
else
    % getei_m_index.dat MUST only contain 2 numbers
    ei_specs=(fileread(IXTmask,'inst_maps:::getei_m_index.dat'));
    % the first two values in the monei_info are the indices of the M1 and M2
    % spectra in the runfile.mon_data.datasets object used to determine ei
    % the third value is a binary flag to specify whether the supplied ei is a
    % fixei or an ei_guess
    monei_info=ei_specs.mask_array;
    if present.fixei
        monei_info(3)=int32(0);
    else
        monei_info(3)=int32(1);
    end
end

if (present.mon_units)
    opt.m_axis=true;
    %implicit creation of object takes code and creates units
    m_axis=IXTaxis(values.mon_units);
else
    %make false object
    opt.m_axis=false;
    m_axis=IXTaxis;
end

% see what happens with giving time as units
% values.d_units will have the default value
if (present.det_units)
    opt.d_axis=true;
    %implicit creation of object takes code and creates units
    d_axis=IXTaxis(values.det_units);
else
% this now fills with the default values for detector units    
    opt.d_axis=false;
    d_axis=IXTaxis;
end

if(present.background)
        opt.bgrd=true;
        bgrd=values.background;
else
        opt.bgrd=false;
        bgrd=[];
end

%mon_eiinfo input to populate
rf=populate(rf,dso,int32(values.period),int32(values.nchunk),ei,monei_info,m_axis,m_rebin,i_lim,d_axis,d_rebin,bgrd,opt);
