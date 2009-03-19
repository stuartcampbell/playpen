function  rf = full_pop (rf,dso, values, present)
opt=IXToptions(IXTbase);

if(present.d_rebin)
    opt.d_rebin=true;
    d_rebin=values.d_rebin;
else
    opt.d_rebin=false;
    d_rebin=[];
end

if(present.background)
    opt.bgrd=true;
    bgrd=values.background;
else
    opt.bgrd=false;
    bgrd=[];
end

if(present.d_int)
    opt.d_int=true;
    i_lim=values.d_int;
else
    opt.d_int=false;
    i_lim=[];
end

if(present.m_rebin)
    opt.m_rebin=true;
    m_rebin=values.m_rebin;
else
    opt.m_rebin=false;
    m_rebin=[];
end

m_axis=IXTaxis;
d_axis=IXTaxis;
ei=[];
opt.d_axis=false;
opt.m_axis=false;
opt.ei=false;
monei_info=[0 0 0];

rf=populate(rf,dso,int32(values.period),default_homer_nchunk,ei,monei_info,m_axis,m_rebin,i_lim,d_axis,d_rebin,bgrd,opt);
