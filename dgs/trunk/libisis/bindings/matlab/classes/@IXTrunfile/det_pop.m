function  rf = det_pop (rf,dso, values, present)
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

opt.d_axis=false;
opt.m_axis=false;
opt.m_rebin=false;
opt.ei=false;
ei=zeros(2);
d_axis=IXTaxis;


rf=populate_det(rf,dso,int32(values.period),ei,default_homer_nchunk,d_axis,d_rebin,bgrd,i_lim,opt);
