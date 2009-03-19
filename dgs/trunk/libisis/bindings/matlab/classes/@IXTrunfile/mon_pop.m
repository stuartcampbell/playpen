function  rf = mon_pop (rf,dso, values, present)
opt=IXToptions(IXTbase);


if(present.m_rebin)
    opt.m_rebin=true;
    m_rebin=values.m_rebin;
else
    opt.m_rebin=false;
    m_rebin=[];
end

    opt.bgrd=false;
    opt.d_axis=false;
    opt.m_axis=false;
    opt.d_rebin=false;
    opt.ei=false;
    m_axis=IXTaxis;
    ei=zeros(2);
 
rf=populate_mon(rf,dso,int32(values.period),opt,ei,m_axis,m_rebin);
