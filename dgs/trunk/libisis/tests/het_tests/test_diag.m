DSOc=IXTdata_source;
DSOc=add_chopinst(DSOc);

rf1= homer(DSOc,'HET','15958','fixei',0.0,'d_int',[12000,18000]);


DSOw=IXTdata_source;
DSOw=add_diffinst(DSOw);
sam_wb=homer(DSOw,'HET','15869','ei','white','d_int',[20,40],'det_units','e','scale',1000,'normalisation',1,'range',[1000,2000],'nchunk',100);
rf_ZC=homer(DSOw,'ei','white','d_int',[NaN('double'),NaN('double')]);

het_bank=IXTmap({[6:49,51:72,75:96,101:356],401:2584});


[mask,cause]=diagnose(rf1,sam_wb,'bank',het_bank,'vv_hi',3.0,'hardmask','het_991.msk','rf_ZC',rf_ZC);