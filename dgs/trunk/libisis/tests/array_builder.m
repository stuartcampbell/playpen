rawfile1=IXTraw_file('het11170.RAW');
ww=IXTdataset_1d;
for i=1:25
    ww(i)=getspectrum(rawfile1,10+i);
    ww(i)=rebin(ww(i),[100,100,20000]);
end
