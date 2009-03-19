% gtk_load_data_script - loads the test data for gtk_test_script. 

global lite_tag;
global full_tag;
global extra_tag;
global default_structure_tag;
global stop_tag; 
global publish_tag; 
global continuous_flag;
global ww

addpath ../../
%libisis_init

rawfile1=IXTraw_file('../HET15870.RAW');
rawfile2=IXTraw_file('../HET16538.RAW');
nsp=geti(rawfile1,'NSP1');
w=getspectrum(rawfile1,25);
w2=getspectrum(rawfile2,233);
wd=getspectra(rawfile1,[1:nsp]);
wd2=getspectra(rawfile2,[1:nsp]);
rawfile3=IXTraw_file('../HET16774.RAW');
array_builder;
ww=rebin(ww,[0,300,10000]);
if lite_tag
    wd = rebin_x(wd,[0,200,20000]);
    wd2 = rebin_x(wd2,[0,200,20000]);
end

ww2=getspectrum(rawfile3,30:45);
ww2 = rebin(ww2, [0, 300, 10000]);

% this is so titles are not the same length

wd2.title = 'test';

wd.title = char({'another test', 'multiline'});

wdd=[wd,wd2];

display('data loaded successfully')

