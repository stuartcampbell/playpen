%% libisis test script - non-graphics libisis functions only
%% Dean Whittaker

progressbar(0);

global lite_tag;
global full_tag;
global extra_tag;
global default_structure_tag;
global stop_tag; 
global publish_tag; 
global continuous_flag;

addpath ../../
%libisis_init

rawfile1=IXTraw_file('../HET15870.RAW');
rawfile2=IXTraw_file('../HET16538.RAW');
nsp=geti(rawfile1,'NSP1');

progressbar(0.01);

w1=getspectrum(rawfile1,25);
w2=getspectrum(rawfile2,233);
wd1=getspectra(rawfile1,[1:500]);
wd2=getspectra(rawfile2,[1:nsp]);

progressbar(0.015);

rawfile3=IXTraw_file('../HET16774.RAW');
rawfile4 = IXTraw_file('../MAP06220.RAW');
array_builder;

progressbar(0.02);

ww2=getspectrum(rawfile3,30:130);
w3 = getspectrum(rawfile3, 37);
w4 = getspectrum(rawfile4, 89);
nsp=geti(rawfile3,'NSP1');
wd3 = getspectra(rawfile3,1:75);

progressbar(0.03)

ww3 = getspectrum(rawfile3,1:75);
nsp=geti(rawfile4,'NSP1');
wd4 = getspectra(rawfile4,1:nsp);
% this is so titles are not the same length
progressbar(0.04)

wd2.title = 'test';
wd.title = 'another test';
wdd=[wd1,wd3];
display('data loaded successfully')

progressbar(0.07)

%% data manipulation for binary tests
% match the lengths of some IXTdataset_1d objects
wa1 = w1;
wa2 = rebin(w2, w1);
wa3 = rebin(w2, w1);
wa4 = rebin(w2, w1);
na1 = 1:length(wa1.signal);
na2 = 10*rand(1,length(wa1(1).signal));

% match a different length of IXTdataset_1d objects
wb1 = rebin(w1, w2);
wb2 = w2;
wb3 = rebin(w3, w2);
wb4 = rebin(w4, w2);
nb1 = 1:length(wb1.signal);
nb2 = 10*rand(1,length(wb1(1).signal));

% match 2 arrays of IXTdatset_1d objects
wwa1 = ww;
wwa1 = rebin(wwa1, wa1);
wwa2 = ww2(1:length(ww));
wwa2 = rebin(wwa2,wwa1);
nna1 = 1:length(wwa1(1).signal);
nna2 = 10*rand(1,length(wwa1(1).signal));
nna3 = 10*rand(length(ww),length(wwa1(1).signal));

% match some IXTdataset_2d objects
wda1 = getspectra(rawfile1, 1:length(wd3.y));
wda1 = rebin_x(wda1,wd1);
wda2 = getspectra(rawfile2,1:length(wd3.y));
wda2 = rebin_x(wda2,wd1);
wda3 = getspectra(rawfile3,1:length(wd3.y));
wda3 = rebin_x(wda3,wd1);
nda1 = 10*rand(size(wda1(1).signal));
nda2 = 10*rand(size(wda1(1).signal));

% wdb1 = getspectra(rawfile1,1:length(wd3.y));
% wdb1 = rebin_x(wdb1,wd2);
% wdb2 = getspectra(rawfile2,1:length(wd3.y));
% wdb3 = getspectra(rawfile2,1:length(wd3.y));
% wdb3 = rebin_x(wdb3,wd2);

wwb1 = getspectrum(rawfile2, 1:length(wd1.y));
wwb1 = rebin(wwb1, wda1);
wwb3 = getspectrum(rawfile3, 1:length(wd4.y));
wwb3 = rebin(wwb3,wd3); 
nnb1 = 10*rand(size(wwb1(1).signal));
nnb2 = 10*rand(size(wwb1(1).signal));

wwda1 = [wda1, wda3];
wwca1 = [wwb1, wwb3];

display('rebinning of data for binary operations complete')

progressbar(0.12)



%% Binary Operators

% simple d1d and d1d 
w11 = wa1 + wa2;
w12 = wa1 - wa3;
w13 = wa1 * wa4;
w14 = wa3 / wa4;
w15 = wa1 ^ wa2;

res(1)= libisis_checker('binary','+',wa1, wa2, w11);
res(2)= libisis_checker('binary','-',wa1, wa3, w12);
res(3)= libisis_checker('binary','.*',wa1, wa4, w13);
res(4)= libisis_checker('binary','./',wa3, wa4, w14);
res(5)= libisis_checker('binary','.^',wa1, wa2, w15);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

%d1d and a number 
w11 = wa1 + 5;
w12 = wa2 - 5;
w13 = wa3 / 6;
w14 = wa4 * 8;
w15 = wa1 ^2; 

res(1)= libisis_checker('binary','+',wa1, 5, w11);
res(2)= libisis_checker('binary','-',wa2, 5, w12);
res(3)= libisis_checker('binary','./',wa3, 6, w13);
res(4)= libisis_checker('binary','.*',wa4, 8, w14);
res(5)= libisis_checker('binary','.^',wa1, 2, w15);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

% other way round.
w11 = 5 + wa1;
w12 = 5 - wa2;
w13 = 6 / wa3;
w14 = 8 * wa2;

res(1)= libisis_checker('binary','+',wa1, 5, w11);
res(2)= libisis_checker('binary','+',-wa2, 5, w12);
res(3)= libisis_checker('binary','.*',1/wa3, 6, w13);
res(4)= libisis_checker('binary','.*',wa2, 8, w14);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

progressbar(0.16)

clear w11 w12 w13 w14 w15; 

% 2d and number
w11 = wda1 + 5;
w12 = wda2 - 5;
w13 = wda3 / 6;
w14 = wda2 * 8;
w15 = wda1 ^2; 

res(1)= libisis_checker('binary','+',wda1, 5, w11);
res(2)= libisis_checker('binary','-',wda2, 5, w12);
res(3)= libisis_checker('binary','./',wda3, 6, w13);
res(4)= libisis_checker('binary','.*',wda2, 8, w14);
res(5)= libisis_checker('binary','.^',wda1, 2, w15);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

% other way round.
w11 = 5 + wda1;
w12 = 5 - wda2;
w13 = 6 / wda3;
w14 = 8 * wda2;

res(1)= libisis_checker('binary','+',wda1, 5, w11);
res(2)= libisis_checker('binary','+',-wda2, 5, w12);
res(3)= libisis_checker('binary','.*',1/wda3, 6, w13);
res(4)= libisis_checker('binary','.*',wda2, 8, w14);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

% 2d and 1d 
w11 = wda1 + wa2;
w12 = wda2 - wa1;
w13 = wda3 / wa4;
w14 = wda2 * wa3;

res(1)= libisis_checker('binary','+',wda1, wa2, w11);
res(2)= libisis_checker('binary','-',wda2, wa1, w12);
res(3)= libisis_checker('binary','./',wda3, wa4, w13);
res(4)= libisis_checker('binary','.*',wda2, wa3, w14);


if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

% w11 = wa2 + wda1;
% w12 = wa1 - wda2;
% w13 = wa4 / wda3;
% w14 = wa3 * wda2;

progressbar(0.2)

% 2d and 2d
w11 = wda1 + wda2;
w12 = wda2 - wda1;
w13 = wda3 / wda2;
w14 = wda2 * wda3;
w15 = wda1 ^ wda1; 

res(1)= libisis_checker('binary','+',wda1, wda2, w11);
res(2)= libisis_checker('binary','-',wda2, wda1, w12);
res(3)= libisis_checker('binary','./',wda3, wda2, w13);
res(4)= libisis_checker('binary','.*',wda2, wda3, w14);
res(5)= libisis_checker('binary','.^',wda1, wda1, w15);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

% arrays of d2d and d1d

w11 = wda1 + nda2;
w12 = wda2 - nda1;
w13 = wda3 / nda2;
w14 = wda2 * nda1;

res(1)= libisis_checker('binary','+',wda1, nda2, w11);
res(2)= libisis_checker('binary','-',wda2, nda1, w12);
res(3)= libisis_checker('binary','./',wda3, nda2, w13);
res(4)= libisis_checker('binary','.*',wda2, nda1, w14);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

%other way round

w11 = nda2  + wda1;
w12 = nda1 - wda2;
w13 = nda2 / wda3;
w14 = nda1 * wda2;

res(1)= libisis_checker('binary','+',wda1, nda2, w11);
res(2)= libisis_checker('binary','+',-wda2, nda1, w12);
res(3)= libisis_checker('binary','.*',1/wda3, nda2, w13);
res(4)= libisis_checker('binary','.*',wda2, nda1, w14);

progressbar(0.25);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

clear w11 w12 w13 w14 w15; 

w11 = wa1 + na1;
w12 = wa2 - na2;
w13 = wa3 / na1;
w14 = wa2 * na2;

res(1)= libisis_checker('binary','+',wa1, na1, w11);
res(2)= libisis_checker('binary','-',wa2, na2, w12);
res(3)= libisis_checker('binary','./',wa3, na1, w13);
res(4)= libisis_checker('binary','.*',wa4, na2, w14);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end

w11 = na1 + wa1;
w12 = na2 - wa2;
w13 = na1 / wa3;
w14 = na2 * wa2;

res(1)= libisis_checker('binary','+',wa1, na1, w11);
res(2)= libisis_checker('binary','+',-wa2, na2, w12);
res(3)= libisis_checker('binary','.*',1/wa3, na1, w13);
res(4)= libisis_checker('binary','.*',wa4, na2, w14);

if ~ all(res)
    warning(['some operations not performed correctly' num2str(res)])
end
clear w11 w12 w13 w14;
display('binary tests with single datasets complete')

progressbar(0.32)
%% test binary with arrays

% d1d array and d1d array
w11 = wwa1 + wwa2;
w12 = wwa1 - wwa1;
w13 = wwa1 * wwa2;
w14 = wwa2 / wwa1;
w15 = wwa1 ^ wwa2;

for i = 1:length(wwa1)
    res(i,1)= libisis_checker('binary','+',wwa1(i), wwa2(i), w11(i));
    res(i,2)= libisis_checker('binary','-',wwa1(i), wwa1(i), w12(i));
    res(i,3)= libisis_checker('binary','.*',wwa1(i), wwa2(i), w13(i));
    res(i,4)= libisis_checker('binary','./',wwa2(i), wwa1(i), w14(i));
    res(i,5)= libisis_checker('binary','.^',wwa1(i), wwa2(i), w15(i));
end

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

% array of d1d and single d1d
w11 = wwa1 + wa2;
w12 = wwa1 - wa1;
w13 = wwa1 * wa3;
w14 = wwa2 / wa4;
w15 = wwa1 ^ wa2;

% %for i = 1:length(wwa1)
%     res(i,1)= libisis_checker('binary','+',wwa1(i), wa2, w11(i));
%     res(i,2)= libisis_checker('binary','-',wwa1(i), wa1, w12(i));
%     res(i,3)= libisis_checker('binary','.*',wwa1(i), wa2, w13(i));
%     res(i,4)= libisis_checker('binary','./',wwa2(i), wa1, w14(i));
%     res(i,5)= libisis_checker('binary','.^',wwa1(i), wa2, w15(i));
% end
% 
% if ~ all(res(:))
%     warning(['some operations not performed correctly'])
% end

% other way round

w11 = wa2 + wwa1;
w12 = wa1 - wwa1;
w13 = wa3 * wwa1;
w14 = wa4 / wwa2;
w15 = wa2 ^ wwa1;

% for i = 1:length(wwa1)
%     res(i,1)= libisis_checker('binary','+',wwa1(i), wa2, w11(i));
%     res(i,2)= libisis_checker('binary','+',-wwa1(i), wa1, w12(i));
%     res(i,3)= libisis_checker('binary','.*',wwa1(i), wa2, w13(i));
%     res(i,4)= libisis_checker('binary','.*',1/wwa2(i), wa1, w14(i));
% end
% 
% if ~ all(res(:))
%     warning(['some operations not performed correctly'])
% end

% array of d1d and array of numbers
% w11 = wwa1 + nna2;
% w12 = wwa1 - nna1;
% w13 = wwa1 * nna2;
% w14 = wwa2 / nna1;
% w15 = wwa1 ^ nna2;
% 
% for i = 1:length(wwa1)
%     res(i,1)= libisis_checker('binary','+',wwa1(i), wa2, w11(i));
%     res(i,2)= libisis_checker('binary','-',wwa1(i), wa1, w12(i));
%     res(i,3)= libisis_checker('binary','.*',wwa1(i), wa2, w13(i));
%     res(i,4)= libisis_checker('binary','./',wwa2(i), wa1, w14(i));
%     res(i,5)= libisis_checker('binary','.^',wwa1(i), wa2, w15(i));
% end
% 
% if ~ all(res(:))
%     warning(['some operations not performed correctly'])
% end

% other way around
% w11 = nna2 + wwa1;
% w12 = nna1 - wwa1;
% w13 = nna2 * wwa1;
% w14 = nna1 / wwa2;
% 
% for i = 1:length(wwa1)
%     res(i,1)= libisis_checker('binary','+',wwa1(i), wa2, w11(i));
%     res(i,2)= libisis_checker('binary','+',-wwa1(i), wa1, w12(i));
%     res(i,3)= libisis_checker('binary','.*',wwa1(i), wa2, w13(i));
%     res(i,4)= libisis_checker('binary','.*',1/wwa2(i), wa1, w14(i));
% end
% 
% if ~ all(res(:))
%     warning(['some operations not performed correctly'])
% end

% array of d1d and a single number

progressbar(0.35);

w11 = wwa1 + 12.29234;
w12 = wwa1 - 3.54234;
w13 = wwa1 * 6;
w14 = wwa2 / 3049.3;
w15 = wwa1 ^ 2;

for i = 1:length(wwa1)
    res(i,1)= libisis_checker('binary','+',wwa1(i), 12.29234, w11(i));
    res(i,2)= libisis_checker('binary','-',wwa1(i), 3.54234, w12(i));
    res(i,3)= libisis_checker('binary','.*',wwa1(i), 6, w13(i));
    res(i,4)= libisis_checker('binary','./',wwa2(i), 3049.3, w14(i));
    res(i,5)= libisis_checker('binary','.^',wwa1(i), 2, w15(i));
end

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

% other way around
w11 = 12.29234 + wwa1;
w12 = 3.54234 - wwa1;
w13 = 6 * wwa1;
w14 = 3049.3 / wwa2;
w15 = 2 ^ wwa1;

for i = 1:length(wwa1)
    res(i,1)= libisis_checker('binary','+',wwa1(i), 12.29234, w11(i));
    res(i,2)= libisis_checker('binary','+',-wwa1(i), 3.54234, w12(i));
    res(i,3)= libisis_checker('binary','.*',wwa1(i), 6, w13(i));
    res(i,4)= libisis_checker('binary','.*',1/wwa2(i), 3049.3, w14(i));
end

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

clear w11 w12 w13 w14 w15; 

% d2d array operations

display('binary operation testing complete')

progressbar(0.4)

%% Data regrouping and moving functions - dataset_1d 

% rebin

w11 = rebin(w1,[100,100,10000]);
w12 = rebin(w1,100,100,10000);
w13 = rebin([w1, w2],[100,200],[100,50],[10000,20000]);
w14 = rebin([w1, w2],[100,100,10000; 200,50,20000]);
w15 = rebin([w1, w2],[100,100,10000]);
w16 = rebin([w1, w2],100,-0.01, 800);
w17 = rebin(w1, [100,200,10000, 400, 20000]);
w17 = 1;
w18 = rebin(w1, [100, 10000]);
w19 = rebin(w1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;

% regroup

w11 = regroup(w3,[100,100,10000]);
w12 = regroup(w3,100,100,10000);
w13 = regroup([w3, w2],[100,200],[100,50],[10000,20000]);
w14 = regroup([w3, w2],[100,100,10000; 200,50,20000]);
w15 = regroup([w3, w2],[100,100,10000]);
w16 = regroup([w3, w2],100,-0.01, 800);
w17 = regroup([w3 w2], [100,200,10000, 10001, 400, 20000]);
% w18 = regroup(w3, [100, 10000]);
w18 = 1;
w19 = regroup(w1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;

% rebunch

w11 = rebunch(w1, 20);
w12 = rebunch([w1, w2], 30);
w13 = rebunch([w3, w2], [30, 40]);
w14 = rebunch(w1);
w15 = rebunch([w3; w2], [30, 40]);

clear w11 w12 w13 w14 w15;

% shift

w11 = shift(w1, 20);
w12 = shift([w1, w2], - 30);
w13 = shift([w3, w2], [30, 40]);
w14 = shift(w3);
w15 = shift([w3, w2], [30; 40]);
clear w11 w12 w13 w14 w15;

display('data regrouping testing complete')

progressbar(0.45)

%% data manipulation functions - dataset_1d

% unpsike

w11 = unspike(w1);
w12 = unspike(w2);
w13 = unspike([w1, w2, w3]);
w14 = unspike([w1; w2; w3]);
w15 = unspike([w1, w2; w3, w1]);

clear w11 w12 w13 w14 w15;   

% integrate

w11 = integrate(w1);
w12 = integrate(w2, 100, 600);
w13 = integrate([w3, w2, w1], 100,10000);
w14 = integrate([w3, w2, w1], [100,100,400],[10000,20000,20000]);
w15 = integrate([w1, w2, w3], [100;100;400],[10000;20000;20000]);

clear w11 w12 w13 w14 w15;   

% differentiate once

w11 = deriv1(w1);
w12 = deriv1(w2);
w13 = deriv1([w1, w2, w3]);
w14 = deriv1([w1; w2; w3]);
w15 = deriv1([w1, w2; w3, w1]);

clear w11 w12 w13 w14 w15; 

w11 = deriv2(w1);
w12 = deriv2(w2);
w13 = deriv2([w1, w2, w3]);
w14 = deriv2([w1; w2; w3]);
w15 = deriv2([w1, w2; w3, w1]);

clear w11 w12 w13 w14 w15;  

display('dataset_1d data manipulation testing complete')

progressbar(0.5)

%% Datatset_2d regrouping and moving functions

% rebin

w11 = rebin_x(wd1,[100,100,10000]);
w12 = rebin_x(wd1,100,100,10000);
w13 = rebin_x([wd1, wd2],[100,200],[100,50],[10000,20000]);
w14 = rebin_x([wd1, wd2],[100,100,10000; 200,50,20000]);
w15 = rebin_x([wd1, wd2],[100,100,10000]);
w16 = rebin_x([wd1, wd2],100,-0.01, 800);
% w17 = rebin_x(wd1, [100,200,10000, 10001, 400, 20000]);
w17 = 1;
w18 = rebin_x(wd1, [100, 10000]);
w19 = rebin_x(wd1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;

% regroup_x

w11 = regroup_x(wd3,[100,100,10000]);
w12 = regroup_x(wd3,100,100,10000);
w13 = regroup_x([wd3, wd2],[100,200],[100,50],[10000,20000]);
w14 = regroup_x([wd3, wd2],[100,100,10000; 200,50,20000]);
w15 = regroup_x([wd3, wd2],[100,100,10000]);
w16 = regroup_x([wd3, wd2],100,-0.01, 800);
w17 = regroup_x([wd3 wd2], [100,200,10000, 10001, 400, 20000]);
% w18 = regroup_x(wd3, [100, 10000]);
w18 = 1;
w19 = regroup_x(wd1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;  

% rebunch_x

w11 = rebunch_x(wd1, 20);
w12 = rebunch_x([wd1, wd2], 30);
w13 = rebunch_x([wd3, wd2], [30, 40]);
w14 = rebunch_x(wd1);
w15 = rebunch_x([wd3; wd2], [30, 40]);

clear w11 w12 w13 w14 w15;

% shift_x

 w11 = shiftx(wd1, 20);
w12 = shiftx([wd1, wd2], -30);
w13 = shiftx([wd3, wd2], [30, 40]);
w14 = shiftx(wd3);
w15 = shiftx([wd3, wd2], [30; 40]);
clear w11 w12 w13 w14 w15;  

display('data regrouping function tests for dataset_2d x array complete')

progressbar(0.55)

%% operations on y 

wd1 = points_to_histogram(wd1);
wd2 = points_to_histogram(wd2);
wd3 = points_to_histogram(wd3);
wd4 = points_to_histogram(wd4);

% rebin

w11 = rebin_y(wd1,[30,2,200]);
w12 = rebin_y(wd1,30,2,200);
w13 = rebin_y([wd1, wd2],[30,200],[2,4],[200,2500]);
w14 = rebin_y([wd1, wd2],[30,2,200; 200,4,2500]);
w15 = rebin_y([wd1, wd2],[30,2,200]);
w16 = rebin_y([wd1, wd2],10,-0.01, 500);
% w17 = rebin_y(wd1, [30,2,200, 201, 3, 500]);
w17 = 1;
w18 = rebin_y(wd1, [100, 300]);
w19 = rebin_y(wd1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;

% regroup_y

w11 = regroup_y(wd3,[30,2,200]);
w12 = regroup_y(wd3,30,2,200);
w13 = regroup_y([wd3, wd2],[30,200],[2,4],[200,2500]);
w14 = regroup_y([wd3, wd2],[30,2,200; 200,4,2500]);
w15 = regroup_y([wd3, wd2],[30,2,200]);
w16 = regroup_y([wd3, wd2],10,-0.01, 500);
w17 = regroup_y([wd3 wd2], [30,2,200, 201, 3, 500]);
% w18 = regroup_y(wd3, [100, 300]);
w18 = 1;
w19 = regroup_y(wd1);

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;  

% rebunch_y

w11 = rebunch_y(wd1, 5);
w12 = rebunch_y([wd1, wd2], 10);
w13 = rebunch_y([wd3, wd2], [5, 10]);
w14 = rebunch_y(wd1);
w15 = rebunch_y([wd3; wd2], [5, 10]);

clear w11 w12 w13 w14 w15;

progressbar(0.6)
% shift_y

w11 = shifty(wd1, 20);
w12 = shifty([wd1, wd2], - 30);
w13 = shifty([wd3, wd2], [30, 40]);
w14 = shifty(wd3);
w15 = shifty([wd3, wd2], [30; 40]);
clear w11 w12 w13 w14 w15;  

display('data regrouping function tests for dataset_2d y array complete')

%% operations on x and y on dataset_2d

% rebin

w11 = rebin_xy(wd1,[100,100,10000],[30,2,200]);
w12 = rebin_xy([wd1, wd2],[100,100,10000; 200,50,20000],[30,2,200; 200,4,2500]);
w13 = rebin_xy([wd1, wd2],[100,100,10000],[30,2,200]);
w14 = rebin_xy([wd1, wd2],[100,-0.01, 800],[10,-0.01, 500]);
% w15 = rebin_xy(wd1, [30,2,200, 201, 3, 500], [5,2,100,101,3,400]);
w15 = 1;
w16 = rebin_xy(wd1, [100, 10000], [100, 300]);
w17 = rebin_xy(wd1);
w18 = rebin_xy([wd1, wd2], [100,10000], [10,3,500;100,6,2000]); 
w19 = rebin_xy([wd1, wd2], [100,10000; 200,20000], [10,3,500]); 

clear w11 w12 w13 w14 w15 w16 w17 w18 w19;

% regroup_xy

w11 = regroup_xy(wd1,[100,100,10000],[30,2,200]);
w12 = regroup_xy([wd1, wd2],[100,100,10000; 200,50,20000],[30,2,200; 200,4,2500]);
w13 = regroup_xy([wd1, wd2],[100,100,10000],[30,2,200]);
w14 = regroup_xy([wd1, wd2],[100,-0.01, 800],[10,-0.01, 500]);
w15 = regroup_xy(wd1, [30,2,200, 201, 3, 500],[5,2,100,101,3,400]);
% w16 = regroup_xy(wd1, [100, 10000], [100, 300]);
w16 = 1;
w17 = regroup_xy(wd1);
% w18 = regroup_xy([wd1, wd2], [100,10000], [10,3,500;100,6,2000]); 
w18 = 1;
% w19 = regroup_xy([wd1, wd2], [100,10000; 200,20000], [10,3,500]); 
w19 = 1;

clear w11 w12 w13 w14 w15 w16 w17 w18 w19; 

% rebunch_xy

w11 = rebunch_xy(wd1, 10, 2);
w12 = rebunch_xy([wd1, wd2], 10, 3);
w13 = rebunch_xy([wd3, wd2], 20, [5, 10]);
w14 = rebunch_xy(wd1);
w15 = rebunch_xy([wd3; wd2], [20, 30], [5, 10]);

progressbar(0.65)

clear w11 w12 w13 w14 w15;

% shift_xy

w11 = shiftxy(wd1, 20, 10);
w12 = shiftxy([wd1, wd2], - 30, 20);
w13 = shiftxy([wd3, wd2], [30, 40], [10, 20]);
w14 = shiftxy(wd3);
w15 = shiftxy([wd3, wd2], [30; 40], 10);
w16 = shiftxy([wd3, wd2], 30, [10,20]);
clear w11 w12 w13 w14 w15;  

display('data regrouping function tests for dataset_2d complete')

%% Dataset_2d data manipulation functions

% universal

% unpsike

w11 = unspike(wd1);
w12 = unspike(wd2);
w13 = unspike([wd1, wd2, wd3]);
w14 = unspike([wd1; wd2; wd3]);
w15 = unspike([wd1, wd2; wd3, wd1]);

clear w11 w12 w13 w14 w15; 
%% acting on x array 

% integrate_x

w11 = integrate_x(wd1);
w12 = integrate_x(wd2, 100, 600);
w13 = integrate_x([wd3, wd2, wd1], 100,10000);
w14 = integrate_x([wd3, wd2, wd1], [100,100,400],[10000,20000,20000]);
w15 = integrate_x([wd1, wd2, wd3], [100;100;400],[10000;20000;20000]);

clear w11 w12 w13 w14 w15;  

% differentiate once

w11 = deriv1x(wd1);
w12 = deriv1x(wd2);
w13 = deriv1x([wd1, wd2, wd3]);
w14 = deriv1x([wd1; wd2; wd3]);
w15 = deriv1x([wd1, wd2; wd3, wd1]);

clear w11 w12 w13 w14 w15; 

w11 = deriv2x(wd1);
w12 = deriv2x(wd2);
w13 = deriv2x([wd1, wd2, wd3]);
w14 = deriv2x([wd1; wd2; wd3]);
w15 = deriv2x([wd1, wd2; wd3, wd1]);

clear w11 w12 w13 w14 w15;  

progressbar(0.7)

display('data manipulation for dataset_2d on x array tests complete')

%% acting on y array 

% integrate_y

w11 = integrate_y(wd1);
w12 = integrate_y(wd2, 10, 1000);
w13 = integrate_y([wd3, wd2, wd1], 10, 70);
w14 = integrate_y([wd3, wd2, wd1], [10,100,100],[70,1000,400]);
w15 = integrate_y([wd1, wd2, wd3], [100;100;10],[400;1000;70]);

clear w11 w12 w13 w14 w15;  

% differentiate once

w11 = deriv1y(wd1);
w12 = deriv1y(wd1);
w13 = deriv1y([wd1, wd3]);
w14 = deriv1y([wd1; wd3]);
w15 = deriv1y([wd1, wd3; wd3, wd1]);

clear w11 w12 w13 w14 w15; 

w11 = deriv2y(wd1);
w12 = deriv2y(wd3);
w13 = deriv2y([wd1, wd3]);
w14 = deriv2y([wd1; wd3]);
w15 = deriv2x([wd1, wd3; wd3, wd1]);

clear w11 w12 w13 w14 w15;  

display('data manipulation for dataset_2d on y array tests complete')

progressbar(0.73)

%% acting on x / y array

% integrate_y

w11 = integrate_xy(wd1);
w12 = integrate_xy(wd2, 10, 10000, 10, 1000);
w13 = integrate_xy([wd3, wd2, wd1], 10, 700, 10, 70);
w14 = integrate_xy([wd3, wd2, wd1], [10,100,100], [200, 10000, 5000], [10,100,100],[70,1000,400]);

clear w11 w12 w13 w14;  

% differentiate once

display('data manipulation for dataset_2d tests complete')

progressbar(0.78)

%% Trig functions dataset_1d and 2d

% cos d1d

w11 = cos(w1);
w12 = cos([w1, w2]);
w13 = cos([w1; w2]);
w14 = cos(cos(w1));

res(1) = libisis_checker('trig','cos',w1, w11);
res(2) = libisis_checker('trig','cos',[w1, w2],w12);
res(3) = libisis_checker('trig','cos',[w1, w2],w13);
res(4) = libisis_checker('trig','cos',cos(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% sin d1d

w11 = sin(w1);
w12 = sin([w1, w2]);
w13 = sin([w1; w2]);
w14 = sin(sin(w1));

res(1) = libisis_checker('trig','sin',w1, w11);
res(2) = libisis_checker('trig','sin',[w1, w2],w12);
res(3) = libisis_checker('trig','sin',[w1, w2],w13);
res(4) = libisis_checker('trig','sin',sin(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% tan d1d

w11 = tan(w1);
w12 = tan([w1, w2]);
w13 = tan([w1; w2]);
w14 = tan(tan(w1));

res(1) = libisis_checker('trig','tan',w1, w11);
res(2) = libisis_checker('trig','tan',[w1, w2],w12);
res(3) = libisis_checker('trig','tan',[w1, w2],w13);
res(4) = libisis_checker('trig','tan',tan(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% cosh d1d

w11 = cosh(w1);
w12 = cosh([w1, w2]);
w13 = cosh([w1; w2]);
w14 = cosh(cosh(w1));

res(1) = libisis_checker('trig','cosh',w1, w11);
res(2) = libisis_checker('trig','cosh',[w1, w2],w12);
res(3) = libisis_checker('trig','cosh',[w1, w2],w13);
res(4) = libisis_checker('trig','cosh',cosh(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

progressbar(0.85)

% sinh d1d

w11 = sinh(w1);
w12 = sinh([w1, w2]);
w13 = sinh([w1; w2]);
w14 = sinh(sinh(w1));

res(1) = libisis_checker('trig','sinh',w1, w11);
res(2) = libisis_checker('trig','sinh',[w1, w2],w12);
res(3) = libisis_checker('trig','sinh',[w1, w2],w13);
res(4) = libisis_checker('trig','sinh',sinh(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% tanh d1d

w11 = tanh(w1);
w12 = tanh([w1, w2]);
w13 = tanh([w1; w2]);
w14 = tanh(tanh(w1));

res(1) = libisis_checker('trig','tanh',w1, w11);
res(2) = libisis_checker('trig','tanh',[w1, w2],w12);
res(3) = libisis_checker('trig','tanh',[w1, w2],w13);
res(4) = libisis_checker('trig','tanh',tanh(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% log d1d

w11 = log(w1);
w12 = log([w1, w2]);
w13 = log([w1; w2]);
w14 = log(log(w1));

res(1) = libisis_checker('trig','log',w1, w11);
res(2) = libisis_checker('trig','log',[w1, w2],w12);
res(3) = libisis_checker('trig','log',[w1, w2],w13);
res(4) = libisis_checker('trig','log',log(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% exp d1d

w11 = exp(w1);
w12 = exp([w1, w2]);
w13 = exp([w1; w2]);
w14 = exp(exp(w1));

res(1) = libisis_checker('trig','exp',w1, w11);
res(2) = libisis_checker('trig','exp',[w1, w2],w12);
res(3) = libisis_checker('trig','exp',[w1, w2],w13);
res(4) = libisis_checker('trig','exp',exp(w1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

clear w11 w12 w13 w14;

% exp d2d 

w11 = exp(wd1);
w12 = exp([wd1, wd2]);
w13 = exp([wd1; wd2]);
w14 = exp(exp(wd1));

res(1) = libisis_checker('trig','exp',wd1, w11);
res(2) = libisis_checker('trig','exp',[wd1, wd2],w12);
res(3) = libisis_checker('trig','exp',[wd1, wd2],w13);
res(4) = libisis_checker('trig','exp',exp(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% cos d2d 

w11 = cos(wd1);
w12 = cos([wd1, wd2]);
w13 = cos([wd1; wd2]);
w14 = cos(cos(wd1));

res(1) = libisis_checker('trig','cos',wd1, w11);
res(2) = libisis_checker('trig','cos',[wd1, wd2],w12);
res(3) = libisis_checker('trig','cos',[wd1, wd2],w13);
res(4) = libisis_checker('trig','cos',cos(wd1),w14);

progressbar(0.9)

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% log d2d 

w11 = log(wd1);
w12 = log([wd1, wd2]);
w13 = log([wd1; wd2]);
w14 = log(log(wd1));

res(1) = libisis_checker('trig','log',wd1, w11);
res(2) = libisis_checker('trig','log',[wd1, wd2],w12);
res(3) = libisis_checker('trig','log',[wd1, wd2],w13);
res(4) = libisis_checker('trig','log',log(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% tanh d2d 

w11 = tanh(wd1);
w12 = tanh([wd1, wd2]);
w13 = tanh([wd1; wd2]);
w14 = tanh(tanh(wd1));

res(1) = libisis_checker('trig','tanh',wd1, w11);
res(2) = libisis_checker('trig','tanh',[wd1, wd2],w12);
res(3) = libisis_checker('trig','tanh',[wd1, wd2],w13);
res(4) = libisis_checker('trig','tanh',tanh(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% sinh d2d 

w11 = sinh(wd1);
w12 = sinh([wd1, wd2]);
w13 = sinh([wd1; wd2]);
w14 = sinh(sinh(wd1));

res(1) = libisis_checker('trig','sinh',wd1, w11);
res(2) = libisis_checker('trig','sinh',[wd1, wd2],w12);
res(3) = libisis_checker('trig','sinh',[wd1, wd2],w13);
res(4) = libisis_checker('trig','sinh',sinh(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

progressbar(0.95)
% cosh d2d 

w11 = cosh(wd1);
w12 = cosh([wd1, wd2]);
w13 = cosh([wd1; wd2]);
w14 = cosh(cosh(wd1));

res(1) = libisis_checker('trig','cosh',wd1, w11);
res(2) = libisis_checker('trig','cosh',[wd1, wd2],w12);
res(3) = libisis_checker('trig','cosh',[wd1, wd2],w13);
res(4) = libisis_checker('trig','cosh',cosh(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% tan d2d 

w11 = tan(wd1);
w12 = tan([wd1, wd2]);
w13 = tan([wd1; wd2]);
w14 = tan(tan(wd1));

res(1) = libisis_checker('trig','tan',wd1, w11);
res(2) = libisis_checker('trig','tan',[wd1, wd2],w12);
res(3) = libisis_checker('trig','tan',[wd1, wd2],w13);
res(4) = libisis_checker('trig','tan',tan(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end


% sin d2d 

w11 = sin(wd1);
w12 = sin([wd1, wd2]);
w13 = sin([wd1; wd2]);
w14 = sin(sin(wd1));

progressbar(0.98)

res(1) = libisis_checker('trig','sin',wd1, w11);
res(2) = libisis_checker('trig','sin',[wd1, wd2],w12);
res(3) = libisis_checker('trig','sin',[wd1, wd2],w13);
res(4) = libisis_checker('trig','sin',sin(wd1),w14);

if ~ all(res(:))
    warning(['some operations not performed correctly'])
end

if ~continuous_flag

    k = menu('libisis test script paused, please press continue or keyboard for keyboard control','Continue','Keyboard');

    switch k
        case 1
        case 2
            keyboard;
        otherwise
            error('value entered on menu is not recognised')
    end
end

clear w11 w12 w13 w14; 

progressbar(1)

close all 