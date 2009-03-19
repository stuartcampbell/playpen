clear classes;
nx1=15;
nx2=nx1;
x1=[1:nx1];
x2=[1:nx2];
s1=[1:nx1-1];
e1=[1:nx1-1];
s2=rand(1,nx2-1);
e2=rand(1,nx2-1);
w1=IXTdataset_1d(IXTbase,'name_1','title_1',int32(nx1),s1,e1,'s-label',x1,'x-label','time',logical(0),logical(1));
w2=IXTdataset_1d(IXTbase,'name_2','title_2',int32(nx2),s2,e2,'s-label',x2,'x-label','time',logical(0),logical(1));
w3=w1+w2
%tic
% rebunch(w1,5)
%toc
% clear classes;
% clear libisisexc;
% clear;
