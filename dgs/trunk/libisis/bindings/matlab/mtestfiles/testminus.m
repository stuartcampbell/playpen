clear classes;
nx1=5;
nx2=nx1;
x1=[1:nx1];
x2=[1:nx2];
% x2(1)=0;
s1=rand(1,nx1);
e1=rand(1,nx1);
s2=rand(1,nx2);
e2=rand(1,nx2);
w1=IXTdataset_1d('name_1','title_1',int32(nx1),s1,e1,'s-label',x1,'x-label','time',logical(0),logical(0))
w2=IXTdataset_1d('name_2','title_2',int32(nx2),s2,e2,'s-label',x2,'x-label','time',logical(0),logical(0))
w3=w1-w2
% clear classes;
% clear libisisexc;
% clear;