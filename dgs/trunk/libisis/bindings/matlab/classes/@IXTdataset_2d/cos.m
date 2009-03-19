function dataset_2d = cos(a)
%--- Help for IXTdataset_1d/cos.m---
% call syntax: dataset_2d = cos(a)
%
% takes the cosine of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = cos(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','cos',res,a);