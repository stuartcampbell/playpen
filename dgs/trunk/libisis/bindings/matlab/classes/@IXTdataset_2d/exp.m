function dataset_2d = exp(a)
%--- Help for IXTdataset_2d/exp.m---
% call syntax: dataset_2d = exp(a)
%
% takes the exponent of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = exp(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','exp',res,a);