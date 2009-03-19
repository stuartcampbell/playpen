function res = exp(a)
%--- Help for IXTdataset_1d/exp.m---
% call syntax: dataset_1d = exp(a)
%
% takes the exponent of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = exp(a)
res = IXTdataset_1d;
res(length(a)) = res(1);
res = libisisexc('IXTdataset_1d','exp',res,a);