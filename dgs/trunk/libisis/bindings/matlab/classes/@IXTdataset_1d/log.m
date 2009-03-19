function dataset_1d = log(a)
%--- Help for IXTdataset_1d/log.m---
% call syntax: dataset_1d = log(a)
%
% takes the logarithm of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = log(a)
res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','log',res,a);