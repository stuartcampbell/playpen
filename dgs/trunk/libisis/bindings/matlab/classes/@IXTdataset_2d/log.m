function dataset_2d = log(a)
%--- Help for IXTdataset_2d/log.m---
% call syntax: dataset_2d = log(a)
%
% takes the logarithm of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = log(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','log',res,a);