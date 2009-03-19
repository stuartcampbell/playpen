function dataset_1d = tanh(a)
%--- Help for IXTdataset_1d/tanh.m---
% call syntax: dataset_1d = tanh(a)
%
% takes the hyperbolic tangent of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = tanh(a)
res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','tanh',res,a);