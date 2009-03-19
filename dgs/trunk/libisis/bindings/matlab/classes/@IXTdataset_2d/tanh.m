function dataset_2d = tanh(a)
%--- Help for IXTdataset_2d/tanh.m---
% call syntax: dataset_2d = tanh(a)
%
% takes the hyperbolic tangent of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = tanh(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','tanh',res,a);