function dataset_1d = cosh(a)
%--- Help for IXTdataset_1d/cosh.m---
% call syntax: dataset_1d = cosh(a)
%
% takes the hyperbolic cosine of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = cosh(a)
res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','cosh',res,a);