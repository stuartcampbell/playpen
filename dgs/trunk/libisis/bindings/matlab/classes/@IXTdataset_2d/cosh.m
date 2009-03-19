function dataset_2d = cosh(a)
%--- Help for IXTdataset_2d/cosh.m---
% call syntax: dataset_2d = cosh(a)
%
% takes the hyperbolic cosine of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = cosh(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','cosh',res,a);