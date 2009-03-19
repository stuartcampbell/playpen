function dataset_1d = sinh(a)
%--- Help for IXTdataset_1d/cos.m---
% call syntax: dataset_1d = sinh(a)
%
% takes the hyperbolic sine of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = sinh(a)
res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','sinh',res,a);