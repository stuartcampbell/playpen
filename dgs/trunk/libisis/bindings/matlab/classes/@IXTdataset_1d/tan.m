function dataset_1d = tan(a)
%--- Help for IXTdataset_1d/cos.m---
% call syntax: dataset_1d = tan(a)
%
% takes the tangent of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object.. dataset_1d = tan(a)
res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','tan',res,a);