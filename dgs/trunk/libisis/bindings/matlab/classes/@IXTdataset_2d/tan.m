function dataset_2d = tan(a)
%--- Help for IXTdataset_2d/cos.m---
% call syntax: dataset_2d = tan(a)
%
% takes the tangent of a IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = tan(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','tan',res,a);