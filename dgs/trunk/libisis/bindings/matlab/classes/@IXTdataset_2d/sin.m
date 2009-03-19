function dataset_2d = sin(a)
%--- Help for IXTdataset_2d/sin.m---
% call syntax: dataset_2d = sin(a)
%
% Takes the sine of a 1XTdataset_2d object
%
% inputs: a= 1XTdataset_2d object 
%
% output: 1XTdataset_2d object... sin(a)
res = IXTdataset_2d;
res(length(a)) = res(1);
dataset_2d = libisisexc('IXTdataset_2d','sin',res,a);