function dataset_1d = sin(a)
%--- Help for IXTdataset_1d/sin.m---
% call syntax: dataset_1d = sin(a)
%
% Takes the sine of a 1XTdataset_1d object
%
% inputs: a= 1XTdataset_1d object 
%
% output: 1XTdataset_1d object... sin(a)

res = IXTdataset_1d;
res(1:length(a)) = res(1);
dataset_1d = libisisexc('IXTdataset_1d','sin',res,a);