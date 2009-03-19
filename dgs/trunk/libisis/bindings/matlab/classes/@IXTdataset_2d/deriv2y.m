function dataset_2d = deriv2y(a)
%--- Help for IXTdataset_1d/deriv2y.m---
% call syntax: dataset_2d = deriv2y(a)
%
% Takes the numerical second derivative along y dimension of a 1XTdataset_2d object
%
% inputs: a = 1XTdataset_2d object 
%
% output: 1XTdataset_2d object

dataset_2d = a;
for i = 1:length(a)
dataset_2d(i) = libisisexc('IXTdataset_2d','deriv2y',IXTdataset_2d,a(i));
end