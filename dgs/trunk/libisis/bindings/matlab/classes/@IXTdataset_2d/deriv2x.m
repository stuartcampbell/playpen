function dataset_2d = deriv2x(a)
%--- Help for IXTdataset_2d/deriv2x.m---
% call syntax: dataset_2d = deriv2x(a)
%
% Takes the numerical second derivative along x dimension of a 1XTdataset_2d object
%
% inputs: a = 1XTdataset_2d object 
%
% output: 1XTdataset_2d object
dataset_2d = a;
for i = 1:length(a)
dataset_2d(i) = libisisexc('IXTdataset_2d','deriv2x',IXTdataset_2d,a(i));
end