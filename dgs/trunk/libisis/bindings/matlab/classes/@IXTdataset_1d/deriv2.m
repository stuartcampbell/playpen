function dataset_1d = deriv2(a)
%--- deriv2 - second derivative of IXTdataset 1d object for libisisexc---
% call syntax: dataset_1d = deriv2(a)
%
% Takes the numerical second derivative of a 1XTdataset_1d object
%
% inputs: a = 1XTdataset_1d object 
%
% output: 1XTdataset_1d object
%
% if a is an array, dataset_1d will be an array such that dataset_1d(i) =
% deriv2(a(i))
dataset_1d = a;
for i = 1:length(a)
dataset_1d(i) = libisisexc('IXTdataset_1d','deriv2',IXTdataset_1d,a(i));
end