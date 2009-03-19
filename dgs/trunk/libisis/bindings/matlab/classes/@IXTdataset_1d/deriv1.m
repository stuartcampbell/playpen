function dataset_1d = deriv1(a)
%--- deriv1 derivative function for IXTdataset 1d objects in libisisexc---
% call syntax: dataset_1d = deriv1(a)
%
% Takes the numerical first derivative of a IXTdataset_1d object
%
% inputs: a = IXTdataset_1d object 
%
% output: IXTdataset_1d object
%
% if given an array of dataset_1d, returns an array of dataset_1d such that
% dataset_1d(i) = deriv1(a(i))
%
% e.g. result = deriv1([w1, w2]) 
% this gives result(1) = the derivative of w1, result(2) = derivative of w2
dataset_1d = a;
for i = 1:length(a)     
dataset_1d(i) = libisisexc('IXTdataset_1d','deriv1',IXTdataset_1d,a(i));
end