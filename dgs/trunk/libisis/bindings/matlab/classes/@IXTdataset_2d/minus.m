function dataset_2d = minus(a,b)
%--- Help for IXTdataset_2d/minus.m---
% call syntax: a-b or minus(a,b)
%
% subtracts 1XTdataset_2d objects from each other element-by-element or scalars 
%
% inputs: a = 1XTdataset_2d object or scalar, b = 1Xtdataset_2d object or
% scalar
%
% output: a 1XTdataset_2d object... dataset_2d = a-b

dataset_2d = d2d_binary_op(a,b,'minus','');