function dataset_2d = minus_x(a,b)
%--- Help for IXTdataset_2d/minus_x.m---
% call syntax: dataset_2d = minus_x(a,b)
%
% subtracts either a dataset_1d or a 1-d array from each row of an  IXTdataset_2d
% object
%
% inputs: a = 1XTdataset_2d object , b = 1-d array or IXTdataset_1d object
%
% output: 1XTdataset_2d object

dataset_2d = d2d_binary_op(a,b,'minus','x');