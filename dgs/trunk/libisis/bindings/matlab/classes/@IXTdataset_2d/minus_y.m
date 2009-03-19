function dataset_2d = minus_y(a,b)
%--- Help for IXTdataset_2d/minus_y.m---
% call syntax: dataset_2d = minus_y(a,b)
%
% subtracts either a dataset_1d or a 1-d array from each column of an  IXTdataset_2d
% object
%
% inputs: a = 1XTdataset_2d object , b = 1-d array or IXTdataset_1d object
%
% output: 1XTdataset_2d object

dataset_2d = d2d_binary_op(a,b,'minus','y')