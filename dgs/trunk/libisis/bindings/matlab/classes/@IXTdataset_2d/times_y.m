function dataset_2d = times_y(a,b)
%--- Help for IXTdataset_2d/times_y.m---
% call syntax: dataset_2d = times_y(a,b)
%
% multiplies either a dataset_1d or a 1-d array with each column of an IXTdataset_2d
% object
%
% When multiplying by a 1d array (or IXTdataset_1d) the division is done such
% that
%
% dataset_2d(i).signal(j,k) = ww(i).signal(j,k) * n(k) 
%
% where n is either the 1d numeric array or signal data in the
% IXTdataset_1d
%
% inputs: a = 1XTdataset_2d object , b = 1-d array or IXTdataset_1d object
%
% output: 1XTdataset_2d object

dataset_2d = d2d_binary_op(a,b,'times','y');