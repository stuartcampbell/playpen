function dataset_1d = plus(a,b)
%--- Help for IXTdataset_1d/plus.m---
% call syntax: a+b or plus(a,b)
%
% adds a 1XTdataset_1d object to either a scalar or another 1XTdataset_1d
% object. 
%
% inputs: a= 1XTdataset_1d object or scalar, b= 1Xtdataset_1d object or
% scalar
%
% output: 1XTdataset_1d object.. dataset_1d = a+b

dataset_1d = d1d_binary_op(a, b, 'plus');
