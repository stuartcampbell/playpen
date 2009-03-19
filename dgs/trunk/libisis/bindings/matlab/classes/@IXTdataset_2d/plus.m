function dataset_2d = plus(a,b)
%--- Help for IXTdataset_2d/plus.m---
% call syntax: a+b or plus(a,b)
%
% adds a 1XTdataset_2d object to either a scalar or another 1XTdataset_2d
% object. 
%
% inputs: a= 1XTdataset_2d object or scalar, b= 1Xtdataset_2d object or
% scalar
%
% output: 1XTdataset_2d object.. dataset_2d = a+b
   
dataset_2d = d2d_binary_op(a,b,'plus','');
