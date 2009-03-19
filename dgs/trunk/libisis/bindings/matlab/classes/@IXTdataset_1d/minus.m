function dataset_1d = minus(a,b)
%--- Help for IXTdataset_1d/minus.m---
% call syntax: a-b or minus(a,b)
%
% subtracts a 1XTdataset_1d objects from each other element-by-element or scalars
%
% inputs: a = 1XTdataset_1d object or scalar, b= 1Xtdataset_1d object or
% scalar
%
% output: 1XTdataset_1d object.. dataset_1d = a-b

dataset_1d = d1d_binary_op(a, b, 'minus');
