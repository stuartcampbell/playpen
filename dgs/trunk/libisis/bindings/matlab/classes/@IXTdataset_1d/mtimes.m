function dataset_1d = mtimes(a,b)
%--- Help for IXTdataset_1d/mtimes.m---
% call syntax: a*b or mtimes(a,b)
%
% multiplies 1XTdataset_1d objects to each other element-by-element or
% scalars
%
% inputs: a = 1XTdataset_1d object or scalar, b= 1Xtdataset_1d object or
% scalar
%
% output: 1XTdataset_1d object.. dataset_1d = a*b

dataset_1d = d1d_binary_op(a, b, 'times');
