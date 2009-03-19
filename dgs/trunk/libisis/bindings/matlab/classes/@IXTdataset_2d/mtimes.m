function dataset_2d = mtimes(a,b)
%--- Help for IXTdataset_2d/mtimes.m---
% call syntax: a*b or mtimes(a,b)
%
% multiplies 1XTdataset_2d objects to each other element-by-element or
% scalars
%
% inputs: a = 1XTdataset_2d object or scalar, b= 1Xtdataset_2d object or
% scalar
%
% output: 1XTdataset_2d object.. dataset_2d = a*b

dataset_2d = d2d_binary_op(a,b,'times','');