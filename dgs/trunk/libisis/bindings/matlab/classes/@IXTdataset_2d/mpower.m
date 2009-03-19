function dataset_2d = mpower(a,b)
%--- Help for IXTdataset_2d/mpower.m---
% call syntax: a^b or mpower(a,b)
%
% raises a 1XTdataset_2d object to the power of another 1XTdataset_2d
% object element-by-element or scaler
%
% inputs: a = 1XTdataset_2d object, b = 1Xtdataset_2d object or
% scalar
%
% output: 1XTdataset_2d object.. dataset_2d = a^b

dataset_2d = d2d_binary_op(a,b,'power','');