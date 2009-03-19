function dataset_1d = mpower(a,b)
%--- Help for IXTdataset_1d/mpower.m---
% call syntax: a^b or mpower(a,b)
%
% raises a 1XTdataset_1d object to the power of another 1XTdataset_1d
% object element-by-element or scaler
%
% inputs: a = 1XTdataset_1d object, b = 1Xtdataset_1d object or
% scalar
%
% output: 1XTdataset_1d object.. dataset_1d = a^b

dataset_1d = d1d_binary_op(a, b, 'power');
