
function w1d = ixf_twod_to_oned(w2d)
%------------ help for GTK ixf_twod_to_oned--------------------------------
%
% purpose: to convert a dataset 2d to an array of dataset_1d
%
% syntax: w1d = ixf_twod_to_oned(w2d)
%
% input: dataset_2d
%
% output: array of dataset_1d
%
%--------------------------------------------------------------------------
w1d = expand_d1d(w2d);
