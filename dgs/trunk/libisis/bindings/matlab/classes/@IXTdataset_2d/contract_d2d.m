function d2d = contract_d2d(arrayd2d)
% d2d = contract_d2d(arrayd2d)
%
%converts an array of IXTdataset_2d's into a IXTdataset_2d, the x array of the first
% element in the array is used for the whole dataset
d2d = libisisexc('IXTdataset_2d','contract_arrayd2d',IXTdataset_2d,arrayd2d);