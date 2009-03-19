function arrayd2d = expand_d2d(d2d)
% arrayd2d = expand_d2d(dataset_2d)
%
%converts an IXTdataset_2d into an array of IXTdataset_2d's
arrayd2d = libisisexc('IXTdataset_2d','expand_arrayd2d',IXTdataset_2d,d2d);