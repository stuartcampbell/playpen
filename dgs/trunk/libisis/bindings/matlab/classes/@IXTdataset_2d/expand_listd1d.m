function arrayd1d = expand_listd1d(d2d,list)
% arrayd1d = expand_listd1d(dataset_2d, list)
%
%converts an IXTdataset_2d into an array of IXTdataset_1d's, using a list of indices
arrayd1d = libisisexc('IXTdataset_2d','expand_listd1d',IXTdataset_1d,d2d,int32(list));