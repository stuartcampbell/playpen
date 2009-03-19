function arrayd2d = expand_listd2d(d2d,list)
% arrayd2d = expand_listd2d(dataset_2d,list)
%
%converts an IXTdataset_2d into an array of IXTdataset_2d's using a list of indices
arrayd2d = libisisexc('IXTdataset_2d','expand_listd2d',IXTdataset_2d,d2d,int32(list));