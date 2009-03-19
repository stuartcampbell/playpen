function arrayd1d = expand_d1d(d2d)
% arrayd1d = expand_d1d(dataset_2d)
%
%converts an IXTdataset_2d into an array of IXTdataset_1d's

arrayd1d = [];
for j = 1:length(d2d)
    arrayd1d = [arrayd1d, libisisexc('IXTdataset_2d','expand_arrayd1d',IXTdataset_1d,d2d)];
end