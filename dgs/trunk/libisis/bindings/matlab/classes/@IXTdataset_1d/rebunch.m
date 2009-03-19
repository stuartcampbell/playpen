function dataset_1d = rebunch(a,nbins)
% new_dataset_1d = rebunch(dataset_1d,nbins)
%
% rebunches an IXTdataset_1d object with nbins elements grouped together
%
% input: dataset_1d - IXTdataset1d object or array, nbins - number of bins
% to group together
%
% output: dataset_1d object
%
% if given an array of IXTdataset1d, each dataset will be rebunched to
% nbins, if nbins is an array of the same length of the dataset array, then
% dataset(i) will be rebunched with nbins(i).
if nargin == 1
    dataset_1d = a;
    return
end

dataset_1d = IXTdataset_1d;
len_a = length(a);
len_nbins = length(nbins);

if len_nbins == len_a       % either both single or both array
    
    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','rebunch',IXTdataset_1d,a(i),int32(nbins(i)));
    end
    
elseif len_a > 1 && len_nbins == 1      % only a is array
    
    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','rebunch',IXTdataset_1d,a(i),int32(nbins));
    end
    
    % errors
elseif len_nbins ~= len_a
    warning('rebunch not performed - array dimensions do not agree');
else
    warning('rebunch not performed - not enough data provided');
end

