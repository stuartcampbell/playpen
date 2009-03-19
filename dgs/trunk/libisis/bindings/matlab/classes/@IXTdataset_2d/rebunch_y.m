function dataset_2d = rebunch_y(a,ybins)
% new_dataset_2d = rebunch_y(dataset_2d,ybins)
%
% rebunch_yes an IXTdataset_2d object with ybins elements grouped together
%
% input: dataset_2d - IXTdataset2d object or array, ybins - number of bins
% to group together
%
% output: dataset_2d object
%
% if given an array of IXTdataset2d, each dataset will be rebunched to
% ybins, if ybins is an array of the same length of the dataset array, then
% dataset(i) will be rebunch_yed with ybins(i).

if nargin == 1
    dataset_2d = a;
    return
end

dataset_2d = a;
len_a = length(a);
len_ybins = length(ybins);

if len_ybins == len_a       % either both single or both array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_y',IXTdataset_2d,a(i),int32(ybins(i)));
    end
    
elseif len_a > 1 && len_ybins == 1      % only a is array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_y',IXTdataset_2d,a(i),int32(ybins));
    end
    
    % errors
elseif len_ybins ~= len_a
    warning('rebunch_y not performed - array dimensions do not agree');
else
    warning('rebunch_y not performed - not enough data provided');
end

