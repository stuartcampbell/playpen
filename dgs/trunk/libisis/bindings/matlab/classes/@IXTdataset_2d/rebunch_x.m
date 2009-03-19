function dataset_2d = rebunch_x(a,xbins)
% new_dataset_2d = rebunch_x(dataset_2d,xbins)
%
% rebunch_xes an IXTdataset_2d object with xbins elements grouped together
%
% input: dataset_2d - IXTdataset2d object or array, xbins - number of bins
% to group together
%
% output: dataset_2d object
%
% if given an array of IXTdataset2d, each dataset will be rebunched to
% xbins, if xbins is an array of the same length of the dataset array, then
% dataset(i) will be rebunch_xed with xbins(i).

if nargin == 1
    dataset_2d = a;
    return
end

dataset_2d = a;
len_a = length(a);
len_xbins = length(xbins);

if len_xbins == len_a       % either both single or both array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_x',IXTdataset_2d,a(i),int32(xbins(i)));
    end
    
elseif len_a > 1 && len_xbins == 1      % only a is array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_x',IXTdataset_2d,a(i),int32(xbins));
    end
    
    % errors
elseif len_xbins ~= len_a
    warning('rebunch_x not performed - array dimensions do not agree');
else
    warning('rebunch_x not performed - not enough data provided');
end