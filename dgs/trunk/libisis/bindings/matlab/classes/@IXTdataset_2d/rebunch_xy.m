function dataset_2d = rebunch_xy(a,xbins, ybins)
% new_dataset_2d = rebunch_xy(dataset_2d,xbins,ybins)
%
% rebunches an IXTdataset_2d object with xbins elements grouped together on
% the x array and ybins elements grouped in the y array
%
% input: dataset_2d - IXTdataset2d object or array, xbins - number of bins
% to group together in the x array, ybins - number of bins to group
% together on the y array 
%
% output: dataset_2d object
%
% if given an array of IXTdataset2d, each dataset will be rebunched to
% xbins, ybins , if xbins and ybins are arrays of the same length of the dataset array, then
% dataset(i) will be rebunched with xbins(i) and ybins(i). 
dataset_2d = a;


if nargin == 1
    dataset_2d = a;
    return
end

len_a = length(a);
len_xbins = length(xbins);
len_ybins = length(ybins);

if len_xbins == len_a  && len_ybins == len_a     % either both single or both array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_xy',IXTdataset_2d,a(i),int32(xbins(i)),int32(ybins(i)));
    end
    
elseif len_a > 1    % only a is array
    if len_xbins == 1 && len_ybins == 1
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_xy',IXTdataset_2d,a(i),int32(xbins),int32(ybins));
        end
    elseif len_xbins == 1 && len_ybins == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_xy',IXTdataset_2d,a(i),int32(xbins),int32(ybins(i)));
        end
    elseif len_ybins == 1 && len_xbins == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebunch_xy',IXTdataset_2d,a(i),int32(xbins(i)),int32(ybins));
        end
    else
        warning('rebunch_xy not performed - array dimensions do not agree');
    end

    % errors
elseif len_xbins ~= len_a || len_ybins ~= len_a
    warning('rebunch_xy not performed - array dimensions do not agree');
else
    warning('rebunch_xy not performed - not enough data provided');
end

