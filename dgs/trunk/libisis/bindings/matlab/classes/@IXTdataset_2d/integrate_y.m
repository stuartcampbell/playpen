function dataset_1d = integrate_y(a,ymin,ymax)
% dataset_1d = integrate_y(dataset_2d,ymin,ymax) 
%
% integrates an IXTdataset_2d object along the y-axis between ymin and ymax to produce
% an IXTdataset_1d output
%
% if given an array of dataset_2d, an array of dataset_1d will be produced
% such that dataset_1d(i) = integrate_y(dataset_2d(i),ymin,ymax). If ymin
% and ymax are also arrays of same length as the dataset array, then the
% output will be
%
% dataset_1d(i) = integrate_y(dataset_2d(i),ymin(i),ymax(i))
%

dataset_1d = IXTdataset_1d;     % this line is required when allocating within a loop   
len_a = length(a);

if nargin == 1          % integrate across the whole dataset
    for i = 1:len_a
        ymin(i) = min(a(i).y);
        ymax(i) = max(a(i).y);
    end
end


len_ymin = length(ymin);
len_ymax = length(ymax);

if   len_a == len_ymin && len_ymin == len_ymax       % all array or all single

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_2d','integrate_y',IXTdataset_1d,a(i),ymin(i),ymax(i));
    end

elseif len_a > 1 && len_ymin == 1 && len_ymax == 1   % only a is array

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_2d','integrate_y',IXTdataset_1d,a(i),ymin,ymax);
    end

elseif len_ymin ~= len_a || len_ymax ~= len_a       % different dimensions
    warning('Integrate_y not performed - array dimensions do not agree');
    return

else                                                % something is empty
    warning('Integrate_y not performed - not enough data provided');
    return
end