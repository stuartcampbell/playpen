function dataset_1d = integrate_x(a,xmin,xmax)
% dataset_1d = integrate_x(dataset_2d,xmin,xmax) 
%
% integrates an IXTdataset_2d object along the x-axis between xmin and xmax to produce
% an IXTdataset_1d output
%
% if given an array of dataset_2d, an array of dataset_1d will be produced
% such that dataset_1d(i) = integrate_x(dataset_2d(i),xmin,xmax). If xmin
% and xmax are also arrays of same length as the dataset array, then the
% output will be
%
% dataset_1d(i) = integrate_x(dataset_2d(i),xmin(i),xmax(i))
%

dataset_1d = IXTdataset_1d;     % this line is required when allocating within a loop   
len_a = length(a);

if nargin == 1          % integrate across the whole dataset
    for i = 1:len_a
        xmin(i) = min(a(i).x);
        xmax(i) = max(a(i).x);
    end
end


len_xmin = length(xmin);
len_xmax = length(xmax);

if   len_a == len_xmin && len_xmin == len_xmax       % all array or all single

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_2d','integrate_x',IXTdataset_1d,a(i),xmin(i),xmax(i));
    end

elseif len_a > 1 && len_xmin == 1 && len_xmax == 1   % only a is array

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_2d','integrate_x',IXTdataset_1d,a(i),xmin,xmax);
    end

elseif len_xmin ~= len_a || len_xmax ~= len_a       % different dimensions
    warning('Integrate_x not performed - array dimensions do not agree');
    return

else                                                % something is empty
    warning('Integrate_x not performed - not enough data provided');
    return
end