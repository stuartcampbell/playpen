function dataset_1d = shift(a,x_shift)

%--shift a dataset 1d function for libisis----
% call syntax: dataset_1d = shift(a,x_shift)
%
% Shifts the xarray of an IXTdataset_1d object 'a' to the right by the amount
% 'x_shift'
%
% If given an array of IXTdataset_1d objects, each IXTdataset_1d will be
% shifted by x_shift, if x_shift is an array of the same size as the array of
% IXTdataset_1d then a(i) will be shifted by x_shift(i)
%
% inputs: a = 1XTdataset_1d object, x_shift = amount to shift a data by
%
% output: 1XTdataset_1d object 
%--------------------------------------------------------------------------

if nargin == 1
    dataset_1d = a;
    return;
end

dataset_1d = IXTdataset_1d;     % this line is required when allocating within a loop   
len_a = length(a);
len_shift = length(x_shift);

if  len_shift == len_a                  % both array or both single

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','shift',IXTdataset_1d,a(i),x_shift(i));
    end

elseif len_a > 1 && len_shift == 1      % only a is array

    for i = 1:len_a
        dataset_1d(i) = libisisexc('IXTdataset_1d','shift',IXTdataset_1d,a(i),x_shift);
    end

    % errors
elseif len_shift ~= len_a               % dimensions wrong
    warning('shift not performed - array dimensions do not agree');
    return
else
    warning('shift not performed - not enough data provided');
    return
end

