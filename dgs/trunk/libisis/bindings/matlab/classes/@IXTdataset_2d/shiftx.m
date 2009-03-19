function dataset_2d = shiftx(a,x_shift)
%--- Help for IXTdataset_2d/shiftx.m---
% call syntax: dataset_2d = shiftx(a,x_shift)
%
% Shifts the x-array of an IXTdataset_2d object 'a' to the right by the amount
% 'x_shift'
%
% inputs: a= 1XTdataset_2d object, x_shift = amount to shift a data by
%
% output: 1XTdataset_2d object 
dataset_2d = a;   

if nargin == 1
    return;
end

len_a = length(a);
len_shift = length(x_shift);

if  len_shift == len_a                  % both array or both single

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift(i));
    end

elseif len_a > 1 && len_shift == 1      % only a is array

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift);
    end

    % errors
elseif len_shift ~= len_a               % dimensions wrong
    warning('shift_x not performed - array dimensions do not agree');
    return
else
    warning('shift_x not performed - not enough data provided');
    return
end

