function dataset_2d = shifty(a,y_shift)
%--- Help for IXTdataset_2d/shifty.m---
% call syntax: dataset_2d = shifty(a,y_shift)
%
% Shifts the y-array of an IXTdataset_2d object 'a' to the right by the amount
% 'y_shift'
%
% inputs: a= 1XTdataset_2d object, y_shift = amount to shift a data by
%
% output: 1XTdataset_2d object 
dataset_2d = a;

if nargin == 1
    return;
end

len_a = length(a);
len_shift = length(y_shift);

if  len_shift == len_a                  % both array or both single

    for i = 1:len_a
        % a zero is sent as the third argument to differentiate from xshift
        % only
        dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),int32(0),y_shift(i));
    end

elseif len_a > 1 && len_shift == 1      % only a is array

    for i = 1:len_a
        % a zero is sent as the third argument to differentiate from xshift
        % only        
        dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),int32(0),y_shift);
    end

    % errors
elseif len_shift ~= len_a               % dimensions wrong
    warning('shift_y not performed - array dimensions do not agree');
    return
else
    warning('shift_y not performed - not enough data provided');
    return
end
