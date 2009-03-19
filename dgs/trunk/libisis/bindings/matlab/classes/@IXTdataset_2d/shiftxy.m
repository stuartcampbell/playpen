function dataset_2d = shiftxy(a,x_shift,y_shift)
%--- shift xy - shift a dataset 2d object in both x and y directions---
% call syntax: dataset_2d = shiftxy(a,x_shift,y_shift)
%
% Shifts the x-array of an IXTdataset_2d object 'a' to the right by the amount
% 'x_shift' and the y-array by the amount 'y_shift'
%
% inputs: a= 1XTdataset_2d object, x_shift = amount to shift the x-array data by, 
% y_shift = amount to shift a y array data by
%
% output: 1XTdataset_2d object 
% 
% If IXTdataset_2d is an array then each dataset will be shifted by x_shift
% and y_shift. If x_shift and y_shift are arrays of the same length as the
% dataset then dataset_2d(i) will be shifted in x by x_shift(i) and y by
% y_shift(i)
if nargin == 1
    dataset_2d = a;
    return;
end

dataset_2d = a;     % this line is required when allocating within a loop   
len_a = length(a);
len_shift_x = length(x_shift);
len_shift_y = length(y_shift);

if len_shift_x == len_a  && len_shift_y == len_a     % either both single or both array
    
    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift(i),y_shift(i));
    end
    
elseif len_a > 1    % only a is array
    if len_shift_x == 1 && len_shift_y == 1
        
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift,y_shift);
        end
        
    elseif len_shift_x == 1 && len_shift_y == len_a
        
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift,y_shift(i));
        end
        
    elseif len_shift_y == 1 && len_shift_x == len_a
        
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','shift',IXTdataset_2d,a(i),x_shift(i),y_shift);
        end
        
    else
       
        warning('shiftxy not performed - array dimensions do not agree');
        
    end

    % errors
elseif len_shift_x ~= len_a  || len_shift_y ~= len_a            % dimensions wrong
    warning('shift_xy not performed - array dimensions do not agree');
    return
else
    warning('shift_xy not performed - not enough data provided');
    return
end