function dataset_2d = rebin_xy(a,x_option,y_option)
% rebin_xy - rebin dataset 2d in x and y directions. 
% new_dataset = rebin_xy(dataset_2d,x_option,y_option)
%
% x_option=xref or xdesc
% y_option=yref or ydesc
% rebins the x-dimension of an IXTdataset_2d object according x_option and
% the y-dimension according to y_option.
% for further explanation of xdesc/ydesc and xref/yref please consult
% the help for rebin_x and/or rebin_y.
%
% This only accepts the x and y options in the array format, i.e.
%
% dataset_2d = rebin_xy(a,[xlo, dx, xhi],[ylo, dy, yhi])  or
%
% dataset_2d = rebin_xy(a,[xlo, dx, xhi, xlo2, dx2, xhi2...],[ylo, dy, yhi,
% ylo2, dy2, yhi2...]) etc.
%
% can be given array data in dataset_2d or column arrays for xlo, dx, xhi, ylo, dy,
% yhi, as in rebin_x and rebin_y.

dataset_2d = a;

if nargin == 1
    return
end

%----------------perform actions-----------------------

len_a = length(a);
len_x = size(x_option, 1);
len_y = size(y_option, 1);

if  len_x == len_a && len_y == len_a          % both arrays or both single

    for i = 1:len_a
        dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_xy',IXTdataset_2d,a(i),x_option(i,:),y_option(i,:));
    end

 elseif len_a > 1      % a is array, b isn't
    if len_y == len_x && len_x == 1
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_xy',IXTdataset_2d,a(i),x_option,y_option);
        end
    elseif len_y == 1 && len_x == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_xy',IXTdataset_2d,a(i),x_option(i,:),y_option);
        end
    elseif len_x == 1 && len_y == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','rebin_xy',IXTdataset_2d,a(i),x_option,y_option(i,:));
        end
    else
            warning('rebin_xy not performed - array dimensions do not agree');
    end
    

      % errors
elseif  len_x ~= len_a  ||  len_y ~= len_a     % if arrays are difference sizes
    warning('rebin_xy not performed - array dimensions do not agree');
    return
else
    warning('rebin_xy not performed - not enough data provided'); % something must be 0
    return
end





