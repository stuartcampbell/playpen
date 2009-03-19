function dataset_2d = regroup_xy (a, x_option, y_option)
% regroup_xy(dataset_2d,xparams,yparams)
%
% regroups an IXTdataset_2d object along the x-dimension according to the
% xparams array and along the y-dimension according to the yparams array.
% where xparams=[xlo,dx,xhi] and yparams=[ylo,dy,yhi] describe the x and y dimension 
% binning paramaters to ensure that bins have minimum width determined by
% the parameter dx/dy, but ensuring the bin boundaries are always coincedent with
% original bin boundaries.
%
% if given an array of dataset_2d, each dataset will be regrouped with the
% x and y parameters. 
%
% xlo, dx, xhi, ylo, dy, yhi can be given as column vectors of the same
% length as dataset_2d such that each dataset_2d will be rebinned with the
% coresponding values in the column vectors. 
%

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
        dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_xy',IXTdataset_2d,a(i),x_option(i,:),y_option(i,:));
    end

 elseif len_a > 1      % a is array, b isn't
    if len_y == len_x && len_x == 1
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_xy',IXTdataset_2d,a(i),x_option,y_option);
        end
    elseif len_y == 1 && len_x == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_xy',IXTdataset_2d,a(i),x_option(i,:),y_option);
        end
    elseif len_x == 1 && len_y == len_a
        for i = 1:len_a
            dataset_2d(i) = libisisexc('IXTdataset_2d','regroup_xy',IXTdataset_2d,a(i),x_option,y_option(i,:));
        end
    else
            warning('regroup_xy not performed - array dimensions do not agree');
    end
    

      % errors
elseif  len_x ~= len_a  ||  len_y ~= len_a     % if arrays are difference sizes
    warning('regroup_xy not performed - array dimensions do not agree');
    return
else
    warning('regroup_xy not performed - not enough data provided'); % something must be 0
    return
end
