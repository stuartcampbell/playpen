function datum = integrate_xy(a,xmin,xmax,ymin,ymax)
% dataset_1d = integrate_xy(dataset_2d,xmin,xmax,ymin,ymax)
%
% integrates an IXTdataset_2d along the x-axis between xmin and xmax, and along the y-axis
% between ymin and ymax to give an IXTdatum output
%
% If dataset_2d is an array, datum will be an array such that 
%
% datum(i) = integrate_xy(dataset_2d(i),xmin,xmax,ymin,ymax)
%
% if xmin, xmax, ymin, ymax are arrays of the same length as dataset_2d
% then dataum will be an array such that
%
% datum(i) = integrate_xy(dataset_2d(i),xmin(i),xmax(i),ymin(i),ymax(i))
%
% can give just x limits to integrate over all y in the dataset or no
% limits to integrate over all data.
datum=IXTdatum;
len_a = length(a);

if nargin == 1          % integrate across the whole dataset
    for i = 1:len_a
        xmin(i) = min(a(i).x);
        xmax(i) = max(a(i).x);
        ymin(i) = min(a(i).y);
        ymax(i) = max(a(i).y);
    end
elseif nargin == 3
    for i = 1:len_a
        ymin(i) = min(a(i).y);
        ymax(i) = max(a(i).y); 
    end
end

len_xmin = length(xmin);
len_xmax = length(xmax);
len_ymin = length(ymin);
len_ymax = length(ymax);

if   len_a == len_xmin && len_xmin == len_xmax && ...
        len_a == len_ymin && len_ymin == len_ymax       % all array or all single

    for i = 1:len_a
        datum(i) = libisisexc('IXTdataset_2d','integrate_xy',IXTdatum, a(i), xmin(i), xmax(i), ymin(i), ymax(i));
    end

elseif len_a > 1 && len_xmin == 1 && len_xmax == 1 && ...
                                len_ymin == 1 && len_ymax == 1    % only a is array

    for i = 1:len_a
        datum(i) = libisisexc('IXTdataset_2d', 'integrate_xy', IXTdatum, a(i), xmin, xmax, ymin, ymax);
    end 

elseif len_xmin ~= len_a || len_xmax ~= len_a ||...
        len_ymin ~= len_a || len_ymax ~= len_a          % different dimensions
    warning('Integrate_xy not performed - array dimensions do not agree');
    return

else                                                    % something is empty
    warning('Integrate_xy not performed - not enough data provided');
    return
    
end







