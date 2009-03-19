function datum = integrate(a,xmin,xmax)
% libisisexec integrate function - integrate a dataset 1d object between
% xmin and xmax
%
% datum = integrate(dataset_1d, xmin, xmax)
% datum_array = integrate(dataset_1d_array, xmin, xmax)
%
% integrates a single IXTdataset_1d object according to the limits xmin and xmax to give
% an IXTdatum result. 
% 
% if dataset_1d is an array then an array of datum is returned, if xmin and
% xmax are arrays then datum(i) = integrate(a(i),xmin(i),xmax(i)) 
%
% e.g.  datum = integrate([w1, w2],[100,1000],[800,10000])
% will give datum(1) = the integral of w1 between x = 100 and x = 800 and
% datum(2) = the integral of w2 between x = 1000 and x = 10000

datum=IXTdatum;     % this line is required when allocating within a loop   
len_a = length(a);

if nargin == 1          % integrate across the whole dataset
    for i = 1:len_a
        xmin(i) = min(a(i).x);
        xmax(i) = max(a(i).x);
    end
end


len_xmin = length(xmin);
len_xmax = length(xmax);

if   len_a == len_xmin && len_xmin == len_xmax   % all array or all single

    for i = 1:len_a
        datum(i) = libisisexc('IXTdataset_1d','integrate',IXTdatum,a(i),xmin(i),xmax(i));
    end

elseif len_a > 1 && len_xmin == 1 && len_xmax == 1  % only a is array

    for i = 1:len_a
        datum(i) = libisisexc('IXTdataset_1d','integrate',IXTdatum,a(i),xmin,xmax);
    end

elseif len_xmin ~= len_a || len_xmax ~= len_a       % different dimensions
    warning('Integrate not performed - array dimensions do not agree');
    return

else                     % something is empty
    warning('Integrate not performed - not enough data provided');
    return
end



