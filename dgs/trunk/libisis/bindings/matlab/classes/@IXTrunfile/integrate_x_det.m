function runfile = integrate_x_det(a,xmin,xmax)
% runfile = integrate_x_det(runfile,xmin,xmax)
%
% integrates the detector data of an IXTrunfile along the x-axis between xmin and xmax to produce
% an IXTrunfile output. the monitor data will remain unchanged.
%
% if given an array of IXTrunfile, an array of IXTrunfile will be produced
% such that runfile(i) = integrate_x_det(runfile(i),xmin,xmax). If xmin
% and xmax are also arrays of same length as the dataset array, then the
% output will be
%
% runfile(i) = integrate_x_det(runfile(i),xmin(i),xmax(i))
%

runfile = IXTrunfile;     % this line is required when allocating within a loop
len_a = length(a);

if nargin == 1          % integrate across the whole dataset
    for i = 1:len_a
        xmin(i) = min(a(i).det_data.datasets(1).x);
        xmax(i) = max(a(i).det_data.datasets(1).x);
    end
end


len_xmin = length(xmin);
len_xmax = length(xmax);

if   len_a == len_xmin && len_xmin == len_xmax       % all array or all single

    for i = 1:len_a
        runfile(i) = libisisexc('IXTrunfile','integrate_x_det',IXTrunfile,a(i),[xmin(i) xmax(i)]);
    end

elseif len_a > 1 && len_xmin == 1 && len_xmax == 1   % only a is array

    for i = 1:len_a
        runfile(i) = libisisexc('IXTrunfile','integrate_x_det',IXTrunfile,a(i),[xmin,xmax]);
    end

elseif len_xmin ~= len_a || len_xmax ~= len_a       % different dimensions
    warning('Integrate_x_det not performed - array dimensions do not agree');
    return

else                                                % something is empty
    warning('Integrate_x_det not performed - not enough data provided');
    return
end
