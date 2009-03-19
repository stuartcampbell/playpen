function xout = ixf_points_to_boundaries (x)
% MIDDLE    Take array of bin centres and return mid-points, adding end points, so that
%    xout(1) = x(1) - 0.5(x(2)-x(1)) ; xout(n+1) = x(n) + 0.5(x(n)-x(n-1))
%           The result is NOT the same as bin boundaries, but can be used as such for 
%    patch plots made from point data.
%
%   >> xout = boundaries (x)
%

if (isa(x,'double') && ndims(x)==2 && min(size(x))==1 && max(size(x))>1)
    nx = length(x);
    if (size(x,1)==1)
        xout = [x(1)-0.5*(x(2)-x(1)), 0.5.*(x(2:nx)+x(1:nx-1)), x(nx)+0.5*(x(nx)-x(nx-1))];
    else
        xout = [x(1)-0.5*(x(2)-x(1)); 0.5.*(x(2:nx)+x(1:nx-1)); x(nx)+0.5*(x(nx)-x(nx-1))];
    end
elseif (isa(x,'double') && ndims(x)==2 && min(size(x))==1 && max(size(x))==1)
    display('A single value has been converted to histogram data, it is assumed that the bin width will be 1')
    xout = [x, x+1];
else
    error ('Check length of array or data type')
end

