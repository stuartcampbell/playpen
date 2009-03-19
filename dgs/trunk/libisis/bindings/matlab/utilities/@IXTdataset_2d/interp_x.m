function wout = interp_x(w,xi,varargin)
% Interpolate an IXTdataset_2d or array of IXTdataset_2d along x axis only
%
% The interpolation options are the same as the Matlab built-in routine interpn.
% For further details look at the Matlab help for interpn.
%
% Syntax:
%   >> wout = interp(w,xi)
%   >> wout = interp(w,wref)
%   >> wout = interp(...,method)
%   >> wout = interp(...,method,'extrap')
%
%   w           Input IXTdataset_2d or array of IXTdataset_2
%
%   xi          Vector that defines the grid points at which interpolated values will be calculated
%                - output will be a point IXTdataset_2d along the x-axis
%               *OR*
%   wref        Another IXTdataset_1d or IXTdataset_2d from which the x values will be taken
%              (in this case must be a single IXTdataset_1d or IXTdataset_2d)
%                - output will be a histogram or point along the x axis according
%                  as the IXTdataset_1d or IXTdataset_2d from which the x  values are taken
%
%   method          'nearest'   Nearest neighbour interpolation
%                   'linear'    Linear interpolation (default)
%                   'spline'    Cubic spline interpolation
%                   'cubic'     
%
%   extrapval   Value given to out-of-range points - NaN or 0 often used
%
% See also:
%   interp, interp_y

% Check input arguments:
if nargin<2; error('Check number of arguments to interp'); end

if isa_size(xi,[1,1],'IXTdataset_1d')
    xinterp = get_centres_1d(xi);
elseif isa_size(xi,[1,1],'IXTdataset_2d')
    xinterp = get_centres_2d(xi,1);
elseif isa_size(xi,'vector','double')
    xinterp = xi;
    if any(diff(xinterp)<=0)    % not monotonic increasing
        error('x values at which to compute interpolated values must be monotonic increasing')
    end
else
    error ('Check axis values for interpolation a numeric vector, or a single IXTdataset_1d or IXTdataset_2d')
end

% Perform interpolation
wout = w;
for i = 1:length(w)
    [xcent,ycent] = get_centres_2d(w(i));
    [xinterp_grid,ycent_grid]=ndgrid(xinterp,ycent);
    if isa(xi,'numeric')
        wout(i).x = xinterp;
    else
        wout(i).x = xi.x;
    end
    wout(i).signal = interpn(xcent, ycent, w(i).signal, xinterp_grid, ycent_grid, varargin{:});
    wout(i).error = zeros(size(wout(i).signal));
end

%-----------------------------------------------------------------
function xcent=get_centres_1d(w)
% Get centres of bins if histogram; or simply return x array if not.
if length(w.x)~=length(w.signal)
    xcent = 0.5*(w.x(1:end-1)+w.x(2:end));
else
    xcent = w.x;
end
%-----------------------------------------------------------------
function varargout=get_centres_2d(w,idim)
% Get centres of bins if histogram; or simply return input array if not.
%   >> [x,y]=get_centres_2d(w)
%   >> x=get_centres_2d(w,1)
%   >> y=get_centres_2d(w,2)
i=0;
if nargin==1 || idim==1
    i=i+1;
    if length(w.x)~=size(w.signal,1)
        varargout{i} = 0.5*(w.x(1:end-1)+w.x(2:end));
    else
        varargout{i} = w.x;
    end
end
if nargin==1 || idim==2
    i=i+1;
    if length(w.y)~=size(w.signal,2)
        varargout{i} = 0.5*(w.y(1:end-1)+w.y(2:end));
    else
        varargout{i} = w.y;
    end
end
