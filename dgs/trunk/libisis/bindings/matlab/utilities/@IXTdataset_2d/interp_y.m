function wout = interp_y(w,yi,varargin)
% Interpolate an IXTdataset_2d or array of IXTdataset_2d along y axis only
%
% The interpolation options are the same as the Matlab built-in routine interpn.
% For further details look at the Matlab help for interpn.
%
% Syntax:
%   >> wout = interp(w,yi)
%   >> wout = interp(w,wref)
%   >> wout = interp(...,method)
%   >> wout = interp(...,method,'extrap')
%
%   w           Input IXTdataset_2d or array of IXTdataset_2
%
%   yi          Vector that defines the grid points at which interpolated values will be calculated
%                - output will be a point IXTdataset_2d along the y-axis
%               *OR*
%   wref        Another IXTdataset_1d or IXTdataset_2d from which the y values will be taken
%              (in this case must be a single IXTdataset_1d or IXTdataset_2d)
%                - output will be a histogram or point along the y axis according
%                  as the IXTdataset_1d or IXTdataset_2d from which the y values are taken
%
%   method          'nearest'   Nearest neighbour interpolation
%                   'linear'    Linear interpolation (default)
%                   'spline'    Cubic spline interpolation
%                   'cubic'     
%
%   extrapval   Value given to out-of-range points - NaN or 0 often used
%
% See also:
%   interp, interp_x

% Check input arguments:
if nargin<2; error('Check number of arguments to interp'); end

if isa_size(yi,[1,1],'IXTdataset_1d')
    yinterp = get_centres_1d(yi);
elseif isa_size(yi,[1,1],'IXTdataset_2d')
    yinterp = get_centres_2d(yi,2);
elseif isa_size(yi,'vector','double')
    yinterp = yi;
    if any(diff(yinterp)<=0)    % not monotonic increasing
        error('y values at which to compute interpolated values must be monotonic increasing')
    end
else
    error ('Check axis values for interpolation a numeric vector, or a single IXTdataset_1d or IXTdataset_2d')
end

% Perform interpolation
wout = w;
for i = 1:length(w)
    [xcent,ycent] = get_centres_2d(w(i));
    [xcent_grid,yinterp_grid]=ndgrid(xcent,yinterp);
    if isa(yi,'numeric')
        wout(i).y = yinterp;
    else
        wout(i).y = yi.y;
    end
    wout(i).signal = interpn(xcent, ycent, w(i).signal, xcent_grid, yinterp_grid, varargin{:});
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
