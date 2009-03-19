function wout = interp(w,varargin)
% Interpolate an IXTdataset_2d or array of IXTdataset_2d
%
% The interpolation options are the same as the Matlab built-in routine interpn.
% For further details look at the Matlab help for interpn.
%
% Syntax:
%   >> wout = interp(w,xi,yi)
%   >> wout = interp(w,wref)
%   >> wout = interp(...,method)
%   >> wout = interp(...,method,extrapval)
%
%   w           Input IXTdataset_2d or array of IXTdataset_2d
%
%   xi,yi       Vectors that define the grid points at which interpolated values will be calculated
%                - output will be a point IXTdataset_2d
%               *OR*
%   wref        Another IXTdataset_2d from which the x values will be taken
%              (in this case must be a single IXTdataset_2d)
%                - output will be a histogram or point along the x and y axes according
%                  as the IXTdataset_2d from which the x and y values are taken
%
%   method          'nearest'   Nearest neighbour interpolation
%                   'linear'    Linear interpolation (default)
%                   'spline'    Cubic spline interpolation
%                   'cubic'     
%
%   extrapval   Value given to out-of-range points - NaN or 0 often used
%
% See also:
%   interp_x, interp_y

% Check input arguments:
narg=length(varargin);
if narg==0; error('Check number of arguments to interp'); end

if narg>=1 && isa_size(varargin{1},[1,1],'IXTdataset_2d')
    [xinterp,yinterp]=get_centres_2d(varargin{1})
    nstart=2;   % first argument to pass to interpn
elseif narg>=2 && isa_size(varargin{1},'vector','double') && isa_size(varargin{2},'vector','double')
    xinterp=varargin{1};
    yinterp=varargin{2};
    nstart=3;   % first argument to pass to interpn
else
    error ('Check axis values for interpolation a numeric vector or a single IXTdataset_2d')
end

if any(diff(xinterp)<=0)||any(diff(yinterp)<=0)    % not monotonic increasing
    error('x and y array values at which to compute interpolated values must be monotonic increasing')
end
[xinterp_grid,yinterp_grid]=ndgrid(xinterp,yinterp);    % ndgrid gives same output for any mix of row or column vectors

% Perform interpolation
wout = w;
for i = 1:length(w)
    [xcent,ycent]=get_centres_2d(w(i));
    if isa(varargin{1},'numeric')
        wout(i).x = xinterp;
        wout(i).y = yinterp;
    else
        wout(i).x = varargin{1}.x;
        wout(i).y = varargin{1}.y;
    end
    wout(i).signal = interpn(xcent, ycent, w(i).signal, xinterp_grid, yinterp_grid, varargin{nstart:end});
    wout(i).error = zeros(size(wout(i).signal));
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
