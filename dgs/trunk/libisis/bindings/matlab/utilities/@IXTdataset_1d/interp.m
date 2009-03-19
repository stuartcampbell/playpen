function wout = interp(w,xi,varargin)
% Interpolate an IXTdataset_1d or array of IXTdataset_1d
%
% The interpolation options are the same as the Matlab built-in routine interp1.
% For further details look at the Matlab help for interp1.
%
% Syntax:
%   >> wout = interp(w,xi)
%   >> wout = interp(w,wref)
%   >> wout = interp(...,method)
%   >> wout = interp(...,method,'extrap')
%   >> wout = interp(...,method,extrapval)
%
%   w           Input IXTdataset_1d or array of IXTdataset_1d
%
%   xi          Values at which interpolated values will be calculated
%                - output will be a point IXTdataset_1d
%               *OR*
%   wref        Another IXTdataset_1d from which the x values will be taken
%              (in this case must be a single IXTdataset_1d)
%                - output will be a histogram or point IXTdataset_1d according
%                  as the type from which the x values are taken
%
%   method          'nearest'   Nearest neighbour interpolation
%                   'linear'    Linear interpolation (default)
%                   'spline'    Cubic spline interpolation
%                   'pchip'     Piecewise cubic Hermite interpolation
%                   'cubic'     (Same as 'pchip')
%                   'v5cubic'   Cubic interpolation used in MATLAB 5
%
%   'extrap'    Extrapolates using the specified method. If not given, then
%               the Matlab default behaviour is followed:
%                   no extrapolation: 'nearest', 'linear', 'v5cubic'
%                   extrapolation:    'spline', 'pchip', 'cubic'
%
%   extrapval   Value given to out-of-range points - NaN or 0 often used

% Check input arguments:
if nargin<2; error('Check number of arguments to interp'); end

if isa_size(xi,[1,1],'IXTdataset_1d')
    xinterp = get_centres_1d(xi);
elseif isa_size(xi,'vector','double')
    xinterp = xi;
else
    error ('Check axis values for interpolation a numeric vector or a single IXTdataset_1d')
end

if any(diff(xinterp)<=0)    % not monotonic increasing
    error('x values at which to compute interpolated values must be monotonic increasing')
end
    
% Perform interpolation
wout = w;
for i = 1:length(w)
    xcent = get_centres_1d(w(i));
    if isa(xi,'numeric')
        wout(i).x = xinterp;
    else
        wout(i).x = xi.x;
    end
    wout(i).signal = interp1(xcent, w(i).signal, xinterp, varargin{:});
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
