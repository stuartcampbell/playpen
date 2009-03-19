function varargout = smooth (varargin)
% Smooths an n dimensional dataset
%
% Syntax:
%   >>        yout = smooth (y)
%   >> [yout,eout] = smooth (y,e)
%   >>         ... = smooth (..., filter)
%   >>         ... = smooth (..., filter, size)
%   >>         ... = smooth (..., filter, size, pars)
%   >>         ... = smooth (..., filter, size, pars,'nonorm')
%
% Input:
% ------
%   y       Input n-dimensional array to be smoothed
%
%   e       Optional array of error bars on the y values. Must have same size as
%           y array.
%
%   filter  Filter function for smoothing
%               - character string:
%                   'box'       box smoothing, with full widths given by size (below)
%                   'gaussian'  Gaussian smoothing
%                  (Default is 'box')
%            OR - handle to a function:
%                   The function must have form:
%                   -  y = my_func(x1,x2,x3...xn,pars)
%                   where x1,x2..xn are arrays that contain the coordinates of
%                   points in n-dimensional space 
%                       e.g. for 3D smoothing:
%                           function y = gauss(x1,x2,x3,p)
%                           y = exp(-0.5*((x1./p(1)).^2 + (x2./p(2)).^2 + (x3./p(3)).^2))
%
%                   The smoothing algorithm will explicitly ensure normalisation of a
%                   user function by summing over the values of y in the box defined
%                   by size and dividing each element by the sum. To turn this feature off,
%                   use the 'nonorm' option below.
%                       
%   size    Size of convolution kernel e.g. [3,5,5,5]. 
%           If a scalar given, then it applies to each dimension. (Default is 3)
%
%   pars    Arguments needed by filter function
%               - built-in filter:
%                   'box'       None required
%                   'gaussian'  Standard deviation along each dimension. 
%                               If scalar, applies to each dimension. Default is 0.65
%               - user filter:
%                   Parameter as needed by function. See example above.
%
%  'nonorm' For user functions only: turn of normalisation in the kernel size.
%           
%
% Output:
% -------
%   yout    Smoothed array.
%
% Example:
%   >> yout = smooth (y)                              % box filter of size [3,3,3...]
%   >> yout = smooth (y, 'gaussian', [5,1,1], 1.2)    % Gaussian filtering along x axis only
%   >> yout = smooth (y, @cosine_filter, [5,3,5], [pi,pi/2,pi])   % user function in 3D
%
%
% NOTE:
% =====
% The smoothing ignores elements of the y-array that contain NaNs.
%
% Is a generalisation of the Matlab intrinsic function smooth3 to arbitrary dimensions
% and to allow user filter functions, as well as smoothing corresponding error bars
%

% -------------------------------------------------------------------------------
% List of available filter functions and defaults
% -------------------------------------------------------------------------------
filter_names = {'box'; 'gaussian'};       % available functions for convolution
filter_handles= {@filter_box_nd; @filter_gauss_nd};   % corresponding function handles
filter_takes_pars =[false,true];
ifilter_default = 1;
size_filter_default = 3;   % must be +ve odd number

% -------------------------------------------------------------------------------
% Check validity of input arguments
% -------------------------------------------------------------------------------
% Check if one or two input arrays given
if nargin>=1 && isa(varargin{1},'double') && ~isa(varargin{2},'double')
    y = varargin{1};
    narr=1;
    nopt=nargin-narr;
elseif nargin>=2 && (isa(varargin{1},'double') && isa(varargin{2},'double')) && ...
        length(size(varargin{1}))==length(size(varargin{2})) && all(size(varargin{1})==size(varargin{2}))
    y = varargin{1};
    e = varargin{2};
    narr=2;
    nopt=nargin-narr;
else
    error('Check input array(s) to be smoothed are numeric, and, if two arrays, match in size')
end
ndim = length(size(y));
if isvector(y);ndim=1;end

% If provided filter, check is a built-in function, or a function handle to a user filter
% At end of this block, has set filter_handle and ifilter>0 if built-in, =0 if not.
if nopt>=1
    filter = varargin{narr+1};
    if isa_size(filter,'row','char')
        ifilter = string_find (filter,filter_names);
        if ifilter>0
            filter_handle = filter_handles{ifilter};
        elseif ifilter<0
            error ('Ambiguous filter function name')
        elseif ifilter==0
            error (['Filter name ''',filter,''' is not recognised as an available option'])
        end
    elseif isa(filter,'function_handle')
        ifilter=0;
        filter_handle = filter;
    else
        error ('Filter type must be a character string or handle to a user filter function')
    end
else
    ifilter=ifilter_default;
    filter_handle = filter_handles{ifilter_default};
end

% Get size of filter as a row vector. Must contain +ve odd numbers only
% If y is a vector, then size of filter will be a scalar
if nopt>=2
    size_filter_in = varargin{narr+2};
else
    size_filter_in = size_filter_default;
end
if isa_size(size_filter_in,'row','double')
    if length(size_filter_in)==1  % scalar kernel size
        size_filter=size_filter_in*ones(1,ndim);
    elseif length(size_filter_in)==length(size(y))
        if ndim==1
            if (isrow(y) && size_filter_in(1)==1)||(iscolumn(y) && size_filter_in(2)==1)
                size_filter=max(size_filter_in);    % ensure scalar size
            else
                error ('Kernel size must correspond to a vector of same type (row or column) as vector to be smoothed')
            end
        else
            size_filter=size_filter_in;
        end
    else
        error ('Check dimension of convolution kernel matches that of array(s) to be smoothed')
    end
    % Check size is all +ve odd numbers
    if any(rem(size_filter,1)) || ~all(rem(size_filter,2)) || min(size_filter)<1
        error ('All elements of SIZE must be odd integers greater than or equal to 1.')
    end
else
    error ('Size of convolution kernel must be numeric row vector')
end

% Get filter parameters, if any provided, and check if built-in filters take parameters
if nopt>=3
    if ifilter>0 && ~filter_takes_pars(ifilter)
        error(['Filter function ''',filter_names{ifilter},''' does not take arguments'])
    end
    pars=true;
    parvals=varargin{narr+3};
else
    pars=false;
end

% Only if user filter is option of 'norm' keyword allowed
if nopt==4
    if isa_size(varargin{narr+4},'row','char') && strncmpi(varargin{narr+4},'nonormalisation',length(varargin{narr+4}))
        if ifilter>0   % not user function
            error('Normalisation option is for user filters only')
        else
            norm=false;
        end
    else
        error(['Unrecognised argument number ',num2str(narr+4)])
    end
else
    norm=true;
end

% -------------------------------------------------------------------------------
% Perform convolution
% -------------------------------------------------------------------------------

% Create convolution array
if ifilter>0 && strcmp(filter_names(ifilter),'box')    % special case of box does not need grid construction
    if ndim==1
        size_filter=[1,size_filter];
        if iscolumn(y); size_filter=fliplr(size_filter); end
    end
    c = ones(size_filter)/prod(size_filter);
else
    half_size = (size_filter-1)/2;
    if ndim==1 % input array is a vector, so don't need to use ndgrid
        grid=-half_size:half_size;
        if iscolumn(y); grid=grid'; end % make column if y is column
    else
        grid_in=cell(1,ndim);
        for i=1:ndim
            grid_in{i}=-half_size(i):half_size(i);
        end
        grid=ndgridcell(grid_in);   % modified version of ndgrid
    end
    % Check parameters for internal filters
    if ifilter>0
        if strcmp(filter_names{ifilter},'gaussian')
            if ~pars
                parvals=0.65;   % default sigma for gaussian filter (matches Matlab default for smooth3)
            elseif ~(isrow(parvals)&&isnumeric(parvals)&&(size(parvals,2)==1|size(parvals,2)==ndim))
                error('sigma to gaussian filter must be scalar or row vector')
            end
            pars=true;
        end
    end
    % Get convolution matrix
    if pars
        c = filter_handle(grid{:},parvals);
    else
        c = filter_handle(grid{:});
    end
    if norm
        c = c/sum(c(:));
    end
end

% Perform convolution, carefully handling NaNs

index = ~isnan(y);  % elements with non-NaN counts
weight = convn(double(index),c,'same'); % weight function including only points where there is data
weight(~index) = NaN;                   % normalisation by weight will ensure that points with NaN signal will be restored to NaN

yout=y;
yout(~index) = 0;   % replace NaNs with zeros in y array so that they do not contribute to smoothing
yout = convn(yout,c,'same')./weight;    % points with no data (i.e. y=0) do not contribute to convolution
if narr==2
    eout=e;
    eout(~index) = 0;   % likewise
    eout = sqrt(convn(eout.^2,c.^2,'same'))./weight;
end

varargout{1}=yout;
if narr==2; varargout{2}=eout; end

%----------------------------------------------------------------------------------------
function y=filter_gauss_nd(varargin)
% n-dimensional Gaussian
%   >> y = filter_gauss_nd(x1,x2,x3...xn,sig)
%
x=varargin(1:end-1);
ndim=length(varargin)-1;
sig=varargin{end};
if isscalar(sig); sig=sig*ones(1,ndim); end

for i=1:ndim
    x{i}=(x{i}.^2)./(sig(i)^2);
end

size_x=length(size(varargin{1}));    % size of x1 array; we assume all arrays xi have same size
y=exp(-0.5*sum(cat(size_x+1,x{:}),size_x+1));  % concatenate all x arrays into one, the outer dimension has size ndim; then sum
