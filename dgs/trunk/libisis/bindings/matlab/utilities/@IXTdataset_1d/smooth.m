function wout = smooth (win, varargin)
% Smooths a IXTdataset_1d or an array of IXTdataset_1d
%
% Syntax:
%   >> wout = smooth (win)
%   >> wout = smooth (win, filter)
%   >> wout = smooth (win, filter, size)
%   >> wout = smooth (win, filter, size, pars)
%   >> wout = smooth (win, filter, size, pars,'norm')
%
% Input:
% ------
%   win     IXTdataset_1d or array of IXTdataset_1d to be smoothed
%
%   filter  Filter function for smoothing
%               - character string:
%                   'box'       box smoothing, with full widths given by size (below)
%                   'gaussian'  Gaussian smoothing
%                  (Default is 'box')
%            OR - handle to a function:
%                   The function must have form:
%                   -  y = my_func(x,pars)
%                   where x is array that contain the x-coordinates of
%                   points in 1-dimensional space 
%                      e.g. function y = gauss(x,p)
%                           y = exp(-0.5*((x./p(1)).^2)
% 
%                   The smoothing algorithm will explicitly ensure normalisation of a
%                   user function by summing over the values of y in the box defined
%                   by size and dividing each element by the sum. To turn this feature off,
%                   use the 'nonorm' option below.
%
%   size    Size of convolution kernel. Must be +ve odd number. Sets the maximum extent
%           of the smoothing function
%
%   pars    Arguments needed by filter function
%               - built-in filter:
%                   'box'       None required
%                   'gaussian'  Standard deviation. Default is 0.65
%               - user filter:
%                   Parameters as needed by function. See example above.
%
%  'nonorm' For user functions only: turn of normalisation in the kernel size.
%           
%
% Output:
% -------
%   wout    Smoothed IXTdataset_1d or array of IXTdataset_1d
%
% Example:
%   >> wout = smooth (w)                        % box filter of size [3,3,3...]
%   >> wout = smooth (w, 'gaussian', 7, 1.2)    % Gaussian filtering
%   >> wout = smooth (w, @cosine_filter, 9, pi) % user function

wout = win;
for i = 1:length(win)
    [wout(i).signal,wout(i).error]=smooth(win(i).signal,win(i).error,varargin{:});
end 
