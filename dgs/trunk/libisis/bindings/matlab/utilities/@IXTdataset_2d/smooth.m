function wout = smooth (win, varargin)
% Smooths a IXTdataset_2d or an array of IXTdataset_2d
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
%   win     IXTdataset_2d or array of IXTdataset_2d to be smoothed
%
%   filter  Filter function for smoothing
%               - character string:
%                   'box'       box smoothing, with full widths given by size (below)
%                   'gaussian'  Gaussian smoothing
%                  (Default is 'box')
%            OR - handle to a function:
%                   The function must have form:
%                   -  y = my_func(x1,x2,pars)
%                   where x1,x2 are arrays that contain the coordinates of
%                   points in 2-dimensional space 
%                     e.g.  function y = gauss2d(x1,x2,p)
%                           y = exp(-0.5*((x1./p(1)).^2 + (x2./p(2)).^2)
%
%                   The smoothing algorithm will explicitly ensure normalisation of a
%                   user function by summing over the values of y in the box defined
%                   by size and dividing each element by the sum. To turn this feature off,
%                   use the 'nonorm' option below.
%                       
%   size    Size of convolution kernel e.g. [3,5]. 
%           If a scalar given, then it applies to each dimension. (Default is 3)
%           To smooth only along the x-axis, set size=[1,n] (n>0)
%           To smooth only along the y-axis, set size=[n,1] (n>0)
%
%   pars    Arguments needed by filter function
%               - built-in filter:
%                   'box'       None required
%                   'gaussian'  Standard deviation e.g. [0.5,1.2]. Default is 0.65
%               - user filter:
%                   Parameters as needed by function. See example above.
%
%  'nonorm' For user functions only: turn of normalisation in the kernel size.
%           
%
% Output:
% -------
%   wout    Smoothed IXTdataset_2d or array of IXTdataset_2d
%
% Example:
%   >> wout = smooth (w)                        % box filter of size [3,3,3...]
%   >> wout = smooth (w, 'gaussian', 5, [1.2,0.5])    % Gaussian filtering, ellispoidal shape
%   >> wout = smooth (w, @cosine_filter, 9, pi) % user function

wout = win;
for i = 1:length(win)
    [wout(i).signal,wout(i).error]=smooth(win(i).signal,win(i).error,varargin{:});
end 
