function [wout, fitdata] = fit(win, func, pin, varargin)
% Fitting routine for IXTdataset_1d. If passed an array of 
% IXTdataset_1d, then each is fitted independently to the same function.
%
% Syntax:
%   >> [yout, fitdata] = fit(win, func, pin)
%   >> [yout, fitdata] = fit(win, func, pin, pfree)
%   >> [yout, fitdata] = fit(win, func, pin, pfree, keyword, value)
%
%   keyword example:
%   >> [yout, fitdata] = fit(..., 'fit', fcp)
%
% Input:
% ======
%   win     IXTdatset_1d object or array of IXTdataset_1d objects to be fitted
%
%   func    Handle of the function to fit to the data. Function should be of form
%               y = myfunc(x,pin)
%           where
%               x = vector of x values at which to calculate the y values
%               pin=vector of parameter values needed 
%             e.g.
%               function y = gauss(x,p)
%               y = p(1)*exp(-0.5*((x-p(2))/p(3)).^2);
%
%   pin     Initial function parameter values [pin(1), pin(2)...]
%
%   pfree   Indicates which are the free parameters in the fit
%           e.g. [1,0,1,0,0] indicates first and third are free
%           Default: all are free
%
%
%   Optional keywords:
%   ------------------
%   'list'  Numeric code to control output to Matlab command window to monitor
%           status of fit
%               =0 for no printing to command window
%               =1 prints iteration summary to command window
%               =2 additionally prints parameter values at each iteration
%
%   'fit'   Array of fit control parameters
%           fcp(1)  relative step length for calculation of partial derivatives
%           fcp(2)  maximum number of iterations
%           fcp(3)  Stopping criterion: relative change in chi-squared
%                   i.e. stops if chisqr_new-chisqr_old < fcp(3)*chisqr_old
%
%   'keep'  Ranges of x to retain for fitting. A range is specified by a pair
%           of numbers which define the lower and upper bounds
%               [xlo,xhi]
%           Several ranges can be given by making an (m x 2) array:
%               [x1_lo, x1_hi; x2_lo, x2_hi; ...]
%
%  'remove' Ranges to remove from fitting. Follows the same format as 'keep'.
%
%   'mask'  Array of ones and zeros, with the same number of elements as the data
%           array, that indicates which of the data points are to be retained for
%           fitting
%
%  'select' Calculates the returned function values, yout, only at the points
%           that were selected for fitting by 'keep' and 'remove'; all other
%           points are set to NaN. This is useful for plotting the output, as
%           only those points that contributed to the fit will be plotted.
%
% Output:
% =======
%   wout    IXTdataset_1d object containing the evaluation of the function for the
%          fitted parameter values.
%
%   fitdata Result of fit for each dataset
%               fitdata.p      - parameter values
%               fitdata.sig    - estimated errors (=0 for fixed parameters)
%               fitdata.corr   - correlation matrix for free parameters
%               fitdata.chisq  - reduced Chi^2 of fit (i.e. divided by
%                                   (no. of data points) - (no. free parameters))
%               fitdata.pnames - parameter names
%                                   [if func is mfit function; else named 'p1','p2',...]
%
% EXAMPLES: 
%
% Fit a Gaussian, starting with height=100, centre=5, sigma=3, and allowing
% height and centre to vary:
%   >> [wfit, fdata] = fit(w, @gauss, [100, 5, 3], [1 1 0])
%
% All parameters free to fit, but use only data in range x=20-100 and 150-300:
%   >> [wfit, fdata] = fit(w, @gauss, [100, 5, 3], 'keep', [20, 100; 150, 300])

wout = win;
for i = 1:length(win)
    if (length(win(i).x)~=size(win(i).signal,2))
        xin = 0.5*(win(i).x(1:end-1)+win(i).x(2:end));
    else
        xin = win(i).x;
    end
    if i>1, fitdata(length(win))=fitdata(1); end    % preallocate
    [signal, fitdata(i)] = fit(xin', win(i).signal', win(i).error', func, pin, varargin{:});
    wout(i).signal = signal';
    wout(i).error = zeros(size(wout(i).signal));
end