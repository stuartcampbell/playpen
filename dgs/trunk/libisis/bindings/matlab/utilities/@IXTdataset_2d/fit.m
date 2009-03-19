function [wout, fitdata] = fit(win, func, pin, varargin)
% Fitting routine for IXTdataset_2d. If passed an array of 
% IXTdataset_2d, then each is fitted independently to the same function.
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
%   win     IXTdatset_2d object or array of IXTdataset_2d objects to be fitted
%
%   func    Handle of the function to fit to the data. Function should be of form
%               z = myfunc(x,y,pin)
%           where
%               x,y = Vectors containing every (x,y) pair at which to
%                    calculate the z values
%               pin = Vector of parameter values needed 
%             e.g.
%               function y = gauss2d(x,y,p)
%               z = p(1).*exp(-0.5*(((x - p(2))/p(4)).^2+((y - p(3))/p(5)).^2);
%
%   pin     Numeric array of initial parameter values
%
%   pfree   Indicates which are the free parameters in the fit
%           e.g. [1,0,1,0,0] indicates first and third are free
%           Default: all are free
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
%   'keep'  Ranges of x and y to retain for fitting. A range is specified by two 
%           pairs of numbers which define a rectangle:
%               [xlo, xhi, ylo, yhi]
%           Several ranges can be defined by making an (m x 4) array:
%               [xlo(1), xhi(1), ylo(1), yhi(1); xlo(2), xhi(2), ylo(2), yhi(2); ...]
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
%   wout    IXTdataset_2d object containing the evaluation of the function for the
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
% Fit a 2D Gaussian, allowing only height and position to vary:
%   >> ht=100; x0=1; y0=3; sigx=2; sigy=1.5;
%   >> [wfit, fdata] = fit(w, @gauss2d, [ht,x0,y0,sigx,0,sigy], [1,1,1,0,0,0])
%
% Allow all parameters to vary, but remove two rectangles from the data
%   >> ht=100; x0=1; y0=3; sigx=2; sigy=1.5;
%   >> [wfit, fdata] = fit(w, @gauss2d, [ht,x0,y0,sigx,0,sigy], ...
%                               'remove',[0.2,0.5,2,0.7; 1,2,1.4,3])

wout = win;
for i = 1:length(win)
    % make sure point data
    if (length(win(i).x)~=size(win(i).signal,1))
        xin = 0.5*(win(i).x(1:end-1)+win(i).x(2:end));
    else
        xin = win(i).x;
    end
    if (length(win(i).y)~=size(win(i).signal,2))
        yin = 0.5*(win(i).y(1:end-1)+win(i).y(2:end));
    else
        yin = win(i).y;
    end

    % get x y and z data in the form of triples instead of co-ordinates
    [xin, yin] = ndgrid(xin, yin);          % mesh x and y 
    xin = reshape(xin,numel(xin),1);        % get x into single column
    yin = reshape(yin,numel(yin),1);        % get y into single column
    zin = reshape(win(i).signal,numel(win(i).signal),1); 
    ein = reshape(win(i).error,numel(win(i).error),1); 

    if i>1, fitdata(length(win))=fitdata(1); end    % preallocate
    [signal, fitdata(i)] = fit([xin, yin], zin, ein, func, pin, varargin{:});
    
    wout(i).signal = reshape(signal,size(win(i).signal));
    wout(i).error = zeros(size(win(i).error));   
end