function wout = func_eval (w, func_handle, varargin)
% Evaluate a function at the x values of an IXTdataset_1d or array of IXTdataset_1d)
% Syntax:
%   >> wout = func_eval (w, func, args)
% Input:
% ======
%   w           Dataset or array of datasets; the function will be evaluated
%              at the x values of the dataset(s)
%
%   func        Handle to the function to be evaluated
%              Function must be of form y = my_func(x,arg1,arg2,...)
%                       e.g. y = gauss (x, [height, cent, sig])
%              and must accept a vector of x values, and return a vector of
%              function values.
%
%   arg1,arg2...Arguments needed by the function. Typically there is only
%               one argument, which is a numeric array, as in the example above
%
% Output:
% =======
%   wout        Output IXTdataset_1d or array of IXTdataset_1d 
%
% e.g.
%   >> wout = func_eval (w, @gauss, [height, cent, sig])
%
%   where the function gauss appears on the matlab path
%           function y = gauss (x, p)
%           y = (p(1)/(sig*sqrt(2*pi))) * exp(-0.5*((x-p(2))/p(3)).^2);

wout = w;
for i=1:length(w)
    if length(w(i).x)==length(w(i).signal)
        xtemp = w(i).x;
    else
        xtemp = 0.5*(w(i).x(2:end)+w(i).x(1:end-1));
    end
    wout(i).signal = func_handle(xtemp,varargin{:});
    wout(i).error = zeros(size(w(i).error));
end