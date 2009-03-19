function wout = func_eval (w, func_handle, varargin)
% Evaluate a function at the x and y values of an IXTdataset_2d or array of IXTdataset_2d)
% Syntax:
%   >> wout = func_eval (w, func, args)
% Input:
% ======
%   w           Dataset or array of datasets; the function will be evaluated
%              at the x and y values of the dataset(s)
%
%   func        Handle to the function to be evaluated
%              Function must be of form z = my_func(x,y,arg1,arg2,...)
%               e.g. z = gauss2d (x, y, [height, cent_x, cent_y, sigxx, sigxy, sigyy])
%              and must accept two vectors of equal length that contain the
%              x and y values. It return a vector of the function values.
%
%   arg1,arg2...Arguments needed by the function. Typically there is only
%               one argument, which is a numeric array, as in the example above
%
% Output:
% =======
%   wout        Output IXTdataset_2d or array of IXTdataset_2d 
%
% e.g.
%   >> wout = func_eval (w, @gauss, [height, cent_x, cent_y, sigxx, sigxy, sigyy])
%
%   where the function gauss appears on the matlab path
%           function z = gauss2d (x, y, p)
%           z = (p(1)/(sig*sqrt(2*pi))) * ...

wout = w;
for i=1:length(w)
    if (length(w(i).x)~=size(w(i).signal,1))
        xtemp = 0.5*(w(i).x(1:end-1)+w(i).x(2:end));
    else
        xtemp = w(i).x;
    end
    if (length(w(i).y)~=size(w(i).signal,2))
        ytemp = 0.5*(w(i).y(1:end-1)+w(i).y(2:end));
    else
        ytemp = w(i).y;
    end
    [xtemp,ytemp]=ndgrid(xtemp,ytemp);
    xtemp=reshape(xtemp,prod(size(xtemp)),1);
    ytemp=reshape(ytemp,prod(size(ytemp)),1);

    wout(i).signal = reshape(func_handle(xtemp,ytemp,varargin{:}),size(w(i).signal));
    wout(i).error = zeros(size(w(i).error));
end