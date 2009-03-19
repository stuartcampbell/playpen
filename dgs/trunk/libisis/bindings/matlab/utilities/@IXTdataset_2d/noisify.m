function wout = noisify (w,varargin)
% Adds noise to y values and associated error bars of an IXTdataset_2d or array
% of IXTdataset_2d
%
% Syntax:
%   >> wout = noisify (w)
%   >> wout = noisify (w,factor)
%           Add noise with Gaussian distribution, with standard deviation
%           = factor*(maximum y value); default factor is 0.1
%
%   >> wout = noisify (w,'poisson')
%           Add noise with Poisson distribution, where the mean value at
%           a point is the value y.
%
wout=w;
for i=1:length(w)
    [wout(i).signal,wout(i).error]=noisify(w(i).signal,w(i).error,varargin{:});
end
