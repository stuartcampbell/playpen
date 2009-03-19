function [xout, yout] = ixf_find_closest(xin, yin, plotHandle_)
% Given x, y and a plot handle, the closest actual data point in that plot
% will be returned
% 
% >> [xout, yout] = ixf_find_closest(xin, yin, plotHandle_)
%
% Inputs
%---------
%
%   xin             x location of point
%   yin             y location of point
%   plotHandle_     handle of plot containing data of interest
%
% Outputs:
%---------
%
%   xout            x location of nearest point
%   yout            y location of nearest point

% Get out if no data given in
if isempty(xin) || isempty(yin);
    xout = [];  yout = [];
    return
end

% get the data from the given plot
xdata = get(plotHandle_,'xdata');

% If many plotHandles are given, xdata will be a cell, if only one then
% xdata will be a number. For generality, make EVERYTHING a cell.
if isnumeric(xdata)
    xdata = {xdata};
end

ydata = get(plotHandle_,'ydata');

if isnumeric(ydata)
    ydata = {ydata};
end

% we minimise the distance between points using pythagoras
min_distances = ones(size(plotHandle_));
min_index = ones(size(plotHandle_));
for i = 1:length(plotHandle_)
    distance = (abs(xdata{i} - xin)).^2 + (abs(ydata{i} - yin)).^2;
    min_distances(i) = min(distance);
    min_index(i) = min(find(min_distances(i) == distance));
end

% there may be many plots, so we have to look at each separately, then take
% the minimum distance again from what we found individually.
plot_index = min(min_distances) == min_distances;
array_index = min_index(plot_index);

% now we have the minimum point from all plots
xout = xdata{plot_index}(array_index);
yout= ydata{plot_index}(array_index);