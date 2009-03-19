function [xlabel, ylabel,zlabel] = make_label(d3d)

% Make Label command for IXTdataset_3d objects
%
% [xlab, ylab, zlab] = make_label(d3d)
%
% inputs: d3d
%
% outputs: 
%   xlab:       label for x axis when plotting d3d
%   ylab:       label for y axis when plotting d3d
%   zlab:       label for z axis when plotting d3d
%
% This function calls IXTunits/make_label.

% Dean Whittaker 2007

[xlabel,ylabel,zlabel] = make_label(d3d.x_axis, d3d.y_axis, d3d.s_axis, d3d.x_distribution,d3d.y_distribution);
