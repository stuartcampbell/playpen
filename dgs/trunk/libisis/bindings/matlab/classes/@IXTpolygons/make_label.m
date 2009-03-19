function [xlab,ylab,zlab] = make_label(win)
% make label function for IXTpolygons
%
% >> [xlabel, ylabel] = make_label(win)
%
% Constructs x and z labels for IXTpolygons objects.

[xlab,ylab, zlab] = make_label(win.x_axis,win.y_axis, win.s_axis, win.x_distribution,win.y_distribution);
