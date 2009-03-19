function [figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ui_get_handles(varargin)
%---------------help for gtk get handles command---------------------------
%
% purpose: get all figure, axes and plot handles related to an object
%
% syntax: [figureHandle_, axesHandle_, plotHandle_, otherHandle_] =
% ui_get_handles(ident)
%
% inputs: ident - any handle relating to an object, or the nametag (with no
% space between the name and tag) of the figure
%
% outputs - all figure, axes, plot and other handles related to that object
% (i.e. parents and children)
%
%--------------------------------------------------------------------------

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_gen_interface('iname', ...
    'getprop_interface','fname','getrelatedhandles', varargin{:});