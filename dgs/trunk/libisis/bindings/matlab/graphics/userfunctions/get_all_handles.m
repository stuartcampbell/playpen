function [fig_out, axes_out, plot_out, other_out] = get_all_handles(varargin)
%---------------help for gtk get all handles command, gallh----------------
%
% syntax: [figureHandle_, axesHandle_, plotHandle_, otherHandle_] =
% GALLH(ident)
%
% >> [figureHandle_, axesHandle_, plotHandle_, otherHandle_] = GALLH(name,
% tag)
%
% inputs: ident - any handle relating to an object
%           OR 
%         name -    name of an object 
%         tag -     tag of an object
% outputs - all figure, axes, plot and other handles related to that object
% (i.e. parents and children)
%
% example:
%
% >> [fh, ah, ph] = GALLH('my_plotmy_tag')
%
%--------------------------------------------------------------------------
% Dean Whittaker 2007
if nargin == 0
    varargin = {0};
end

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ui_get_handles(varargin{:});

% display('figure handles:')
% display(num2str(figureHandle_))
% display(' ')
% display('axes handles:')
% display(num2str(axesHandle_))
% display(' ')
% display('plot handles:')
% display(num2str(plotHandle_))
% display(' ')
% display('other handles:')
% display(num2str(otherHandle_))
% display(' ')

if nargout > 0
    fig_out = figureHandle_;
    axes_out = axesHandle_;
    plot_out = plotHandle_;
    other_out = otherHandle_;
end
