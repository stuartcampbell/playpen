function [varargout] = dxy(varargin)
%------help for gtk display x, y and z coordinates command dxy----------------
% Function Syntax: 
%
% >> dxyz
% >> dxyz(‘text’)
% >> [x,y] = dxy
% >> [x,y] = dxy
% >> [x,y] = dxy('text')
% >> [x,y] = dxy(‘property’, ’value’)
% >> [x,y] = dxy(‘text’, ’property’, ’value’, ...)
%
% Purpose: Display x, y coordinates of plot and add text to the plot
%
%  Output: x coordinates, y coordinates
%
%   If called with no inputs or outputs, then the co-ordinates will
%   be placed as text at the point selected.  
% 
% Inputs: inputs should be property, value pairs, available options are:
%
% 
%           'no_points'         value will be the number of points which you
%                               want to get from the plot (default is until
%                               keypress - same as empty)
%
%           'display_coords'    value will be true to add the co-ordinates
%                               to the end of any text, false not to (default) 
%
%           'text'              value will be a string to add to the plot
%                               at the selected point
%
%           'closest_point'     Instead of the nearest intersecting
%                               surface, the closest actual data point will
%                               be found.
%
%           if a string 'text' is given as the first argument, it is
%           assumed that this is text to add to the selected point. 
%
%
% Example:
% [x, y] = DXY('my point', 'display_coords', true)
%
% If the point (1, 3) is selected on a plot using the above example,
% text will be placed on the plot at the point (1, 3):
%
% "my point (1, 3)"
%--------------------------------------------------------------------------
% Dean Whittaker 2008

% use the xy type option with ui_showxyz - this now performs xy, xyz, xyc
% options. 
[varargout{1:nargout}] = ui_showxyz('xy',varargin{:});