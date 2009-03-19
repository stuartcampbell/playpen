function [varargout] = dxyc(varargin)
%------help for gtk display x, y and c coordinates command dxyc----------------
%
% USE DXY FOR ONE DIMENSIONAL PLOTS!!
% 
% Function Syntax: 
%
% >> dxyc
% >> dxyc(‘text’)
% >> [x,y] = dxyc
% >> [x,y,z] = dxyc
% >> [x,y,z] = dxyc('text')
% >> [x,y,z] = dxyc(‘property’, ’value’)
% >> [x,y,z] = dxyc(‘text’, ’property’, ’value’, ...)
%
% Purpose: Display x, y & colour coordinates of plot and add text to the plot
%
%   For surface and area plots: 
%   x,y,c correspond to a point on the first 
%   patch or surface face intersected along the 
%   selection ray (into the screen). If no 
%   face is encountered along the selection ray, 
%   x,y,c returns NaN. See documentation for details. 
%
% Output: x coordinates, y coordinates and colour coordinates
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
% [x, y, z] = dxyc('my point', 'display_coords', true)
%
% If the point (1, 3, 2) is selected on a plot using the above example,
% text will be placed on the plot at the point (1, 3, 2):
%
% "my point (1, 3, 2)"
%--------------------------------------------------------------------------
% Dean Whittaker 2008

[varargout{1:nargout}] = ui_showxyz('xyc',varargin{:});