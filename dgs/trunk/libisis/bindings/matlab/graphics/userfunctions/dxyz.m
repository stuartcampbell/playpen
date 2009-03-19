function [varargout] = dxyz(varargin)
%------help for gtk display x, y and z coordinates command dxy----------------
%
% USE DXY FOR ONE DIMENSIONAL PLOTS!!
% 
% Function Syntax: 
%
% >> dxyz
% >> dxyz(‘text’)
% >> [x,y] = dxyz
% >> [x,y,z] = dxyz
% >> [x,y,z] = dxyz('text')
% >> [x,y,z] = dxyz(‘property’, ’value’)
% >> [x,y,z] = dxyz(‘text’, ’property’, ’value’, ...)
%
% Purpose: Display x, y & z coordinates of plot and add text to the plot
%
%   For surface and area plots: 
%   x,y,z correspond to a point on the first 
%   patch or surface face intersected along the 
%   selection ray (into the screen). If no 
%   face is encountered along the selection ray, 
%   x,y,z returns NaN. See documentation for details. 
%
%   For multiplot:
%   x,y,z correspond to the closest data point on the closest intersecting
%   face to the selection ray.
%
% Output: x coordinates, y coordinates and z coordinates
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
% [x, y, z] = DXYZ('my point', 'display_coords', true)
%
% If the point (1, 3, 2) is selected on a plot using the above example,
% text will be placed on the plot at the point (1, 3, 2):
%
% "my point (1, 3, 2)"
%--------------------------------------------------------------------------
% Dean Whittaker 2007

%[x,y] = ixf_gen_interface('iname','show_interface','fname','show');
IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

[figureHandle_, axesHandle_, plotHandles_, otherHandle_] = ixf_get_related_handles(gca);
area_flag=[];


% check for area plot, use colour instead of z limits

for i = 1:length(plotHandles_)
    
    plot_type = ixf_plotdata('get', plotHandles_(i), 'plot_type');

   if (plot_type == IXG_ST_STDVALUES.area_type) || plot_type == (IXG_ST_STDVALUES.patch_type)
        area_flag(i) = 1;
   end
end

if all(area_flag)
    [varargout{1:nargout}] = ui_showxyz('xyc',varargin{:});
else
    [varargout{1:nargout}] = ui_showxyz('xyz',varargin{:});
end