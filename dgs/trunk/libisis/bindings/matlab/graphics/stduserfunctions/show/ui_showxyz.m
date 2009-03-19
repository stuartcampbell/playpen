function [varargout] = ui_showxyz(type, varargin)
%help for libisis graphics general user function display x, y and z coordinates command ui_showxyz
% Function Syntax:
%
% >> ui_showxyz
% >> ui_showxyz(type, ‘text’)
% >> [x,y] = ui_showxyz(type)
% >> [x,y,z] = ui_showxyz(type)
% >> [x,y,z] = ui_showxyz(type, 'text')
% >> [x,y,z] = ui_showxyz(type, ‘property’, ’value’)
% >> [x,y,z] = ui_showxyz(type, ‘text’, ’property’, ’value’, ...)
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
%           'closest point'     Instead of the nearest intersecting
%                               surface, the closest actual data point will
%                               be found.
%
%           if a string 'text' is given as the first argument, it is
%           assumed that this is text to add to the selected point.
%
%
% Example:
% [x, y, z] = ui_showxyz('my point', 'display_coords', true)
%
% If the point (1, 3, 2) is selected on a plot using the above example,
% text will be placed on the plot at the point (1, 3, 2):
%
% "my point (1, 3, 2)"
%--------------------------------------------------------------------------
t = '';

if nargout == 0 && nargin == 1
    ixg_input = {'display_coords',true};
else
    ixg_input = varargin;
end

switch type
    case 'xyz'

        if nargin==1                % if no text ixg_input;
            [x, y, z] = ixf_gen_interface('iname', 'show_interface', 'fname', 'showz', ixg_input{:});

        else                        % add text to plot as well
            [x, y, z, t] = ixf_gen_interface('iname', 'show_interface', 'fname','showtextz',  ixg_input{:});
        end
        if nargout > 0
            varargout{1} = x;
            varargout{2} = y;
            varargout{3} = z;
            varargout{4} = t;
        end

    case 'xy'

        if nargin==1                % if no text ixg_input;
            [x, y] = ixf_gen_interface('iname', 'show_interface', 'fname', 'show', ixg_input{:});


        else                        % add text to plot as well
            [x, y, t] = ixf_gen_interface('iname', 'show_interface', 'fname','showtext',  ixg_input{:});
        end
        if nargout > 0
            varargout{1} = x;
            varargout{2} = y;
            varargout{4} = t;
        end

    case 'xyc'

        if nargin==1                % if no text ixg_input;
            [x, y, z] = ixf_gen_interface('iname', 'show_interface', 'fname', 'showc', ixg_input{:});

        else                        % add text to plot as well
            [x, y, z, t] = ixf_gen_interface('iname', 'show_interface', 'fname','showtextc',  ixg_input{:});
        end
        if nargout > 0
            varargout{1} = x;
            varargout{2} = y;
            varargout{3} = z;
            varargout{4} = t;
        end
end




    
%