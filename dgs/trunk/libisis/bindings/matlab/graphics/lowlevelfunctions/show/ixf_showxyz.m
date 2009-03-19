function [xcoord,ycoord,zcoord,t]  = ixf_showxyz(varargin)
%-----help for libisis graphics ixf_showxyz show xyz function--------------
%Function Syntax: 
%
% >> [xcoord,ycoord,zcoord] = ixf_showxyz('property','value'...)
% or
% >> [xcoord, ycoord, zcoord] = ixf_showxyz('text','property','value',...)
%
%Purpose: Display x, y, z coordinates of plot
%
%Output: x coordinates, y coordinates and z coordinates.
%
%Inputs: inputs should be property, value pairs, available options are:
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
%Example:
%
% >>  [x, y, z] = ixf_showxy('text','display_coords',1)
%
%--------------------------------------------------------------------------


% This script is complicated because we need to find the z component, which
% is very difficult and different for every plot type. 

[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

t = '';
xcoord = [];
ycoord = [];
zcoord = [];


% parse varargin 
arglist = struct('no_points',[],'display_coords',false,'text','','closest_point',false);

[parse, args, present] = parse_arguments(varargin, arglist);

% assume any text at the start is text to place on the plot

if ~isempty(parse) && isempty(args.text)
    if length(parse) == 1
        args.text = parse{1};
    else
        warning('unrecognised property-value pairs passed');
    end
end

%---- error checks

if ~ ischar(args.text)
    error('incorrect format for property "text",  requires a single character string')
end

if ~ isnumeric(args.no_points)
    error('incorrect format for property "no_points", requires a numerical value')
end

if ~ islogical(args.display_coords)
    error('incorrect format for property "display_coords", requires a logical value (TRUE or FALSE)')
end

%-----------check figure
flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

if ~ ixf_check_graphicfigure('handle',gcf)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(gcf);

%---get handles and data
plot_types = zeros(size(plotHandle_));
for i = 1:length(plotHandle_)
    plot_types(i) = ixf_plotdata('get',plotHandle_(i),'plot_type');
end

%% Main part of function
button = 1;
i = 1;
while button == 1   % loop until told not to 
    % use ginput to give user a crosshair and get standard x,y coords.
    [x,y,button] = ginput(1);
    
    % Get more general z co-ordinate from intersecting surface.
    if button == 1
    warning off   % select3d will find the closest intersection of a surface. 
        [point, vertex, vi, face, facei] = ixf_select3d();
    warning on
    
    % vertex information will effectively be the closest point. Use this
    % for multiplot or for line types, since there is not surface to
    % intersect. 
    if all(plot_types == IXG_ST_STDVALUES.waterfall_type) || args.closest_point || all(plot_types == IXG_ST_STDVALUES.twod_line_type)
        point = vertex;
    end

    if isempty(point) % If the point is not an intersection then NaN 
        xcoord(i) = NaN;    ycoord(i) = NaN;   zcoord(i) = NaN;
        warning('No surface intersects selected point.')
    else
        xcoord(i) = point(1);   ycoord(i) = point(2);   zcoord(i) = point(3);
    end

      
%?????
%         if all(object_type == IXG_ST_STDVALUES.patch_type)
%             xyzdata = ixf_plotdata('get',objectHandle_,'xyzdata');
%             point(3) = xyzdata.z(facei);
%         end
%?????

    % add coordinates to text if required.
        if args.display_coords
            if ~all(plot_types == IXG_ST_STDVALUES.oned_type)
                text = [args.text, ' (' num2str(xcoord(i)) ', ' num2str(ycoord(i)) ', ' num2str(zcoord(i)) ')'];
            else
                text = [args.text, ' (' num2str(xcoord(i)) ', ' num2str(ycoord(i)) ')'];
            end
        else
            text = [args.text];
        
        end
        
% display any text
        if ~isempty(text) && ~(all(isnan([xcoord(i), ycoord(i), zcoord(i)])) || isempty([xcoord(i), ycoord(i), zcoord(i)]))
            t = ixf_place_text(xcoord(i),ycoord(i),zcoord(i),text);
        end

% use no_points if it's given.
        if ~ isempty(args.no_points)
            args.no_points = args.no_points -1;
                if args.no_points < 1
                    button = 0;
                end
        end
        i = i + 1;
    end
end

    
