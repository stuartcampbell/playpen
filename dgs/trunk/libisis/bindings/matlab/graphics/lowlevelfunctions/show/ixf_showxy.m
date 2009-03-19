function [x,y,t] = ixf_showxy(varargin)
%--------------------------------------------------------------------------
%Function Syntax: [xcoord,ycoord] = ixf_showxy
%Purpose: Display x & y coordinates of plot
%Output: x coordinates and y coordinates.
%Input: None
%Example:
%[x,y] = ixf_showxy
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%--- Parse the input arguments
arglist = struct('no_points',[],'display_coords',false,'text',[],'closest_point',false);
[parse, args, present] = parse_arguments(varargin, arglist);
t='';
%--- standard checks and obtain handles for later use
flag = ixf_checkinit('Currentfigure');
[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(gcf);

if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

% check for text input without the flag. 
if ~isempty(parse) && isempty(args.text)
    if length(parse) == 1
        args.text = parse{1};
    else
        warning('unrecognised property-value pairs passed');
    end
end

% Begin with empty x, y and button currently 1 - i.e. keep going while
% button is 1
button = 1;
x = [];
y = [];
i=1;

while button == 1

    [x_temp,y_temp, button] = ginput(1);
    
    if button == 1
        
        % If the user wants closest point, use x, y and the plot handles to get
        % the closest point to the given x,y
        if args.closest_point
            [x_temp, y_temp] = ixf_find_closest(x_temp, y_temp, plotHandle_);
        end

        % add co-ordinates to text if required
        if args.display_coords
            text = [args.text, ' (' num2str(x_temp) ', ' num2str(y_temp) ')'];
        else
            text = [args.text];
        end

        % place any text.
        if ~isempty(text)
            t = ixf_place_text(x_temp,y_temp,text);
        end

        x = [x; x_temp];
        y = [y; y_temp];

        % sort out no_points data.
        if ~ isempty(args.no_points)
            args.no_points = args.no_points -1;
                if args.no_points < 1
                    button = 0;
                end
        end
        i = i + 1;
    end
end