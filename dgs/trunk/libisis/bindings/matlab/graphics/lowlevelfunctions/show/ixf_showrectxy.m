
function [ret] = ixf_showrectxy
%--------------------------------------------------------------------------
%Function Syntax: ret = ixf_showrectxy
%Output: rect coordinates in form of a structure.
%structure format can be displayed by executing on cmd line
%>> IXG_ST_RECTXY
%Input: None
%Purpose: Display rectangular coordinates 
%Example: ret = ixf_showrectxy
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR , IXG_ST_STDVALUES , IXG_ST_RECTXY] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_RECTXY');

ret = IXG_ST_RECTXY;

%check my figure
flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

%code for draggable box
k = waitforbuttonpress;
point1 = get(gca,'CurrentPoint');    % button down detected
finalRect = rbbox   ;                % return figure units
point2 = get(gca,'CurrentPoint');    % button up detected
point1 = point1(1,1:2);            % extract x and y
point2 = point2(1,1:2);
p1 = min(point1,point2);             % calculate locations
offset = abs(point1-point2);        % and dimensions

%the way this is calculated
%low x, right x, high x, left x
%low y, right y, high y, left y
%(1) for x and (2) for y
%fill up the structure
ret.xs = p1(1);
ret.ys = p1(2);
ret.xr = p1(1)+offset(1);
ret.yr = p1(2);
ret.xh = p1(1)+offset(1);
ret.yh = p1(2)+offset(2) ;
ret.xl = p1(1);
ret.yl = p1(2)+offset(2);
