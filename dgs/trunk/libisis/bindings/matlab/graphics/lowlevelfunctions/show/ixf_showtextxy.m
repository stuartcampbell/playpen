function [x,y,t]=ixf_showtextxy(txt)

%---------help for libisis ixf_showtextxy----------------------------------
%Function Syntax: [xcoord,ycoord,text] = ixf_showtextxy(text)
%Purpose: function to place text on a graph and show the co-ordinates
%Output: x coordinates and y coordinates, text placed
%Input: Text
%Example:
%[x,y,text] = ui_showxy('text')
%--------------------------------------------------------------------------

% initial checks
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end
if nargin ~= 1
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
end

% main function
[x,y] = ginput(1);
text(x,y,txt);
t=txt;