
function ixf_crosshair_pressed
%--------------------------------------------------------------------------
%Function Syntax: ixf_crosshair_pressed
%Purpose: call ui_display_xy displaying xy coordinates
%Input: none
%Output: none
%Example: ixf_crosshair_pressed
%--------------------------------------------------------------------------

%call show xy
[x,y] = ixf_showxy;
x
y
ixf_gui_endisable('hdl',gcbo,'field','tb_crosshair');
%