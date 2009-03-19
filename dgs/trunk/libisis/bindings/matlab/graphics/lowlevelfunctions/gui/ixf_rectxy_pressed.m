
function ixf_rectxy_pressed
%--------------------------------------------------------------------------
%Function Syntax: ixf_rectxy_pressed
%Purpose: call ui_display_rectxy displaying rectangular xy coordinates
%Input: none
%Output: none
%Example: ixf_rectxy_pressed
%--------------------------------------------------------------------------

%call display rect xy
rectxy = ixf_showrectxy
%set state to off again
ixf_gui_endisable('hdl',gcbo,'field','tb_rectxy');
%