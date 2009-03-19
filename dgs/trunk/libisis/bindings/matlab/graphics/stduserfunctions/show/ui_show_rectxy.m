

function ret = ui_show_rectxy
%--------------------------------------------------------------------------
%Function Syntax: ret = ui_show_rectxy
%Output: rect coordinates in form of a structure.
%structure format can be displayed by executing on cmd line
%>> IXG_ST_RECTXY = ixf_global_var('libisis_graphics','get','IXG_ST_RECTXY')
%Input: None
%Purpose: Display rectangular coordinates 
%Example: ret = ui_show_rectxy
%--------------------------------------------------------------------------

ret = ixf_gen_interface('iname','show_interface','fname','showrect');
%