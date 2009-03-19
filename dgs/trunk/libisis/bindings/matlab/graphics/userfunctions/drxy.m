function ret = drxy
%------help for gtk display x, y rectangular coordinates function drxy-----
% Function Syntax: 
%
% >> ret = drxy
%
% Output: rect coordinates in form of a structure.
% structure format can be displayed by executing on cmd line
%
% >> IXG_ST_RECTXY = ixf_global_var('libisis_graphics,'get','IXG_ST_RECTXY')
%
% Input: None
% Purpose: Display rectangular coordinates 
%
% Example: ret = drxy
%--------------------------------------------------------------------------
% Dean Whittaker 2007

ret = ui_show_rectxy;

