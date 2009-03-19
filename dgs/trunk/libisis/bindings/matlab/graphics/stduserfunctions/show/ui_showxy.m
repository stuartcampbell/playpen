function [varargout] = ui_showxy(varargin)
%--------------------------------------------------------------------------
%Function Syntax: [xcoord,ycoord,text] = ui_showxy(text)
%Purpose: Display x & y coordinates of plot and add text to a graph
%Output: x coordinates and y coordinates, text placed
%Input: Text (optional)
%Example:
%[x,y,text] = ui_showxy('text')
%--------------------------------------------------------------------------
ixg_input = {};

if nargout == 0 && nargin == 0
    ixg_input = {ixg_input{:}, 'display_coords',true};
end

if nargin == 1      % assume single input is text. 
    ixg_input = {ixg_input{:}, 'text', varargin{1}};
else 
    ixg_input = {ixg_input{:}, varargin{:}};
end

warndlg('ui_showxy and dxy are redundant functions which will not be supported in future versions - please use ui_showxyz and dxyz with 2 output arguments instead', 'Warning: Redundant Function')

[varargout{1:nargout}] = ixf_gen_interface('iname', 'show_interface', 'fname', 'show', ixg_input{:});

