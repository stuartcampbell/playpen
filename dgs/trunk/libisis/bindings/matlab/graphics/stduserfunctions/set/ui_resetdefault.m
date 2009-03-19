

function ret = ui_resetdefault(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ui_resetdefault
%Purpose: reset default properties
%Output: success or error
%Input: none
%Example: 
%res = ui_resetdefault
%-------------------------------------------------------------------------

%global structure
if nargin == 0
    varargin{1} = gcf;
end

[IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_MESSAGE] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

ret = ixf_gen_interface('iname','setprop_interface','fname','resetdefaultprop',varargin{:});
if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
else
    return;
end
