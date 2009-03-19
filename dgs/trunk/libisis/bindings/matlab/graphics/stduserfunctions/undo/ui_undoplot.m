function ret = ui_undoplot
%--------------------------------------------------------------------------
%Function Syntax: ui_undoplot
%Purpose: remove plot
%Input: none
%Output: result true or false
%Example: ret = ui_undoplot
%--------------------------------------------------------------------------

%global structure
[IXG_ST_STDVALUES, IXG_ST_MESSAGE]=ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

ret = ixf_gen_interface('iname','undointerface','fname','undoplot');

if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
end





