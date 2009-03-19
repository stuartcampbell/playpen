function ret=ui_resetall()
%-----------------help for GTK ui_resetall---------------------------------
% purpose: user interface for reset all function
% input: none                   output: success or fail
% syntax: ret=ui_resetall
% updated 11/08/2006            Dean Whittaker
%--------------------------------------------------------------------------
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

ret = ixf_gen_interface('iname','setprop_interface','fname','resetallprop');
if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
else

end
