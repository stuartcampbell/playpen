

function ret = ui_setbinning(bin_value)
%--------------------------------------------------------------------------
%Function Syntax: ret = ui_setbinning(bin_value)
%Purpose: set binning value
%Output: success or error
%Input: none
%Example: 
%ret = ui_setbinning(2)
%the above example sets the binning value to 2
%-------------------------------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_MESSAGE] =ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');


ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_setbinning,nargin);
ret = ixf_gen_interface('iname','setprop_interface','fname','setbinning',bin_value);
if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.cannot_set);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end

