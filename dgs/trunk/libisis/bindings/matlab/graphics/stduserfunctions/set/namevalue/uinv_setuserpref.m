

function ret = uinv_setuserpref(varargin)
%--------------------------------------------------------------------------
%Function Syntax: uinv_setuserpref(property_name,property_value,...)
%Purpose: set default properties
%Output: success or error
%Input: name and value
%Example: 
% uinv_setuserpref('color','red') --> plot color
% uinv_setuserpref('acolor','red') --> axes color 
% uinv_setuserpref('fcolor','red') --> figure color
%-------------------------------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION IXG_ST_ERROR IXG_ST_STDVALUES IXG_ST_MESSAGE] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

std = 2;
totArg = numel(varargin);
if ( totArg == 0)
    ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_chngdefaultprop,totArg);
else
    ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_null_value,rem(totArg,IXG_ST_INTERFACEVALIDATION.uinv_chngdefaultprop));    
end

ret = ixf_gen_interface('iname','setprop_interface','fname','setuserpref',varargin{:});
if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end
