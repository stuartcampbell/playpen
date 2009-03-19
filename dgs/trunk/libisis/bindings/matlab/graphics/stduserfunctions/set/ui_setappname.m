
function hdl = ui_setappname(varargin)
%--------------------------------------------------------------------------
%Function syntax: ret = ui_setappname(fig_handle,app_name,app_tag)
%Purpose: set application name and tag for the figure handle passed
%Input: figure handle,app name,app tag
%Output: handle is returned if the figure is set else error
%Example: ui_setappname(2,'tobie','1d') 
%The above example sets 2 is figure handle, tobie is name and 1d is tag
%--------------------------------------------------------------------------

%global structure
[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_MESSAGE]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

iArg = 1;
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_setappname,tot);
hdl = varargin{iArg};
appname = varargin{iArg + 1};
tag = varargin{iArg + 2};
[hdl] = ixf_gen_interface('iname','setprop_interface','fname','setappname','hdl',hdl,'appname',appname,'tag',tag);
if ( hdl == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_h);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end

