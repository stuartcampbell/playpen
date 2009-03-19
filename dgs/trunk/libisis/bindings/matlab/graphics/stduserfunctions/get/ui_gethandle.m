

function hdl = ui_gethandle(varargin)
%--------------------------------------------------------------------------
%Function Syntax: figureHandle_ = ui_gethandle(app_name,app_tag)
%Purpose: To return handle if any figure matches passed property values
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE].
%Input: application name and tag
%Example:  fh = ui_gethandle('tobie','1d')
%the above example returns figure handle if any figure matches name as
%tobie and tag as 1d
%--------------------------------------------------------------------------

%global structures
[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES');

iArg = 1;
totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_gethandle,totArg);
appname = varargin{iArg};
tag = varargin{iArg + 1};
[hdl] = ixf_gen_interface('iname','getprop_interface','fname','gethandle','appname',appname,'tag',tag);
if ( hdl == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_at);
end