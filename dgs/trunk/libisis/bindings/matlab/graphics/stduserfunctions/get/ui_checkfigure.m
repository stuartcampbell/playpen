
function hdl = ui_checkfigure(varargin)
%--------------------------------------------------------------------------
%Function Syntax: figureHandle_ = ui_checkfigure(fig_handle,
%app_name,app_tag)
%Purpose: To check any figure exists which matches passed property values
%Input: figure handle, application name, application tag
%Output: return figure handle. if 0 is returned consider it as empty or NO
%figure exists [ 0 stands for NONE ].
%Example: 
% fh = ui_checkfigure(1,'tobie','1d')
%the abov examples check whether figure with handle 1, name tobie and tag
%as 1d exists or not
%--------------------------------------------------------------------------

%global structures
[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR, IXG_ST_STDVALUES ]=ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR','IXG_ST_STDVALUES');

iArg = 1;
totArg = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_checkfigure,totArg);
hdl = varargin{iArg};
appname = varargin{iArg + 1};
tag = varargin{iArg + 2};
[hdl] = ixf_gen_interface('iname','getprop_interface','fname','checkfigure','hdl',hdl,'appname',appname,'tag',tag);
if ( hdl == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_at);
end