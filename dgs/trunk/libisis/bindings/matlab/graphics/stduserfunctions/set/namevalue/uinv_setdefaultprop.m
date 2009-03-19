function ret = uinv_setdefaultprop(name, tag, varargin)
%--------------------------------------------------------------------------
%Function Syntax: uinv_setdefaultprop(name, tag, property_name,property_value,property_name,...)
%Purpose: set default properties
%Output: success or error
%Input: 
% name: the name of the graph type to attach the default properties to
%Example: 
% uinv_setdefaultprop('myplotname','myplottag','color','red') --> plot color
% uinv_setdefaultprop('myplotname','myplottag','acolor','red') --> axes color 
%-------------------------------------------------------------------------

%global structure

[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION', 'IXG_ST_ERROR', 'IXG_ST_STDVALUES');

if strcmp(varargin{1},'array')
    array_tag = varargin{2};
    for i = 3:length(varargin)
        option{i-2} = varargin{i};
    end
else
    array_tag = false;
    option = varargin;
end

totArg = numel(varargin);
if ( totArg == 0)
    ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.uinv_chngdefaultprop,totArg);
end

ret = ixf_gen_interface('iname','setprop_interface','fname','setdefaultprop','name',name,'tag',tag,'array',array_tag,option{:});
if ( ret == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figure);
end