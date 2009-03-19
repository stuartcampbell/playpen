

function res = ui_setcurrfig(varargin)
%--------------------------------------------------------------------------
%Function syntax: ui_setcurrfig(fig_handle,[app name, app tag])
%Purpose: set current figure
%Input: figure handle or name,tag or handle,name and tag
%Output: handle is returned if the figure is set else error
%Example: ui_setcurrfig(2) 
%The above example sets figure handle 2 as current
%ui_setcurrfig(2,'tobie','1d')
%The above example sets figure handle 2 with name tobie and tag 1d as current
%--------------------------------------------------------------------------

%global structure

[IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_MESSAGE] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_MESSAGE');

% [res] = ixf_gen_interface('iname','setprop_interface','fname','setcurrfig','istyle',IXG_ST_ISTYLE.default,varargin{:});    
totArg = numel(varargin);
if (totArg == 1)
    [res] = ixf_gen_interface('iname','setprop_interface','fname','setcurrfig','hdl',varargin{:});    
elseif (totArg == 2)
    [res] = ixf_gen_interface('iname','setprop_interface','fname','setcurrfig','appname',varargin{1},'tag',varargin{2});        
elseif (totArg == 3)
    [res] = ixf_gen_interface('iname','setprop_interface','fname','setcurrfig','hdl',varargin{1},'appname',varargin{2},'tag',varargin{3});    
else
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
end
if ( res == IXG_ST_STDVALUES.false )
    ixf_display_error(IXG_ST_ERROR.no_figfound_hta);
else
    ixf_display_message(IXG_ST_MESSAGE.set);
end