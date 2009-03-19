function ui_keepcmd(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ui_keepcmd(fig_handle)
%Purpose: keep figure, it will not be used for new plots
%Input: figure handle
%Output: none
%Example: ui_keepcmd(1) where 1 is figure handle
%--------------------------------------------------------------------------

%global structure
if length(varargin{1}) > 1
    varargin = num2cell(varargin{1});
end

for i = 1:length(varargin)
    ixf_gen_interface('iname','storeinterface','fname','keepcmd','hdl',varargin{i});
end


