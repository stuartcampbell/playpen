function ui_releasecmd(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ui_releasecmd(fig_handle)
%Purpose: release a kept figure, The figure will be re-opened for
%overplotting etc. etc.
%Input: figure handle
%Output: none
%Example: ui_releasecmd(1) where 1 is figure handle that is being kept. 
%--------------------------------------------------------------------------

%global structure
if length(varargin{1}) > 1
    varargin = num2cell(varargin{1});
end

for i = 1:length(varargin)
    ixf_gen_interface('iname','storeinterface','fname','releasecmd','hdl',varargin{i});
end


