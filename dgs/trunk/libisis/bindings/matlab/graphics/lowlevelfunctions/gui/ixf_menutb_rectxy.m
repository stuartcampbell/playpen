
function ixf_menutb_rectxy
%--------------------------------------------------------------------------
%Function Syntax: ixf_menutb_rectxy
%Purpose: create rectxy menu icon on toolbar
%Input: none
%Output: none
%Example: ixf_menutb_rectxy
%--------------------------------------------------------------------------

%global structure
IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

%set invisible handles visible
set(0,'ShowHiddenHandles','on');

%find the default toolbar
toolbarHandle_ = findobj(0,'type','uitoolbar','tag','FigureToolBar','Parent',gcf);
if (isempty(toolbarHandle_))
    %if not display error
    ixf_display_error(IXG_ST_ERROR.no_toolbar);    
end

%for rect xy
img = imread(IXG_ST_STDVALUES.rectxy);

%check already exists or not
ind = findobj(gcf,'CData',img,'TooltipString','RectXY');
%if not exists, create it
if (isempty(ind))
    crosshairHandle_ = uitoggletool(toolbarHandle_,'CData',img,'TooltipString','RectXY');
    set(crosshairHandle_,'Oncallback','ixf_rectxy_pressed');
end

%set invisible handles visible off
set(0,'ShowHiddenHandles','off');

% set globals