
function ixf_linlog_button
%-------------------------------------------------------------------- 
%Function Name: ixf_linlog_button
%Purpose: This function operates on current figure and current axis.
%This adds buttons Lin/Log X Lin/Log Y.
%Output: None
%Input: None
%Example: ixf_linlog_button
%--------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES, IXG_ST_X_BUTTONPOSITION, IXG_ST_Y_BUTTONPOSITION]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_X_BUTTONPOSITION','IXG_ST_Y_BUTTONPOSITION');


%check the current axis is linear, if it is then put logX symbol
%if not then put linX symbol
%do the same for Y axis.
%Depending on button press callback procedure chnglinlog
%check the current figure
flag = ixf_checkinit('Currentfigure');

if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
    return;
end
ind = findobj(gcf,'Style','Pushbutton','String','LogX');
if (~isempty(ind))
    ixf_display_error(IXG_ST_ERROR.already_exist,'Buttons');
end
ind = findobj(gcf,'Style','Pushbutton','String','LinX');
if (~isempty(ind))
    ixf_display_error(IXG_ST_ERROR.already_exist,'Buttons');
end

% set(0,'ShowHiddenHandles','on');
% toolbarHandle_ = findobj(0,'type','uitoolbar','tag','FigureToolBar','Parent',gcf)
% set(0,'ShowHiddenHandles','off');
% get(toolbarHandle_)



if ( strcmpi(get(gca,'XScale'),'linear')) 
    %if linear
    %add button
    bt_LogX_= uicontrol(gcf,'Style','Pushbutton','String','LogX','Position',[IXG_ST_X_BUTTONPOSITION.length IXG_ST_X_BUTTONPOSITION.breadth IXG_ST_X_BUTTONPOSITION.width IXG_ST_X_BUTTONPOSITION.height],...
        'Tag','LogX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
else
    %if log
    %add button
    bt_LogX_= uicontrol(gcf,'Style','Pushbutton','String','LinX','Position',[IXG_ST_X_BUTTONPOSITION.length IXG_ST_X_BUTTONPOSITION.breadth IXG_ST_X_BUTTONPOSITION.width IXG_ST_X_BUTTONPOSITION.height],...
        'Tag','LinX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);    
end

if ( strcmpi(get(gca,'YScale'),'linear')) 
    %if linear
    %add button
    bt_LogY_= uicontrol(gcf,'Style','Pushbutton','String','LogY','Position',[IXG_ST_Y_BUTTONPOSITION.length IXG_ST_Y_BUTTONPOSITION.breadth IXG_ST_Y_BUTTONPOSITION.width IXG_ST_Y_BUTTONPOSITION.height],...
        'Tag','LogY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
else
    %if log
    %add button
    bt_LogY_= uicontrol(gcf,'Style','Pushbutton','String','LinY','Position',[IXG_ST_Y_BUTTONPOSITION.length IXG_ST_Y_BUTTONPOSITION.breadth IXG_ST_Y_BUTTONPOSITION.width IXG_ST_Y_BUTTONPOSITION.height],...
        'Tag','LinY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);
end

%after all thigns are over
set(gcf,'toolbar','figure');
ixf_menutb_crosshair;
ixf_menutb_rectxy;