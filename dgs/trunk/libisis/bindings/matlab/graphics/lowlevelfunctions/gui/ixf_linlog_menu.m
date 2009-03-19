function ixf_linlog_menu
%-------------------------------------------------------------------- 
%Function Name: ixf_linlog_menu
%Output: None
%Input: None
%Purpose: This adds menu items Lin/Log X Lin/Log Y. 
%This function toggles between linear and logarthimic axis for x/y.
%Example:ixf_linlog_menu
%--------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]= ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%check for current figure
%Add the menu on 6th position
%check the current axis is linear, if it is then put logX symbol
%if not then put linX symbol
%do the same for Y axis.
%Depending on button press callback procedure chnglinlog_button
position = IXG_ST_STDVALUES.menu_position;
flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
    return;
end
ind = findobj(gcf,'Label','ChngAxis','Tag','ChngAxisMenu');
%error if already exist
if (~isempty(ind))
    ixf_display_error(IXG_ST_ERROR.already_exist,'Menu');
end
mn_chngAxis_= uimenu(gcf,'Label','ChngAxis','Tag','ChngAxisMenu','Position',position);
if ( strcmpi(get(gca,'XScale'),'linear')) 
    %if linear
    %add menu item
    mn_linX_ = uimenu(mn_chngAxis_,'Label','LogX','Tag','XlogItem','Callback',...
        ['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);    
else
    %if log
    %add menu item
    mn_logX_ = uimenu(mn_chngAxis_,'Label','LinX','Tag','XlinItem','Callback',...
        ['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);
end
if ( strcmpi(get(gca,'YScale') , 'linear') )
    %if linear
    %add menu item
    mn_linY_ = uimenu(mn_chngAxis_,'Label','LogY','Tag','YlogItem','Callback',...
        ['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
else
    %if log
    %add menu item
    mn_logY_ = uimenu(mn_chngAxis_,'Label','LinY','Tag','YlinItem','Callback',...
        ['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);
end

% set default 
