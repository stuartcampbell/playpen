function ixf_chnglinlog(is_button_or_menu,isButtonMenu,is_x_y,isXY,is_lin_log,isLinLog)
%--------------------------------------------------------------------------
%Function Syntax: ixf_chnglinlog_menu(is_button_or_menu,value,is_x_y,value,
%is_lin_log,value)
%Purpose:This function changes the linear to log axis for X & Y (viceversa)
%Output: None
%Input: button or menu,x or y,lin or log
%Example:
%ixf_chnglinlog('isButtonMenu',IXG_ST_STDVALUES.menu,'isXY',IXG_ST_STDV
%ALUES.x,'isLinLog',IXG_ST_STDVALUES.log)')
%the above example shows for menu, x and log values
%ixf_chnglinlog('isButtonMenu',IXG_ST_STDVALUES.button,'isXY',IXG_ST_STDV
%ALUES.y,'isLinLog',IXG_ST_STDVALUES.lin)')
%the above example shows for button, y and linear values
%--------------------------------------------------------------------------

%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES , IXG_ST_X_BUTTONPOSITION , IXG_ST_Y_BUTTONPOSITION] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES','IXG_ST_X_BUTTONPOSITION','IXG_ST_Y_BUTTONPOSITION');

%check my figure
flag = ixf_checkinit('Currentfigure');
if (flag == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end


%validate
% ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ixf_chnglinlog_nv,nargin);


%tot arguments
% totArg =  numel(varargin);
% %parse args
% for iArgLoop = IXG_ST_STDVALUES.start:IXG_ST_STDVALUES.incr:totArg
%     switch(lower(varargin{iArgLoop}))
%     case 'isbuttonmenu'
%         isButtonMenu = varargin{iArgLoop + 1};
%         if (isButtonMenu ~= IXG_ST_STDVALUES.menu && isButtonMenu ~= IXG_ST_STDVALUES.button)
%             ixf_display_error(IXG_ST_ERROR.wrong_value,'isButtonMenu');
%         end  
%     case 'isxy'
%         isXY = varargin{iArgLoop + 1};
%         if (isXY ~= IXG_ST_STDVALUES.x && isXY ~= IXG_ST_STDVALUES.y)
%             ixf_display_error(IXG_ST_ERROR.wrong_value,'isXY');
%         end  
%     case 'islinlog'
%         isLinLog = varargin{iArgLoop + 1};
%         if (isLinLog ~= IXG_ST_STDVALUES.lin && isLinLog ~= IXG_ST_STDVALUES.log)
%             ixf_display_error(IXG_ST_ERROR.wrong_value,'isLinLog');
%         end
%     otherwise
%         ixf_display_error(IXG_ST_ERROR.wrong_arg);
%     end
% end

%take action depending on values
if (isButtonMenu == IXG_ST_STDVALUES.menu) %for menu
    if (isXY == IXG_ST_STDVALUES.x)
        %for x axis
        if (isLinLog == IXG_ST_STDVALUES.lin)
            %set scale to linear if isXY 0 and isLinLog 0
            set(gca,'XScale','linear');
            %set the label and callback
            set(gcbo,'Label','LogX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
        else
            %set scale to log if isXY 0 and isLinLog 1
            set(gca,'XScale','log');
            %set the label and callback
            set(gcbo,'Label','LinX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);            
        end
    else
        %for y axis
        if (isLinLog == IXG_ST_STDVALUES.lin)
            %set scale to linear if isXY 1 and isLinLog 0
            set(gca,'YScale','linear');
            %set the label and callback
            set(gcbo,'Label','LogY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
            
        else
            %set scale to log if isXY 1 and isLinLog 1
            set(gca,'YScale','log');
            %set the label and callback
            set(gcbo,'Label','LinY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.menu),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);
            
        end
    end
elseif (isButtonMenu == IXG_ST_STDVALUES.button) %for button
    if (isXY == IXG_ST_STDVALUES.x)
        %for x axis
        if (isLinLog == IXG_ST_STDVALUES.lin)
            %set scale to linear if isXY 0 and isLinLog 0
            set(gca,'XScale','linear');
            %add button
            bt_LogX_= uicontrol(gcf,'Style','Pushbutton','String','LogX','Position',[IXG_ST_X_BUTTONPOSITION.length IXG_ST_X_BUTTONPOSITION.breadth IXG_ST_X_BUTTONPOSITION.width IXG_ST_X_BUTTONPOSITION.height],...
                'Tag','LogX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);            
        else
            %set scale to log if isXY 0 and isLinLog 1
            set(gca,'XScale','log');
            %add button
            bt_LogX_= uicontrol(gcf,'Style','Pushbutton','String','LinX','Position',[IXG_ST_X_BUTTONPOSITION.length IXG_ST_X_BUTTONPOSITION.breadth IXG_ST_X_BUTTONPOSITION.width IXG_ST_X_BUTTONPOSITION.height],...
                'Tag','LinX','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.x), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);                    
        end
    else
        %for y axis
        if (isLinLog == IXG_ST_STDVALUES.lin)
            %set scale to linear if isXY 1 and isLinLog 0
            set(gca,'YScale','linear');
            %add button
            bt_LogY_= uicontrol(gcf,'Style','Pushbutton','String','LogY','Position',[IXG_ST_Y_BUTTONPOSITION.length IXG_ST_Y_BUTTONPOSITION.breadth IXG_ST_Y_BUTTONPOSITION.width IXG_ST_Y_BUTTONPOSITION.height],...
                'Tag','LogY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.log), ')']);
        else
            %set scale to log if isXY 1 and isLinLog 1
            set(gca,'YScale','log');
            %add button
            bt_LogY_= uicontrol(gcf,'Style','Pushbutton','String','LinY','Position',[IXG_ST_Y_BUTTONPOSITION.length IXG_ST_Y_BUTTONPOSITION.breadth IXG_ST_Y_BUTTONPOSITION.width IXG_ST_Y_BUTTONPOSITION.height],...
                'Tag','LinY','Callback',['ixf_chnglinlog(''isButtonMenu'',', num2str(IXG_ST_STDVALUES.button),',''isXY'',',num2str(IXG_ST_STDVALUES.y), ',''isLinLog'',', num2str(IXG_ST_STDVALUES.lin), ')']);
        end
    end
end

