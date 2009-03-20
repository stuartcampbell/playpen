function ms_analysis_mode;

% function ms_analysis_mode;
% draw menu options for single crystal analysis mode (single crystal or powder mode)

% === if Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.')
   return;
end

% === if Sample not identifyable or not sigle crystal, return
sampobj=findobj(fig,'Tag','ms_sample');
if isempty(sampobj),
   disp('Could not locate object with sample type. Return.')
   return;
end
sample=get(sampobj,'Value');
if sample~=1,
   disp('Different analysis modes are only available for single crystal samples. Return.')
   return;
end

% === if Analysis Mode not defined, return
analobj=findobj(fig,'Tag','ms_analysis_mode');
if isempty(analobj),
   disp('Could not locate object with analysis mode. Return.')
   return;
end
analmode=get(analobj,'Value');

% === if no change to the type of analysis, return
if (analmode==1) & (~isempty(findobj(fig,'Tag','ms_det_type'))),
   % === single crystal analysis mode unchanged
   % disp('Analysis = single crystal mode , unchanged');
   return;
elseif (analmode==2)&isempty(findobj(fig,'Tag','ms_det_type')),
   % === powder analysis mode unchanged
 	% disp('Analysis = powder mode , unchanged');
   return;
end
   
% === if AnalysisMode=single crystal put menu option for detector types  
if (analmode==1),	% === change analysis from powder to single crystal mode 
   
   % === get position parameters for items displayed on the Control Window
	pos=get(findobj(fig,'String','AnalysisMode'),'Position');
	lineheight=pos(4);
	oneline=[0 lineheight 0 0];
	pos1=get(sampobj,'Position');
	interlines=pos1(4)-lineheight;
	white=get(sampobj,'BackgroundColor');

   h=uicontrol('Parent',fig,'Style','text','String','Detectors',...
   	'Position',pos+[3*pos(3) 0 0 0],'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','popupmenu','String',{'PSD','conventional (non-PSD)'},...
   	'Tag','ms_det_type','BackgroundColor',white,...
   	'Position',pos+[4*pos(3) 0 1.3*pos(3) interlines],...
      'Callback','ms_disp_or_slice','Value',1);
   ms_disp_or_slice;
else	% powder analysis mode
   delete(findobj(fig,'Type','uicontrol','Style','text','String','Detectors'));
   delete(findobj(fig,'Tag','ms_det_type'));
   ms_powder_menu;
   
   % === update detector trajectories axes lists
   strings={'Q_x','Q_y','Q_z','H','K','L','Energy','|Q|','2Theta','Azimuth','Det Group Number'};
	set(findobj(fig,'Tag','ms_plot_traj_x'),'Value',2,'String',strings);
	set(findobj(fig,'Tag','ms_plot_traj_y'),'Value',1,'String',strings);
	strings{length(strings)+1}='none';
	set(findobj(fig,'Tag','ms_plot_traj_z'),'String',strings,'Value',length(strings));
end