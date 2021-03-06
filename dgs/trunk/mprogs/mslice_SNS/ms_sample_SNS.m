function ms_sample_SNS

% function ms_sample_SNS
% draw ControlWindow menu options according to the selected sample type 

% === if ControlWindow non-existent or Sample popupmenu not present, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('ControlWindow non existent');
   return
end

% === identify sample type 
sampobj=findobj(fig,'Tag','ms_sample');
if isempty(sampobj),
   disp('Sample type could not be identified. Return');
   return;
end
sample=get(sampobj,'Value');	% =1 if single crystal sample, =2 if powder sample

if sample==1,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
	% SAMPLE IS A SINGLE CRYSTAL %%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	if ~isempty(findobj(fig,'Tag','ms_psi_samp')),
   	% single crystal sample already selected, return
   	return
	end
   
   % === get position parameters for a typical item displayed on the Control Window
	pos=get(findobj(fig,'String','Sample'),'Position');
	lineheight=pos(4);
	oneline=[0 lineheight 0 0];
	pos1=get(sampobj,'Position');
	interlines=pos1(4)-lineheight;
	pos=pos-oneline;
	white=get(sampobj,'BackgroundColor');

	% === delete previous sample parameters 
	h=findobj('Parent',fig,'Type','uicontrol');
	for i=1:length(h),
		cpos=get(h(i),'Position');
   	if cpos(2)<=pos(2),
     		delete(h(i));
   	end
	end
	
	%========= Lattice parameters ==================
	h=uicontrol('Parent',fig,'Style','text','String','Unit cell lattice parameters',...
	   'Position',pos+[0 0 1.5*pos(3) 0]);
	
	%========= as bs cs (Angs)==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text','String','a (�)',...
	   'Position',pos);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_as',...
	   'Position',pos+[pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','b (�)',...
	   'Position',pos+[2*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_bs',...
	   'Position',pos+[3*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','c (�)',...
	   'Position',pos+[4*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_cs',...
	   'Position',pos+[5*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	
	%========= aa bb cc (deg)==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text','String','aa (deg)',...
	   'Position',pos);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_aa',...
	   'Position',pos+[pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','bb (deg)',...
	   'Position',pos+[2*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_bb',...
	   'Position',pos+[3*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','cc (deg)',...
	   'Position',pos+[4*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_cc',...
	   'Position',pos+[5*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	
	%========= Crystal orientation ==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text',...
	   'String',...
	   'Crystal orientation:(u,v) reciprocal space axes in principal scattering plane (Qx || ki,Qy), i.e. u=[100]=a*,v=[010]=b*',...
	   'Position',pos+[0 0 7*pos(3) 0]);

	%========= ux uy uz ==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text','String','ux',...
	   'Position',pos);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_ux',...
	   'Position',pos+[pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','uy',...
	   'Position',pos+[2*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_uy',...
	   'Position',pos+[3*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','uz',...
	   'Position',pos+[4*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_uz',...
	   'Position',pos+[5*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	
	%========= vx vy vz ==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text','String','vx',...
	   'Position',pos);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_vx',...
	   'Position',pos+[pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','vy',...
	   'Position',pos+[2*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_vy',...
	   'Position',pos+[3*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text','String','vz',...
	   'Position',pos+[4*pos(3) 0 0 0]);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_vz',...
	   'Position',pos+[5*pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	
	%========= psi_samp (deg) ==================
	pos=pos-oneline;
	h=uicontrol('Parent',fig,'Style','text','String','Psi(deg)',...
	   'Position',pos);
	h=uicontrol('Parent',fig,'Style','edit','Enable','on','Tag','ms_psi_samp',...
	   'Position',pos+[pos(3) 0 0 interlines],...
	   'HorizontalAlignment','left',...
	   'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','text',...
	   'String',' angle between ki and u, +ve if (ki x u) || Q_z',...
	   'HorizontalAlignment','left','Position',pos+[2*pos(3) 0 6*pos(3) 0]);
	%extent=get(h,'Extent');
	%set(h,'Position',pos+[2*pos(3) 0 extent(3) 0])
	
	%========= Analysis mode and Detector type (if single crystal sample): conventional(non-PDS)/PSD ==================
	pos=pos-1.3*oneline;
	h=uicontrol('Parent',fig,'Style','text','String','AnalysisMode',...
	   'Position',pos,'BackgroundColor',white);
	h=uicontrol('Parent',fig,'Style','popupmenu','String',{'single crystal'},...%'powder'},...
	   'Tag','ms_analysis_mode','BackgroundColor',white,...
	   'Position',pos+[pos(3) 0 0.5*pos(3) interlines],...
	   'Callback','ms_analysis_mode_sns;','Value',1);
	ms_analysis_mode_sns;
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%not implemented yet in SNS version
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elseif sample==2,   
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% SAMPLE IS POWDER %%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   if isempty(findobj(fig,'Tag','ms_psi_samp')),
   % powder mode already selected
   	return
	end   
   % === sample is powder, draw menu options in ControlWindow      
   ms_powder_menu;  
else
  disp(sprintf('Unknown sample type (=1 single crystal, =2 powder), not %g',sample)); 
end

% === update detector trajectories axes
if sample==2, % powder sample
   lineoffset=2;
   strings={'Energy','|Q|','2Theta','Azimuth','Det Group Number'};
elseif sample==1,	% single crystal sample 
   detobj=findobj(fig,'Tag','ms_det_type');
   if isempty(detobj),
      disp(['Could not identify detector type for the single crystal sample in single crystal analysis mode. Return.']);
      return;
   end
   % === by default in single crystal analysis mode
   psd=get(detobj,'Value');	%=1 psd , =2 conventional
	if psd==1,		% single crystal + PSD detectors
      strings={'Q_x','Q_y','Q_z','H','K','L','u1','u2','u3','Energy','|Q|','2Theta','Azimuth','Det Group Number'};
      lineoffset=0;
	elseif psd==2,	% single crystal + conventional detectors
      strings={'Q_x','Q_y','Q_z','H','K','L','u1','u2','Energy','|Q|','2Theta','Azimuth','Det Group Number'};
      lineoffset=2;
   else
      disp(['Single crystal sample with unknown type of detectors ' num2str(psd)]);
      return;
   end   
end
     

return;
%========= Note on Optional Parameters ==================
if isempty(findobj(fig,'String','* Optional Parameter')),
	figure(findobj('Tag','ms_ControlWindow'));
	pos=[0 0 pos(3) pos(4)]; 
	h=uicontrol('Parent',fig,'Style','text','String','* Optional Parameter',...
   	'Position',pos+[0 0 pos(3) 0]);
end
