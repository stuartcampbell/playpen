function ms_cut_axes

% function ms_cut_axes
% update Cut menu axis labels on the Control Window 

% === return if ControlWindow not present
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window not opened. Labels not updated.');
   return;
end

% === return if Sample popupmenu not present
sampobj=findobj(fig,'Tag','ms_sample');
if isempty(sampobj),
   disp(['Sample type not defined. Labels not updated.']);
   return;
end
samp=get(sampobj,'Value');	% 1 single crystal, 2 powder
if samp==1,	% === if single crystal sample
   analobj=findobj(fig,'Tag','ms_analysis_mode');
   if isempty(analobj),
      disp(['Single crystal sample but undefined analysis mode. Labels not updated.']);
      return;
   end   
   analmode=get(analobj,'Value');	% =1 single crystal mode, =2 powder mode
   if analmode==1, 	% if single crystal analysis mode
	   detobj=findobj(fig,'Tag','ms_det_type');
   	if isempty(detobj),
      	disp(['Single crystal sample in single crystal analysis mode, but undefined detector type. Labels not updated.']);
      	return;
   	end
      psd=get(detobj,'Value');	% detector type = 1 if psd, =2 if conventional detectors
   end
elseif samp~=2,
   % === if sample is noeither single crystal nor powder, give error message and return 
   disp(sprintf('Unknown operational mode in mslice with sample type %g',samp));
   return;
end

x=get(findobj('Tag','ms_cut_x'),'Value');
if (samp==1)&(analmode==1)&(psd==1), % single crystal sample in single crystal analysis mode + PSD detectors
	if x==1,
   	y=2;
   	z=3;
	elseif x==2,
  		y=3;
   	z=1;
	else
   	y=1;
   	z=2;
   end
elseif (samp==2)|((samp==1)&(analmode==2))|((samp==1)&(analmode==1)&(psd==2))	% powder or single crystal + non-PSD detectors
	if x==1,
   	y=2;
	else 
  		y=1;
   end
end   

labely=get(findobj('Tag',['ms_u' num2str(y) 'label']),'String');
set(findobj('Tag','ms_cut_y_axis'),'String',['thickness range  ' labely ]);
if (samp==1)&(analmode==1)&(psd==1),	% add label for the extra thickness direction 
	labelz=get(findobj('Tag',['ms_u' num2str(z) 'label']),'String');
   set(findobj('Tag','ms_cut_z_axis'),'String',['thickness range  ' labelz ]);
end