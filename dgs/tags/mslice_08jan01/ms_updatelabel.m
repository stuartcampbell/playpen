function ms_updatelabel(n)

% function ms_updatelabel(n)

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
   if analmode==1, 	% if single crystal mode
	   detobj=findobj(fig,'Tag','ms_det_type');
   	if isempty(detobj),
      	disp(['Single crystal sample in single crystal analysis mode, but undefined detector type. Labels not updated.']);
      	return;
   	end
      psd=get(detobj,'Value');	% 1 psd 2 conventional detectors
   end
end
   
if ~exist('n','var')|~isnumeric(n)|(prod(size(n))~=1),
   disp(['Calling syntax ms_updatelabel(n) requires n=1,2,3 single numeric']);
   return
end

% === check consistency of calling syntax for powder and single crystal +psd/conventional detectors
if (samp~=2)&~((samp==1)&(analmode==2))&~((samp==1)&(analmode==1)&((psd==1)|(psd==2))),
	disp(sprintf('Unknown operational mode in Mslice with sample=%g',samp));
   if exist('analmode','var'),
      disp(sprintf('analmode=%g',analmode));
   end
   if exist('psd','var'),
      disp(sprintf('dettype=%g',psd));
   end
   return;      
end   

if ((samp==2)|((samp==1)&(analmode==2)))&(sum(n==[1 2])~=1),
   disp(['For powder mode n=1,2 not ' num2str(n)]);
   return;
elseif (samp==1)&(analmode==1)&(psd==2)&(sum(n==[1 2])~=1),
   disp(['For single crystal samples and conventional detectors n=1,2 not ' num2str(n)]);
   return;
elseif (samp==1)&(analmode==1)&(psd==1)&(sum(n==[1 2 3])~=1),
   disp(['For single crystal samples and psd detectors n=1,2,3 not ' num2str(n)]);
   return;
end

label=get(findobj('Tag',['ms_u' num2str(n) 'label']),'String');
if (samp==1)&(analmode==1),	% single crystal sample in single crystal analysis mode, axes are in wavevector-enery space 
	u=[str2num(get(findobj('Tag',['ms_u' num2str(n) '1']),'String')) ...
   	str2num(get(findobj('Tag',['ms_u' num2str(n) '2']),'String')) ...
		str2num(get(findobj('Tag',['ms_u' num2str(n) '3']),'String')) ...
		str2num(get(findobj('Tag',['ms_u' num2str(n) '4']),'String'))];
	% ==== update slice/display and cut menu
	if psd==1,
		% update label of slice plane perpendicular to axis menu
		h=findobj('Tag','ms_slice_z');
		strings=get(h,'String');
		strings{n}=combil(label,u);	%  full label [h,k,l] or Energy
		%strings{n}=label;					% short label
   	set(h,'String',strings);
		ms_slice_axes;   
   	% update cut along axis menu
      set(findobj('Tag','ms_cut_x'),'String',strings);   
		ms_cut_axes;   
	else
   	ms_disp_axes;
	   % update cut along axis menu
  		h=findobj('Tag','ms_cut_x');
		strings=get(h,'String');
		strings{n}=combil(label,u);	%  full label [h,k,l] or Energy
		%strings{n}=label;					% short label
   	set(h,'String',strings);
   	ms_cut_axes;
   end   
   % === update labels for y-axis selection for plot cut  
  	h=findobj('Tag','ms_cut_intensity');
	strings=get(h,'String');
   strings{n+1+3+3}=combil(label,u);
   set(h,'String',strings);
   % === update labels for x-axis selection for plot cut     
   h=findobj('Tag','ms_cut_xaxis');
	strings=get(h,'String');
   strings{n+1+3+3}=combil(label,u);
   set(h,'String',strings);
   % === update labels for plot trajectories        
	h=findobj('Tag','ms_plot_traj_x');
	strings=get(h,'String');
	strings{n+3+3}=combil(label,u);
	set(h,'String',strings);
	set(findobj('Tag','ms_plot_traj_y'),'String',strings);
	strings{size(strings,1)+1}='none';
	set(findobj('Tag','ms_plot_traj_z'),'String',strings);
else	% sample is powder or is single crystal in powder analysis mode
   ms_disp_axes;
	% update cut along axis selection list
  	h=findobj('Tag','ms_cut_x');
	strings=get(h,'String');
   strings{n}=label;	
   set(h,'String',strings);
   ms_cut_axes;
end	   
