function ms_updatelabelu(n)

% function ms_updatelabelu(n)
% updates label for u1,u2 axes for powder samples (or powder analysis mode in the case of single crystal samples)

if ~exist('n','var')|~isnumeric(n)|~sum(n==[1 2]),
   return
end

% === return if Control Window not present
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window not opened. Labels not updated.');
   return;
end

% === return if not powder sample or not powder analysis mode for single crystal sample 
sampobj=findobj(fig,'Tag','ms_sample');
if isempty(sampobj),
   disp(['Sample type could not be located. Labels not updated.']);
   return;
end
powder_sample=(get(sampobj,'Value')==2);	% =0 (FALSE) if single crystal sample, =1 (TRUE) if powder sample
if ~powder_sample,	% === if single crystal sample
   analobj=findobj(fig,'Tag','ms_analysis_mode');
   if isempty(analobj),
      disp(['Could not identify analysis mode for single crystal sample. Labels not updated.']);
      return;
   end
   powder_mode=(get(analobj,'Value')==2); % =1 (TRUE) if powder analysis mode, =0 (FALSE) if single crystal analysis mode
end   
if ~powder_sample&~powder_mode,
   disp('Neither powder sample nor powder analysis mode for single crystal sample. Labels not updated.');
   return;
end
   
% == read off default label
obj=findobj(fig,'Tag',['ms_u' num2str(n)]);
strings=get(obj,'String');
value=get(obj,'Value');
label=strings{value};
obj=findobj(fig,'Tag',['ms_u' num2str(n) 'label']);
set(obj,'String',label);

ms_updatelabel(n);
