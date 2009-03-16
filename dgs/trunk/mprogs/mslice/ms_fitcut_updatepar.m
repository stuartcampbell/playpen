function ms_fitcut_updatepar

% function ms_fitcut_update
% update parameters in the ms_fitcut window

% === return if Ms_fitcut window not opened 
h_cw=findobj('Tag','ms_fitcut');
if isempty(h_cw),
   disp(['No Mslice FitCut widow opened, no .cut data to be loaded.']);
   return;
end

% === update icross number in fit parameter list  
h_icross=findobj(h_cw,'Tag','ms_fitcut_icross');
if isempty(h_icross),
   disp('Could not locate handle of object "icross". Update not performed.');
   return;
end
cut=get(h_cw,'UserData'); 
cut.fit_pars(2)=eval(get(h_icross,'String'));
set(h_cw,'UserData',cut);

% === update MFIT parameters list (names only) if it exists
upars = findobj('tag','mf_ParWindow');
if ~isempty(upars),
   h=get(upars,'UserData'); % get handle on MFIT: Parameters list 
   h=h(:,3); % handles to parameter names
   [y, name, pnames, pin]=fitcut([],[],1); % get names of parameters 
   for i=1:length(h),
      set(h(i),'String',pnames(i,:));
   end
end
