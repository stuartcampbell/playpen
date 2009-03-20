function ms_load_data_sns

% function ms_load_data
% callback function for the 'Load Data' button on the ControlWindow

% === return if ControlWindow not opened 
h_cw=findobj('Tag','ms_ControlWindow');
if isempty(h_cw),
   disp(['No Control widow opened, no .spe data to be loaded.']);
   return;
end

%=== read parameters of .spe data file
h_dir=findobj(h_cw,'Tag','ms_DataDir');
%h_file=findobj(h_cw,'Tag','ms_DataFile');
if isempty(h_dir)
   disp(['Could not associate objects to .spe directory and filename. Do data file could be read.']);
   return;
end
sns_dir=get(h_dir,'String');
sns_dir=sns_dir(~isspace(sns_dir));	% remove white spaces
if isempty(sns_dir)
    sns_dir=uigetdir;
end

% === highlight red busy button
h_status=findobj(h_cw,'Tag','ms_status');
if ~isempty(h_status)&ishandle(h_status),
   red=[1 0 0];
   set(h_status,'BackgroundColor',red);
%   set(h_status,'Visible','on');      
   drawnow;
end

% === construct .spe data structure 
data=build_ms_struct(sns_dir,base)
if ~isempty(data),
   set(h_cw,'UserData',data);
end

% === highlight green button
if ~isempty(h_status)&ishandle(h_status),
   green=[0 1 0];
   set(h_status,'BackgroundColor',green);
%   set(h_status,'Visible','off');   
   drawnow;
end