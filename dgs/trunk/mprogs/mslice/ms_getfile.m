function cancel=ms_getfile(hdir,hfile,filter,titlewindow);

% function ms_getfile(hdir,hfile,filter,titlewindow);
% uses the uiwindow to select a file with a given filter
% starting directory in get(hdir,'String');
% store final pathname and filename in the 'String' properties of hdir and hfile

% === return if ControlWindow not opened 
h_cw=findobj('Tag','ms_ControlWindow');
%if isempty(h_cw),
%   disp(['No ControlWidow opened, no file can be selected using the uiwindow menu.']);
%   return;
%end

pathname=get(hdir,'String');

% === if pathname is empty or could not be located in the current search path
% === replace with MSlice directory
if isempty(pathname)|~(exist(pathname,'dir')==7), 
   if ~isempty(h_cw),
	   h_mslicedir=findobj(h_cw,'Tag','ms_MSliceDir');
   	if ~isempty(h_mslicedir),
      	MSliceDir=get(h_mslicedir,'String');
      	if ~isempty(MSliceDir),
         	pathname=MSliceDir;
      	else   
      		disp(['MSlice path appears to be empty. Return.']);
         	cancel=1;
         	return;
   		end 
   	else
      	disp(['Could not determine MSlice path. Return.']);
      	cancel=1;
      	return;
      end
   end
end

[filename,pathname]=uigetfile([pathname filter],titlewindow);
if ischar(filename),
	set(hdir,'String',pathname);
   set(hfile,'String',filename);
   cancel=0;
else
   cancel=1;
end