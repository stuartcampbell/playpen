function ms_load_data

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
h_file=findobj(h_cw,'Tag','ms_DataFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to .spe directory and filename. Do data file could be read.']);
   return;
end
spe_file=get(h_file,'String');
spe_file=spe_file(~isspace(spe_file));	% remove white spaces
if ~isempty(spe_file),
   spe_filename=[get(h_dir,'String') spe_file];
else	% whole filename is empty if no file given
   spe_filename=[];
end

% === read parameters of .phx detector layout file
h_dir=findobj('Tag','ms_PhxDir');
h_file=findobj('Tag','ms_PhxFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to .phx directory and filename. No data file could be read.']);
   return;
end
phx_file=get(h_file,'String');
phx_file=phx_file(~isspace(phx_file));
if ~isempty(phx_file),
	phx_filename=[get(h_dir,'String') phx_file];
else
   phx_filename=[];
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
data=buildspe(spe_filename,phx_filename);
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