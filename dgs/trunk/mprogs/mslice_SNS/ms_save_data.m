function ms_save_data

% function ms_load_data
% callback function for the 'Load Data' button on the ControlWindow

% === return if ControlWindow not opened 
h_cw=findobj('Tag','ms_ControlWindow');
if isempty(h_cw),
   disp(['No Control widow opened, no .spe data can be saved.']);
   return;
end

% === return if no data stored in the ControlWindow
data=get(h_cw,'UserData');
if isempty(data),
   disp(['No data to be saved.']);
   return;
end

% === choose .spe file name
h_dir=findobj(h_cw,'Tag','ms_DataDir');
h_file=findobj(h_cw,'Tag','ms_DataFile');
if isempty(h_dir)|isempty(h_file),
   disp(['Could not find objects assocuiated with the .spe directory and filename. Data cannot be saved.']);
   return;
end
old_dir=get(h_dir,'String');
old_file=get(h_file,'String');
% === Browse to select file to save data 
cancel=ms_putfile(h_dir,h_file,'*.spe','Save data to .spe file');
% === if cancel button pressed then return, do not save data
if cancel,
	return
end
newfile=[get(h_dir,'String') get(h_file,'String')];
set(h_dir,'String',old_dir);
set(h_file,'String',old_file);
save_spe(data,newfile);
