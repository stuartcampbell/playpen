function [data,filename]=dv_load_file(tag)

% function dv_load_file(tag)
% tag = 'Sum' or 'Msk'
% callback function for the 'Load Sum' and 'Load Mask' buttons on DetectorView

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. ' tag ' file not loaded.']);
   return;
end

%=== read parameters of .sum or .msk file
h_dir=findobj(h_cw,'Tag',['dv_' tag 'Dir']);
h_file=findobj(h_cw,'Tag',['dv_' tag 'File']);
if isempty(h_dir)|isempty(h_file),
   disp(['Could not associate objects to directory and filename. ' tag ' file not loaded.']);
   return;
end
file=get(h_file,'String');
file=file(~isspace(file));	% remove white spaces
if ~isempty(file),
   filename=[get(h_dir,'String') file];
   data=feval(['load_' lower(tag)],filename);
   set(h_file,'UserData',data);   
else
   disp('No file to load.');   
   set(h_file,'UserData',[]);   
end   
