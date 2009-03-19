function dv_save_msk

% function dv_save_msk

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. Return.']);
   return;
end

% === return if no msk stored 
h_msk=findobj(h_cw,'Tag','dv_MskFile');
if isempty(h_msk)|~ishandle(h_msk),
   disp('Could not locate stored msk data.');
   return;
end
msk=get(h_msk,'UserData');
if isempty(msk),
   disp('Load/create msk file first, then save new mask.');
   return;
end   

% === sort spectrum numbers in ascending order
msk=sort(msk); 

% === eliminate double masking
n=1;
while n<length(msk),
   if msk(n)==msk(n+1),
%     disp(['Eliminate double masking of spec #' num2str(msk(n))]);
      msk(n+1)=[];
   else
      n=n+1;
   end
end
set(h_msk,'UserData',msk);

% === find path where to save new mask file
h_dir=findobj(h_cw,'Tag','dv_MskDir');
if isempty(h_dir)|~ishandle(h_dir),
   disp('Could not locate path to msk file.');
   return;
end

% === save spectra list as an ascii mask file 
save_msk(msk,[get(h_dir,'String') get(h_msk,'String')]);