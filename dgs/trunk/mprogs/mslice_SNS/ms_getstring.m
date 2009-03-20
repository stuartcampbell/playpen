function str=ms_getstring(h_cw,tag)

% function ms_getstring(h_cw,tag)
% get string property of object with given tag in the ControlWindow (handle h_cw)

% === return if handle to COntrol Window is invalidor if error reading tag 
if ~exist('h_cw','var')|isempty(h_cw)|~ishandle(h_cw),
   disp('Could not locate ControlWindow. Return.');
   str=[];
   return;   
end
if ~exist('tag','var')|isempty(tag)|~ischar(tag),
   disp('Error reading handle Tag. Return.');
   str=[];
   return;
end


% === locate object in the ControlWindow
h=findobj(h_cw,'Tag',tag);
if isempty(h),
   disp(['Could not find handle of object with Tag ' tag ' in the figure with handle' num2str(h_cw)]);
   str=[];
   return;
end
if length(h)>1,
   disp(['Have located more instances of an object with the same handle.']);
   disp(['Can not extract string property uniquely. Return.']);
   str=[];
   return;
end

% === extract string property if editable text, simple text, 
% === selected entry in a popupmenu or on/of if checkbox
if strcmp(get(h,'Style'),'popupmenu'),
   value=get(h,'Value');
   strings=get(h,'String');
   str=strings{value};
elseif strcmp(get(h,'Style'),'checkbox'),
   value=get(h,'Value');
   if value==0,
      str='off';
   else
      str='on';
   end
elseif strcmp(get(h,'Style'),'text')|strcmp(get(h,'Style'),'edit'),
   str=get(h,'string');
else
   disp(['Have not defined string property of an object of style ' get(h,'Style')]);
   str=[];
   return;
end 