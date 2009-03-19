function  ms_pick_output

% function  ms_pick_output
% pick output file in the cut section of the MSlice Control Window 

% === if Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.')
   return;
end

hdir=findobj(fig,'Tag','ms_cut_OutputDir');
hfile=findobj(fig,'Tag','ms_cut_OutputFile');
htype=findobj(fig,'Tag','ms_cut_OutputType');
if isempty(hdir)|isempty(hfile)|isempty(htype),
   disp('Could not locate all handles to define the OutputFile. Return.')
   return;
end

value=get(htype,'String');
value=value{get(htype,'Value')};
if strcmp('none',value(~isspace(value))),
   % no output required
   disp(['Select type of OutputFile first, then select file name.'])
   return;
else
   if ~isempty(findstr(lower(value),'mfit')),
      value='*.cut';	% 'Mfit .cut'
   else
      value=['*' value];	% for '*.cut', '*.xye', '*.smh'
   end
   ms_putfile(hdir,hfile,value,'Choose output file.');
end