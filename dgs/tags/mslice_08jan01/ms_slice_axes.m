function ms_slice_axes

% function ms_slice_axes
% update Slice menu axes labels on the Control Window 

h=findobj('Tag','ms_slice_z');
z=get(h,'Value');
if z==1,
   x=2;
   y=3;
elseif z==2,
   x=3;
   y=1;
else
   x=1;
   y=2;
end

labelx=get(findobj('Tag',['ms_u' num2str(x) 'label']),'String');
set(findobj('Tag','ms_slice_x_axis'),'String',['horizontal range  ' labelx ]);

labely=get(findobj('Tag',['ms_u' num2str(y) 'label']),'String');
set(findobj('Tag','ms_slice_y_axis'),'String',['vertical range  ' labely ]);

% === if z has been changed then update current value and 
% === clear slice_data (assumed to come from previous settings)
old_z=get(h,'UserData');
if isempty(old_z)|~isnumeric(old_z)|(prod(size(old_z))~=1)|(old_z~=z),
   set(findobj('Tag','ms_slice_z'),'UserData',z);
   ms_slice('clear');
end