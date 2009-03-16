function ms_disp_axes

% function ms_disp_axes
% update Display menu axes labels on the Control Window 

labelx=get(findobj('Tag',['ms_u1label']),'String');
set(findobj('Tag','ms_disp_x_axis'),'String',['horizontal range* ' labelx ]);

labely=get(findobj('Tag',['ms_u2label']),'String');
set(findobj('Tag','ms_disp_y_axis'),'String',['vertical range* ' labely ]);
