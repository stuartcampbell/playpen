function ms_ax_linear_log(h,scale)

% function ms_ax_linear_log(h,scale)
% calling syntax : ms_ax_linear_log(findobj(gcf,'Tag','Log_y_axis'),'YScale');

checked=get(h,'Checked');
if strcmp(checked,'on'),
   set(gca,scale,'linear');
   set(h,'Checked','off');
else
   set(gca,scale,'log');
   set(h,'Checked','on');
end   