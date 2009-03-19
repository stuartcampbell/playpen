function ms_toggle(menu_option)

% function ms_toggle(menu_option)
% used by the surf_slice menu: ms_toggle('rotate3d')

h=findobj(gcf,'Type','uimenu','Tag',menu_option);
if ~isempty(h),
   on_off=get(h,'Checked');
   if strcmp(on_off,'on');
      set(h,'Checked','off');
   elseif strcmp(on_off,'off');
      set(h,'Checked','on');
   else
      disp(['Unidentified check option for menu item ' menu_option ]);
      return;
   end
else
   disp(['Unable to locate handle for menu item ' menu_option ]);
end
