function putb(bkg)

% function putb(bkg)

% === if MSlice Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.')
   return;
end

% === if no background place in found in 'ControlWindow', return
hh=findobj(fig,'Tag','ms_bkg_E');
if isempty(hh),
   disp('Cound not identify where to store background in Control Window.');
   return;
end

disp('Stored energy-dependent ''background'' in ControlWindow;');
set(hh,'UserData',bkg);



