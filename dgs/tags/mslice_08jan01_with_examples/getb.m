function bkg=getb

% function bkg=getb
% returns the currently stored energy-dependent background
% in the form of a data structre with the following fields
%           x: [1x68 double]       Energy bin centres (meV)
%           y: [1x68 double]		  Intensities 
%           e: [1x68 double]		  Errors			
%     x_label: 'Energy  (meV)'
%     y_label: 'Intensity(abs.units)'
%        axis: [-10 599 0 8.08]
%    datafile: 'spe750_sw_msk.spe'
%       title: {1x2  cell  }

% === if MSlice Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.');
   bkg=[];
   return;
end

% === if no background stored in ControlWindow, return
hh=findobj(fig,'Tag','ms_bkg_E');
if isempty(hh)&isempty(hh,'Userdata'),
   disp('No background stored in Control Window.');
   bkg=[];
   return;
end

disp('Extracting stored energy-dependent ''background'' form ControlWindow;');
bkg=get(hh,'UserData');



