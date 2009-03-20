function data=fromwindow

% function data=fromwindow
% extract data from mslice ControlWindow into the command line 

data=get(findobj('Tag','ms_ControlWindow'),'UserData');
if isempty(data),
   disp('No data set to extract from the Control Window.');
end
