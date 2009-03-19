function display(monitor)
% Display an ISISEXCmonitor object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' monitor.entry_name])
disp(['## name                  =  ' monitor.name])
disp(['## integral              =  ' num2str(monitor.integral)]);
disp(['## error                 =  ' num2str(monitor.error)]);
disp(['## range                 =  [' num2str(monitor.range),']']);
disp(['## integral_units        =  ' monitor.integral_units])
disp(['## moments               =  moments'])
disp(['## moments_units         =  ' monitor.moments_units])
disp(['## det                   =  ISISEXCdetarray'])

 
     
disp(' ');
