function display( tofndgs )
% Display an ISISEXCtofndgs object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' tofndgs.entry_name])
disp(['## title                 =  ' tofndgs.title])
disp(['## start_time            =  ' tofndgs.start_time])
disp(['## end_time              =  ' tofndgs.end_time])
disp(['## run_number            =  ']),disp(tofndgs.run_number);
disp(['## total_charge          =  ' num2str(tofndgs.total_charge)]);
disp(['## total_raw_frames      =  ' num2str(tofndgs.total_raw_frames)]);
disp(['## total_good_frames     =  ' num2str(tofndgs.total_good_frames)]);
disp(['## program_name          =  ' tofndgs.program_name]);
disp(['## command_line          =  ' tofndgs.command_line]);

disp(['## user                  =  ISISEXCuser']);
disp(['## sample                =  ISISEXCsample']);
disp(['## instrument            =  ISISEXCchopper_instrument']);
disp(['## monitor               =  ISISEXCmonitor']);
disp(['## data                  =  ISISEXCdataset_2d']);

disp(' ');
