function display( proj_tofndgs )
% Display an ISISEXCproj_tofndgs object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' proj_tofndgs.entry_name]);
disp(['## title                 =  ' proj_tofndgs.title]);
 disp('## dataset               =  ISISEXCtofndgs');
 disp('## proj_info             =  ISISEXCproj_info');
 disp('## proj_data             =  ISISEXCproj_data');
 disp('## det_eff               =  ISISEXCdetarray');
 disp('## det                   =  ISISEXCdetarray');

disp(' ');


 
    proj_tofndgs.det_eff = ISISEXCdetarray;
    proj_tofndgs.det = ISISEXCdetarray;   