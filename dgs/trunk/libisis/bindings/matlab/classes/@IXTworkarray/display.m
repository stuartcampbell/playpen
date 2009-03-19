function display(workarray)
% Display an ISISEXCworkarray object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name    =  ' workarray.entry_name])
disp(['## name          =  ' workarray.name])
 disp('## nwork_tot     =  '),disp(workarray.nwork_tot);
 disp('## work_no       =  []');
 disp('## nspec         =  []');
 disp('## spec_ind      =  []');
 disp('## det_eff_ind   =  []'); 
 disp('## nspec_tot     =  '),disp(workarray.nspec_tot);
 disp('## spec_no       =  []');
 disp('## masked        =  []');
 disp('## ndet          =  []'); 
 disp('## det_ind       =  []');
 disp('## det_eff       =  ISISEXCdetarray');
 disp('## det           =  ISISEXCdetarray');
     
disp(' ');
