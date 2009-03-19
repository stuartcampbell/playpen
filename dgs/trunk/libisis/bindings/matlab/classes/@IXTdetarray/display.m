function display(detarray)
% Display an ISISEXCdetarray object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name    =  ' detarray.entry_name])
disp(['## name          =  ' detarray.name])
disp('## ndet_tot       =  '),disp(detarray.ndet_tot);
disp('## det_no         =  []');
disp('## delta          =  []');
disp('## distance       =  []');
disp('## code           =  []');
disp('## theta          =  []');
disp('## phi            =  []');
disp('## w              =  []');
disp('## f              =  []');
disp('## alpha          =  []');
disp('## det            =  []');
    
disp(' ');
