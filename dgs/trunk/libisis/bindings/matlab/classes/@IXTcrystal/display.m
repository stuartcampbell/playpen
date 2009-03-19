function display(crystal)
% Display an ISISEXCcrystal object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name    =  ' crystal.entry_name])
disp(['## name          =  ' crystal.name])
disp(['## distance      =  ' num2str(crystal.distance)]);
disp(['## energy        =  ' num2str(crystal.energy)]);
disp(['## dspace        =  ' num2str(crystal.dspace)]);
disp('## relection      =  '),disp(crystal.relection);
disp(['## rho_h         =  ' num2str(crystal.rho_h)]);
disp(['## rho_v         =  ' num2str(crystal.rho_v)]);
disp(['## horiz_ap      =  ' num2str(crystal.horiz_ap)]);
disp(['## vert_ap       =  ' num2str(crystal.vert_ap)]);

disp(' ');