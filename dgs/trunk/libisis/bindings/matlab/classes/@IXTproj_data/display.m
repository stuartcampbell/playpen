function display( proj_data )
% Display an ISISEXCproj_data object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' proj_data.entry_name]);
disp(['## name                  =  ' proj_data.name]);
 disp('## s                     =  [n_pixels]');
 disp('## e                     =  [n_pixels]');
 disp('## u                     =  [n_axes, n_pixels]');
 disp('## id                    =  [n_pixels]');
 disp('## iw                    =  [n_pixels]');
 disp('## xlo                   =  [n_pixels]');
 disp('## xhi                   =  [n_pixels]');

disp(' ');

