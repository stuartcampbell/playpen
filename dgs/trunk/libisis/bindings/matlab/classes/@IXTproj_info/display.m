function display( proj_info )
% Display an ISISEXCproj_info object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' proj_info.entry_name]);
disp(['## name                  =  ' proj_info.name]);
 disp('## n_axes                =  '),disp(proj_info.n_axes);
disp(['## caption               =  [' proj_info.caption,']']);
 disp('## uindex                =  '),disp(proj_info.uindex);
 disp('## u_string              =  []');
 disp('## u                     =  '),disp(proj_info.u);
 disp('## ulength               =  '),disp(proj_info.ulength);
 disp('## u2ortho               =  '),disp(proj_info.u2ortho);
 
disp(' ');
