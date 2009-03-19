function display(cut_tofndgs)
% Display an ISISEXCcut_tofndgs object

disp(' ');
disp([inputname(1),' = '])
disp(' ');
disp(['## entry_name            =  ' cut_tofndgs.entry_name])
disp(['## title                 =  ' cut_tofndgs.title])
disp(['## plot                  =  ISISEXCdataset_nd']);
disp(['## viewing_axes          =  ']),disp(cut_tofndgs.viewing_axes);
disp(['## viewing_axes_label    =  ']),disp(cut_tofndgs.viewing_axes_label);
disp(['## cut_spec_min          =  ']),disp(cut_tofndgs.cut_spec_min);
disp(['## cut_spec_max          =  ']),disp(cut_tofndgs.cut_spec_max);
disp(['## cut_spec_xyz          =  ']),disp(cut_tofndgs.cut_spec_xyz);
disp(['## cut_spec_delta        =  ']),disp(cut_tofndgs.cut_spec_delta);
disp(['## npix                  =  []']);
disp(['## pix_ind               =  []']);
disp(['## pixels                =  ISISEXCproj_tofndgs']);

    
disp(' ');
