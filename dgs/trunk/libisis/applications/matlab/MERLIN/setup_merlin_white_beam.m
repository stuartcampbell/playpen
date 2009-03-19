function setup_merlin_white_beam

ixf_global_var('homer_white_beam','remove');

% particular to white beam operation
ixf_global_var('homer_white_beam','set','ei','white');
ixf_global_var('homer_white_beam','set','d_int',[20,50]);

% more general settings 
ixf_global_var('homer_white_beam','set','peak_scale',1000000);
ixf_global_var('homer_white_beam','set','uamp_scale',1000);
ixf_global_var('homer_white_beam','set','mon_scale',1000);
ixf_global_var('homer_white_beam','set','det_units','$e');

% default normalisation is with monitor 1
ixf_global_var('homer_white_beam','set','normalisation',int32(1));
ixf_global_var('homer_white_beam','set','range',[1000,2000]);

