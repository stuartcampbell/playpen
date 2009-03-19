function setup_merlin_mono_sample
ixf_global_var('homer_mono_sample','remove');
ixf_global_var('homer_mono_sample','set','peak_scale',1000000);
ixf_global_var('homer_mono_sample','set','uamp_scale',1000);
%this is instrument specific currently value for MAPS
ixf_global_var('homer_mono_sample','set','mon_scale',1.7016e8);

ixf_global_var('homer_mono_sample','set','background',[12000, 18000]);
ixf_global_var('homer_mono_sample','set','det_units','$w');
% default normalisation is with monitor 1
ixf_global_var('homer_mono_sample','set','normalisation',int32(1));
ixf_global_var('homer_mono_sample','set','range',[1000,2000]);

