function setup_maps_mono_van

ixf_global_var('homer_mono_van','remove');
% particular to mono van operation
ixf_global_var('homer_mono_van','set','d_int',[-1 1]);
 %this is instrument specific currently value for MAPS
ixf_global_var('homer_mono_van','set','mon_scale',1.7016e8);
% more general settings 
ixf_global_var('homer_mono_van','set','background',[12000, 18000]);
ixf_global_var('homer_mono_van','set','peak_scale',1000000);
ixf_global_var('homer_mono_van','set','uamp_scale',1000);
ixf_global_var('homer_mono_van','set','det_units','$w');
% default normalisation is with monitor 1
ixf_global_var('homer_mono_van','set','normalisation',int32(1));
ixf_global_var('homer_mono_van','set','range',[1000,2000]);
