function abs=monitor_mono_van(dso,run_no,line_int)
dso=add_item(dso,'DEFINITION','diff_inst');

monitor_map=ixf_global_var('data_source','get','monitor_monovan_map'); %

rf=mon(dso,run_no,'mon_map',monitor_map);
energy=get_ei(run_no,ei);
abs = libisisexc('IXTrunfile','moncorr',rf,energy,line_int);
% factor is a value extracted from comparing real monochromatic
% vanadium runs with the monitor to determine absolute units
factor=ixf_global_var('data_source','get','monitor_monovan_factor'); %
abs=abs*factor;
