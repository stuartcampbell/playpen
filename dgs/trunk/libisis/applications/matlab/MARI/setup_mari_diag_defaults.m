function setup_mari_diag_defaults

% to make sure no homer settings interfere with diag operation

ixf_global_var('diag','remove');
% particular to white beam operation
ixf_global_var('diag','set','vv_lo',0.1);
ixf_global_var('diag','set','vv_hi',3.0);
ixf_global_var('diag','set','sv_hi',2.0);
ixf_global_var('diag','set','bmin',12000);
ixf_global_var('diag','set','bmax',18000);
ixf_global_var('diag','set','summary',true);
ixf_global_var('diag','set','messages',true);

