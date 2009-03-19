function setup_het_diag_defaults

ixf_global_var('diag','remove');
het_bank=IXTmap({[6:49,51:72,75:96,101:356],401:2584});
ixf_global_var('diag','set','bank',het_bank);
% particular to white beam operation
ixf_global_var('diag','set','vv_lo',0.1);
ixf_global_var('diag','set','vv_hi',3.0);
ixf_global_var('diag','set','sv_hi',1.5);
ixf_global_var('diag','set','r',10.0);
ixf_global_var('diag','set','bmin',12000);
ixf_global_var('diag','set','bmax',18000);

ixf_global_var('diag','set','hardmask','inst_masks:::het_991.msk');

ixf_global_var('diag','set','summary',true);
ixf_global_var('diag','set','messages',false);

