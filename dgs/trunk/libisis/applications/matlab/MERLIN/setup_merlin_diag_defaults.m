function setup_merlin_diag_defaults

ixf_global_var('diag','remove');
%maps_bank=IXTmap({1:17280,17281:18432,18433:32256,32257:41472});
%ixf_global_var('diag','set','bank',maps_bank);
% particular to white beam operation
ixf_global_var('diag','set','vv_lo',0.1);
ixf_global_var('diag','set','vv_hi',2.0);
ixf_global_var('diag','set','sv_hi',1.5);
ixf_global_var('diag','set','r',10.0);
%ixf_global_var('diag','set','hardmask','inst_masks:::4to1_022.msk');
ixf_global_var('diag','set','bmin',12000);
ixf_global_var('diag','set','bmax',18000);
ixf_global_var('diag','set','summary',true);
ixf_global_var('diag','set','messages',false);

