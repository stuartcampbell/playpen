function res = ixf_reset_all()
%-------------help for GTK ixf_reset_all function--------------------------
% function syntax: res = ixf_reset_all
% purpose: to reset the global default structure (for all graphs)
% Input: none                        Output: true or false
% example: res = ixf_reset_all
% the above example will reset the default structure
% updated: 11/08/2006               Dean Whittaker
%--------------------------------------------------------------------------

IXG_ST_STDVALUES= ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');
% currently no forseeable error - simply run the initiation command to get
% all the defaults from the gtk_globaldefaults mfile. 

ixf_globaldefault;
res=IXG_ST_STDVALUES.true;
res=ixf_reset_default(0);

% call the reset figure command for all open figures


