function r = add_diffinst(self)
% dso=add_diffinst(dso)
% this will add a diffraction instrument defintion to the data_source object
r = libisisexc('IXTdata_source','additem',self,'DEFINITION','diff_inst');