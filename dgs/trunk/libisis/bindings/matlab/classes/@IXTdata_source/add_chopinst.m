function r = add_chopinst(self)
% dso=add_chopinst(dso)
% this will add a chopper instrument definition to the data_source
% object
r = libisisexc('IXTdata_source','additem',self,'DEFINITION','chop_inst');