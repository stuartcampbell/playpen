function r = add_monmask(self,path)
%dso=add_monmask(dso,monitor_mask_file)
%dso is an IXTdata_source object 
%monitor_mask_file is a string defining the location of the monitor mask file
%to be loaded, eg. 'X:\maskfiles\inst.mask'
%this will add a monitor mask file location to an [IXTdata_source] object
r = libisisexc('IXTdata_source','additem',self,path,'monmaskfile');