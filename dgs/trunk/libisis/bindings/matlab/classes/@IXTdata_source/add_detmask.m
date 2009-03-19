function r = add_detmask(self,path)
%dso=add_detmask(dso,detector_mask_file)
%dso is an IXTdata_source object 
%detector_mask_file is a string defining the location of the detector mask file
%to be loaded, eg. 'X:\maskfiles\inst.mask'
%this will add a detector mask file location to an [IXTdata_source] object
r = libisisexc('IXTdata_source','additem',self,path,'detmaskfile');