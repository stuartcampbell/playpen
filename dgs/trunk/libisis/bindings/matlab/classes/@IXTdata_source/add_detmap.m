function r = add_detmap(self,path)
%dso=add_detmap(dso,detector_map_file)
%dso is an IXTdata_source object 
%detector_map_file is a string defining the location of the detector map file
%to be loaded, eg. 'X:\mapfiles\inst.map'
%this will add a detector map file location to an [IXTdata_source] object
r = libisisexc('IXTdata_source','additem',self,path,'detmapfile');