function r = add_monmap(self,path)
%dso=add_monmap(dso,monitor_map_file)
%dso is an IXTdata_source object 
%monitor_map_file is a string defining the location of the monitor map file
%to be loaded, eg. 'X:\mapfiles\inst.map'
%this will add a monitor map file location to an [IXTdata_source] object
r = libisisexc('IXTdata_source','additem',self,path,'monmapfile');