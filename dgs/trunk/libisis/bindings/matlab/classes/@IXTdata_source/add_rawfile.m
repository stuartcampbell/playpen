function r = add_rawfile(self,path)
%dso=add_rawfile(dso,raw_file)
%dso is an IXTdata_source object 
%raw_file is a string defining the location of the raw data file
%to be loaded, eg. 'X:\rawfiles\inst.RAW'
r = libisisexc('IXTdata_source','additem',self,path,'rawfile');