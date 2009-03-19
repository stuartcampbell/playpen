function [path,found] = findpath(self,dtype)
[path,found] = libisisexc('IXTdata_source','findpath',self,dtype);