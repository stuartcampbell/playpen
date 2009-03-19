function [paths,found] = findpaths(self,dtype)
[paths,found] = libisisexc('IXTdata_source','findpaths',self,dtype);