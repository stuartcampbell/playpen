function r = add_item(self,varargin)
% dso=add_item(dso,path,type,object_name)
% creates an entry in the dso with a given path, type and object_name
% (optional)
r = libisisexc('IXTdata_source','additem_varargin',self,varargin);