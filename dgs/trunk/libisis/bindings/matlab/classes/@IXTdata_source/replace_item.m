function r = replace_item(self,varargin)
% dso=replace_item(dso,path,type,object_name)
% searches for the first matching 'type' in the dso and replaces its
% corresponding path and (optional) object_name properties
if nargin > 3
    error('too many arguments')
    return
end
r = libisisexc('IXTdata_source','replaceitem_varargin',self,varargin);