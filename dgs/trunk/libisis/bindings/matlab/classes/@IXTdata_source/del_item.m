function r = del_item(self,dtype)
% dso=del_item(dso,datatype), this will delete all items defined as
% 'datatype' in the data source object
r = libisisexc('IXTdata_source','delitem',self,dtype);
