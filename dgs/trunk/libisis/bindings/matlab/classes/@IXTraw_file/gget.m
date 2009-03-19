function r = gget(self,field)
% gget(rawfile,'FIELD')
%
% function to extract any  variable from an IXTraw_file object,
%eg. gget(rawfile,'NDET')
r = libisisexc('IXTraw_file', 'get_generic', self, field);