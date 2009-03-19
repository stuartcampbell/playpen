function r = getr(self,field)
% getr(rawfile,'FIELD')
%
% function to extract a real variable from an IXTraw_file object,
%eg. getr(rawfile,'LEN2')
r = libisisexc('IXTraw_file', 'getr', self, field);