function r = getc(self,field)
% getr(rawfile,'FIELD')
%
% function to extract a char variable from an IXTraw_file object,
%eg. getr(rawfile,'TITL')
r = libisisexc('IXTraw_file', 'getc', self, field);