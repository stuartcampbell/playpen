function r = geti(self,field)
% getr(rawfile,'FIELD')
%
% function to extract an integer variable from an IXTraw_file object,
%eg. getr(rawfile,'NDET')
r = libisisexc('IXTraw_file', 'geti', self, field);