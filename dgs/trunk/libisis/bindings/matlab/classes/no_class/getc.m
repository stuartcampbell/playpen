function r = getc(field)
% getr(rawfile,'FIELD')
%
% function to extract a char variable from an IXTraw_file object,
%eg. getr(rawfile,'TITL')

self = ixf_global_var('data_source','get','rawfile');

if isempty(self)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end
r = libisisexc('IXTraw_file', 'getc', self, field);