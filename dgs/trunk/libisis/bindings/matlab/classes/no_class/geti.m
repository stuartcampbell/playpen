function r = geti(field)
% getr(rawfile,'FIELD')
%
% function to extract an integer variable from an IXTraw_file object,
%eg. getr(rawfile,'NDET')

self = ixf_global_var('data_source','get','rawfile');

if isempty(self)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end

r = libisisexc('IXTraw_file', 'geti', self, field);