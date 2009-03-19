function r = getr(field)
% getr(rawfile,'FIELD')
%
% function to extract a real variable from an IXTraw_file object,
%eg. getr(rawfile,'LEN2')

self = ixf_global_var('data_source','get','rawfile');

if isempty(self)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end

r = libisisexc('IXTraw_file', 'getr', self, field);