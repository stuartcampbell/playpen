function r = gget(field)
% gget(rawfile,'FIELD')
%
% function to extract any  variable from an IXTraw_file object,
%eg. gget(rawfile,'NDET')

self = ixf_global_var('data_source','get','rawfile');

r = libisisexc('IXTraw_file', 'get_generic', self, field);