function raw_head()
% raw_head(rawfile)
%
% function to extract any  variable from an IXTraw_file object,
%eg. raw_head(rawfile,'NDET')

rawfile = ixf_global_var('data_source','get','rawfile');

if isempty(rawfile)
    error('No default RAW file currently assigned and no IXTraw_file object given')
end

libisisexc('IXTraw_file', 'head', rawfile);