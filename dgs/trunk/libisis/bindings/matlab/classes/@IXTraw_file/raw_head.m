function raw_head(rawfile)
% raw_head(rawfile)
%
% function to extract any  variable from an IXTraw_file object,
%eg. raw_head(rawfile,'NDET')
libisisexc('IXTraw_file', 'head', rawfile);