function r = IXTraw_file(filename)
% rawfile=IXTraw_file('FILE.RAW')
% or 
% rawfile=IXTraw_file('drive:\fullpath\FILE.RAW')
% or
% rawfile=IXTraw_file('inst_data:::FILE.RAW')
% function to create an IXTraw_file object in matlab. FILE.RAW must be
% in the current working directory of matlab, if not the full path to the
% file must be stipulated.
r.runid = ' ';
r.found = false;
r.ntc1 = 0;
r.nsp1 = 0;
r.ndet = 0;
r.nper = 0;
r.nmon=0;
r.nuse=0;
r.daeh=[ 0 0 ];
r = class(r,'IXTraw_file');
if (nargin > 0)
    r = libisisexc('IXTraw_file', 'create', r, translate_read(filename));
    r = libisisexc('IXTraw_file', 'open', r);
end
