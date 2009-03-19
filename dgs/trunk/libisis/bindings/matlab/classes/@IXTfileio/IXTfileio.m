%
% usage:  IXTfileio()
%
function r = IXTfileio(varargin)
% create a blank object - this is needed as fortran cannot create real
% classes
r.file_name = 'unknown';
r.file_id = [ 0 0 ];
r.mode = 0;
r = class(r,'IXTfileio');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTfileio','create',r,varargin);
end
