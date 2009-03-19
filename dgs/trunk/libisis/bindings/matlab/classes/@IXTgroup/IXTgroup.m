%
% usage:  IXTgroup(name,id,parent)
%
function r = IXTgroup(varargin)
% create a blank object - this is needed as fortran cannot create real
% classes
r.name = ' ';
r.id = -1;
r.parent = 0;
r = class(r,'IXTgroup');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTgroup','create',r,varargin);
end
