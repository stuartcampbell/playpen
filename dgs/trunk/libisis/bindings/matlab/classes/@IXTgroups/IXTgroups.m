%
% usage:  IXTgroups(entry_name)
%
function r = IXTgroups(varargin)
% create a blank object - this is needed as fortran cannot create real
% classes
r.list = IXTgroup;
r.n = 0;
r = class(r,'IXTgroups');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTgroups','create',r,varargin);
end
