function r = IXTbase(varargin)
%
% usage:  IXTbase(entry_name)
%
% create a blank object - this is needed as fortran cannot create real
% classes
r.entry_name = 'unknown';
r.initialised = int32(15738456); % value must agree with IXCobject_initialised constant in IXMbase.f90
r.valid = logical(0);
r = class(r,'IXTbase');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTbase','create',r,varargin);
end