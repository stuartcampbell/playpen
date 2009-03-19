function r = IXTpath(varargin)
%
%
% create a blank object - this is needed as fortran cannot create real
% classes
r.name = 'name';
r.directory = [ 'line1'; 'line2' ];
r.counter = int32(0);
r = class(r,'IXTpath');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTpath','create',r,varargin);
end