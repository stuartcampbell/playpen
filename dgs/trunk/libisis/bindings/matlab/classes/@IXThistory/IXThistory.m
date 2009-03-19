function r = IXThistory(varargin)
%
%
% create a blank object - this is needed as fortran cannot create real
% classes
r.entry = [ 'line1'; 'line2' ];
r.counter = int32(0);
r = class(r,'IXThistory');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXThistory','create',r,varargin);
end