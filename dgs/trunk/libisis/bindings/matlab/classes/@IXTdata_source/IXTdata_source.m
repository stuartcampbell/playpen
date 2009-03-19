function r = IXTdata_source(varargin)
%
%

% create a blank object - this is needed as fortran cannot create real
% classes
r.path = [ 'line1'; 'line2' ];
r.datatype = [ 'line1'; 'line2' ];
r.object_name = ['line1';'line2'];
r.counter = int32(0);
r = class(r,'IXTdata_source');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTdata_source','create',r,varargin);
end