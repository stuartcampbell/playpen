function det_solid = IXTdet_solid(varargin)
% IXTdet_solid(IXTbase,[checksum],[macro_xs])
% creates an IXTdet_solid object 
det_solid.base = IXTbase;
det_solid.checksum = [int32(1)];
det_solid.macro_xs = [0.0];
det_solid = class(det_solid,'IXTdet_solid');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    det_solid = libisisexc('IXTdet_solid','create',det_solid,varargin);
end
