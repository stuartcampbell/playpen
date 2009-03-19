function det_he3 = IXTdet_he3(varargin)
% IXTdet_he3(IXTbase,[checksum],[gas_pressure],[wall_thickness])
% creates an IXTdet_hes object 
det_he3.base = IXTbase;
det_he3.checksum = [int32(1)];
det_he3.gas_pressure = [0.0];
det_he3.wall_thickness = [0.0];
det_he3 = class(det_he3,'IXTdet_he3');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    det_he3 = libisisexc('IXTdet_he3','create',det_he3,varargin);
end
