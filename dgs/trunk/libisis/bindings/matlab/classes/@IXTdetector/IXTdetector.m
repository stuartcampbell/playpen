function detector = IXTdetector(varargin)
% IXTdetector(IXTdetector,[checksum],)
% creates an IXTdetector object 
detector.base = IXTbase;
detector.checksum = [int32(1)];
detector.det_no = [int32(1)];
detector.geometry = IXTgeometry;
detector.det_lookup = [int32(1)];
detector.ref_count = int32(1);
detector.delay_time = [0.0];
detector.dead_time = [0.0];
detector.theta = [0.0];
detector.L2 = [0.0];
detector.phi = [0.0];
detector.group_index = [int32(1)];
detector.det_type = [int32(1)];
detector.det_he3 = IXTdet_he3;
detector.det_solid = IXTdet_solid;
detector.type_index = [int32(1)];
detector.omega = [0.0];
detector = class(detector,'IXTdetector');
% now call the fortran constructor which will fill the obejct
% and also check its arguments
if (nargin > 0)
    detector = libisisexc('IXTdetector','create',detector,varargin);
end
