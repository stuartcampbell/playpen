function omega = solid_angle(shape,vp)
% solid_angle of a shape
omega = libisisexc('IXTshape','solid_angle',shape,vp);