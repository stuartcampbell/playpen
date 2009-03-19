function omega = solid_angle(geometry,vp)
% solid_angle of a geometry object
omega = libisisexc('IXTgeometry','solid_angle',geometry,vp);