function v = volume(geometry)
% volume of a geometry object
v = libisisexc('IXTgeometry','volume',geometry);