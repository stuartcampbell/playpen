function map_out = fileread(map_in,filename)
% IXTmap=fileread(IXTmap,'filename')
% this function will read an ascii type map file into the IXTmap object
map_out = libisisexc('IXTmap','fileread',IXTmap,map_in,filename);

