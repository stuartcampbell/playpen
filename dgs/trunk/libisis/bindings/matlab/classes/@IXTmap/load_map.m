function map_out = load_map(map_in,filename)
% IXTmap=fileread(IXTmap,'filename')

map_out = libisisexc('IXTmap','fileread',IXTmap,map_in,filename);

