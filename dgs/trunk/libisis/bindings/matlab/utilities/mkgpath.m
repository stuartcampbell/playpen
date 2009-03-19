function  mkgpath(name,varargin)
% addgpath('name','direc1','direc2',...,'direcN')
% creates a global path entry 'name' and adds a variable number of directories to it 
% mkgpath('my_data_area','c:\rawfiles','d:\scratch\rawfiles')
% this global path can also be used in the matlab environment using the
% ixf_global_props function
r = libisisexc('IXTpath','addpath_varargin',IXTpath,name,varargin);
ixf_global_var('fortran','set','GPATH',r);