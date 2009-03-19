function  rmgpath(name,varargin)
% rmgpath('name','directory')
% this will remove a particular directory path from the 'name' searchpath
% if it exists. If the directory is not present then there will be no
% change
% this global path can also be used in the matlab environment using the
% ixf_global_props function
r = libisisexc('IXTpath','deldir_varargin',IXTpath,name,varargin);
ixf_global_var('fortran','set','GPATH',r);