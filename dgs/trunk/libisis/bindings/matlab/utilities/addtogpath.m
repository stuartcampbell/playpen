function addtogpath(name,varargin)
% addtogpath('name','directory')
% adds a directory or directories to an existing global path entry 'name'
% this global path can also be used in the matlab environment using the
% ixf_global_props function
r = libisisexc('IXTpath','addtoend_varargin',IXTpath,name,varargin);
ixf_global_var('fortran','set','GPATH',r);