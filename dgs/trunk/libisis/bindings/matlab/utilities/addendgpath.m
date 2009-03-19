function addendgpath(name,varargin)
% addtoend_gpath('name','direc1','direc2',...,'direcN')
% adds a directory or directories to an existing global path entry 'name'
% the directories will be added to the end of the list.
% this global path can also be used in the matlab environment using the
% ixf_global_props function
r = libisisexc('IXTpath','addtoend_varargin',IXTpath,name,varargin);
ixf_global_var('fortran','set','GPATH',r);