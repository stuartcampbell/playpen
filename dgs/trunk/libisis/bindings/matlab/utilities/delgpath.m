function delgpath(name)
% del_gpath('name','directory')
% this will delete a searchpath 'name' from the global path object if it
% exists, otherwise no change will occur
% this global path can also be used in the matlab environment using the
% ixf_global_props function
r = libisisexc('IXTpath','delpath',IXTpath,name);
ixf_global_var('fortran','set','GPATH',r);