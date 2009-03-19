function r = getgpath
% r = getgpath
% will return the global path object to r, which will be an IXTpath object or array of objects 
r=ixf_global_var('fortran','get','GPATH');