function datum_array = mtimes(a,b)
% multiplies two datum_array 
datum_array = libisisexc('IXTdatum_array','times',IXTdatum_array([0],[0]),a,b);