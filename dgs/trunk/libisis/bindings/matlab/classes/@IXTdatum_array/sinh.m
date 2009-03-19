function datum_array = sinh(a)
%  function to take the hyperbolic sine of a datum_array
datum_array = libisisexc('IXTdatum_array','sinh',IXTdatum_array([0],[0]),a);