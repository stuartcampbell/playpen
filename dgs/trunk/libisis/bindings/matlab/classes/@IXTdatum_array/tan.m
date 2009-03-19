function datum_array = tan(a)
%  function to take the tangent of a datum_array
datum_array = libisisexc('IXTdatum_array','tan',IXTdatum_array([0],[0]),a);