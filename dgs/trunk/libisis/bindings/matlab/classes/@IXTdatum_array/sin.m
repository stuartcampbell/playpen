function datum_array = sin(a)
%  function to take the sine of a datum_array
datum_array = libisisexc('IXTdatum_array','sin',IXTdatum_array([0],[0]),a);