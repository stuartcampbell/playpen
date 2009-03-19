function datum_array = plus(a,b)
% adds two datum_array together
datum_array = libisisexc('IXTdatum_array','plus',IXTdatum_array([0],[0]),a,b);