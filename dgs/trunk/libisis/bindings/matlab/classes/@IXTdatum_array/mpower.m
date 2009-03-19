function datum_array = mpower(a,b)
% raising one datum_array to the power of the other i.e w3=w2^w1 
datum_array = libisisexc('IXTdatum_array','power',IXTdatum_array([0],[0]),a,b);