function datum_array = tanh(a)
% function to take the hyperbolic tangent of a datum_array
datum_array = libisisexc('IXTdatum_array','tanh',IXTdatum_array([0],[0]),a);