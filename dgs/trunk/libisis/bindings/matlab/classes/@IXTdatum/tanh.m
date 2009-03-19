function datum = tanh(a)
% takes the hyperbolic tangent of a datum
datum = libisisexc('IXTdatum','tanh',IXTdatum([0],[0]),a);