function r = cross(a,b)
% Calculates the cross product of two translation vectors
%
% Syntax
%   >> length = cross (a,b)    % a, b are translations

r = libisisexc('IXTtranslation','cross',IXTtranslation,a,b);