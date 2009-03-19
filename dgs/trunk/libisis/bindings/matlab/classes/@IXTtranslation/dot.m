function r = dot(a,b)
% Calculates the dot product of two translation vectors
%
% Syntax:
%   >> length = dot (a, b)    % a, b are translations

r = libisisexc('IXTtranslation','dot',a,b);