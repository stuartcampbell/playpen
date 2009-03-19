function [wout] = scale_x(w, fac)
% Multiply the x-axis by the factor fac
%
% Syntax:
%   >> w_out = scale_x (w_in, fac)

% Catch trivial case of single input, or non-numeric scale factor:
wout = w;

if (nargin == 2)
    if ~isa_size(fac,[1,1],'numeric')       % check 2nd argument a single number
        error ('Check scale factor is a number')
    end
    for i=1:length(w)
        wout(i).x = w(i).x*fac;
    end
end