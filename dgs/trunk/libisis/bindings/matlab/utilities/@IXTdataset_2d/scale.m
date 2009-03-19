function [wout] = scale(w, fac_x, fac_y)
% Multiply the x and y axes by the factors fac_x and fac_y
%
% Syntax:
%   >> w_out = scale (w_in, fac_x, fac_y)

% Catch trivial case:
wout = w;

if (nargin == 3)
    if ~(isa_size(fac_x,[1,1],'numeric') && isa_size(fac_y,[1,1],'numeric'))       % check 2nd argument a single number
        error ('Check both scale factors are numbers')
    end
    for i=1:length(w)
        wout(i).x = w(i).x*fac_x;
        wout(i).y = w(i).y*fac_y;
    end
end

