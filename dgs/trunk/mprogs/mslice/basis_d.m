function [d1,d2,d3]=basis_d(ar,br,cr)

% function [d1,d2,d3]=basis_d(ar,br,cr)
% Radu Coldea 02-Oct-1999

d1=ar/norm(ar);
d3=cross(ar,br)/norm(cross(ar,br));
d2=cross(d3,d1);
