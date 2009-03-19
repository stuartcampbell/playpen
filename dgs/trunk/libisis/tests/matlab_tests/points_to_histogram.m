function new_d2d = points_to_histogram(old_d2d)
%---- points_to_histogram--------
% turns the y array in a d2d into
% histogram data so that it can be
% tested fully
%
% >> new_d2d = points_to_histogram(old_d2d)
%

new_d2d = old_d2d;

zb = old_d2d.signal;

if length(old_d2d.x) == size(old_d2d.signal,1)
    xb=ixf_points_to_boundaries(old_d2d.x);
else
    xb = old_d2d.x;
end

if length(old_d2d.y) == size(old_d2d.signal,2)
    yb=ixf_points_to_boundaries(old_d2d.y);
else 
    yb = old_d2d.y;
end

new_d2d.x = xb;
new_d2d.y = yb;