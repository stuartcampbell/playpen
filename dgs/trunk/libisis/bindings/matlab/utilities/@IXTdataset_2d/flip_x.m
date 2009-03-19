function wout = flip_x(w)
% Reverses the order of the points along the x axis.
%
%   >> wout = flip_x(win)
%
% Handy if the data has been read from a file in which x
% is in decreasing order; many routines assume that the data is
% in increasing x order.

wout = w;
for i=1:length(w)
    wout(i).x = fliplr(w(i).x);
    wout(i).signal = flipud(w(i).signal);
    wout(i).error = flipud(w(i).error);
end

