function wout = flip_y(w)
% Reverses the order of the points along the y axis.
%
%   >> wout = flip_y(win)
%
% Handy if the data has been read from a file in which y
% is in decreasing order; many routines assume that the data is
% in increasing x order.

wout = w;
for i=1:length(w)   
    wout(i).y = fliplr(w(i).y);
    wout(i).signal = fliplr(w(i).signal);
    wout(i).error = fliplr(w(i).error);
end

