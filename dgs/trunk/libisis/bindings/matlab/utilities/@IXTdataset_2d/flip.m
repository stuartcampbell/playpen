function wout = flip(w)
% Reverses the order of the points along the x axis.
%
%   >> wout = flip(win)
%
% Handy if the data has been read from a file in which x
% is in decreasing order; many routines assume that the data is
% in increasing x order.

wout = w;
for i=1:length(w) 
    wout(i).x = fliplr(w(i).x);                     % flip x
    wout(i).y = fliplr(w(i).y);                     % flip y
    wout(i).signal = flipud(fliplr(w(i).signal));   % flip z
    wout(i).error = flipud(fliplr(w(i).error));     % flip e
end
