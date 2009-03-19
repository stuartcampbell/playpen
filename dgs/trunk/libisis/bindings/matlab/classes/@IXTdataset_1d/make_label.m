function [xlab,ylab] = make_label(d1d)
% >> [xlab, ylab] = make_label(d1d)
% ylabel is the signal label for a 1d plot
[xlab,ylab] = libisisexc('IXTdataset_1d','make_label',d1d);
xlab=cellstr(xlab);
ylab=cellstr(ylab);
