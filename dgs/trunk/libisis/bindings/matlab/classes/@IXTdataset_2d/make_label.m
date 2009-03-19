function [xlab,ylab,slab] = make_label(d2d)
% >> [xlab, ylab,slab] = make_label(d2d)
% xlab/ylab  labels for x,y axes
% slab is the signal label (or zlabel)

[xlab,ylab,slab] = libisisexc('IXTdataset_2d','make_label',d2d);
xlab=cellstr(xlab);
ylab=cellstr(ylab);
slab=cellstr(slab);