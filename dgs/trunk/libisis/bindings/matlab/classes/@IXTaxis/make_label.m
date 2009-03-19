function [xlab,varargout] = make_label(x_axis,varargin)%(x_axis,s_axis,dist)
% [xlab,ylab]= make_label(x-axis,y-axis,x-dist)
% [xlab,ylab,zlab]= make_label(x-axis,y-axis,x-dist)
% makes signal and x labels according to supplied IXTaxis objects and
% x distribution property of a dataset

if(nargin == 3)
    [xlab,slab] = libisisexc('IXTaxis','make_label_1d_varargin',x_axis,varargin(:));
else
    [xlab,ylab,slab] = libisisexc('IXTaxis','make_label_2d_varargin',x_axis,varargin(:));
end

xlab=cellstr(xlab);
if nargout == 2
    varargout(1)={cellstr(slab)};
elseif nargout==3
    varargout(1)={cellstr(ylab)};
    varargout(2)={cellstr(slab)};
end