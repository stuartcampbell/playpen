function [xb,yb] = ixf_compute_values(x,y,e,format)
%-------------------------------------------------------------------
%function syntax: [xmod,ymod] = ixf_compute_values(x,y,e,format)
%input: x, y, e and format (e can be empty string '')
%output: x and y values (modified)
%example: [xret,yret] = ixf_compute_values(x,y,e,format)
%compute values for line,error,marker and histogram format
%-------------------------------------------------------------------

%global structures

nx = length(x);
ny = length(y);

switch(format)

    case 'e'          %for error bars
        xb=zeros(1,3*ny);       % x array for plotting error bars
        if (nx == ny)           % point data
            xb(1:3:end)=x;
            xb(2:3:end)=x;
            xb(3:3:end)=NaN;
        else
            temp=0.5*(x(2:nx) + x(1:nx-1));
            xb(1:3:end)=temp;
            xb(2:3:end)=temp;
            xb(3:3:end)=NaN;
        end
        yb=zeros(1,3*ny);       % y array for plotting error bars
        yb(1:3:end)=y-e;
        yb(2:3:end)=y+e;
        yb(3:3:end)=NaN;
    case {'m', 'pp', 'd', 'l'}     % for markers
         if (nx == ny)        % point data
            xb = x;
            yb = y;
        else            
            xb = 0.5*(x(2:nx) + x(1:nx-1));
            yb = y;
        end
    case 'p'    %for normal plot (point or histogram)
        xb=zeros(1,2*ny);    % x array for plotting histogram
        yb=zeros(1,2*ny);    % y array for plotting histograms
        if (nx == ny)        % point data
            del0=0.5*(x(2)-x(1));
            xb(1)=x(1)-del0;
            xb(2:2:2*ny-2)=0.5*(x(2:ny)+x(1:ny-1));
            xb(3:2:2*ny-1)=0.5*(x(2:ny)+x(1:ny-1));
            del1=0.5*(x(ny)-x(ny-1));
            xb(2*ny)=x(ny)+del1;
        else
            xb(1)=x(1);
            xb(2:2:2*ny-2)=x(2:ny);
            xb(3:2:2*ny-1)=x(2:ny);
            xb(2*ny)=x(nx);
        end
        yb(1:2:end)=y;
        yb(2:2:end)=y;
end