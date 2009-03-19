function [u1, u2, u3, signal] = ixf_compute_values_threed(x,y,z,sig,oid,format)
%-------------------------------------------------------------------
%function syntax: [xmod,ymod] = ixf_compute_values(x,y,e,format)
%input: x, y, e and format (e can be empty string '')
%output: x and y values (modified)
%example: [xret,yret] = ixf_compute_values(x,y,e,format)
%compute values for line,error,marker and histogram format
%-------------------------------------------------------------------

%global structures

nx = numel(x);
ny = numel(y);
nz = numel(z);
nsigx = size(sig,1);
nsigy = size(sig,2);
nsigz = size(sig,3);

switch(format)
    case 'sliceomatic'    %for normal plot (point or histogram)
        
        if nx == nsigx
            u1 = [min(x), max(x)];
        else
            dp1 = (x(end)-x(1))/(length(x)-1);
            u1 = [x(1)+dp1/2, x(end)-dp1/2];
        end
        
        if ny == nsigy
            u2 = [min(y), max(y)];
        else
            dp1 = (y(end)-y(1))/(length(y)-1);
            u2 = [y(1)+dp1/2, y(end)-dp1/2];
        end
        
        if nz == nsigz
            u3 = [min(z), max(z)];
        else
            dp1 = (z(end)-z(1))/(length(z)-1);
            u3 = [z(1)+dp1/2, z(end)-dp1/2];
        end
        signal = permute(sig,[2,1,3]);
            
end