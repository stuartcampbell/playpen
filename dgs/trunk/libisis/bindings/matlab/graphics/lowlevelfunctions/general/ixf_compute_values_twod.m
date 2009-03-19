function [xb yb zb]=ixf_compute_values_twod(x,y,z,oid,format);
%--------------Help for gtk ixf_compute_values_twod------------------------
%
% purpose: convert x, y and z values into the correct format for use with
% the 2d plotting functions. 
%
% call syntax: [xb yb zb]=ixf_compute_values_twod(x,y,z,oid,format)
%
% inputs: x = xdata, y = ydata, z = z data, oid = the id of format (i.e.'format'), format = type of graph 
% outputs: xb,yb,zb - the formatted x,y and z data 
%
% example: [x,y,z]=ixf_compute_values_twod(w.x,w.y,w.signal,'type','area')
%
%---------------------updated: 15/08/2006, Dean Whittaker------------------
IXG_ST_STDVALUES=ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

                % setup flags, script will not execute unless all flags have been set to true
xflag=IXG_ST_STDVALUES.false;
yflag=IXG_ST_STDVALUES.false;
zflag=IXG_ST_STDVALUES.false;
zb=z;

% check for types of graph and switch the type for the more general types used here
if strcmp(format,'surface') || strcmp(format,'contour') || strcmp(format,'stem') || strcmp(format,'points')
    format = 'point';
elseif strcmp(format,'area')
    format='histogram';
end
% checkvalues checks that dimensions agree, and that dimensions are
% correct for purposes.

switch format
    case 'histogram'
        if checkvalues('x',x,zb)==IXG_ST_STDVALUES.true   % check for point data and convert
            xb=ixf_points_to_boundaries(x);
            zb((end+1),:)=0;                % dummy final column for z
            xflag=checkvalues('x',xb,zb);
        else                                % x is histogram data, add a dummy z
            xb = x;
            zb((end+1),:)=0;                
            xflag=checkvalues('x',xb,zb);
        end
        if checkvalues('y',y,zb)==IXG_ST_STDVALUES.true   % check for point data and convert
            yb=ixf_points_to_boundaries(y);
            zb(:,(end+1))=0;                % dummy final column for z
            yflag=checkvalues('y',yb,zb);
        else                                % y is histogram data
            yb = y;
            zb(:,(end+1))=0;
            yflag=checkvalues('y',yb,zb);
        end
        xy.y=yb;
        xy.x=xb;
        if ~checkvalues('z',xy,zb)
        [xb,yb]=meshgrid(xb,yb);            % format x and y data for plotting
        xb=xb';
        yb=yb';
        xy.y=yb;
        xy.x=xb;
        end
        if ~checkvalues('z',xy,zb)
            zflag=IXG_ST_STDVALUES.false;
            error('dimensions do not match')
        else
            zflag=IXG_ST_STDVALUES.true;
        end
        if ~(xflag && zflag && yflag)       % final error catch
            error('incorrectly formatted data, unable to plot');
        end
    case 'point'
        if checkvalues('x',x,zb)==IXG_ST_STDVALUES.true   % check for point data and convert
            xb=x;
            zb=z;            
            xflag=checkvalues('x',xb,zb);
        else                                % x is histogram data
            xb = 0.5*(x(2:end) + x(1:(end-1)));
            zb=z;               
            xflag=checkvalues('x',xb,zb);
        end
        if checkvalues('y',y,zb)==IXG_ST_STDVALUES.true   % check for point data and convert
            yb=y;
            zb=z;             
            yflag=checkvalues('y',yb,zb);
        else                                % y is histogram data
             yb = 0.5*(y(2:end) + y(1:(end-1)));
            zb=z;
            yflag=checkvalues('y',yb,zb);
        end

        xy.y=yb;
        xy.x=xb;        % check for already meshed data
        if ~checkvalues('z',xy,zb)
        [xb,yb]=meshgrid(xb,yb);            % format x and y data for plotting
        xb=xb';
        yb=yb';
        xy.y=yb;
        xy.x=xb;
        end
        
        if ~checkvalues('z',xy,zb)
            zflag=IXG_ST_STDVALUES.false;
            error('dimensions do not match')
        else
            zflag=IXG_ST_STDVALUES.true;
        end
        if ~(xflag && zflag && yflag)       % final error catch
            error('incorrectly formatted data, unable to plot');
        end
end

%--------------------------------------------------------------------------
function result = checkvalues(format,xy,z)
% syntax: result=checkvalues(type,xy,z) where xy is x or y data, z is z
% data and type is the type of check to perform ('x', 'y', 'xysize')
% function tests that x, y and z lengths are correct
% returns true if x and z lengths are equal, or if the size of y in the 
% x or y dimensions is equal to the length of x and y respectively, false if not.


IXG_ST_STDVALUES=ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

[nxyx nxy]=size(xy);
mxy=length(xy);
[nzx nzy]=size(z);
mz=length(z);

switch format
    case 'x'
        if nxy==nzx&&isnumeric(xy)&&isnumeric(z)
            result=IXG_ST_STDVALUES.true;
        elseif mxy==mz && isnumeric(xy) && isnumeric(z) && nxyx~=1
            result=IXG_ST_STDVALUES.true;
        else
            result=IXG_ST_STDVALUES.false;
        end
    case 'y'
        if (nxy==nzy)&&isnumeric(xy)&&isnumeric(z)
            result=IXG_ST_STDVALUES.true;
        elseif mxy==mz && isnumeric(xy) && isnumeric(z) && nxyx~=1
            result=IXG_ST_STDVALUES.true;
        else 
            result = IXG_ST_STDVALUES.false;
        end
    case 'xysize'
        if (nxyx ~= 1)||(nzx ~= 1)
            result=IXG_ST_STDVALUES.false;
        else 
            result=IXG_ST_STDVALUES.true;
        end
    case 'z'
        checkxz= size(xy.x)==size(z);
        checkyz= size(xy.y)==size(z);
        if (checkxz(1) && checkxz(2) && checkyz(1) && checkyz(2))
            result=IXG_ST_STDVALUES.true;
        else
            result=IXG_ST_STDVALUES.false;
        end
    otherwise
        error('wrong input arguments')
end
    