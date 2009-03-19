function wout= combine(varargin)

% COMBINE(w1, x1, w2, ..., xn-1, wn, delta)
% Combines workspaces with identical x axes into a new workspace.
% The data set are "glued" together at the points x1,x2..xn-1
% with a smoothing function that extends +/-(delta/2) about those points.
%
% Syntax:
%   >> wout = combine (w1, x1, x2, delta)                   % minimum case
%
%   >> wout = combine (w1, x1, w2, x2 ... xn-1, wn, delta)    % general case
%

% [Joost van Duijn: 28-08-03 modified for Libisis by Dean Whittaker:
% 22.08.2007]

 
nwork= floor(nargin/2); %number of workspaces that will be combined
if nwork<=1,
    disp(['Error not enough input paramters given! Minimum input consists'...
        ' of eg. wout= combine(w1,x1,w2,delta)']);
    return;
end

% Check if input parameters 1, 3, 5, ..., 2*nwork-1 are of type spectrum
% and all have the same x-axis.
j=0;
for i=1:2:2*nwork-1,
   j= j+1;
%   eval(['w', int2str(j), '= varargin{i}']);
%    w(j)=varargin{i};
    if ~isa(varargin{i} , 'IXTdataset_1d'),
        disp(['Error! Input argument ' int2str(i) ' is not an IXTdataset_1d']);
        return;
    end
    
    xrange= w(1).x;
    yrange= length(w(1).signal);
    
    if (length(xrange)~=length(w(j).x))||(xrange~=w(j).x),
        disp(['Error! Input spectra do not have identical xrange']);
        return;
    end
    if yrange~=length(w(j).signal),
        disp('Error! You are trying to combine histogram and point data');
        return;
    end
end

% Check if the input paramters 2, 4, ..., 2*nwork-2 are double, these
% paramters contain the boundaries of the different spectra, and 
% are in increasing oder. Will give x1, x2, x3, ..., xn-1.
j= 0;
for i=2:2:2*nwork-2,
   j= j+1;
   if ~isa(varargin{i}, 'double'),
       disp(['Error! Input agument ' int2str(i) ' is not a number']);
       return;
   else
       x(j)= varargin{i};
   end
   if (j==1)&&(x(j)<xrange(1)),
       disp(['Error! The 1st boundary condition does not lie within the xrange']);
       return;
   elseif (j>1)&&(x(j-1)>x(j)),
       disp('Error! the boundary conditions are not in increasing order');
       return;
   elseif (i==2*nwork-2)&&(x(j)>xrange(length(xrange))),
       disp('Error! The final boundary condition does not lie within the xrange');
       return;
   else
   end
end

% Check if the last agument is a double. This is the delta that will be
% used in the smoothing function, determines the range over which data 
% is smoothed near the boundaries. Also check whether delta is bigger then
% the individual bin sizes in the xrange.
if ~isa(varargin{2*nwork}, 'double')||(varargin{2*nwork}<0),
    disp('Error! The final input argument is not a number or is negative!! This is delta.');
    return;
else
    delta=varargin{2*nwork};
end

% Calculate the smoothing functions for the different data sets. The 2 hat
% functions are going to be defined as:
% 1) width= xhi-xlo for each data set, height=1;
% 2) width= delta, height= 1/delta;
for i=1:nwork,
    if i==1,
        xhi= x(1);
        xlo= xrange(1)-abs(x(1));
    elseif i==nwork,
        xhi= xrange(yrange)+abs(x(nwork-1));
        xlo= x(nwork-1);
    else
        xhi= x(i);
        xlo= x(i-1);
    end
    xav= (xhi+xlo)/2;
    y(i,:)= hat2(abs(xhi-xlo), 1, delta, 1/delta, xrange(1:yrange)-xav);
    ytemp(i,:)= y(i,:).*w(i).signal';
    etemp(i,:)= w(i).error';
end
wy= sum(ytemp,1);
we= sqrt(sum(etemp.^2,1));
wout = IXTdatset_1d(w(1).base, w(1).title, wy, we, w(1).s_axis,...
        xrange, w(1).x_axis, w(1).x_distribution);