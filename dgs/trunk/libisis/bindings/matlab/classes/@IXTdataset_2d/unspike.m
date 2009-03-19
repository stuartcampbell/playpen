function dataset_2d = unspike(a,ymin,ymax,fac,sfac)
%--- Help for IXTdataset_2d/unspike.m---
% call syntax: dataset_2d = unspike(a)
%
% unspikes the signal data of an IXTdataset_2d object
%
% inputs: a = IXTdataset_2d object 
%
% output: IXTdataset_2d object.. dataset_2d = unspike(a,ymin,ymax,fac,sfac)
%
% There are four optional parameters: 
%
%   YMIN    If y < ymin then the point is considered a spike [Default: NaN i.e. ignored]
%   YMAX    If y > ymax then the point is considered a spike [Default: NaN i.e. ignored]
%   FAC     If a point is within a factor FAC of both of its neighbours
%          then is NOT a spike [Default: FAC = 2]
%   SFAC    If the difference of a point w.r.t. both of its neighbours is
%          less than SFAC standard deviations then the point is NOT a spike
%          [Default: 5]
%
% spikes are replaced with an interpolated value
%
% Use NaN to skip over an optional parameter (see examples below).
%
% Syntax:
%   >> wout = unspike (w1)
%   >> wout = unspike (w1, NaN, NaN, 1.5)   % to alter FAC to 1.5


res = IXTdataset_2d;
res(1:length(a)) = res(1);

values  = [NaN;NaN;2;5];     % 3rd and 4th are default values for FAC and SFAC

if (nargin > 1)
    if (isa(ymin,'double') & size(ymin)==[1,1])
        if (isfinite(ymin))
            values(1)=ymin;
        end
    else
        error ('Check YMIN for type and/or number of elements')
    end
end
if (nargin > 2)
    if (isa(ymax,'double') & size(ymax)==[1,1])
        if (isfinite(ymax))
            values(2)=ymax;
        end
    else
        error ('Check YMAX for type and/or number of elements')
    end
end
if (nargin > 3)
    if (isa(fac,'double') & size(fac)==[1,1])
        if (isfinite(fac))
            values(3)=fac;
        end
    else
        error ('Check FAC for type and/or number of elements')
    end
end
if (nargin > 4)
    if (isa(sfac,'double') & size(sfac)==[1,1])
        if (isfinite(sfac))
            values(4)=sfac;
        end
    else
        error ('Check SFAC for type and/or number of elements')
    end
end
if (nargin > 5)
    error('too many arguments')
end

dataset_2d = libisisexc('IXTdataset_2d','unspike_varargin',res,a,values(1),values(2),values(3),values(4));