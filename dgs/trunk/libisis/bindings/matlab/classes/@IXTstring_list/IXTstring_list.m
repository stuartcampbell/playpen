function r = IXTstring_list(varargin)
%
%
% create a blank object - this is needed as fortran cannot create real classes
r.list = [ ' ' ];
r.count = int32(0);
r = class(r,'IXTstring_list');
% now call the fortran constructor which will fill the object and also check its arguments
if (nargin > 0)
    r = libisisexc('IXTstring_list','create',r,varargin);
end