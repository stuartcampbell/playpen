function orientation = IXTorientation(varargin)
% MATLAB constructor for IXTorientation:
%   >> orientation = IXTorientation([three_vector])

% create a blank object - this is needed as fortran cannot create real classes
orientation.base=IXTbase;
orientation.rotmat=zeros(3,3,2);
orientation = class(orientation,'IXTorientation');

if (nargin > 0)
% Now call the fortran constructor which will fill the object and also check its arguments
    orientation = libisisexc('IXTorientation','create',orientation,varargin);
end