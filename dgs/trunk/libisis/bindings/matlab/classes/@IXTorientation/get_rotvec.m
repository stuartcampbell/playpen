function rotvec = get_rotvec (orientation)
% Get the rotation vector that is equivalent to the orientation
%
% Syntax
%   >> rotvec = get_rotvec (orientation)
%
rotvec = libisisexc('IXTorientation','get_rotvec',orientation);