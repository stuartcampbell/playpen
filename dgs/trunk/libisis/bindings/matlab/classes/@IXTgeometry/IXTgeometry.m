function geometry = IXTgeometry(varargin)
% MATLAB constructor for IXTgeometry:
%   >> geometry = IXTgeometry( [translation], [orientation], [shape])

% create a blank object - this is needed as fortran cannot create real classes
geometry.base=IXTbase;
geometry.translation=IXTtranslation;
geometry.orientation=IXTorientation;
geometry.elmt_to_shape=[int32(0)];
geometry.elmt_to_index=[int32(0)];
geometry.shape=[IXTshape];
geometry = class(geometry,'IXTgeometry');

if (nargin > 0)
%    Now call the fortran constructor which will fill the object and also check its arguments
    geometry = libisisexc('IXTgeometry','create',geometry,varargin);
end