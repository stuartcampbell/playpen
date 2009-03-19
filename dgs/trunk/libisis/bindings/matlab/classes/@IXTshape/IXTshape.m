function shape = IXTshape(varargin)
% MATLAB constructor for IXTshape:
%   >> shape = IXTshape( [type], [dimensions])

% create a blank object - this is needed as fortran cannot create real classes
shape.base=IXTbase;
shape.shape_type=int32(-100);
shape.dimensions=[1,1];
shape = class(shape,'IXTshape');

if(nargin >0)
% Now call the fortran constructor which will fill the object and also check its arguments
    shape = libisisexc('IXTshape','create',shape,varargin);
end