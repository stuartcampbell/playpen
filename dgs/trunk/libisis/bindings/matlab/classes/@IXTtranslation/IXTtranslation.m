function translation = IXTtranslation(varargin)
% MATLAB constructor for IXTtranslation:
%   >> translation = IXTtranslation([three_vector])

% create a blank object - this is needed as fortran cannot create real classes
translation.base=IXTbase;
translation.vector=zeros(3,1);
translation = class(translation,'IXTtranslation');

if (nargin > 0)
% Now call the fortran constructor which will fill the object and also check its arguments
    translation = libisisexc('IXTtranslation','create',translation,varargin);
end