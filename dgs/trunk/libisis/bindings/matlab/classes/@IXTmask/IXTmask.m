function mask = IXTmask( varargin )
% IXTmask(IXTbase,[mask_array]);
% or IXTmask([mask_array])
mask.base = IXTbase;
mask.mask_array=[int32(1)];
mask = class(mask,'IXTmask');
if (nargin == 1) && isnumeric(varargin{1})
    if(size(varargin{1},1)==1)
% to avoid writing a new constructor in the fortran, implicitly create a
% cell array as input for the libisisexc create call
        mask=libisisexc('IXTmask','create',mask,{IXTbase('entry',false,true),int32(sort(varargin{1}))});
    end
elseif (nargin > 0)
    mask = libisisexc('IXTmask','create',mask,varargin);
end