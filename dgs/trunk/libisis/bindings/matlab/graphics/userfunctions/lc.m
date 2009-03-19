function lc(varargin)
%---------Help for GTK color limits change command lc
% Function Syntax: LC(zdata_lo,zdata_hi)
% Purpose: Specifies the z datavalue for the first colour in the colour map 
% and the z datavalue for the last colour in the colour map. 
%
% Output: None
% Input: xlo = low, xhi = high limits
%
% Example: 
%  LC(1,2) --> colour starts at z = 1 and ends at z = 2, values between are
%  linearly interpolated
%  LC --> colour goes from min z to max z
%-----------------------------------------------------
% Dean Whittaker 2007

if (nargin == 0)
    [varargin{1},varargin{2}] = ixf_get_limits('hdl',gca,'c');
end
ui_setaxes(3,varargin{:});
colorbar
