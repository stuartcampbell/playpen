function lx(varargin)
%-------help for gtk set x limits command lx-------------------------------
% Function Syntax: LX(xlo,xhi)
% Purpose: Shift X axes by gap specified
%
% Output: None
% Input: xlo = low, xhi = high limits
%
% Example: 
% LX(1,2) or lx 1 2 --> for x axes xlo = 1 xhi = 2
% LX --> stretch x limit to fullest
%-----------------------------------------------------
% Dean Whittaker 2007
% I.Bustinduy 20/11/07
if (nargin == 0)
    [varargin{1},varargin{2}] = ixf_get_limits('hdl',gca,'x');
end
for i=1:nargin,
    if(~isnumeric(varargin{i})), varargin{i}=str2double(varargin{i}); end
end
ui_setaxes(0,varargin{:});

