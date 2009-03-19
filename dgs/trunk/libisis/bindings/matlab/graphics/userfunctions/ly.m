function ly(varargin)
%------help for gtk change y limits command - ly---------------------------
% Function Syntax: LY(ylo,yhi)
% Purpose: Shift Y ayes by limits specified
%
% Output: None
% Input: ylo-lower limit,yhi-upperlimit
%
% Example: 
% LY(1,2) or ly 1 2 --> for y axes ylo = 1 yhi = 2
% LY --> stretch y limit to fullest
%--------------------------------------------------------------------------
% Dean Whittaker 2007
% I.Bustinduy 20/11/07
if (nargin == 0)
    [varargin{1},varargin{2}] = ixf_get_limits('hdl',gca,'y');
end
for i=1:nargin,
    if(~isnumeric(varargin{i})), varargin{i}=str2double(varargin{i}); end
end
ui_setaxes(1,varargin{:});

