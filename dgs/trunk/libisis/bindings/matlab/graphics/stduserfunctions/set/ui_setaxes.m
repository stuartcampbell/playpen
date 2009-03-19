

function ui_setaxes(varargin)
%----------------------------------------------------
%Function Syntax: ui_setaxes(isXY,logap,higap)
%Purpose: Shift X or Y axes by gap specified
%
%Output: None
%Input: isXY for denoting for (0)X or (1)ediY axes, gap to denote
%how much to shift
%
%Example: 
% ui_setaxes(1,1,2) --> for y axes ylo = 1 yhi = 2
% ui_setaxes(0,1,2) --> for x axes xlo = 1 xhi = 2
%-----------------------------------------------------

%global structures

[IXG_ST_INTERFACEVALIDATION, IXG_ST_ERROR] = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION','IXG_ST_ERROR');

set(gca,'XLimMode','manual')
set(gca,'YLimMode','manual')
set(gca,'ZLimMode','manual')

iArg = 1;
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_chngaxis,tot);
isXYZ = varargin{1};
logap = varargin{2};
higap =varargin{3};

if (higap < logap)
    ixf_display_error(IXG_ST_ERROR.greater_value,'higap','logap');
end
ixf_gen_interface('iname','setprop_interface','fname','setaxes','isXYZ',isXYZ,'logap',logap,'higap',higap);
