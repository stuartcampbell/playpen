


function ui_setaxis(varargin)
%----------------------------------------------------
%Function Syntax: ui_setaxis(isXY,logap,higap)
%Purpose: Shift X or Y axes by gap specified
%Output: None
%Input: isXY for denoting for (0)X or (1)ediY axes, gap to denote
%how much to shift
%Example: 
% ui_setaxis(1,1,2) --> for y axes ylo = 1 yhi = 2
% ui_setaxis(0,1,2) --> for x axes xlo = 1 xhi = 2
%-----------------------------------------------------

%global structures

IXG_ST_INTERFACEVALIDATION = ixf_global_var('libisis_graphics','get','IXG_ST_INTERFACEVALIDATION');

iArg = 1;
tot = numel(varargin);
ixf_validate_arg(IXG_ST_INTERFACEVALIDATION.ui_chngaxis,tot);
isXY = varargin{1};
logap = varargin{2};
higap =varargin{3};
if (higap < logap)
    ixf_display_error(IXG_ST_ERROR.greater_value,'higap','logap');
end
ixf_gen_interface('iname','setprop_interface','fname','setaxes','isXY',isXY,'logap',logap,'higap',higap);