function [flag] = ixf_checkinit(arg)
%--------------------------------------------------------------------------
%Function Syntax: return_value = ixf_checkinit(argument_string)
%Input: argument string
%Output: flag (stating error = 1 or 0). 0 means false, 1 means true
%Example: ret = ixf_checkinit('Currentfigure') 
%The above example checks any current figure exists
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%check arg < 1
if (nargin < 1)
    ixf_display_error(IXG_ST_ERROR.wrong_arg);
    flag = IXG_ST_STDVALUES.false;
    return;
end
%parse the argument
switch lower(arg)
    case 'currentfigure'
        figureHandle_ = get(0,'currentfigure');
        if (isempty(figureHandle_))
            % no figure
            flag = IXG_ST_STDVALUES.false;
            return;
        end        
        %figure is there
        %check my figure
        flag = ixf_check_graphicfigure('hdl',figureHandle_);
    otherwise
        % default figure is there
        flag = IXG_ST_STDVALUES.true;
        return;
end