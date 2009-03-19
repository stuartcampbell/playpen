function ret_out = undop
%----------help for gtk undo command - undop-------------------------------
%Function Syntax: UNDOP
%Purpose: remove plot
%Input: none
%Output: result true or false
%Example: ret = UNDOP
%--------------------------------------------------------------------------
% Pranav Amin

ret = ui_undoplot;

if nargout > 0
    ret_out = ret;
end