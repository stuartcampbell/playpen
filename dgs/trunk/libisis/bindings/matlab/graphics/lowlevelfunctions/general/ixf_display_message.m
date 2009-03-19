

function ixf_display_message(varargin)
%--------------------------------------------------------------------------
%Syntax: ixf_display_message(varargin)
%Purpose: to display message like success, set property
%Input: message no
%Output: none
%Example: ixf_display_message(IXG_ST_WARNING.success)
%the above example displays a success message
%--------------------------------------------------------------------------

%global structure
IXG_ST_MESSAGE=ixf_global_var('libisis_graphics','get','IXG_ST_MESSAGE');
message_no = varargin{1};
if ( message_no == IXG_ST_MESSAGE.success)
    disp('Command Execution Successful');
elseif (message_no == IXG_ST_MESSAGE.set)
    disp('Property Set');
end