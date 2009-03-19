function count_data = ixf_get_countdata(handle_,type,do_count)
%-----------help for gtk ixf_get_countdata---------------------------------
%
% purpose: get the count data for the figure handle 'handle_' and then
% increment, keep or reset the counter, then set it back into the figure
%
% syntax: 
%
% >> count_data = ixf_get_countdata(handle_,type,do_count)
%
% inputs: 
%
%   handle_: the figure handle_ to get the count data for
%
%   type: defines which counter to perform the action on 
%
%   do_count: can take one of 3 inputs:
%       IXG_ST_STDVALUES.counter_increment - increment the counter by 1
%       IXG_ST_STDVALUES.counter_reset - reset the counter to 1
%       IXG_ST_STDVALUES.counter_keep - do not alter the counter value.
%
% output: 
%   count_data - the count number after action has been performed
%
% example: 
%
% >> count_data = ixf_get_countdata(gcf, 'color', IXG_ST_STDVALUES.reset)
%
% will reset the 'color' counter in the current figure to 1. If the 'color'
% counter does not yet exist, it will be created and stored in the figure
% properties which can be accessed by:
%
% >> counter_info = get(gcf, 'userdata')
% >> counter_info.color
% 
% as well as the count_data output.
%
% Note that if the counter that is asked for does not exist, it will be
% created and initialised at 1, not 0.
%
%--------------------------------------------------------------------------

[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

if ~ ixf_check_graphicfigure('handle',handle_)    
   ixf_display_error(IXG_ST_ERROR.no_figure);
end

count = ixf_plotdata('get',handle_,'count');
try
    count_data = count.(type);
    if isempty(count_data)
        count_data = 0;
    end
    if do_count == IXG_ST_STDVALUES.counter_increment
        count_data = count_data + 1;
    elseif do_count == IXG_ST_STDVALUES.counter_reset
        count_data = 1;
    elseif do_count == IXG_ST_STDVALUES.counter_keep
    else
        error('incorrect counter information - please specify the count type (keep, increment or reset)')
    end

catch
    count_data = 1;
end

count.(type) = count_data;
ixf_plotdata('set', handle_, 'count', count);
