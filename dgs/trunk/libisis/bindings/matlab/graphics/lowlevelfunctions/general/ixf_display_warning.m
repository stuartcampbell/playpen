function ixf_display_warning(varargin)
%--------------------------------------------------------------------------
%Syntax: ixf_display_warning(warning_no)
%Purpose: to display warning
%Input: warning no 
%Output: none
%Example: ixf_display_warning(IXG_ST_WARNING.no_figure)
%the above example displays a warning message of no figure
%--------------------------------------------------------------------------

%global structure
IXG_ST_WARNING=ixf_global_var('libisis_graphics','get','IXG_ST_WARNING');

warning_no = varargin{1};

if (warning_no == IXG_ST_WARNING.no_figure)
    warning('Figure not found with specified dimension');
end