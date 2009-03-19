function res = ixf_set_legend(varargin)
%--------------------------------------------------------------------------
%Function Syntax: ixf_set_legend
%Purpose: set legend
%Input: strings
%Output: true or false
%Example: ixf_set_legend('y=sinx','y=cosx')
%--------------------------------------------------------------------------


%global structure

[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

res = true;
res = ixf_checkinit('currentfigure');
if (res == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
    return;
end
tot = numel(varargin);
ret = ixf_getallhandle('hdl',gcf);
totph = numel(ret.ph);
%check for zero plots
if ( totph == 0)
    res = false;
    ixf_display_error(IXG_ST_ERROR.cannot_plot);
end
%check no of plots and args must be same
if (totph < tot)
    res = false;
    ixf_display_error(IXG_ST_ERROR.plot_legend_wrong);
end
legend(ret.ph(end:1),varargin{:});