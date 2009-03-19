

function res = ixf_undo_plot
%--------------------------------------------------------------------------
%function syntax: res = ixf_undo_plot
%purpose: undo or remove plot 
%input: none
%output: result true or false
%example: res = ixf_undo_plot
%the above example removes the plot
%--------------------------------------------------------------------------



%global structure
[IXG_ST_ERROR, IXG_ST_STDVALUES]=ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

%check for current figure;
res = true;
res = ixf_checkinit('currentfigure');
if (res == IXG_ST_STDVALUES.false)
    ixf_display_error(IXG_ST_ERROR.no_figure);
    return;
end

%if there then proceed
ret = ixf_getallhandle('hdl',gcf);
%total plots
tot = ret.ph;

%first is the recent one
if (tot > 0)
    delete(ret.ph(1));
    res = true;
end