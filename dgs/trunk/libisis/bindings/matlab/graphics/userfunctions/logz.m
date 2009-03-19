function logz
%-------help for gtk log z command logz------------------------------------
%Function Syntax: LOGZ()
%Purpose: set z to log scale
%
%Input: none
%Output: none
%
%Example: LOGZ
%--------------------------------------------------------------------------
% Dean Whittaker 2007

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

plotHandles_ = get(gca,'children');
area_flag = false;

for i = 1:length(plotHandles_)
    
    plot_type = ixf_plotdata('get', plotHandles_(i), 'plot_type');
    
    if plot_type == IXG_ST_STDVALUES.area_type
        area_flag(i)=true;
    end

    if plot_type == IXG_ST_STDVALUES.oned_type
        error('one d data only - logz is not applicable')
    end

end

if all(area_flag)
    logc;
    return
end

set(gca,'Zscale','log');
