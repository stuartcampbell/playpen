
function linz
%---------help for gtk linear z function linz------------------------------
%Function Syntax: LINZ()
%Purpose: set z to linear scale
%Input: none
%Output: none
%Example: LINZ
%--------------------------------------------------------------------------
% Dean Whittaker 2007

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

plotHandles_ = get(gca,'children');
area_flag = [];

for i = 1:length(plotHandles_)
    plot_type = ixf_plotdata('get',plotHandles_(i),'plot_type');

    if plot_type == IXG_ST_STDVALUES.area_type
        area_flag(i)=1;
    end
    
end

if all(area_flag)
    linc;
    return
end

set(gca,'Zscale','lin');

