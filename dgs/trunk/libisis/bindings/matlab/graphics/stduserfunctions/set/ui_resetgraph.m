function ui_resetgraph(axisHandle_)
%----------------Help for GTK rg function----------------------------------
%
% call syntax:      rg(axisHandle_)   or simply,       rg
%
% purpose: reset current graph to original x,y,z values
%
% inputs: axis handle, or nothing - if nothing, will take the current axis
% handle
%
%-----------------Updated 1/9/2006, Dean Whittaker-------------------------
xlim = [];
ylim = [];
zlim = [];

IXG_ST_STDVALUES = ixf_global_var('libisis_graphics','get','IXG_ST_STDVALUES');

plotHandle_=get(axisHandle_,'children');

for i=1:length(plotHandle_)
    [userdata, userdata.type] = ixf_plotdata('get',plotHandle_(i),'xyzdata','plot_type');
    
    % get min and max values of the data
    xlim_temp = [min(userdata.x) max(userdata.x)];
    xlim = [min([xlim_temp xlim]) max([xlim_temp xlim])]; 
    ylim_temp = [min(userdata.y) max(userdata.y)];
    ylim = [min([ylim_temp ylim]) max([ylim_temp ylim])]; 
    if  userdata.type ~= IXG_ST_STDVALUES.oned_type
        zlim_temp = [min(userdata.z) max(userdata.z)];
        zlim = [min([zlim_temp zlim]) max([zlim_temp zlim])]; 
    end
    
end

set(axisHandle_,'xlim',xlim,'ylim',ylim);

if  userdata.type ~= IXG_ST_STDVALUES.oned_type
    set(axisHandle_,'zlim',zlim);
end

ixf_redraw_graph(axisHandle_);
    
%