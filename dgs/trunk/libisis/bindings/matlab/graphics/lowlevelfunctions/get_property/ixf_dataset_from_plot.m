
function dataset = ixf_dataset_from_plot(inHandle_)
%-- help for libisis graphics dataset from plot function--
%
% >> dataset = ixf_dataset_from_plot(Handle_)
%
% inputs:
%       Handle_     The handle of the plot to get the datasets for. This
%                   can be a figure, plot or axes handle - the plots
%                   contained in that figure or axes will be output
%
% outputs:
%       dataset     dataset (or array of datasets) containing the data for
%                   from the graph - including title and axes information.
%                   However, axes label codes are set to null and no error
%                   information is output. 
%
% note that the dataset should only be used for re-plotting, as no error or
% detailed information is output. Therefore, any data manipulation will not
% be accurate.

[IXG_ST_ERROR, IXG_ST_STDVALUES] = ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

[figureHandle_, axesHandle_, plotHandle_, otherHandle_] = ixf_get_related_handles(inHandle_);

if ~ ixf_check_graphicfigure('handle',figureHandle_)
    ixf_display_error(IXG_ST_ERROR.no_figure);
end

userdata.plot = cell(size(plotHandle_));
plot_types = cell(size(plotHandle_));

for i = 1:length(plotHandle_)
    [userdata.plot{i} plot_types{i}] = ixf_plotdata('get',plotHandle_(i),'xyzdata','plot_type');
end
titleHandle_ = get(axesHandle_,'title');
xlabelHandle_ = get(axesHandle_,'xlabel');
ylabelHandle_ = get(axesHandle_,'ylabel');

title = get(titleHandle_,'string');
xlabel = get(xlabelHandle_,'string');
ylabel = get(ylabelHandle_,'string');

if ~ all(plot_types == IXG_ST_STDVALUES.oned_type)
    
    zlabelHandle_ = get(axesHandle_,'zlabel');
    zlabel = get(zlabelHandle_,'string');
    dataset = IXTdataset_2d;
    
else
    dataset = IXTdataset_1d;
    
end
% array of datasets.

for i = 1:length(userdata.plot)

    if plot_types(i) == IXG_ST_STDVALUES.oned_type
        
        dataset(i) = IXTdataset_1d(IXTbase, title, userdata.plot{i}.y, zeros(size(userdata.plot{i}.y)), IXTaxis(ylabel), userdata.plot{i}.x, IXTaxis(xlabel), 0);

    else
        z = userdata.plot{i}.z;
        y = userdata.plot{i}.y(1,:);
        x = userdata.plot{i}.x(:,1);
        dataset(i) = IXTdataset_2d(IXTbase, title,z, zeros(size(z)),  IXTaxis(zlabel), ...
            x', IXTaxis(xlabel), 0, ...
            y, IXTaxis(ylabel), 0);

    end

end