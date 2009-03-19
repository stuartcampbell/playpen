function dataset = ixf_get_dataset(inHandle_, varargin)
%-- libisis graphics get dataset function
%
% purpose: creates an IXTdataset_2d with as much information as possible 
% from the plot contained in it. This can be used to recreate the plot, 
% however does not contain information such as units codes etc.
%
% syntax: dataset = ixf_get_dataset(inHandle_, property, value, ...)
%
% inputs: 
%       inHandle_:      Any figure, axes or plot handle. The related plots
%                       will be used
%       property:       Property such as "x_units", "x_distribution etc. to
%                       over-ride the automatically filled fields
%       value:          Value corresponding to the property, such as
%                       IXTunits(IXTbase('unknown', false, true),'null','mylabel') 
%                       or true.
%
% outputs:
%       dataset:    IXTdataset_2d with fields:
%                           x       - ALL x data used to make the plot (even
%                                     if not within the current limits)
%                           y       - ALL y data used to make the plot (even
%                                     if not within the current limits)
%                           signal  - ALL z data used to make the plot (even
%                                     if not within the current limits)
%                           error   - ALL error data will be 0 
%                           x_units - units will be xlabel, 
%                                     code will be NULL 
%                           y_units - units will be ylabel,
%                                     code will be NULL
%                           s_units - units will be zlabel,
%                                     code will be NULL
%                           title -   will 
%                           x_distribution - FALSE
%                           y_distribution - FALSE
%
%               on a 1 dimensional plot, signal data and units will
%               correspond to the y axis data and units, ydata will be "1" and 
%               units will be NULL.
%
%               on an errorbar plot, the errorbars themselves are stored as
%               x and y data.
%                           
% example:
%       >> dataset_2d = ixf_get_dataset(gcf, 'y_units',
%       IXTunits(IXTbase('unknown', false, true),'spno','Spectrum Number'), 
%       'x_distribution',true)
%
%   will create a dataset from the data in the current figure, uses spno as
%   the units code for the y units and the units to 'spectrum number' 
%   and sets x distribution to true

if ~exist('inHandle_','var')
    inHandle_ = gcf;
end

[figureHandle_, axesHandle_, plotHandle_] = ixf_get_related_handles(inHandle_);

dataset = IXTdataset_2d;

for i = 1:length(plotHandle_)
    % get all the data into structures, so we can check fields
    plotdata = get(plotHandle_(i));
    userdata = ixf_plotdata('get',plotHandle_(i),'xyzdata');
    axesdata = get(axesHandle_);
    
    % get the data - try from userdata first, then plot.
    if isfield(userdata,'x')
        if size(userdata.x,2) > 1 && size(userdata.x,1) == 1
            xdata = userdata.x';
        else
            xdata = userdata.x(:,1);
        end
    else
        if size(plotdata.XData,2) > 1 && size(plotdata.XData,1) == 1
            xdata = plotdata.XData(:);
        else
            xdata = plotdata.XData(1,:)';
        end
    end

    if isfield(userdata,'y')
        ydata = userdata.y(1,:);
    else
        if size(plotdata.YData,2) > 1 && size(plotdata.YData,1) == 1
          ydata = plotdata.YData(:)';
        else
            ydata = plotdata.YData(:,1)';
        end
    end

    if isfield(userdata,'z')
        zdata = userdata.z;
    elseif ~ isempty(plotdata.ZData)
        if ~ any(plotdata.ZData(:)) && isfield(plotdata,'CData')
            zdata = plotdata.CData';
        else
            zdata = plotdata.ZData';
        end
    else
        zdata = ydata';
        ydata = 1;
    end


% get title and label information    
    titleHandle_ = axesdata.Title;
    xlabelHandle_ = axesdata.XLabel;
    ylabelHandle_ = axesdata.YLabel;
    % check for zdata
    if isfield(axesdata,'ZLabel')
        zlabelHandle_ = axesdata.ZLabel;
    end
    
    title = char(get(titleHandle_,'string'));
    xlabel = char(get(xlabelHandle_,'string'));
    ylabel = char(get(ylabelHandle_,'string'));

    if exist('zlabelHandle_','var')
        if  any(plotdata.ZData(:))
            zlabel = char(get(zlabelHandle_,'string'));
        else
            zlabel = ylabel;
            ylabel = 'null';
        end
    else
        zlabel = ylabel;
        ylabel = 'null';
    end
    
arglist = struct('x_distribution',false,'y_distribution',false,'title',title, ...
    'x_units', IXTaxis(xlabel), ...
    'y_units',IXTaxis(ylabel),...
    's_units', IXTaxis(zlabel),...
    'x', xdata', 'y', ydata, 'signal', zdata, 'error', zeros(size(zdata)));

[parse, args, present] = parse_arguments(varargin, arglist);
    
    dataset(i) = IXTdataset_2d(IXTbase, args.title,args.signal, args.error, args.s_axis, ...
                args.x,args.x_axis, args.x_distribution, ...
                args.y, args.y_axis, args.y_distribution);
    
end
    