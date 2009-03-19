
function [figureHandle_,axesHandle_,plotHandle_] = ixf_plotfigure(xdata,ydata,varargin)
%--------------------------------------------------------------------------
%Function Syntax: 
%[figureHandle_,axesHandle_,plotHandle_] = 
%ixf_plotfigure(xdata,ydata,[control_namevalue_pairs])
%Output: figure,axes and plot handle
%Input: xdata, ydata and other control parameters
%list of control propertie names
%IXG_ST_DEFAULT.figure, IXG_ST_DEFAULT.plot, IXG_ST_DEFAULT.axes
%Purpose: plot the data according to values and control properties (for
%figure, axes and plot)
%Example: 
%ixf_plotfigure(x,y) --> default structure plot
%ixf_plotfigure(x,y,'Color','r') --> override default structure values 
%ixf_plotfigure(x,y,'default','my_struct','Color','r') --> override values 
%ixf_plotfigure(x,y,'default','my_struct') --> from structure
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');

IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');

%null values for this properties
title = '';
xlabel = '';
ylabel = '';
xlim = [];
ylim = [];
redraw_flag = false;

%tot arguments
totArg = numel(varargin);

%---------------------set values into userdata structure-------------------

xyzdata.x = xdata;     xyzdata.y = ydata;
clear xdata;            clear ydata;

%---------------set up global defaults structure for type of graph---------

st_local_default = ixf_get_standard_default(gcf, IXG_ST_DEFAULT, varargin{:});

%---------------find and set optional arguments----------------------------

st_local_default = ixf_set_prop(st_local_default, totArg, varargin{:});

%---------------Get / set count data from the graph for individual parts------------------------

% markersize / linewidth _flag = true if the marker / line has a size, false if it does not 

markersize_flag = true;         linewidth_flag = true;

% cover case where could be cell of numbers by using flags - checking that
% data is to be plotted
if iscell(st_local_default.plot.markersize)
    if all(cell2mat(st_local_default.plot.markersize) == 0)
        markersize_flag = false;
    end
elseif all(st_local_default.plot.markersize == 0)
    markersize_flag = false;
end

if iscell(st_local_default.plot.linewidth)
    if all(cell2mat(st_local_default.plot.linewidth) == 0)
        linewidth_flag = false;
    end
elseif all(st_local_default.plot.linewidth == 0)
    linewidth_flag = false;
end

% if the marker / color / line exists, then do a count, if not then do not
% do a count at all.

if  ~all(strcmp(st_local_default.plot.color,'none'))
    count.color = ixf_get_countdata(gcf, 'color', st_local_default.figure.counter);
else
    count.color = 0;
end

if  ~all(strcmp(st_local_default.plot.marker,'none')) && markersize_flag
    count.marker = ixf_get_countdata(gcf, 'marker', st_local_default.figure.counter);
    count.markersize = ixf_get_countdata(gcf, 'markersize', st_local_default.figure.counter);
else
    count.marker = 0;       count.markersize = 0;
end

if  ~all(strcmp(st_local_default.plot.linestyle,'none')) && linewidth_flag
    count.linestyle = ixf_get_countdata(gcf, 'linestyle', st_local_default.figure.counter);
    count.linewidth = ixf_get_countdata(gcf, 'linewidth', st_local_default.figure.counter);
else
    count.linestyle = 0;       count.linewidth = 0;
end

%---------------check for several color or marker values and pick the correct one----

% if it's a cell, then it must be an array, 

if  iscell(st_local_default.plot.color)
    st_local_default.plot.color = ixf_get_arrayvalue(st_local_default.plot.color,count.color);
    st_local_default.plot.color = char(st_local_default.plot.color);
end

if  iscell(st_local_default.plot.marker)
    st_local_default.plot.marker = ixf_get_arrayvalue(st_local_default.plot.marker,count.marker);
    st_local_default.plot.marker = char(st_local_default.plot.marker);
end

if  iscell(st_local_default.plot.linestyle)
    st_local_default.plot.linestyle = ixf_get_arrayvalue(st_local_default.plot.linestyle,count.linestyle);
    st_local_default.plot.linestyle = char(st_local_default.plot.linestyle);
end

if  iscell(st_local_default.plot.linewidth)
    st_local_default.plot.linewidth = ixf_get_arrayvalue(st_local_default.plot.linewidth,count.linewidth);
end

if  iscell(st_local_default.plot.markersize)
    st_local_default.plot.markersize = ixf_get_arrayvalue(st_local_default.plot.markersize,count.markersize);
end

%------------------set the type, plot the data-----------------------------

  
    plotHandle_ = plot(xyzdata.x,xyzdata.y,'color',st_local_default.plot.color,'linestyle',st_local_default.plot.linestyle,'tag',st_local_default.plot.ptag,...
        'marker',st_local_default.plot.marker,'markersize',st_local_default.plot.markersize,'linewidth',st_local_default.plot.linewidth);
    
if (isempty(plotHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_plot);
end

% set plot data into the plot
ixf_plotdata('set',plotHandle_,'plot_type',IXG_ST_STDVALUES.oned_type);
ixf_plotdata('set',plotHandle_,'xyzdata',xyzdata)

%%set axes properties
axesHandle_ = get(plotHandle_,'parent');
figureHandle_ = get(axesHandle_,'parent');
ixf_plotdata('set',axesHandle_,'object_type',IXG_ST_STDVALUES.axes_object_type)
ixf_plotdata('set',figureHandle_,'object_type',IXG_ST_STDVALUES.figure_object_type)
ixf_plotdata('set',plotHandle_,'object_type',IXG_ST_STDVALUES.plot_object_type)

if (isempty(axesHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_axes);
end

% set the axes properties 
set(axesHandle_,'color',st_local_default.axes.acolor,'xgrid',st_local_default.axes.xgrid,'ygrid',st_local_default.axes.ygrid,...
    'units',st_local_default.axes.aunits,...
    'fontname',st_local_default.axes.afontname,'fontsize',st_local_default.axes.afontsize,'fontangle',st_local_default.axes.afontangle,'fontweight',st_local_default.axes.afontweight,'fontunits',st_local_default.axes.afontunits,...
    'tag',st_local_default.axes.atag,'xscale',st_local_default.axes.xscale,'yscale',st_local_default.axes.yscale,'gridlinestyle',st_local_default.axes.gridlinestyle,...
    'xcolor',st_local_default.axes.xcolor,'ycolor',st_local_default.axes.ycolor, 'position',st_local_default.axes.aposition);

%other parameters settings
if (~isempty(title))
    thdl = get(axesHandle_,'title');
    set(thdl,'string',cellstr(title));  

elseif (~isempty(st_local_default.axes.title))
    thdl = get(axesHandle_,'title');
    set(thdl,'string',cellstr(st_local_default.axes.title));  

end

if (~isempty(xlabel))
    xhdl = get(axesHandle_,'xlabel');
    set(xhdl,'string',cellstr(xlabel));   
elseif (~isempty(st_local_default.axes.xlabel))
    xhdl = get(axesHandle_,'xlabel');
    set(xhdl,'string',cellstr(st_local_default.axes.xlabel));       
end

if (~isempty(ylabel))
    yhdl = get(axesHandle_,'ylabel');
    set(yhdl,'string',cellstr(ylabel));   
elseif (~isempty(st_local_default.axes.ylabel))
    yhdl = get(axesHandle_,'ylabel');
    set(yhdl,'string',cellstr(st_local_default.axes.ylabel));       
end
%if desired
%generally the limits will be used after plotting only
%but for advance usage this is given
if (~isempty(xlim))
    set(axesHandle_,'xlim',xlim);
    redraw_flag = true;
elseif (~isempty(st_local_default.axes.xlim))
    set(axesHandle_,'xlim',st_local_default.axes.xlim);
    redraw_flag = true;
end

if (~isempty(ylim))
    set(axesHandle_,'ylim',ylim);    
    redraw_flag = true;
elseif (~isempty(st_local_default.axes.ylim))
    set(axesHandle_,'ylim',st_local_default.axes.ylim);
    redraw_flag = true;
end


%%set figure properties
if (isempty(figureHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_figure);
end

set(figureHandle_,'color',st_local_default.figure.fcolor,'units',st_local_default.figure.funits,...
    'name',[st_local_default.figure.name],'tag',st_local_default.figure.tag,'menubar',st_local_default.figure.menubar,'resize',st_local_default.figure.resize,...
    'papersize',st_local_default.figure.paper.papersize,'paperposition',st_local_default.figure.paper.paperposition,...
    'paperunits',st_local_default.figure.paper.paperunits,'paperpositionmode',st_local_default.figure.paper.paperpositionmode,...
    'paperorientation',st_local_default.figure.paper.paperorientation,'papertype',st_local_default.figure.paper.papertype,'CloseRequestFcn',st_local_default.figure.CloseRequestFcn)

%set label properties

set(xhdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize);

    
set(yhdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize);
    

set(thdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize);

if ~isempty(st_local_default.axes.dataaspectratio)
    daspect('manual')
    daspect(st_local_default.axes.dataaspectratio)
else
    daspect('auto')
end

if ~isempty(st_local_default.axes.aspect)
    aspect(st_local_default.axes.dataaspectratio)
end

if strcmp(st_local_default.axes.color_slider,'on')
    
    ixf_color_slider('figure',figureHandle_,'option','update');

else

    ixf_color_slider('figure',figureHandle_,'option','delete');
    
end

if redraw_flag
    ixf_redraw_graph(axesHandle_);
end
