function [figureHandle_ axesHandle_ plotHandle_ ] = ixf_patchplot(vertices,faces,signal,varargin)
%--------------------------------------------------------------------------
%Function Syntax: plot_handle = ixf_multiplot(xdata,ydata,zdata,{optional arguments})
%Output: figure handle
%Input: x, y, z data
%Purpose: multiplot or 3d plot data
%Example: 
%ixf_multiplot(x(1:1382),y(1:6),z(1:1382)) 
%the above example multiplot spectrum(w) data
% updated: 04/08/2006
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');

%null values for these properties
title = '';
xlabel = '';
ylabel = '';
zlabel = '';
xlim = [];
ylim = [];
zlim=[];
clim=[];
xscale = '';
yscale = '';
zscale='';
shading_val='';
redraw_flag = false;

%tot arguments
totArg = numel(varargin);
%set default values


%---------------set up global defaults structure for type of graph---------

st_local_default = ixf_get_standard_default(gcf, IXG_ST_DEFAULT, varargin{:});

%--------------------------------Get settings------------------------------

st_local_default = ixf_set_prop(st_local_default, totArg, varargin{:});

%---------------Get / set count data from the graph-----------------------

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

if  iscell(st_local_default.plot.color)
    st_local_default.plot.color = ixf_get_arrayvalue(st_local_default.plot.color, count.color);
    st_local_default.plot.color = char(st_local_default.plot.color);
end

if  iscell(st_local_default.plot.marker)
    st_local_default.plot.marker = ixf_get_arrayvalue (st_local_default.plot.marker, count.marker);
    st_local_default.plot.marker = char(st_local_default.plot.marker);
end

if  iscell(st_local_default.plot.linestyle)
    st_local_default.plot.linestyle = ixf_get_arrayvalue (st_local_default.plot.linestyle, count.linestyle);
    st_local_default.plot.linestyle = char(st_local_default.plot.linestyle);
end

if  iscell(st_local_default.plot.linewidth)
    st_local_default.plot.linewidth = ixf_get_arrayvalue (st_local_default.plot.linewidth, count.linewidth);
end

if  iscell(st_local_default.plot.markersize)
    st_local_default.plot.markersize = ixf_get_arrayvalue (st_local_default.plot.markersize, count.markersize);
end

if iscell(st_local_default.plot.facealpha)
    st_local_default.plot.facealpha = ixf_get_arrayvalue (st_local_default.plot.facealpha, count.facealpha);
end

if iscell(st_local_default.plot.edgealpha)
    st_local_default.plot.edgealpha = ixf_get_arrayvalue (st_local_default.plot.edgealpha, count.edgealpha);
end
%--------------------call appropriate twod or multiplot function-----------

   
switch st_local_default.plot.format
    case 'area'
        
       newplot
       
       plotHandle_ = patch('Faces',faces,'Vertices',vertices,'FaceVertexCData',signal,...
      'FaceColor',st_local_default.plot.facecolor);
       axesHandle_ = get(plotHandle_,'parent');

       resize_plot(axesHandle_,IXG_ST_STDVALUES)

       % set user data (x, y, z, type)
       xyzdata.vertices= vertices;     xyzdata.faces = faces;     xyzdata.z = signal;
       
       plot_type=IXG_ST_STDVALUES.patch_type;
        set(plotHandle_, 'facealpha',st_local_default.plot.facealpha, ...
    'edgealpha',st_local_default.plot.edgealpha);

  
   
    otherwise
        error('Invalid plot format.')
       
end

ixf_plotdata('set',plotHandle_,'plot_type',plot_type);
ixf_plotdata('set',plotHandle_,'xyzdata',xyzdata);

%-----------------------------Get Handles----------------------------------

axesHandle_ = get(plotHandle_(1),'parent');
if (isempty(axesHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_axes);
end

% get the figure handle
figureHandle_ = get(axesHandle_,'parent');
if (isempty(figureHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_figure);
end

%------settings that should be done automatically if not given-------------

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
if (~isempty(zlabel))
    zhdl = get(axesHandle_,'zlabel');
    set(zhdl,'string',cellstr(zlabel));   
elseif (~isempty(st_local_default.axes.zlabel))
    zhdl = get(axesHandle_,'zlabel');
    set(zhdl,'string',cellstr(st_local_default.axes.zlabel));       
end
% if desired
% generally the limits will be used after plotting only
% but for advance usage this is given
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
if (~isempty(zlim))
    set(axesHandle_,'zlim',zlim);    
    redraw_flag = true;
elseif (~isempty(st_local_default.axes.zlim))
    set(axesHandle_,'zlim',st_local_default.axes.zlim);
    redraw_flag = true;
end
if (~isempty(clim))
    set(axesHandle_,'clim',clim);    
    redraw_flag = true;
elseif (~isempty(st_local_default.axes.clim))
    set(axesHandle_,'clim',st_local_default.axes.clim);
    redraw_flag = true;
end

if (~isempty(shading_val))
    shading(shading_val);
elseif (~isempty(st_local_default.plot.shading))
    shading(st_local_default.plot.shading);
end

%--------------------set figure properties---------------------------------

if (isempty(figureHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_figure);
end

%----------------------set all properties----------------------------------

% set plot values
for i=1:length(plotHandle_)
set(plotHandle_(i),'tag',st_local_default.plot.ptag,'linestyle',st_local_default.plot.linestyle,...
    'linewidth',st_local_default.plot.linewidth,'marker',st_local_default.plot.marker,'markersize',st_local_default.plot.markersize);
end
% set axis values

set(axesHandle_,'fontsize',st_local_default.axes.afontsize,'color',st_local_default.axes.acolor,...
    'xgrid',st_local_default.axes.xgrid,'ygrid',st_local_default.axes.ygrid,...
    'zgrid',st_local_default.axes.zgrid,'fontname',st_local_default.axes.afontname,...
    'fontunits',st_local_default.axes.afontunits,'xscale',st_local_default.axes.xscale,...
    'yscale',st_local_default.axes.yscale,'zscale',st_local_default.axes.zscale,...
    'projection',st_local_default.axes.projection,'drawmode',st_local_default.axes.drawmode,...
    'colororder',st_local_default.axes.colororder,'fontangle',st_local_default.axes.afontangle,...
    'fontweight',st_local_default.axes.afontweight,'yaxislocation',st_local_default.axes.yaxislocation,...
    'xaxislocation',st_local_default.axes.xaxislocation,'tag',st_local_default.axes.atag, ...
    'xcolor',st_local_default.axes.xcolor,'ycolor',st_local_default.axes.ycolor,'zcolor',st_local_default.axes.zcolor);
% set figure values
set(figureHandle_,'color',st_local_default.figure.fcolor,...
    'paperorientation',st_local_default.figure.paper.paperorientation,'papertype',st_local_default.figure.paper.papertype,...
    'paperpositionmode',st_local_default.figure.paper.paperpositionmode,'CloseRequestFcn',st_local_default.figure.CloseRequestFcn,'tag',st_local_default.figure.tag, ...
    'name',st_local_default.figure.name);
colormap(st_local_default.plot.colormap);
% set x label font settings
if exist('xhdl','var')
set(xhdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
    'tag',st_local_default.text.ttag);
end
% y label    
if exist('yhdl','var')
set(yhdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
    'tag',st_local_default.text.ttag);
end
% title
if exist('zhdl','var')
set(zhdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
    'tag',st_local_default.text.ttag);
end
if exist('thdl','var')
set(thdl,'color',st_local_default.text.tcolor,...
    'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
    'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
    'tag',st_local_default.text.ttag);
end

if ~isempty(st_local_default.axes.dataaspectratio)
    daspect('manual')
    daspect(st_local_default.axes.dataaspectratio)
else
    daspect('auto')
end

if ~isempty(st_local_default.axes.aspect)
    aspect(st_local_default.axes.aspect)
end

if strcmp(st_local_default.axes.color_slider,'on')
    
    ixf_color_slider('figure',figureHandle_,'option','update');

else

    ixf_color_slider('figure',figureHandle_,'option','delete');
    
end

if redraw_flag
    ixf_redraw_graph(axesHandle_);
end

%--------------- resize plot function
% call syntax: resize_plot(axesHandle_)
%
% resizes plot to fit the data.

function resize_plot(axesHandle_, IXG_ST_STDVALUES)

    plots = get(axesHandle_,'children');       
    xmax = [];    xmin = [];    ymax = [];    ymin = [];
    
    for i = 1:length(plots)
        
        if any(strcmp(get(plots(i),'type'),IXG_ST_STDVALUES.plot_types))
            
        xmax = [xmax, max(get(plots(i),'xdata'))];      xmin = [xmin, min(get(plots(i),'xdata'))]; 
        ymax = [ymax, max(get(plots(i),'ydata'))];      ymin = [ymin, min(get(plots(i),'ydata'))]; 
       
        end
        
    end
    
    xmax = max(xmax);   xmin = min(xmin);   ymax = max(ymax);    ymin = min(ymin);
    
        % catch case where there are no limits.
    if xmin == xmax
        xmax = xmin + 1;
        xmin = xmin - 1;
    end
    
    if ymin == ymax
        ymax = ymin + 1;
        ymin = ymin - 1;
    end
    
    set(axesHandle_,'xlimmode','auto', ...
           'ylimmode','auto','zlimmode','auto','xlim',[xmin,xmax],'ylim',[ymin,ymax]);

% to do: Extensive testing



