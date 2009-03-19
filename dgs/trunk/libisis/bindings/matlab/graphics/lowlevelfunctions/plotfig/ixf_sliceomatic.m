function [figureHandle_ axesHandle_ plotHandle_ ] = ixf_sliceomatic(u1,u2,u3, signal, varargin)
%--------------------------------------------------------------------------
%Function Syntax: [fig, axes, plot_handle] = ixf_slicomatic(u1,u2,u3,signal,{optional arguments})
%Output: 
%
% figure handle, 
% axes handle, 
% plot handle
%
%Inputs: 
%       U1      Limits along x axis, [xlo, xhi]. The data are assumed
%               to be given on a uniformly spaced grid, with x values at
%               linspace(u1(1),u1(2),size(S,2))
%       U2      Limits along y axis (data at linspace(u1(1),u1(2),size(S,1))
%       U3      Limits along y axis (data at linspace(u1(1),u1(2),size(S,3))
%       S       Data values
%
%       opt     optional arguments can be given to control the axes labels,
%               slider labels, aspect ratio or titles.
%
%Purpose: slicomatic plot of data.
%
%Example: 
% >> ixf_sliceomatic(u1, u2, u3, signal, varargin) 
%
% the above example slicomatic plots the x,y,z data data
% updated: 04/08/2006
%--------------------------------------------------------------------------

%global structures
[IXG_ST_ERROR, IXG_ST_STDVALUES] =  ixf_global_var('libisis_graphics','get','IXG_ST_ERROR','IXG_ST_STDVALUES');
IXG_ST_DEFAULT = ixf_name_tag_properties('get','IXGDEFAULT','IXGDEFAULT');



%null values for these properties
title = '';

%tot arguments
totArg = numel(varargin);
%set default values


%---------------set up global defaults structure for type of graph---------

st_local_default = ixf_get_standard_default(gcf, IXG_ST_DEFAULT, varargin{:});

%--------------------------------Get settings------------------------------

st_local_default = ixf_set_prop(st_local_default, totArg, varargin{:});

%--------------------call appropriate twod or multiplot function-----------

   
switch st_local_default.plot.format
    case 'sliceomatic'
        
       sliceomatic(u1, u2, u3, signal, st_local_default.axes.x_sliderlabel, st_local_default.axes.y_sliderlabel, ...
           st_local_default.axes.z_sliderlabel, st_local_default.axes.xlabel, st_local_default.axes.ylabel, ...
           st_local_default.axes.zlabel, st_local_default.axes.clim,st_local_default.figure.isonormals);
       
       axesHandle_= gca;
       figureHandle_ = gcf;
       plotHandle_ = [];

       
%        set user data (x, y, z, type)
       
xyzdata.u1 = u1;    xyzdata.u2 = u2;     xyzdata.u3 = u3;

    ixf_plotdata('set',axesHandle_,'xyzdata',xyzdata);
    
ixf_plotdata('set',axesHandle_,'object_type',IXG_ST_STDVALUES.axes_object_type)
ixf_plotdata('set',figureHandle_,'object_type',IXG_ST_STDVALUES.figure_object_type)
    otherwise
        error('plot type not recognised in ixf_sliceomatic')

end




%-----------------------------Get Handles----------------------------------

if (isempty(axesHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_axes);
end

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




%--------------------set figure properties---------------------------------

if (isempty(figureHandle_))
    ixf_display_error(IXG_ST_ERROR.cannot_figure);
end

%----------------------set all properties----------------------------------

% set axis values
% 
% set(axesHandle_,'fontsize',st_local_default.axes.afontsize,'color',st_local_default.axes.acolor,...
%     'tag',st_local_default.axes.atag, ...
%     'xcolor',st_local_default.axes.xcolor,'ycolor',st_local_default.axes.ycolor,'zcolor',st_local_default.axes.zcolor, ...
%     'position', st_local_default.axes.aposition, 'units', st_local_default.axes.aunits);

% set figure values

set(figureHandle_,'color',st_local_default.figure.fcolor,...
    'paperorientation',st_local_default.figure.paper.paperorientation,'papertype',st_local_default.figure.paper.papertype,...
    'paperpositionmode',st_local_default.figure.paper.paperpositionmode,'CloseRequestFcn',st_local_default.figure.CloseRequestFcn,'tag','sliceomatic', ...
    'name',st_local_default.figure.name);
colormap(st_local_default.plot.colormap);

% set x label font settings

% if exist('xhdl','var')
% set(xhdl,'color',st_local_default.text.tcolor,...
%     'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
%     'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
%     'tag',st_local_default.text.ttag);
% end
% % y label    
% 
% if exist('yhdl','var')
% set(yhdl,'color',st_local_default.text.tcolor,...
%     'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
%     'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
%     'tag',st_local_default.text.ttag);
% end
% % title
% 
% if exist('zhdl','var')
% set(zhdl,'color',st_local_default.text.tcolor,...
%     'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
%     'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
%     'tag',st_local_default.text.ttag);
% end
% 
% if exist('thdl','var')
% set(thdl,'color',st_local_default.text.tcolor,...
%     'fontname',st_local_default.text.tfontname,'fontangle',st_local_default.text.tfontangle,...
%     'fontweight',st_local_default.text.tfontweight,'fontunits',st_local_default.text.tfontunits,'fontsize',st_local_default.text.tfontsize,...
%     'tag',st_local_default.text.ttag);
% end

if ~isempty(st_local_default.axes.dataaspectratio)
    daspect('manual')
    daspect(st_local_default.axes.dataaspectratio)
else
    daspect('auto')
end

if ~isempty(st_local_default.axes.aspect)
    aspect(st_local_default.axes.aspect)
end





