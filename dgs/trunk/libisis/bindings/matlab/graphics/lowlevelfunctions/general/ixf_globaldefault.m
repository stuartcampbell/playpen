function ixf_globaldefault()
% Help for gtk ixf_globaldefault
% 
% syntax: ixf_globaldefault()
%
% inputs: none
% outputs: none
%
% This m file is used to set the global default properties. To alter the
% properties, open the mfile and change them directly. See the technical
% documentation for more information about global defaults. These are
% stored into the ixf_global_var.m file under 'libisis_graphics'.
%
% This should only be run when gtk is started

%global structures
%purpose: To maintain a generic structure. This structure then can be 
%used by any function.

%for error
IXG_ST_ERROR = struct('wrong_arg',1,'wrong_value',2,'wrong_function',3,'wrong_interface',4,'no_figure',5,'invalid_number',6,...
    'invalid_character',7,'greater_value',8,'not_exists',9,'cannot_plot',10,'no_figfound_at',11,'no_figfound_h',12,...
    'cannot_set',13,'cannot_get',14,'cannot_axes',15,'cannot_figure',16,'no_figfound_ht',17,'no_object',18,'wrong_field',19,...
    'no_toolbar',20,'already_exist',21,'no_format',22,'single_objreq',23,'no_figfound_hta',24,'plot_legend_wrong',25,...
    'not_valid_color',26,'not_valid_linestyle',27,'not_valid_markerstyle',28,'no_size',29);
ixf_global_var('libisis_graphics','set','IXG_ST_ERROR',IXG_ST_ERROR);
%for warning

IXG_ST_WARNING = struct('no_figure',1);
ixf_global_var('libisis_graphics','set','IXG_ST_WARNING',IXG_ST_WARNING);

%for message

IXG_ST_MESSAGE = struct('success',1,'set',2);
ixf_global_var('libisis_graphics','set','IXG_ST_MESSAGE',IXG_ST_MESSAGE);

%for interface style

IXG_ST_ISTYLE = struct('default',0,'namevalue',1);
ixf_global_var('libisis_graphics','set','IXG_ST_ISTYLE',IXG_ST_ISTYLE);

%for x button position and y

IXG_ST_X_BUTTONPOSITION = struct('length',10,'breadth',30,'width',40,'height',20);
ixf_global_var('libisis_graphics','set','IXG_ST_X_BUTTONPOSITION',IXG_ST_X_BUTTONPOSITION);

IXG_ST_Y_BUTTONPOSITION = struct('length',10,'breadth',55,'width',40,'height',20);
ixf_global_var('libisis_graphics','set','IXG_ST_Y_BUTTONPOSITION',IXG_ST_Y_BUTTONPOSITION);
%for standard values

IXG_ST_STDVALUES = struct('false',0,'true',1,'x',0,'y',1,'z',2,'c',3,'lin',1,'log',0,'button',0,'menu',1,'root',0,'start',1,'incr',2,...
    'row',1,'col',2,'xstart',1,'xend',1382,'ystart',1,'xye',1,'xyze',2,'menu_position',6,'gui',1,'cmd',0,'identifier','graphic',...
    'old',0,'new',1,'appname','General','surface_name','Default Surface Plot','area_name','Default Area Plot','stem_name','Default Stem Plot','oned_name','Default One Dimensional Plot',...
    'multiplot_name','default multiplot','points_name','default 2d marker plot','contour_name','default contour plot', 'sliceomatic_name','Sliceomatic','oned_type',1,'area_type',2,'surface_type',3,'points_type',4,'waterfall_type',5,'stem_type',6,...
    'contour_type',7,'twod_line_type',8,'patch_type',9,'surface_patch_type',10, 'sliceomatic_type', 11, 'plot_types',{{'surface','line','patch','hggroup'}},'figure_object_type',1,'axes_object_type',2,'plot_object_type',3,'oned','','twod','','multioned','','currentfigure','currentfigure','hold','(HOLD)','curr','(CURRENT)',...
    'keep_label','Keep Figure','curr_label','Make Current Figure','keep_tag','keep','curr_tag','curr','dataset1d','ixtdataset_1d',...
    'binning',1,'rectxy','rectxy.JPG','crosshair','crosshair.JPG','y_value_limit',3000,'x_value_limit',3000, 'counter_increment', 1, 'counter_reset', 2, 'counter_keep', 3);
ixf_global_var('libisis_graphics','set','IXG_ST_STDVALUES',IXG_ST_STDVALUES);

IXG_ST_DATASET_DEFAULT = struct('name','dataset from file data','title','',...
    'xunits','unknown','yunits','unknown','zunits','unknown','xunitcode','unk','yunitcode','unk',...
    'zunitcode','unk','basename','unknown');
ixf_global_var('libisis_graphics','set','IXG_ST_DATASET_DEFAULT',IXG_ST_DATASET_DEFAULT);
%interface validation purpose

IXG_ST_INTERFACEVALIDATION = struct(...
     'ui_gethandle',2,'uinv_gethandle',4,...
    'uinv_setcurrfig',2,...
    'ui_chngaxis',3,'uinv_chngaxis',6,...
    'ui_setappname',3,'uinv_setappname',6,...
    'uinv_chngdefaultprop',2,...
    'ixf_null_value',0,...
    'uib_plotfigure',1,...
    'ixf_gen_interface_nv',4,...
    'ui_checkfigure',3,'uinv_checkfigure',6,...
    'ui_keepcmd',1, ...    
    'ixf_setcurrfig_nv',[2 4 6],...
    'ui_setlegend',1,...
    'ui_setbinning',1);
ixf_global_var('libisis_graphics','set','IXG_ST_INTERFACEVALIDATION',IXG_ST_INTERFACEVALIDATION);
%user functions validation purpose
IXG_ST_USERVALIDATION = struct(...
    'ui_plothistogram',1,...
    'uib_plotfigure',2);
ixf_global_var('libisis_graphics','set','IXG_ST_USERVALIDATION',IXG_ST_USERVALIDATION);

%paper

IXG_ST_DEFAULT_PAPER = struct('papersize',[20.984 29.6774],'paperposition',[0.6345    6.3452   20.3046   15.2284],'paperunits','centimeters','paperorientation','portrait','papertype','A4','paperpositionmode','auto');
ixf_global_var('libisis_graphics','set','IXG_ST_DEFAULT_PAPER',IXG_ST_DEFAULT_PAPER);
%figure

%figure color is color of whole window (RGB)
IXG_ST_DEFAULT_FIGURE = struct('fcolor',[1 1 1],'funits','pixels','fposition',[3 276 560 420],'name','IXGDEFAULT','tag','IXGDEFAULT','menubar','figure','resize','on',...
    'paper',IXG_ST_DEFAULT_PAPER,'CloseRequestFcn','ixf_figureclose','counter',2,'isonormals',false);
ixf_global_var('libisis_graphics','set','IXG_ST_DEFAULT_FIGURE',IXG_ST_DEFAULT_FIGURE);
%axes

%while setting values for xlabel,ylabel and title you need handle
%hence u need to code it
IXG_ST_DEFAULT_AXES = struct('acolor',[1 1 1],'xgrid','off','ygrid','off','zgrid','off','aunits','normalized','aposition',[0.13 0.11 0.775 0.815],'title','','xlabel','','ylabel','','zlabel','',...
    'afontname','helvetica','afontsize',[10],'afontangle','normal','afontweight','normal','afontunits','points',...
    'atag','','xlim',[],'ylim',[],'zlim',[],'clim',[],'xscale','linear','yscale','linear','zscale','linear','gridlinestyle',':',...
    'xcolor','black','ycolor','black','zcolor','black','yaxislocation','left','xaxislocation','bottom','projection','orthographic',...
    'colororder', [0 0 1.0000; 0 0.5000 0; 1.0000 0 0; 0 0.7500 0.7500;0.7500 0 0.7500; 0.7500 0.7500 0; 0.2500 0.2500 0.2500],'drawmode','normal', ...
    'dataaspectratio', [], 'aspect', [], 'color_slider','off');
ixf_global_var('libisis_graphics','set','IXG_ST_DEFAULT_AXES',IXG_ST_DEFAULT_AXES);
% text

IXG_ST_DEFAULT_TEXT = struct('tcolor',[0 0 0],'tunits','data','tposition',[],'string','',...
    'tfontname','helvetica','tfontsize',[10],'tfontangle','normal','tfontweight','normal','tfontunits','points',...
    'horizontalalignment','left','verticalalignment','middle','ttag','');
ixf_global_var('libisis_graphics','set','IXG_ST_DEFAULT_TEXT',IXG_ST_DEFAULT_TEXT);
%plot

%do remember this plot is only for one datavalue
%as if more than two will be there the plot function uses it color order to
%make difference
IXG_ST_DEFAULT_PLOT = struct('color',[0 0 1],'punits','','linestyle','-',...
    'ptag','','marker','o','markersize',[6],'linewidth',[0.5],'format',...
    'waterfall','shading','flat','facecolor','flat','levels',[],'colormap','jet','facealpha',1, ...
    'edgealpha',1);
%combine all (plot,axes,figure,text) in one default structure

IXG_ST_DEFAULT = struct('figure',IXG_ST_DEFAULT_FIGURE,...
    'axes',IXG_ST_DEFAULT_AXES,...
    'plot',IXG_ST_DEFAULT_PLOT,...
    'text',IXG_ST_DEFAULT_TEXT); 
ixf_name_tag_properties('set','IXGDEFAULT','IXGDEFAULT', IXG_ST_DEFAULT);

%for rectangular coordinates

IXG_ST_RECTXY = struct('xs',0,'ys',0,'xr',0,'yr',0,'xh',0,'yh',0,'xl',0,'yl',0);
ixf_global_var('libisis_graphics','set','IXG_ST_RECTXY',IXG_ST_RECTXY);
%for handles of all objects (in hierarchy)

IXG_ST_HDL = struct('fh',[],'name',[],'ah',[],'ph',[],'app',[]);
ixf_global_var('libisis_graphics','set','IXG_ST_HDL',IXG_ST_HDL);
%for valid values of line,marker and color




IXG_LINE_VALIDVALUES = {'-' '--' ':' '-.' 'none'
                           's' 'da' 'do' 'dd' 'n'};
ixf_global_var('libisis_graphics','set','IXG_LINE_VALIDVALUES',IXG_LINE_VALIDVALUES);                 

IXG_MARKER_VALIDVALUES = {'o' '+' '*' '.' 'square' 'x' 'diamond' '^' 'v' '>' '<' 'pentagram' 'hexagram' 'none'
                             'c' 'pl' 's' 'do' 's' 'x' 'di' 'e' 'v' 'g' 'l' 'pe' 'h' 'n'};

ixf_global_var('libisis_graphics','set','IXG_MARKER_VALIDVALUES',IXG_MARKER_VALIDVALUES);

IXG_COLOR_VALIDVALUES = {'red' 'yellow' 'magenta' 'cyan' 'red' 'green' 'blue' 'white' 'black'};
ixf_global_var('libisis_graphics','set','IXG_COLOR_VALIDVALUES',IXG_COLOR_VALIDVALUES);

IXG_GRID_VALIDVALUES = {'-','--',':','-.'};
ixf_global_var('libisis_graphics','set','IXG_GRID_VALIDVALUES',IXG_GRID_VALIDVALUES);

IXG_ALL_VALIDVALUES = {IXG_LINE_VALIDVALUES{:}, IXG_MARKER_VALIDVALUES{:}, IXG_COLOR_VALIDVALUES{:}, IXG_GRID_VALIDVALUES{:}};
ixf_global_var('libisis_graphics','set','IXG_ALL_VALIDVALUES',IXG_ALL_VALIDVALUES);

IXG_ST_PRINTERS = struct('Phaser560',' \\NDAIRETON\Phaser560:','Phaser0',' \\NDAIRETON\Phaser0:', ... 
    'DACColour',' \\NDAIRETON\DACColour:','CRISPColour',' \\NDAIRETON\CRISPColour:', ...
    'MAPSColour',' \\NDAIRETON\MAPSColour:','default',' \\ndablagrave\LASER00:');
ixf_global_var('libisis_graphics','set','IXG_ST_PRINTERS',IXG_ST_PRINTERS);

%=== TO ADD ANOTHER POSTSCRIPT PRINTER IN THIS LIST INSERT ANOTHER LINE AS ABOVE AND 
%=== EDIT NAME (LABEL) 'Phaser0' AND NETWORK PATH '' \\NDAIRETON\Phaser0:''
