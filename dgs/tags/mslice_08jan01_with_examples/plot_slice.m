function plot_slice(slice_data,i_min,i_max,shad,map,linearlog)

% function plot_slice(slice_data,i_min,i_max,shad,map,linearlog)
% produces a colourmap plot of the (vx,vy,intensity) data 
% with a colour_axis in the range [i_min i_max], by default [min(intensity(:)) max(intensity(:))]
% and shading {flat}|{faceted}|{interp}
% map = array(m,3) optional RGB colour map, default = jet
% linearlog {'linear'}|{'log'} = colour scale type
% Radu Coldea 02-Oct-1999
% to add more postscript printers to the list under 'ColorPrint' menu go to line 42 

% === return if slice_d is empty
if isempty(slice_data),
   disp('Slice contains no data. Plot not performed.');
   return;
end

% === create a regular meshgrid for the plot
[X,Y]=meshgrid(slice_data.vx,slice_data.vy);	% contain grid of edges of bin bioundaries

% === if 'plot_slice' figure does not exist, create a new one;  put menu option to keep figure
fig=findobj('Tag','plot_slice');
if isempty(fig),
   colordef none;
   fig=figure('Tag','plot_slice','PaperPositionMode','auto','Name','MSlice : Plot Slice','Renderer','zbuffer');
   pos=get(gcf,'Position');
   set(fig,'Position',[pos(1) pos(2)-0.1*pos(4) pos(3) 1.1*pos(4)]);
   % === find any old plot slice figure and set current figure size to match 
   % === the dimensions of the last old figure of the same type
   oldfig=findobj('Tag','old_plot_slice'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
      set(fig,'Position',get(oldfig(length(oldfig)),'Position'));
   end
else
   figure(fig);
   clf;
end

% === create menu option to be able to do colour hardcopy printouts   
h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');      
uimenu(h,'Label','MAPSColour','Callback','ms_printc('' \\NDAIRETON\MAPSColour:'');');      
%=== TO ADD ANOTHER POSTSCRIPT PRINTER IN THIS LIST INSERT ANOTHER LINE AS ABOVE AND 
%=== EDIT NAME (LABEL) 'Phaser0' AND NETWORK PATH '' \\NDAIRETON\Phaser0:''
uimenu(h,'Label','default printer','Callback','ms_printc;');   
% === create menu option to be able to keep figure
h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
% === create menu option to be able to make old slice figures current
h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');


% ==== establish limits for the caxis (colour range along the intensity axis)
if ~exist('i_min','var')|isempty(i_min)|~isnumeric(i_min)|(length(i_min)~=1),
   i_min=min(slice_data.intensity(:));
end
if ~exist('i_max','var')|isempty(i_max)|~isnumeric(i_max)|(length(i_max)~=1),
   i_max=max(slice_data.intensity(:));
   if i_max-i_min<eps,
      i_min=min(0,i_min);   
   end
end

% ==== establish shading option
if ~exist('shad','var')|~ischar(shad)|...
      ~(strcmp(deblank(shad),'faceted')|strcmp(deblank(shad),'flat')|strcmp(deblank(shad),'interp')),
   shad='flat';
   disp(['Default "flat" shading option chosen.'])
end

hplot=pcolor(X,Y,slice_data.intensity);

% === establish axis limits
min_vx=slice_data.vx(1);
bin_vx=slice_data.vx(2)-slice_data.vx(1);
max_vx=slice_data.vx(length(slice_data.vx));
min_vy=slice_data.vy(1);
bin_vy=slice_data.vy(2)-slice_data.vy(1);
max_vy=slice_data.vy(length(slice_data.vy));
axis([min_vx-bin_vx/2 max_vx+bin_vx/2 min_vy-bin_vy/2 max_vy+bin_vy/2 ]);

% ==== choose colormap
if exist('map','var')&isnumeric(map)&(size(map,2)==3), 
   colormap(map);
else
   colormap jet;
end

% === plot colorbar, title and axis labels
caxis([i_min i_max]);
shading(shad);   

% === set aspect ratio 1:1 if units along x and y are the same and {Å^{-1}}
figure(fig);
if (~isempty(findstr(slice_data.axis_label(1,:),'Å^{-1}')))&...
      (~isempty(findstr(slice_data.axis_label(2,:),'Å^{-1}'))),
   set(gca,'DataAspectRatioMode','manual');
   a=get(gca,'DataAspectRatio');
   l1=slice_data.axis_unitlength(1);
   l2=slice_data.axis_unitlength(2);
   set(gca,'DataAspectRatio',[1/l1 1/l2 (1/l1+1/l2)/(a(1)+a(2))*a(3)]);
else
   set(gca,'dataAspectRatioMode','auto');
   a=get(gca,'DataAspectRatio');
   set(gca,'DataAspectRatioMode','manual');
   set(gca,'DataAspectRatio',[a(1) 1.15*a(2) a(3)]); 
   % arbitrarily set y scale bigger such that the graph will be scaled down verically and allow for title on top
end

% === put title and axis labels
h=title(slice_data.title);
YLim=get(gca,'YLim');
pos=get(h,'Position');
set(h,'Position',[pos(1) YLim(2)+0*pos(3) pos(3)]);
xlabel(deblank(slice_data.axis_label(1,:)));
ylabel(deblank(slice_data.axis_label(2,:)));

% === adjust colorbar height, determine real height of graph, after adjustment of units  
h=colorbar;
h_colourbar=h;
set(h,'Tag','Colorbar');
aspect=get(gca,'DataAspectRatio');
YLim=get(gca,'Ylim');
XLim=get(gca,'XLim');
pos=get(gca,'Position');
x=1/aspect(1)*[XLim(2)-XLim(1)];
y=1/aspect(2)*[YLim(2)-YLim(1)];
scx=pos(3)/x;
scy=pos(4)/y;
if scx<scy,
  	height=scx*y;	% real height of graph 
  	pos=get(h,'Position');
    set(h,'PlotBoxAspectRatio',[0.975*pos(3) height 1]); 
   % arbitrarily put a smaller scale along x to have colorbar height=graph height 
   % (probably tick label height is included in total length along y ?) 
end


h=h_colourbar;
% === establish linear or log colour scale 
if exist('linearlog','var')&ischar(linearlog)&strcmp(linearlog(~isspace(linearlog)),'log'),
   set(h,'YScale','log');
   index=(slice_data.intensity==0);
   slice_data.intensity(index)=NaN;
   set(hplot,'Cdata',log10(slice_data.intensity));
   caxis([log10(i_min) log10(i_max)]);
else
   % === use linear scale and enable colour sliders
   % === put sliders for adjusting colour axis limits
   pos_h=get(h,'Position');
   range=abs(i_max-i_min);
   hh=uicontrol(fig,'Style','slider',...
     'Units',get(h,'Units'),'Position',[pos_h(1) pos_h(2) pos_h(3)*1.5 pos_h(4)/20],...
     'Min',i_min-range/2,'Max',i_max-range*0.1,...
     'SliderStep',[0.01/1.4*2 0.10/1.4],'Value',i_min,'Tag','color_slider_min','Callback','color_slider(gcf,''slider'')');  
     % the SliderStep is adjusted such that in real terms it is [0.02 0.10] of the displayed intensity range 
   hh_value=uicontrol(fig,'Style','edit',...
     'Units',get(h,'Units'),'Position',get(hh,'Position')+pos_h(3)*1.5*[1 0 0 0],...
     'String',num2str(get(hh,'Value')),'Tag','color_slider_min_value','Callback','color_slider(gcf,''min'')');
   hh=uicontrol(fig,'Style','slider',...
     'Units',get(h,'Units'),'Position',[pos_h(1) pos_h(2)+pos_h(4)-pos_h(4)/20 pos_h(3)*1.5 pos_h(4)/20],...
     'Min',i_min+range*0.1,'Max',i_max+range/2,...
     'SliderStep',[0.01/1.4*2 0.10/1.4],'Value',i_max,'Tag','color_slider_max','Callback','color_slider(gcf,''slider'')');
   hh_value=uicontrol(fig,'Style','edit',...
     'Units',get(h,'Units'),'Position',get(hh,'Position')+pos_h(3)*1.5*[1 0 0 0],...
     'String',num2str(get(hh,'Value')),'Tag','color_slider_max_value','Callback','color_slider(gcf,''max'')');
end