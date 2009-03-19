function dv_plot_sum

% function dv_plot_sum

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. Return.']);
   return;
end

% === return if no sum file loaded 
h=findobj(h_cw,'Tag','dv_SumFile');
if isempty(h)|~ishandle(h),
   disp('Could not locate stored sum data.');
   return;
end
data=get(h,'UserData');
if isempty(data),
   disp('Load sum file first, then do plot.');
   return;
end   
file=get(h,'String');

% ===== read plot parameters from ControlWindow
spectramap=ms_getstring(h_cw,'dv_spectramap');
i_min=ms_getstring(h_cw,'dv_i_min');
if ~isempty(i_min),
   i_min=eval(i_min);
else
   i_min=[];
end
i_max=ms_getstring(h_cw,'dv_i_max');
if ~isempty(i_max),
   i_max=eval(i_max);
else
   i_max=[];
end
[matrices,ax,textpos]=spec2matrix(data,spectramap);

if isempty(matrices),
   disp('No data to plot.')
   return;
end

% === identify plotting figure 
fig=findobj('Tag','plot_sum');
if isempty(fig),	% new figure
 	colordef none;	% black background
   % === create a new figure with 'Tag' property 'detector_map'
   fig=figure('Name','DetectorView: Plot Sum','Tag','plot_sum','PaperPositionMode','auto');	
   % === find any old figures and set current figure size to match the dimensions of the 
   % === last old figure of the same type
   oldfig=findobj('Tag','old_plot_sum'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
     	set(fig,'Position',get(oldfig(end),'Position'));
   end
   % === define 'UserData' field of current figure     
   set(fig,'UserData',[]);
   % === create menu option to be able to do colour hardcopy 
   h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
   uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
   uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
	uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
	uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');   
   uimenu(h,'Label','default printer','Callback','ms_printc;');   
   % === create menu option to be able to keep figure
   h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
   uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
   % === create menu option to be able to make old plot cut figures current
   h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
   uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');
end
figure(fig);
hold off;
box on;
load coltab.dat -ascii;
colormap(coltab);

% === establish axes limits and plot matrices 
for i=1:length(matrices),
   pcolor(matrices{i}(:,:,1),matrices{i}(:,:,2),matrices{i}(:,:,3));
   hold on;
   if i==1,
      i1=min(min(matrices{i}(:,:,3)));
      i2=max(max(matrices{i}(:,:,3)));   
   else
      i1=min([i1 min(min(matrices{i}(:,:,3)))]);
      i2=max([i2 max(max(matrices{i}(:,:,3)))]);
   end
end
if isempty(i_min),
   i_min=i1;
end
if isempty(i_max),
   i_max=i2;
end

caxis([i_min i_max]);
colorbar;
shading faceted;
axis(ax);
aa=get(gca,'DataAspectRatio');
set(gca,'DataAspectRatioMode','manual','DataAspectRatio',[max(aa(1:2)) max(aa(1:2)) aa(3)]);
xlabel('Horizontal Direction (mm)');
ylabel('Vertical Direction (mm)');
h=title(avoidtex(file));
YLim=get(gca,'YLim');
pos=get(h,'Position');
set(h,'Position',[pos(1) YLim(2) pos(3)]);

% === ENABLE SENSITIVE MAP READING OF INTENSITIES
text(textpos(1,1),textpos(1,2),'Pointer at :');
text_h=text(textpos(2,1),textpos(2,2),'None');
hold off;
set(text_h,'Tag','Text');
set(fig,'WindowButtonMotionFcn','dv_read_int;');
d.spectramap=spectramap;
d.data=data;
set(fig,'UserData',d);