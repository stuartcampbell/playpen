function dv_plot_msk

% function dv_plot_msk
% plot stored mask data (yellow appears as un-masked detectors)

% === return if DetectorView ControlWindow not opened 
h_cw=findobj('Tag','detectorview');
if isempty(h_cw),
   disp(['No DetectorView Control Widow found. Return.']);
   return;
end

% === return if no msk file loaded 
h_msk=findobj(h_cw,'Tag','dv_MskFile');
if isempty(h_msk)|~ishandle(h_msk),
   disp('Could not locate stored msk data.');
   return;
end
msk=get(h_msk,'UserData');
if isempty(msk),
   disp('Load/create msk file first, then do plot.');
   return;
end   
file=get(h_msk,'String');

% ===== read plot parameters from ControlWindow
spectramap=ms_getstring(h_cw,'dv_spectramap');
int_cont=ms_getstring(h_cw,'dv_int_cont');
cont_min=ms_getstring(h_cw,'dv_cont_min');
cont_max=ms_getstring(h_cw,'dv_cont_max');
cont_step=ms_getstring(h_cw,'dv_cont_step');

% === transform mask into matrix form 
msk=[msk ones(size(msk)) zeros(size(msk))];
[matrices,ax,textpos]=spec2matrix(msk,spectramap);
if isempty(matrices),
   disp('No data to plot.')
   return;
end

% === prepare plotting figure 
fig=findobj('Tag','plot_msk');
if isempty(fig),	% new figure
 	colordef none;	% black background
   % === create a new figure with 'Tag' property 'plot_msk'
   fig=figure('Name','DetectorView: Plot Sum','Tag','plot_msk','PaperPositionMode','auto');	
   % === find any old figures and set current figure size to match the dimensions of the 
   % === last old figure of the same type
   oldfig=findobj('Tag','old_plot_msk'); 
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
%grey=[0.752941 0.752941 0.752941];
%coltab=[grey; 1 1 0];
%load coltab.dat -ascii;
coltab=[0 0 0; 1 1 0];
colormap(coltab);
% === plot matrices 
for i=1:length(matrices),
   int=isnan(matrices{i}(:,:,3));
   pcolor(matrices{i}(:,:,1),matrices{i}(:,:,2),int);
   hold on;
end

caxis([0 1]);
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
text_h=text(textpos(2,1),textpos(2,2),'Spectrum none');
set(text_h,'Tag','Text');
set(fig,'WindowButtonMotionFcn','dv_read_int;');
% === get intensity data 
h_sum=findobj(h_cw,'Tag','dv_SumFile');
if isempty(h_sum)|~ishandle(h_sum),
   disp('Could not locate sum data. Return.');
   return;
end
d.data=get(h_sum,'UserData');
d.spectramap=spectramap;
set(fig,'UserData',d);

% === draw intensity contour levels if required
if strcmp(int_cont,'intensity'),
   % === get intensity data 
   h_sum=findobj(h_cw,'Tag','dv_SumFile');
   if isempty(h_sum)|~ishandle(h_sum),
      disp('Could not locate sum data. Return.');
      return;
   end
   data=get(h_sum,'UserData');
   if isempty(data),
      disp('Load Sum file first, then plot intensity contours.');
      return;
   end
   if isempty(cont_min)|isempty(cont_max)|isempty(cont_step),
      disp('Need to specify all contour parameters. No contours poltted. Return.');
      return;
   end
   cont_min=eval(cont_min);
   cont_max=eval(cont_max);
   cont_step=eval(cont_step);
   [matrices,ax]=spec2matrix(data,spectramap);
   
   for i=1:length(matrices),
      contour(matrices{i}(:,:,1),matrices{i}(:,:,2),matrices{i}(:,:,3),[cont_min:cont_step:cont_max]);
   end   
end
hold off
