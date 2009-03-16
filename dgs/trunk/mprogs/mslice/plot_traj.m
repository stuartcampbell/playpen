function a=plot_traj(vx,labelx,unitx,vy,labely,unity,ax,vz,clevels,titlestr)

if isempty(vz_min),
   vz_min=min(vz(:));
end
if isempty(vz_max),
   vz_max=max(vz(:));
end

if isempty(vx_min),
   vx_min=min(vx(index));
end
if isempty(vx_max),
   vx_max=max(vx(index));
end

if isempty(vy_min),
   vy_min=min(vy(index));
end
if isempty(vy_max),
   vy_max=max(vy(index));
end

%=== if 'plot_traj' figure does not exist, create
fig=findobj('Tag','plot_traj');
if isempty(fig),
   figure;
   set(gcf,'Tag','plot_traj','PaperPositionMode','auto','Name','MSlice : Detector Trajectories'); 
else
   figure(fig);
   clf;
end

% === create menu option to be able to keep figure
h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
% === create menu option to be able to do colour hardcopy printouts   
h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');      
uimenu(h,'Label','MAPSColour','Callback','ms_printc('' \\NDAIRETON\MAPSColour:'');');      
uimenu(h,'Label','LASER00','Callback','ms_printc('' \\ndablagrave\LASER00:'');'); 
uimenu(h,'Label','LASER01','Callback','ms_printc('' \\ndablagrave\LASER01:'');');      
uimenu(h,'Label','default printer','Callback','ms_printc;');   

h=plot(vx(:),vy(:),'.');
set(h,'MarkerSize',1);
if ~isempty(clevels),
   hold on;
   a=contour(vx,vy,vz,clevels);
   load coltab;
   colormap(coltab);
   colorbar;
   hold off;
end
box on;
axis(ax);
xlabel([labelx unitx]);
ylabel([labely unity]);
title(titlestr);

%=== Same lengths if axis are measuring same units
if strcmp(unitx,unity),	% same lengths along x and y
   set(gca,'DataAspectRatioMode','manual');
   a=get(gca,'DataAspectRatio');
   set(gca,'DataAspectRatio',[min(a(1:2)) min(a(1:2)) a(3)]);
   set(gcf,'PaperPositionMode','manual');
end
