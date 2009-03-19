function surf_slice(slice_data,i_min,i_max,shad,map,offset)

% function surf_slice(slice_data,i_min,i_max,shad)
% produces a 3d surface plot of the (vx,vy,intensity) data 
% with a colouraxis in the range [i_min i_max]
% and shading {flat}|{faceted}
% map = array(m,3) optional RGB colour map, default = jet

% === return if slice_d is empty
if isempty(slice_data),
   disp('Slice contains no data. Surface plot not performed.');
   return;
end

%slice_data,
% === construct grid for the surface plot
nx=length(slice_data.vx);
centres_vx=(slice_data.vx(1:nx-1)+slice_data.vx(2:nx))/2;
ny=length(slice_data.vy);
centres_vy=(slice_data.vy(1:ny-1)+slice_data.vy(2:ny))/2;
[X,Y]=meshgrid(centres_vx,centres_vy);

algorithm=1;
if algorithm==2,
	% === perform transformation of axes from orthorhombic to tetragonal
	disp('perform transformation of axes from orthorhombic to tetragonal');
	temp_X=X;
	X=1/sqrt(2)*(-1/sqrt(2)*temp_X+1/sqrt(2)*Y);
	Y=1/sqrt(2)*(-1/sqrt(2)*temp_X-1/sqrt(2)*Y);
	slice_data.axis_label=str2mat('h (rlu) in 1.64 Å^{-1}','k (rlu) in 1.64 Å^{-1}',deblank(slice_data.axis_label(3,:)));
	slice_data.axis_unitlength=[1.64 ; 1.64];
end
Z=slice_data.intensity(1:ny-1,1:nx-1);

% === if 'surf_slice' figure does not exist, create a new one;  put menu option to keep figure
fig=findobj('Tag','surf_slice');
if isempty(fig),
   colordef none;
   fig=figure('Tag','surf_slice','PaperPositionMode','auto','Name','MSlice : Surf Slice');
   % === find any old plot slice figure and set current figure size to match 
   % === the dimensions of the last old figure of the same type
   oldfig=findobj('Tag','old_surf_slice'); 
   if ~isempty(oldfig),
      oldfig=sort(oldfig);
      set(fig,'Position',get(oldfig(end),'Position'));
      temp=get(get(oldfig(end),'CurrentAxes'),'View');	% remember viewing angle of old figure
      az=temp(1);
      el=temp(2);
   end
else
   figure(fig);
   [az,el]=view;
   if ~ishold,	
      clf;
   end
end

% === create menu option to be able to do colour hardcopy printouts   
h=uimenu(fig,'Label','ColourPrint','Tag','Color_Print','Enable','on');
uimenu(h,'Label','Phaser560','Callback','ms_printc('' \\NDAIRETON\Phaser560:'');');
uimenu(h,'Label','Phaser0','Callback','ms_printc('' \\NDAIRETON\Phaser0:'');');
uimenu(h,'Label','DACColour','Callback','ms_printc('' \\NDAIRETON\DACColour:'');');
uimenu(h,'Label','CRISPColour','Callback','ms_printc('' \\NDAIRETON\CRISPColour:'');');
%=== TO ADD ANOTHER POSTSCRIPT PRINTER IN THIS LIST INSERT ANOTHER LINE AS ABOVE AND 
%=== EDIT NAME (LABEL) 'Phaser0' AND NETWORK PATH '' \\NDAIRETON\Phaser0:''
uimenu(h,'Label','default printer','Callback','ms_printc;');   
% === create menu option to be able to keep figure
h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
uimenu(h,'Label','Keep figure','Callback','keep(gcf);');
% === create menu option to be able to make old slice figures current
h=uimenu(fig,'Label','Make Current','Tag','make_cur','Enable','off');
uimenu(h,'Label','Make Figure Current','Callback','make_cur(gcf);');

%h=uimenu(fig,'Label','Rotate');
%uimenu(h,'Label','Rotate 3d','Tag','rotate3d','Callback','rotate3d; ms_toggle(''rotate3d'');','Checked','on');

% ==== establish limits for the caxis (colour range along the intensity axis)
if ~exist('i_min','var')|isempty(i_min)|~isnumeric(i_min)|(length(i_min)~=1),
   i_min=min(slice_data.intensity(:));
end
if ~exist('i_max','var')|isempty(i_max)|~isnumeric(i_max)|(length(i_max)~=1),
   i_max=max(slice_data.intensity(:));
end

% ==== establish shading option
if ~exist('shad','var')|~ischar(shad)|...
      ~(strcmp(deblank(shad),'faceted')|strcmp(deblank(shad),'flat')|strcmp(deblank(shad),'interp')),
   shad='flat';
   disp(['Default "flat" shading option chosen.'])
end


if exist('offset','var'),
   surf(X,Y,Z+offset,Z);
else
	surf(X,Y,Z,Z);
end      
      
% ==== choose colormap
if exist('map','var')&isnumeric(map)&(size(map,2)==3), 
   colormap(map);
else
   colormap jet;
end

% === set axes limits and perspective (viewing angles)
caxis([i_min i_max]);
%axis([min(slice_data.vx) max(slice_data.vx) min(slice_data.vy) max(slice_data.vy) i_min i_max]);
axis([min(X(:)) max(X(:)) min(Y(:)) max(Y(:)) i_min i_max]);
if exist('az','var')&exist('el','var'),
	view(az,el);
else	% set default 3d viewing mode
   view(3);
end   

% === put colorbar, title and axes labels
colorbar_flag=(1>0);	% true or false
if colorbar_flag,
   colorbar;
else
   disp('Colorbar not plotted');
end   
shading(shad);
title(slice_data.title);
xlabel(deblank(slice_data.axis_label(1,:)));
ylabel(deblank(slice_data.axis_label(2,:)));
zlabel(deblank(slice_data.axis_label(3,:)));
%rotate3d on;

return;
% === set aspect ratio 1:1 if units along x and y are the same and {Å^{-1}}
figure(fig);
if (~isempty(findstr(slice_data.axis_label(1,:),'Å^{-1}')))&...
      (~isempty(findstr(slice_data.axis_label(2,:),'Å^{-1}'))),
   set(gca,'DataAspectRatioMode','manual');
   a=get(gca,'DataAspectRatio');
   l1=slice_data.axis_unitlength(1);
   l2=slice_data.axis_unitlength(2);
   ax=axis;
   set(gca,'DataAspectRatio',[1/l1 1/l2 (i_max-i_min)/max(l1*(ax(2)-ax(1)),l2*(ax(4)-ax(3)))]);
else
   set(gca,'dataAspectRatioMode','auto');
   a=get(gca,'DataAspectRatio');
   set(gca,'DataAspectRatioMode','manual');
   set(gca,'DataAspectRatio',[a(1) 1.15*a(2) a(3)]); 
   % arbitrarily set y scale bigger such that the graph will be scaled down verically and allow for title on top
end
