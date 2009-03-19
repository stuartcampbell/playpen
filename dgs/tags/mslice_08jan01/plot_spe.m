function plot_spe(data,x,vx_min,vx_max,y,vy_min,vy_max,z_range)

% function plot_spe(data,x,vx_min,vx_max,y,vy_min,vy_max,z_range)
% produces a colourmap plot of the (vx,vy,intensity) data 
% with a colour_axis in the range z_range
% and shading {flat}|{faceted}


%=== if 'plot_spe' figure does not exist, create
fig=findobj('Tag','plot_spe');
if isempty(fig),
   fig=figure('Tag','plot_spe');
   colordef white;
else
   figure(fig);
   clf;
end
h=uimenu(fig,'Label','Keep','Tag','keep','Enable','on');
uimenu(h,'Label','Keep figure','Callback','keep(gcf);');

% ==== establish limits for intensity axis
if ~exist('z_range','var')|(exist('z_range','var')&isempty(z_range)),
   z_range=[0 max(data.S(:))];
elseif length(z_range)==1
	z_range=[0 z_range];   
elseif length(z_range)>2,
   disp(['Warning: erroneous z-range for plot. Default limits chosen.']);
	z_range=[0 z_range];      
end

% === plot graph 
vx=data.v(:,:,x);
vy=data.v(:,:,y);
[nx,ny]=size(data.v(:,:,1));
vx(nx+1,:)=vx(nx,:)+(vx(nx,:)-vx(nx-1,:));
vx(:,ny+1)=vx(:,ny)+(vx(:,ny)-vx(:,ny-1));
vy(nx+1,:)=vy(nx,:)+(vy(nx,:)-vy(nx-1,:));
vy(:,ny+1)=vy(:,ny)+(vy(:,ny)-vy(:,ny-1));
int=data.S;
int(nx+1,:)=NaN*ones(1,ny);
int(:,ny+1)=NaN*ones(nx+1,1);

pcolor(vx,vy,int);
shading flat;
load coltab;
colormap(coltab);
%colormap jet;
caxis(z_range);
colorbar;
axis([vx_min vx_max vy_min vy_max]);

% === label graph
titlestr=avoidtex(data.title);
if isfield(data,'efixed'),
   titlestr=[titlestr ' E_{fixed} = ' num2str(data.efixed,'%5.3f') ' meV'];
end
if isfield(data,'psi_samp'),
   titlestr=[titlestr ', Psi = ' num2str(data.psi_samp*180/pi,'%5.3f') ' deg'];
end
title(titlestr);
xlabel([deblank(data.axis_label(x,:)) ' ' data.axis_unit(x,:)]);
ylabel([deblank(data.axis_label(y,:)) ' ' data.axis_unit(y,:)]);
