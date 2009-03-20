function plot_de(data,z_range,det_index)

% function plot_de(data,z_range,det_index)
% DISPLAY DATA (detector_group,energy) ,intensity as colour
% z_range can restrict the colour axis , example z_range=[0 0.5]
% det_index can restrict which detectors to plot (== all detectors by default)
% example det_index=[1:10] for detectors 1 to 10 inclusive

if ~exist('data','var'),
   help plot_de;
   return
end

%=== if 'plot_de' figure does not exist, create
fig=findobj('Tag','plot_de');
if isempty(fig),
   figure;
	set(gcf,'Tag','plot_de');   
else
   figure(fig);
   clf;
end

% ==== establish limits for plot along z-axis
if ~exist('z_range','var'),
   z_range=[0 max(data.S(:))];
elseif length(z_range)==1
	z_range=[0 z_range];   
elseif length(z_range)>2,
   disp(['Warning: erroneous z-range for plot. Default limits chosen.']);
	z_range=[0 z_range];      
end

% ==== establish which detectors to display
if ~exist('det_index','var'),
%	det_index=data.det_group;
	det_index=(1:size(data.S,1))';
elseif (min(det_index)<data.det_group(1))|(max(det_index)>data.det_group(length(data.det_group))),
   disp(['Out of range detector index ']);
   disp(['All-detector plot performed by default.']);
%   det_index=data.det_group;
	det_index=(1:size(data.S,1))';   
else
   det_index=sort(det_index(:));  
   disp('Given det_index selected');
end

de=data.en(2)-data.en(1);
ne=length(data.en);
en=[data.en-de/2 data.en(ne)+de/2];
det_group=data.det_group;
ndet=length(det_group);
det_group=[det_group-0.5 ;det_group(ndet)+0.5];
int=NaN*ones(size(data.S));
int(det_index,:)=data.S(det_index,:);
int(ndet+1,:)=NaN*ones(1,ne);
int(:,ne+1)=NaN*ones(ndet+1,1);

[X,Y]=meshgrid(det_group',en);
pcolor(X,Y,int');
caxis(z_range);
axis([det_index(1)-0.5 det_index(length(det_index))+0.5 en(1) en(length(en+1))]);
load coltab.dat;
colormap(coltab);
% colormap jet;
colorbar;
shading flat;
temp=avoidtex(data.filename);
if isfield(data,'efixed'),
   temp=[temp ' E_f = ' num2str(data.efixed,'%5.3f') ' meV'];
end
if isfield(data,'psi_samp'),
   temp=[temp ', Psi = ' num2str(data.psi_samp*180/pi,'%5.3f') ' deg'];
end
title(temp);
ylabel('Energy (meV)');
xlabel('Detector Group Number');

