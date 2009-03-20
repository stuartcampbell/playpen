function data=simulate(psi_samp,z_range)

% function data=simulate(psi_samp,z_range)
% available models '1D spinons'
%                  '1DAF spin-waves'

if ~exist('psi_samp','var'),
   help simulate;
   return
end

%model='1D spinons';
model='1DAF spin-waves';

% === define sampling energy range 
emin=-0.25;
emax=1.25;
de=0.005;
data.en=emin+de/2:de:emax-de/2;

% === define spectrometer setup
data.emode=2;	% indirect geometry
data.efixed=1.8590;	% fixed final energy (meV)
phx_file='m:\matlab\anal_spe\phx\irispg002.phx';	% file with detector position information
phx=load_phx(phx_file);
data.det_theta=phx(:,2)*pi/180;
data.det_psi=phx(:,3)*pi/180;
data.det_group=(1:size(phx,1))';

% === define sample data (Cs2CuCl4)
data.psi_samp=psi_samp*pi/180;
data.ar=[1 0 0]*2*pi/9.65;		% reciprocal crystal lattice [Å], line vectors size(1,3)
data.br=[0 1 0]*2*pi/7.48;
data.cr=[0 0 1]*2*pi/12.35;

% === simulate scattering in each detector
Q=spe2sqe(data);	% create wavevector Q (ndet,ne,3) in spectrometer reference frame 			
Q=sqe2samp(Q,data.psi_samp);	% transform (Q,E)->(Qx',Qy',Qz',E) into sample reference frame Q[Å^{-1}]
hkl=q2rlu(Q,data.ar,data.br,data.cr);
kmin=min(min(hkl(:,:,2)));
kmax=max(max(hkl(:,:,2)));
hmin=min(min(hkl(:,:,1)));
hmax=max(max(hkl(:,:,1)));
E=ones(size(data.det_theta))*data.en;	% (ndet,1)*(1,ne) -> matrix (ndet,ne)
data.S=sqw(hkl,E,model);
if isempty(data.S),
   return
end
data.ERR=0.1*data.S;	% put arbitrary errors of 10% intensity

% === take some slices and cuts, define axes and labels
data.title=['simulation Cs_2CuCl_4'];
u1=[1 0 0 0];					
u2=[0 1 0 0];
u3=[0 0 0 1];
data.u=[u1;u2;u3];
data.axis_label=str2mat('h','k','E');
data.axis_unit='Intensity (abs. units)';	% axes labels, string matrix (4,:)
data.symbol='wo';					% data plot symbol for cuts
data=calcproj(data);

% ==== establish range on the intensity axis
if ~exist('z_range','var'),
   z_range=[0 10];  
end

slice_d=slice_spe(data,1,hmin,hmax,kmin,kmax,0.05,emin,emax,0.01,z_range);
hold on;
k=linspace(-1,1.5,101);
w=0.6*abs(sin(2*pi*k));
h=plot(k,w,'r-');
hold off;

plot_de(data,z_range);

if psi_samp==-90,
%	cut_d=cut_spe(data,3,emin,emax,0.01,0,2,0.2,0.3);
end

% === tobyplot 
ax=[0 2.5 -1 0];
tobyplot(data,ax,'yes','yes');

%=== draw 'tobyplot3d' with the 3d dispersion on top
fig=findobj('Tag','tobyplot3d');
if isempty(fig),   
   figure;  
   set(gcf,'Tag','tobyplot3d');
else
   figure(fig);
   [az,el]=view;
   clf;
end

% ==== establish plot limits
Q=spe2sqe(data);
if ~exist('ax','var'),
	ax=[min(min(Q(:,:,1))) max(max(Q(:,:,1))) min(min(Q(:,:,2))) max(max(Q(:,:,2))) ];
end
% === plot horizontal scattering plane
X=Q(:,:,1);
Y=Q(:,:,2);
E=ones(size(data.det_theta,1),1)*data.en;
h=plot3(X(:),Y(:),E(:),'m.');
if exist('az','var')&exist('el','var'),
   view(az,el);
end
set(h,'MarkerSize',1);
hold on;
contour3(X,Y,E,[0:data.efixed/5:data.efixed]);
box on;
axis(ax);
temp=[data.title ' E_f = ' num2str(data.efixed,'%5.3f') ' meV'];
title([temp ', Psi = ' num2str(data.psi_samp*180/pi,'%5.3f') ' deg']);
xlabel('Q parallel axis in Å^{-1}');
ylabel('Q perpendicular axis in Å^{-1}');
zlabel('Energy (meV)');

% ========================================================
% ==== plot reciprocal lattice points in horizontal plane
% ========================================================
Q=[ax(1) ax(3) 0; ax(2) ax(3) 0; ax(2) ax(4) 0; ax(1) ax(4) 0];	% === get coordinates Q(4,3)[Å^{-1}] of visible box edges
Q=sqe2samp(Q,data.psi_samp);	% === transform Q [Å^{-1}] into the sample d-basis
hkl=q2rlu(Q,data.ar,data.br,data.cr); 
[h,k,l]=meshgrid(floor(min(hkl(:,1))):ceil(max(hkl(:,1))),...
   floor(min(hkl(:,2))):ceil(max(hkl(:,2))),floor(min(hkl(:,3))):ceil(max(hkl(:,3))));
hkl=[h(:)';k(:)';l(:)']';
Q=rlu2q(hkl,data.ar,data.br,data.cr);
Q=sqe2samp(Q,-data.psi_samp);
index=(Q(:,1)>=ax(1))&(Q(:,1)<=ax(2))&(Q(:,2)>=ax(3))&(Q(:,2)<=ax(4));
plot(Q(index,1),Q(index,2),'mo');

% =============================================================
% === plot dispersion mesh
% =============================================================
[X,Y]=meshgrid(ax(1):(ax(2)-ax(1))/50:ax(2),ax(3):(ax(4)-ax(3))/50:ax(4));
Q=sqe2samp(cat(3,X,Y,zeros(size(X))),data.psi_samp);
hkl=q2rlu(Q,data.ar,data.br,data.cr);
[S,wdisp]=sqw(hkl,zeros(size(X)),model);
S=sqw(hkl,wdisp,model);
mesh(X,Y,wdisp,S);
colormap jet;
caxis(z_range);
colorbar;
rotate3d on;
hold off;   

