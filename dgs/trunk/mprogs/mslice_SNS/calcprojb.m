function data_out=calcprojb(data_in)

% function data_out=calcprojb(data_in)
% calculate projections of data points onto the viewing basis for bin centres and also bin boundaries
% data_in.a, b, c (1,3)[Å] = direct criystal lattice
%        .psi_samp [rad] = rotation angle of (a,b) axes in the horizontal scattering plane of the spectrometer
%        .det_theta (ndet,1)[rad] = total scattering angles of the detectors 
%        .det_psi(ndet,1)[rad] = azimuthal angle of detectors 
%         (not required for IRIS - all detectors in the horozontal scattering plane)  
%        .emode=1 (HET,MARI) =2 (IRIS)
%        .efixed [meV] fixed energy of the spectrometer Ei(HET,MARI) EF(IRIS) 
%        .u (3,4)[adimensional] = viewing basis
%        .axis_unit [string] intensity label
% returns data_out with same fields as data_in plus 
%        .v (ndet,ne,3) [rlu and meV] = projections of data points onto the viewing basis u1,u2,u3
%        .axis_unit (4,:)[string] 

disp('Calculating projections of data points and bin boundaries onto the viewing basis');
drawnow;
data_out=data_in;

% === calculate absolute basis (1,4) [either Å^{-1} or meV] in the 4-dimensional wavevector + energy space
U1=basis_u(data_in.u(1,:),data_in.ar,data_in.br,data_in.cr);	
U2=basis_u(data_in.u(2,:),data_in.ar,data_in.br,data_in.cr);

cosang=dot(U1,U2)/norm(U1)/norm(U2);
if abs(cosang)>=cos(pi/2*(1-1/90)),	% if angle between U1,U2 is more than 1 deg away from 90 deg then warn that axes are not orthogonal 
   disp('  ');
   disp(['Warning: Chosen viewing axes are not orthogonal to each other. The angle is']);
   form='%5.2f';
   disp([ '(u1,u2)=' num2str(acos(cosang)*180/pi,form) ' deg']);
   disp(['Projections not calculated.']);
   disp(['Redefine viewing axes to be orthogonal.']);
   data_out.v=[];
   data_out.vb=[];
   disp('Possible suggestions are : ');
   planeperp(data_in.u(1,:),data_in.ar,data_in.br,data_in.cr);
   planeperp(data_in.u(2,:),data_in.ar,data_in.br,data_in.cr);
   return;
end   


% construct automatic labels for the units along the viewing axes
[unitlabel1,unitlength1]=units(U1);
[unitlabel2,unitlength2]=units(U2);
data_out.axis_unitlabel=str2mat(unitlabel1,unitlabel2,deblank(data_in.axis_unitlabel(3,:)));
data_out.axis_unitlength=[unitlength1; unitlength2];

% === transform data points (angles(detector position),energy) 
% === into (Qx,Qy,Qz,E) points Q(Å^{-1}),E(meV) in the spectrometer reference frame
try 
   data_out.v=zeros(length(data_in.det_theta),length(data_in.en),2);
   [data_out.v(:,:,1),data_out.v(:,:,2),temp]=...
      spe2proj_df(data_in.emode,data_in.efixed,data_in.en,...
      data_in.det_theta,data_in.det_psi,data_in.psi_samp,...
      U1/dot(U1,U1),U2/dot(U2,U2),[0 0 0 0]);
   %disp('Using spe2proj_df for calcprojb.')   
catch 
	Q=spe2sqe(data_in); % Q(ndet,ne,3) (Å^{-1}) bin centre points
	% === transform wavevector Q into sample reference frame by a rotation of the coordinate system
	Q=sqe2samp(Q,data_in.psi_samp); % Q(ndet,ne,3) [Å^{-1}]	

	% === compute projections of data points(Qx,Qy,Qz,E) onto the viewing basis u1,u2
	data_out.v=sqe2proj(Q,data_in.en,U1,U2);	% (ndet,ne,3) [rlu and meV]
end
   
Qb=spe2sqeb(data_in); % Qb(ndet,nb+1,3)  (Å^{-1}) bin boundary points

% === compute projections of bin boundary and centre trajectories in the (U1,U2) space 
ndet=size(data_in.det_theta,1);	% number of unmasked detectors
ne=length(data_in.en);
nb=size(Qb,2)-1;	% number of bin edge trajectories theta+/-dtheta/2 and psi+/-dpsi/2, total trajectories including centre = nb+1

de=data_in.en(2)-data_in.en(1);	% energy bin width for equally-spaced energy boundary points 
en=data_in.en(1)-de/2;	% energy bin boundary for first bin
vb=zeros(ndet,nb+1,2);	% (ndet,nb+1,2) will contain projections of first bin edge and centre trajectories onto the (U1,U2) space
Qb=sqe2samp(Qb,data_in.psi_samp);	% Qb(ndet,nb+1,3) in sample reference frame
vb=sqe2proj(Qb,en,U1,U2);	% vb(ndet,nb+1,1:2) projections on the viewing axes U1,U2 [rlu and meV]    

% === compute orientational vectors for the origins of the edge trajectories for each detector
dxy=zeros(ndet,nb,2);	% (ndet,nb,2) vectors in (U1,U2) space of the origins ( at energy bin #1) 
											% of the 4 trajectories bounding the edges of a pixel bin  
for i=1:nb,
   dxy(:,i,1:2)=vb(:,i+1,1:2)-vb(:,1,1:2);	% for all detectors at once calculate displacement of 
   														% edge ++, +-, -+ and -- from centre
end
u=dxy(:,1,1:2);	% (ndet,1,2) fix one vector (++) in the (U1,U2) plane for each detector
d=zeros(ndet,nb);	% (ndet,nb) projections of edge trajectories origins onto the vector u for each detector
for i=1:nb,
   d(:,i)=dot(dxy(:,i,:),u(:,1,:),3);	% (ndet,1) column vector scalar numbers, projections of ++, +-, -+ and -- vectors onto the ++ direction
end
[d,perm]=sort(d,2);
data_out.vb=zeros(ndet,ne+1,4);
en=[data_in.en(1)-de/2 data_in.en+de/2];
data_out.vb(:,:,1:2)=sqe2proj(sqe2samp(spe2sqeb(data_in,perm(:,1)+1),data_in.psi_samp),en,U1,U2);
data_out.vb(:,:,3:4)=sqe2proj(sqe2samp(spe2sqeb(data_in,perm(:,4)+1),data_in.psi_samp),en,U1,U2);  