function proj_data  = project_crystal_psd( tofndgs, proj_info, analmode, psd,emode)
%PROJECT_CRYSTAL_PSD 


% -- NATURE OF THE SAMPLE --


unit_cell=tofndgs.sample.unit_cell;
psi=tofndgs.sample.psi;
uvec=tofndgs.sample.uvec;
vvec=tofndgs.sample.vvec;

data=tofngds.data;
s=data.s;
e=data.e;
det_group=data.y;

instrument=tofngds.instrument;
workspaces=instrument.workspaces;
workspaces=workspaces.workarray;
det_eff=workarray.det_eff;


det_no=det_eff.det_no;
theta=det_eff.theta;
phi=det_eff.phi;



as=unit_cell(1);
bs=unit_cell(2);
cs=unit_cell(3);
   
aa=unit_cell(4);
bb=unit_cell(5);
cc=unit_cell(6);
   
ux=uvec(1);
uy=uvec(2);
uz=uvec(3);
   
vx=vvec(1);
vy=vvec(2);
vz=vvec(3);   
   
psi=tofndgs.sample.psi;
psi=psi*pi/180;

% === Determine reciprocal basis
[ar,br,cr]=basis_r(as,bs,cs,aa*pi/180,bb*pi/180,cc*pi/180);


% === Determine crystal orientation 
% old convention when u and v are expressed in the reference frame of the direct lattice
%	u=[ux uy uz];
%	v=[vx vy vz];
% new convetion where u and v are vectors expresses in terms of the reciprocal lattice vectors
% u and v need not be orthogonal
u=ux*ar+uy*br+uz*cr;	
v=vx*ar+vy*br+vz*cr;
% now u and v are (1,3) vectors expressed in the same reference frame as ar, br and cr
u=u/norm(u);
v=v-dot(v,u)*u;
v=v/norm(v);	% (u,v) define the principal scattering plane of the spectrometer (Horiz HET&IRIS, Vert MARI)
w=cross(u,v);	% w is vertically up on the (u,v) plane 
% now (u,v,w) is a right-handed cooordinate system with all vectors orthogonal to each other and of unit length
   
% === Determine components of the reciprical basis in terms of the (u,v,w) frame
ar=[dot(ar,u) dot(ar,v) dot(ar,w)];
br=[dot(br,u) dot(br,v) dot(br,w)];
cr=[dot(cr,u) dot(cr,v) dot(cr,w)];

uv=[ux uy uz; vx vy vz];



u=u=proj_info.u;

% === calculate absolute basis (1,4) [either ≈^{-1} or meV] in the 4-dimensional wavevector + energy space

U1=basis_u(u(1,:),ar,br,cr);	
U2=basis_u(u(2,:),ar,br,cr);	
U3=basis_u(u(3,:),ar,br,cr);	


cosang=[dot(U1,U2)/norm(U1)/norm(U2) dot(U2,U3)/norm(U2)/norm(U3) dot(U3,U1)/norm(U3)/norm(U1)];

%======================================================
%==========check the ortogonality of U1 U2 and U3======
%======================================================

if any(abs(cosang)>=cos(pi/2*(1-1/90))),	% if angles between U1,U2,U3 are more than 1 deg away from 90 deg then warn that axes are not orthogonal 
   disp('  ');
   disp(['Warning: Chosen viewing axes are not orthogonal to each other. The angles are']);
   form='%5.2f';
   disp([ '(u1,u2)=' num2str(acos(cosang(1))*180/pi,form) ' deg, (u2,u3)=' num2str(acos(cosang(2))*180/pi,form) ' deg, (u3,u1)=' num2str(acos(cosang(3))*180/pi,form) ' deg']);
   disp(['Projections are not calculated.']);
   disp(['Redefine orthogonal viewing axes.']);
   disp('Possible suggestions are : ');
   planeperp(data_in.u(1,:),data_in.ar,data_in.br,data_in.cr);
   planeperp(data_in.u(2,:),data_in.ar,data_in.br,data_in.cr);
   planeperp(data_in.u(3,:),data_in.ar,data_in.br,data_in.cr);
   
   return;
end   


%=====================================================
%=====================================================





% === transform data points (angles(detector position),energy) 
% === into (Qx,Qy,Qz,E) points Q[≈^{-1}],E[meV] in the spectrometer reference frame
    
% where x=[1 0 0] along +k_i, [0 1 0] in the horiz scattering plane 
% and z=[0 0 1] \perp to the horizontal plane
% 11-sep-98 modify const E=2.07k^2 to compare results with PHOENIX, exact match
% restore E/k^2=2.072

if emode==1,
   
   % ======================================================================
	% For direct-geometry spectrometers like HET, MARI
	% efixed = monochromatic incident energy ei(meV) 
	% ======================================================================

   ki=sqrt(efixed/2.07);	% scalar
   kf=sqrt((efixed-en)/2.07); % line-vector (1,ne)
    
   Qx=ki*ones(size(theta,1),size(en,2))-cos(theta)*kf; % matrix (ndet,ne)
   Qy=-(sin(theta).*cos(phi))*kf;
   Qz=-(sin(theta).*sin(phi))*kf;
   Q=cat(3,Qx,Qy,Qz);
   
elseif emode==2,
   
   % ========================================================================
   % For indirect-geometry spectrometers like IRIS
	% efixed = monochromatic scattered energy (meV) for a white incident beam  
   % here all detectors are in the horizontal plane with Psi=0
   % scattering geometry diagram in notebook computing 2, page 2-14
   % ========================================================================
   
    ki=sqrt((efixed+en)/2.07); % line-vector (1,ne)
    kf=sqrt(efixed/2.07);	% scalar
   
	Qx=ones(size(theta))*ki-kf*cos(theta)*ones(size(en)); % matrix (ndet,ne)
	Qy=-kf*(sin(theta))*ones(size(en));	% (ndet,1)*(1,ne)
	Qz=zeros(size(Qx));
	Q=cat(3,Qx,Qy,Qz);

else
   disp('Only inelastic direct-geometry (emode=1, HET, MARI)');
   disp(' or inelastic indirect-geometry (emode=2, IRIS) spectrometer modes available.');
	disp(['emode=' num2str(emode) ' not implemented. Transformation not performed']);
   Q=[];
   return
end



% === transform wavevector Q into sample reference frame by a rotation of the coordinate system

% transforms (Qx,Qy,Qz,E) from the spectrometer reference frame to
% (Q_samp,E)=(Qx',Qy',Qz',E) in the sample reference frame
% (Qx||k_i,Qy) define principal scattering plane (where Azimuth angle of detectors in .phx file is Psi=0 )

R=[cos(psi) sin(psi) 0; -sin(psi) cos(psi) 0; 0 0 1]; 
% rotation matrix spectrometer -> sample according to PHOENIX convention
Q_samp=Q;
if ndims(Q)==3,
	Q_samp(:,:,1)=R(1,1)*Q(:,:,1)+R(1,2)*Q(:,:,2)+R(1,3)*Q(:,:,3);
	Q_samp(:,:,2)=R(2,1)*Q(:,:,1)+R(2,2)*Q(:,:,2)+R(2,3)*Q(:,:,3);
	Q_samp(:,:,3)=R(3,1)*Q(:,:,1)+R(3,2)*Q(:,:,2)+R(3,3)*Q(:,:,3);
elseif ndims(Q)==2,
   Q_samp(:,1)=R(1,1)*Q(:,1)+R(1,2)*Q(:,2)+R(1,3)*Q(:,3);
	Q_samp(:,2)=R(2,1)*Q(:,1)+R(2,2)*Q(:,2)+R(2,3)*Q(:,3);
   Q_samp(:,3)=R(3,1)*Q(:,1)+R(3,2)*Q(:,2)+R(3,3)*Q(:,3);
elseif ndims(Q)==1,
   Q_samp(1)=R(1,1)*Q(1)+R(1,2)*Q(2)+R(1,3)*Q(3);
	Q_samp(2)=R(2,1)*Q(1)+R(2,2)*Q(2)+R(2,3)*Q(3);
   Q_samp(3)=R(3,1)*Q(1)+R(3,2)*Q(2)+R(3,3)*Q(3);   
else
   disp(['Call to sqe2samp(Q,psi) with ndims(Q)=' num2str(ndims(Q)) ' not executed']);
   Q_samp=[];
   return;
end   

Q=Q_samp;

% === compute projections of data points(Qx,Qy,Qz,E) onto the viewing basis u1,u2,u3

% function v=sqe2proj(Q,en,U1,U2,U3)
% Q (ndet,ne or nb+1,3), en (1,ne), Ui(1,4), returns v (ndet,ne or nb+1,3), i=1:2 for detector array, = 1:3 for PSD (area) detectors 
% calculates projections of points (Qx,Qy,Qz,E) (3x≈^{-1},meV)
% onto the absolute basis ui (each vector has dimensions of ≈ or meV^{-1})

UU1=U1/dot(U1,U1);
UU2=U2/dot(U2,U2);

if length(en)==1,
 	E=ones(size(Q,1),size(Q,2))*en; % (ndet,nb+1) for first bin edges
else 
    E=ones(size(Q,1),1)*en; % matrix(ndet,ne) for centres, (ndet,ne+1) for boundary trajectories
end
v(:,:,1)=Q(:,:,1)*UU1(1)+Q(:,:,2)*UU1(2)+Q(:,:,3)*UU1(3)+E*UU1(4);
v(:,:,2)=Q(:,:,1)*UU2(1)+Q(:,:,2)*UU2(2)+Q(:,:,3)*UU2(3)+E*UU2(4);

% three viewing axes for (PSD) data 
UU3=U3/dot(U3,U3);
v(:,:,3)=Q(:,:,1)*UU3(1)+Q(:,:,2)*UU3(2)+Q(:,:,3)*UU3(3)+E*UU3(4);



% === CONSTRUCT THE ISISEXCproj_data object

s=reshape(s,1,[]);
e=reshape(e,1,[]);

u(1,:)=reshape(v(:,:,1),1,[]);
u(2,:)=reshape(v(:,:,2),1,[]);
u(3,:)=reshape(v(:,:,3),1,[]);

id=1;

det_group=det_group*ones(1,size(s,2));
iw=reshape(NDET,1,[]);
xlo=reshape(E,1,[]);
xhi=xlo;


proj_data=ISISEXCproj_data('entry_name','name',s,e,u,id,iw,xlo,xhi);
