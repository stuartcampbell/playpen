function data_out=calcproj(data_in)

% data_in.a, b, c (1,3)[Å] = direct criystal lattice
%        .psi_samp [rad] = rotation angle of (a,b) axes in the horizontal scattering plane of the spectrometer
%        .det_theta (ndet,1)[rad] = total scattering angles of the detectors 
%        .det_psi(ndet,1)[rad] = azimuthal angle of detectors 
%         (not required for IRIS - all detectors in the horozontal scattering plane)  
%        .emode=1 (HET,MARI) =2 (IRIS)
%        .efixed [meV] fixed energy of the spectrometer Ei(HET,MARI) EF(IRIS) 
%        .u (3,4)[adimensional] = viewing basis
%        .axis_unit [string] intensity label
% returns data_out with new fields
%        .v=(ndet,ne,3) [rlu and meV] = projections of data points onto the viewing basis u1,u2,u3
%        .axis_unitlabel (4,:)[string]
%        .axis_unitlength (3,1) 

disp('Calculating projections of data points onto the viewing axes');
drawnow;
data_out=data_in;

% === calculate absolute basis (1,4) [either Å^{-1} or meV] in the 4-dimensional wavevector + energy space
U1=basis_u(data_in.u(1,:),data_in.ar,data_in.br,data_in.cr);	
U2=basis_u(data_in.u(2,:),data_in.ar,data_in.br,data_in.cr);	
U3=basis_u(data_in.u(3,:),data_in.ar,data_in.br,data_in.cr);
cosang=[dot(U1,U2)/norm(U1)/norm(U2) dot(U2,U3)/norm(U2)/norm(U3) dot(U3,U1)/norm(U3)/norm(U1)];
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
   data_out.v=[];
   data_out.axis_unitlabel=[];
   data_out.axis_unitlength=[];
   return;
end   

% construct automatic labels for the units along the viewing axes
[unitlabel1,unitlength1]=units(U1);
[unitlabel2,unitlength2]=units(U2);
[unitlabel3,unitlength3]=units(U3);
data_out.axis_unitlabel=str2mat(unitlabel1,unitlabel2,unitlabel3,deblank(data_in.axis_unitlabel(4,:)));
data_out.axis_unitlength=[unitlength1; unitlength2; unitlength3];

try % fortran optimised routine
   data_out.v=zeros(length(data_in.det_theta),length(data_in.en),3);
   [data_out.v(:,:,1),data_out.v(:,:,2),data_out.v(:,:,3)]=...
      spe2proj_df(data_in.emode,data_in.efixed,data_in.en,...
      data_in.det_theta,data_in.det_psi,data_in.psi_samp,...
      U1/dot(U1,U1),U2/dot(U2,U2),U3/dot(U3,U3));
   %disp('Using fortran spe2proj_df routine for calcproj');   
catch   
	% === transform data points (angles(detector position),energy) 
   % === into (Qx,Qy,Qz,E) points Q[Å^{-1}],E[meV] in the spectrometer reference frame
	Q=spe2sqe(data_in); % Q(ndet,ne,3) [Å^{-1}]
	
	% === transform wavevector Q into sample reference frame by a rotation of the coordinate system
	Q=sqe2samp(Q,data_in.psi_samp); % Q(ndet,ne,3) [Å^{-1}]	

	% === compute projections of data points(Qx,Qy,Qz,E) onto the viewing basis u1,u2,u3
   data_out.v=sqe2proj(Q,data_in.en,U1,U2,U3);	% (ndet,ne,3) [rlu and meV]
end