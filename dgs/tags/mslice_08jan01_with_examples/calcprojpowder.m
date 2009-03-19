function data_out=calcprojpowder(data_in)

% function data_out=calcprojpowder(data_in)
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

disp('Powder mode: calculating projections of data points and bin boundaries onto the viewing basis');
drawnow;
data_out=data_in;

[vx,vbx,axislabelx]=pickvarb(data_in,1);
[vy,vby,axislabely]=pickvarb(data_in,2);

data_out.axis_unitlabel=str2mat(axislabelx,axislabely,deblank(data_in.axis_unitlabel(3,:)));
data_out.v=cat(3,vx,vy);
data_out.axis_unitlength=[1;1];
data_out.vb=cat(3,vbx(:,:,1),vby(:,:,1),vbx(:,:,2),vby(:,:,2));
