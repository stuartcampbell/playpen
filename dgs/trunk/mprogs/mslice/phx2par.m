function phx2par(phxfile,parfile,l2)

% programme to convert a .phx file into a .par format 

phx=load_phx(phxfile);
if isempty(phx),
	return
end

theta=phx(:,2);	% scattering angle (deg)
psi=phx(:,3);		% azimuthal angle (deg)
dtheta=phx(:,4)*pi/180;		% detector width (deg -> radians)
dpsi=phx(:,5)*pi/180;		% detector height (deg -> radians)
d=l2.*ones(size(theta));

w=2*d.*tan(dtheta/2);	% detector width (m)
h=2*d.*tan(dpsi/2);	% detector height (m)

par=[d';theta';psi';dtheta';dpsi']';

fid=fopen(parfile,'wt');
fprintf(fid,'%5d \n',size(par,1));
fprintf(fid,'%10.3f%10.3f%10.3f%10.3f%10.3f\n',par');
disp(['Saving information for ' num2str(size(par,1)) ' detectors to .par file : ' parfile]);
fclose(fid);
