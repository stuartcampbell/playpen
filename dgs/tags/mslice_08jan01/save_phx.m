function save_phx(spe_data,phx_filename)

% function save_phx(spe_data,phx_filename)
% to save detector information in a .phx file
% data=rows of [detector_number theta(deg) psi(deg)]

% === read number of unmasked detectors in current data file 
ndet=length(spe_data.det_group);

% === return if error at opening text file phx_filename
fid=fopen(phx_filename,'wt');
if fid==-1,
   disp(['Error opening file ' phx_filename '. File not created.']);
   return
end

% === wriete ndet at beginning of phx_filename
fprintf(fid,'%7d\n',ndet);

% === establish values for 2theta, psi, d2thata, dpsi
if isfield(spe_data,'det_theta'),
   theta=spe_data.det_theta*180/pi;
else
   theta=zeros(ndet,1);
end 
if isfield(spe_data,'det_psi'),
   psi=spe_data.det_psi*180/pi;
else
   psi=zeros(ndet,1);
end 
if isfield(spe_data,'det_dtheta'),
   dtheta=spe_data.det_dtheta*180/pi;
else
   dtheta=zeros(ndet,1);
end 
if isfield(spe_data,'det_dpsi'),
   dpsi=spe_data.det_dpsi*180/pi;
else
   dpsi=zeros(ndet,1);
end

% === write angle information for all unmasked detectors
for i=1:ndet,
   fprintf(fid,'%11d%10.4g%10.4g%10.4g%10.4g%10.4g%6d\n',...
      i,0,theta(i),psi(i),dtheta(i),dpsi(i),i);
end   
fclose(fid);
