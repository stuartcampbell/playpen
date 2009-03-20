function Q=spe2sqeb(spe_data,index)

% function Q=spe2sqeb(spe_data,index)
% calculates trajectories for the edge boundaries of each bin in wavevector-energy space
% if index=(ndet,1) with values 1,2,3,4 or 5 then return trajectory of centre,++,+-,-+, -- trajectory as defined below
% and Q=(ndet,ne+1,3)
% otherwise returns Q(ndet,nb+1,3) for the origin and centre of the first bin
% 
% fields required of spe_data: emode(1 or 2), efixed[meV], en(1,ne)[meV], det_theta(ndet,1)[rad], 
%                   det_psi(ndet,1)[rad](not necessary for IRIS)
%
% emode=1 if efixed=ei (HET and MARI) and emode=2 if efixed=ef (IRIS)
% transforms data points in the form (detector_angles,energy) -> (Qx,Qy,Qz,energy)
% in the spectrometer frame where (Qx||k_i,Qy) define 
% the principal scattering plane (psi=0 for all detectors in this plane) 
% and (Qx,Qy,Qz) form a right-handed coordinate system

% === establish angles of bin centre and edge boundaries 
det_theta=[spe_data.det_theta';...
         transpose(spe_data.det_theta+spe_data.det_dtheta/2);...
         transpose(spe_data.det_theta+spe_data.det_dtheta/2);...
 		   transpose(spe_data.det_theta-spe_data.det_dtheta/2);...
         transpose(spe_data.det_theta-spe_data.det_dtheta/2)]'; % (ndet,nb+1) nb=number of boundaries ++,+-,-+,--
if ~isfield(spe_data,'det_psi'), % case of IRIS with all detectors in horizontal scattering plane PSI=0
   det_psi=zeros(size(det_theta),1); 
else
   det_psi=spe_data.det_psi;
end
det_dpsi=[zeros(size(det_psi'));...
      	 transpose(spe_data.det_dpsi);...
         -transpose(spe_data.det_dpsi);...
      	 transpose(spe_data.det_dpsi);...
      	-transpose(spe_data.det_dpsi)]';   
det_psi=[det_psi';...
			det_psi';...   
			det_psi';...   
			det_psi';...   
			det_psi']';	
% (ndet,nb+1)   
   
if ~exist('index','var')|isempty(index)|~isnumeric(index)|~(all(index>=1)&all(index<=5)),
   % === calculate Q for the edge trajectories and bin centre for each detector and first energy bin
 	de=spe_data.en(2)-spe_data.en(1);	% scalar, energy bin width for equally-spaced energy points
 	en=spe_data.en(1)-de/2; % scalar, first energy bin boundary
   if spe_data.emode==1,
   
	   % ======================================================================
		% For direct-geometry spectrometers like HET, MARI
		% efixed = monochromatic incident energy ei(meV) 
		% ======================================================================

	   ki=sqrt(spe_data.efixed/2.07);	% scalar
    	kf=sqrt((spe_data.efixed-en)/2.07); % scalar
      Q=zeros(size(det_theta,1),size(det_theta,2),3);
      Q(:,:,1)=ki*ones(size(det_theta))-cos(det_theta)*kf; % Qx matrix (ndet,nb+1)
      % === assume detector tube on the Debye-Scherrer cone
      Q(:,:,2)=-(sin(det_theta).*cos(det_psi))*kf+(sin(det_dpsi/2).*sin(det_psi))*kf;	% Qy
      Q(:,:,3)=-(sin(det_theta).*sin(det_psi))*kf-(sin(det_dpsi/2).*cos(det_psi))*kf;	% Qz
% === more corrections to projections are complicated because d varies along the tube length and consequently 
% === points with the same Energy transfer, same |k_f| have k_f_perp and along the tube slightly different 
% === formulas above assume same k_f value along the tube given by value at centre (better approximation)
% === formulas below assume same k_f value along the tube given by value at end points (produces artificial shifts of peaks
% === along the enrgy axis)
%      Q(:,:,1)=ki*ones(size(det_theta))-(cos(det_theta).*cos(det_dpsi/2))*kf; % Qx matrix (ndet,nb+1)
%      % === assume detector tube on the Debye-Scherrer cone
%      Q(:,:,2)=-(cos(det_dpsi/2).*sin(det_theta).*cos(det_psi))*kf+(sin(det_dpsi/2).*sin(det_psi))*kf;	% Qy
%      Q(:,:,3)=-(cos(det_dpsi/2).*sin(det_theta).*sin(det_psi))*kf-(sin(det_dpsi/2).*cos(det_psi))*kf;	% Qz
      
   elseif spe_data.emode==2,
   
	   % ========================================================================
   	% For indirect-geometry spectrometers like IRIS
		% efixed = monochromatic scattered energy (meV) for a white incident beam  
		% ========================================================================
   
     	ki=sqrt((spe_data.efixed+en)/2.07); % scalar
      kf=sqrt(spe_data.efixed/2.07);	% scalar
      Q=zeros(size(det_theta,1),size(det_theta,2),3);
		Q(:,:,1)=ones(size(det_theta))*ki-kf*cos(det_theta); % matrix (ndet,nb+1)
		Q(:,:,2)=-kf*(sin(det_theta));
		Q(:,:,3)=-kf*(sin(det_dpsi/2));
  	else
   	disp('Only inelastic direct-geometry (emode=1, HET, MARI)');
   	disp(' or inelastic indirect-geometry (emode=2, IRIS) spectrometer modes available.');
		disp(['emode=' num2str(emode) ' not implemented. Transformation not performed']);
   	Q=[];
   	return
	end
else
   theta=zeros(size(det_theta,1),1); % (ndet,1)
   psi=theta;
   dpsi=psi;
   for i=1:size(det_theta,1),
      theta(i,1)=det_theta(i,index(i)); 
      psi(i,1)=det_psi(i,index(i));
      dpsi(i,1)=det_dpsi(i,index(i));
   end
   if spe_data.emode==1,
   
	   % ======================================================================
		% For direct-geometry spectrometers like HET, MARI
		% efixed = monochromatic incident energy ei(meV) 
		% ======================================================================

	   ki=sqrt(spe_data.efixed/2.07);	% scalar
   	de=spe_data.en(2)-spe_data.en(1);	% energy bin width for equally-spaced energy points
   	en=[spe_data.en(1)-de/2 spe_data.en+de/2];	% energy bin boundaries (1,ne+1)
   	kf=sqrt((spe_data.efixed-en)/2.07); % line-vector (1,ne+1), kf bin boundaries
    	Qx=ki*ones(size(theta,1),size(en,2))-cos(theta)*kf; % matrix (ndet,ne+1)
      % === assume detector tube on the Debye-Scherrer cone 
      Qy=-(sin(theta).*cos(psi))*kf+(sin(dpsi/2).*sin(psi))*kf;
   	Qz=-(sin(theta).*sin(psi))*kf-(sin(dpsi/2).*cos(psi))*kf;
   	Q=cat(3,Qx,Qy,Qz);
	elseif spe_data.emode==2,
   
	   % ========================================================================
   	% For indirect-geometry spectrometers like IRIS
		% efixed = monochromatic scattered energy (meV) for a white incident beam  
		% ========================================================================
   
   	de=spe_data.en(2)-spe_data.en(1);	% energy bin width for equally-spaced energy points
   	en=[spe_data.en(1)-de/2 spe_data.en+de/2];	% energy bin boundaries
   	ki=sqrt((spe_data.efixed+en)/2.07); % line-vector (1,ne+1)
   	kf=sqrt(spe_data.efixed/2.07);	% scalar
		Qx=ones(size(theta))*ki-kf*cos(theta)*ones(size(en)); % matrix (ndet,ne+1)
		Qy=-kf*(sin(theta))*ones(size(en));	% (ndet,1)*(1,ne+1)
		Qz=zeros(size(Qx))-kf*(sin(dpsi/2))*ones(size(en));
      Q=cat(3,Qx,Qy,Qz);
  	else
   	disp('Only inelastic direct-geometry (emode=1, HET, MARI)');
   	disp(' or inelastic indirect-geometry (emode=2, IRIS) spectrometer modes available.');
		disp(['emode=' num2str(emode) ' not implemented. Transformation not performed']);
   	Q=[];
   	return
	end
end
%Q(end,1:5,3),