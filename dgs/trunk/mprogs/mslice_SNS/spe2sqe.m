function Q=spe2sqe(spe_data)

% function Q=spe2sqe(spe_data)
% fields required of spe_data: emode, efixed[meV], en(1,ne)[meV], det_theta(ndet,1)[rad], 
%                   det_psi(ndet,1)[rad](not necessary for IRIS)
% returns : Q(ndet,ne,3)[Å^{-1}]
% emode=1 if efixed=ei (HET and MARI) and emode=2 if efixed=ef (IRIS)
% transforms data points in the form (detector_angles,energy) -> (Qx,Qy,Qz,energy)
% in the spectrometer frame where x=[1 0 0] along +k_i, [0 1 0] in the horiz scattering plane 
% and z=[0 0 1] \perp to the horizontal plane
% 11-sep-98 modify const E=2.07k^2 to compare results with PHOENIX, exact match
% restore E/k^2=2.072

if spe_data.emode==1,
   
   % ======================================================================
	% For direct-geometry spectrometers like HET, MARI
	% efixed = monochromatic incident energy ei(meV) 
	% ======================================================================

   ki=sqrt(spe_data.efixed/2.07);	% scalar
	kf=sqrt((spe_data.efixed-spe_data.en)/2.07); % line-vector (1,ne)
   Qx=ki*ones(size(spe_data.det_theta,1),size(spe_data.en,2))...
      -cos(spe_data.det_theta)*kf; % matrix (ndet,ne)
   if ~isfield(spe_data,'det_psi'),
      data.det_psi=zeros(size(spe_data.det_theta));
      disp(['Assume psi=0 for all detectors']);
   end
   Qy=-(sin(spe_data.det_theta).*cos(spe_data.det_psi))*kf;
	Qz=-(sin(spe_data.det_theta).*sin(spe_data.det_psi))*kf;
	Q=cat(3,Qx,Qy,Qz);
   
elseif spe_data.emode==2,
   
   % ========================================================================
   % For indirect-geometry spectrometers like IRIS
	% efixed = monochromatic scattered energy (meV) for a white incident beam  
   % here all detectors are in the horizontal plane with Psi=0
   % scattering geometry diagram in notebook computing 2, page 2-14
   % ========================================================================
   
   ki=sqrt((spe_data.efixed+spe_data.en)/2.07); % line-vector (1,ne)
   kf=sqrt(spe_data.efixed/2.07);	% scalar
	Qx=ones(size(spe_data.det_theta))*ki-kf*cos(spe_data.det_theta)*ones(size(spe_data.en)); % matrix (ndet,ne)
	Qy=-kf*(sin(spe_data.det_theta))*ones(size(spe_data.en));	% (ndet,1)*(1,ne)
	Qz=zeros(size(Qx));
	Q=cat(3,Qx,Qy,Qz);

else
   disp('Only inelastic direct-geometry (emode=1, HET, MARI)');
   disp(' or inelastic indirect-geometry (emode=2, IRIS) spectrometer modes available.');
	disp(['emode=' num2str(emode) ' not implemented. Transformation not performed']);
   Q=[];
   return
end

