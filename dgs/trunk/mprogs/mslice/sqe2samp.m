function Q_samp=sqe2samp(Q,psi_samp)

% function   Q_samp=sqe2samp(Q,psi_samp)
% Q = (ndet,ne,3) or (n,3) or [Qx Qy Qz] ( units Å^{-1}), 
% Q_samp same size as Q,  psi_samp = scalar (radians)
% transforms (Qx,Qy,Qz,E) from the spectrometer reference frame to
% (Q_samp,E)=(Qx',Qy',Qz',E) in the sample reference frame
% (Qx||k_i,Qy) define principal scattering plane (where Azimuth angle of detectors in .phx file is Psi=0 )

R=[cos(psi_samp) sin(psi_samp) 0; -sin(psi_samp) cos(psi_samp) 0; 0 0 1]; 
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
   disp(['Call to sqe2samp(Q,psi_samp) with ndims(Q)=' num2str(ndims(Q)) ' not executed']);
   Q_samp=[];
   return;
end   
   
