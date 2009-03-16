function v=sqe2proj(Q,en,U1,U2,U3)

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

if nargin==5,	% three viewing axes for (PSD) data 
	UU3=U3/dot(U3,U3);
	v(:,:,3)=Q(:,:,1)*UU3(1)+Q(:,:,2)*UU3(2)+Q(:,:,3)*UU3(3)+E*UU3(4);
end