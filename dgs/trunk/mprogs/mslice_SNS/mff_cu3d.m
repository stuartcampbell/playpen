function fQ=mff_cu3d(Qx,Qy,Qz)

% calculates anisotropic form factor for a Cu2+ ion in a 3d (x^2-y^2) symmetry
% z axis perp to the symmetry plane (x,y)
% Qx, Qy, Qz matrices with wavevector components in Å^{-1}

modQ=sqrt(Qx.^2+Qy.^2+Qz.^2);
beta=acos(Qz./modQ);

t=[0.0232 34.969 0.4023 11.564 0.5882 3.843 -0.0137;
   1.5189 10.478 1.1512 3.813  0.2918 1.398  0.0017;
  -0.3914 14.740 0.1275 3.384  0.2548 1.255  0.0103];
s=modQ/(4*pi);
j0=t(1,1)*exp(-t(1,2)*s.^2)+t(1,3)*exp(-t(1,4)*s.^2)+t(1,5)*exp(-t(1,6)*s.^2)+t(1,7);
j2=(t(2,1)*exp(-t(2,2)*s.^2)+t(2,3)*exp(-t(2,4)*s.^2)+t(2,5)*exp(-t(2,6)*s.^2)+t(2,7)).*s.^2;
j4=(t(3,1)*exp(-t(3,2)*s.^2)+t(3,3)*exp(-t(3,4)*s.^2)+t(3,5)*exp(-t(3,6)*s.^2)+t(3,7)).*s.^2;
fQ=j0-5/7*(1-3*cos(beta).^2).*j2+9/56*(1-10*cos(beta).^2+35/3*cos(beta).^4).*j4;
