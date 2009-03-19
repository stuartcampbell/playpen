function U=basis_u(u,ar,br,cr)

% function U=basis_u(u,ar,br,cr)
% u (1,4)[adimensional], ar, br, cr (1,3)[Å^{-1}]
% returns 4-component line vector U in the 
% 4-dimensional wavevector and energy space
% ex: u1=[1 0 0 0]; u2=[0 0 1 0]; u3=[0 0 0 1]; % x,z and energy
%     ar=(2*pi/5.4165)*[1 0 0]; br=(2*pi/13.3685)*[0 1 0]; cr=(2*pi/5.4165;)*[0 0 1]; 
%     U1=[1.16 0 0 0] [Å^{-1}]; U1=[0 0 1.16 0] [Å^{-1}]; U1=[0 0 0 1] [meV] 

% see logbook Computing 2-page 32 

U=u(1)*[ar 0]+u(2)*[br 0]+u(3)*[cr 0]+[0 0 0 u(4)];

