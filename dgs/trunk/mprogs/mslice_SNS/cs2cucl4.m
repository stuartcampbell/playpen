function [A,B,C,D]=cs2cucl4(h,k,l,J,Jp)

% calculates the A,B, C and D Hamiltonian parameters for 
% a cyloidal dispersion relation appropriate for Cs2CuCl4 
% formulae from thesis page 85

gam=cos(2*pi*k);
gamp=cos(pi*k).*cos(pi*l);
q=acos(Jp/(2*J))/pi;	% cycloidal ordering wavevector
gamp_kmq=cos(pi*(k-q)).*cos(pi*l);
gamp_kpq=cos(pi*(k+q)).*cos(pi*l);
theta=2*pi*q;
spin=1/2;
A=-spin*J*(2*cos(theta)-(cos(theta)+1)*gam)+4*spin*Jp*cos(theta/2);
B=2*spin*Jp*(gamp-(gamp_kpq+gamp_kmq)/2);
C=spin*J*(cos(theta)-1)*gam;
D=-2*spin*Jp*(gamp+(gamp_kpq+gamp_kmq)/2);
