function [A,B,C]=basis_hkl(ar,br,cr)

% function [A,B,C]=basis_hkl(ar,br,cr)
% requires vectors (1,3) ar,br,cr of the reciprocal lattice
% returns vectors A,B,C such that if Q=h*ar+k*br+l*cr (with h,k,l in rlu)
% then h=dot(Q,A), k=dot(Q,B), l=dot(Q,C)
% Radu Coldea 02-Oct-1999

if ~exist('ar','var')|isempty(ar)|(size(ar,1)~=1)|(size(ar,2)~=3)|...
      ~exist('br','var')|isempty(br)|(size(br,1)~=1)|(size(br,2)~=3)|...
      ~exist('cr','var')|isempty(cr)|(size(cr,1)~=1)|(size(cr,2)~=3),
   disp('Wrong syntax. Basis to hkl transformation not executed.');
   disp('Check dimensions of input reciprocal lattice vectors.');
   A=[];
   B=[];
   C=[];
   help basis_hkl;
   return;
end
	   
t=cross(cross(ar,br),cross(cr,br));
A=cross(br,cross(cross(cr,br),t))/norm(t)^2;

t=cross(cross(br,ar),cross(cr,ar));
B=cross(ar,cross(cross(cr,ar),t))/norm(t)^2;

t=cross(cross(cr,ar),cross(br,ar));
C=cross(ar,cross(cross(br,ar),t))/norm(t)^2;
