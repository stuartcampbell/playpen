function u=pick_wv(x,n,psi_samp,u,ar,br,cr)

% function u=pick_wv(x,n,psi_samp,ar,br,cr)
% choose wavevector direction from Qx,Qy,Qz,H,K,L,u1,u2 (u3 if PSD)
% n = total number of wavevector axes 
% psi_samp = angle (radians)
% u=(2,4) ((3,4)if PSD) with the viewing axes
% ar,br,cr (1,3) reciprocal lattice vectors (Angs^{-1})

if n==9,	% single crystal and PSD detectors
   viewing_axes={7,8,9};
elseif n==8,	% single crystal and conventional detectors
   viewing_axes={7,8};
elseif n==6,	% single crystal and powder analysis mode
   viewing_axes={};
else
   disp(['Unknown mode with ' num2str(n) ' wavevector directions']);
   u=[];
   return;
end

switch x,
	case {1},	% Qx (Å^{-1})
   	u=sqe2samp([1 0 0],psi_samp);
	case {2},	% Qy (Å^{-1})
   	u=sqe2samp([0 1 0],psi_samp);
	case {3},	% Qz (Å^{-1})
   	u=sqe2samp([0 0 1],psi_samp);
	case {4},	% H (rlu)
   	u=ar;
	case {5},   % K (rlu)  
   	u=br;
	case {6},	% L (rlu)
      u=cr;
   case viewing_axes,
      u=u(x-6,:);
      % check if u is a true wavevector direction
      if any(u(1:3)~=0)&(u(4)==0),
         u=u(1)*ar+u(2)*br+u(3)*cr;
      else
         u=[];
      end
   otherwise,
%      disp(['Could not identify wavevector direction with index ' num2str(x) ' out of ' num2str(n) ' possibilities.']);
      u=[];
      return;
end
   
   
   