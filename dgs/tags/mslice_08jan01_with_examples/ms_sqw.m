function [s,wdisp]=ms_sqw(icross,parameters,Q,w,theta,ar,br,cr)

% icross=cross-section number 
% parameters=[p1 p2 ...] parameter values in the cross-section
% calculate S(Q,w) for a list of points Q=[h k l] (n,3), w(n) (meV) energy bins 
% theta (n) (rad) total scattering angle 
% ar, br, cr (1,3) (Angs^-1) reciprocal lattice vectors

switch icross,
   case {1},	% '1DAF spin waves with Gaussian broadening in energy'
      % parameters=[amp J x fwhm_sw A_inc fwhm_inc cen bkg]
      
      amp=parameters(1);	   % spin-wave amplitude of gaussian lineshape   
      J=abs(parameters(2));	% exchange J(meV) >0 antiferromagnetic	
      x=parameters(3);    		% x axis index, 1 is a*, 2 is b* ...
      dE=parameters(4)/2.355; % fwhm->variance of Gaussian broadening of spin-wave lineshape [meV]      
      q=Q(:,x);
      cen=parameters(5);
      wdisp=J*abs(sin(2*pi*q));	% dispersion relation
	  s=(1-cos(2*pi*q))./(abs(sin(2*pi*q))+0.001);	% to avoid divergence at small q's
      s=amp/sqrt(2*pi)/dE*s.*exp(-(w-wdisp-cen).^2/(2*dE^2));
      if length(parameters)>=8,
	    A_inc=parameters(5);		% incoherent scattering amplitude
   	    sig_inc=parameters(6)/2.355;	% fwhm -> variance of the incoherent quasielastic scattering [meV]
      	cen=parameters(7);		% centere of incoherent scattering [meV] 
      	bkg=parameters(8);		% flat background    
	    s=s+A_inc/sqrt(2*pi)/sig_inc*exp(-(w-cen).^2/(2*sig_inc^2))+bkg*(1-0.2*rand(size(s)));
      end
      
   case {2}, % '1D spinons'   
      % parameters=[amp J x dummy A_inc fwhm_inc cen bkg]
      % exchange J(meV) >0 antiferromagnetic
      % x axis label, 2 is b*
      
      amp=parameters(1);
      J=parameters(2); % zone boundary energy pi/2*J
      x=parameters(3);
      q=Q(:,x);
      w1=pi/2*J*abs(sin(2*pi*q));
	  wdisp=w1;
	  w2=pi*J*abs(sin(pi*q));
      s=amp*(w>(w1+0.00001)).*(w<=w2).*1./sqrt(w.^2-w1.^2+0.001);
      if length(parameters)>=8,
	    A_inc=parameters(5);		% incoherent scattering amplitude
   	    sig_inc=parameters(6)/2.355;	% fwhm -> variance of the incoherent quasielastic scattering [meV]
      	cen=parameters(7);		% centere of incoherent scattering [meV] 
      	bkg=parameters(8);		% flat background    
      	s=s+A_inc/sqrt(2*pi)/sig_inc*exp(-(w-cen).^2/(2*sig_inc^2))+bkg*(1-0.2*rand(size(s)));   
      end

   case {3}, % 'KCuF3 dispersion'   
      % parameters=[amp J J' dE]
      % exchange intrachain J(meV) >0 antiferromagnetic, interchain J' ferromagnetic      
      amp=parameters(1);
      J=parameters(2);
      Jp=parameters(3);
 	  dE=parameters(4); 
      h=Q(:,1);
      k=Q(:,2);
      l=Q(:,3);
      A=(2*J-4*Jp+2*Jp*(cos(2*pi*h)+cos(2*pi*k)));
      B=2*J*cos(2*pi*l);
      wdisp=sqrt(A.^2-B.^2);
      u=sqrt((A./(wdisp+0.001)+1)/2);
      v=sign(B).*sqrt((A./(wdisp+0.001)-1)/2);
      s=(u-v).^2;
	  s=amp/(sqrt(2*pi)*dE)*s.*exp(-(w-wdisp).^2/(2*dE^2));
      
   case {4}, % 'CFTD dispersion'   
      % parameters=[A J dE]
      % exchange J(meV) >0 antiferromagnetic
      % dE(meV) FWHM Gaussian energy broadening
      amp=parameters(1);
      J=parameters(2);
 	  dE=parameters(3);
      h=Q(:,1);
      k=Q(:,2);
      l=Q(:,3);
      gamma=cos(pi*h).*cos(pi*k);
      wdisp=2*J*sqrt(1-gamma.^2);
      s=(1-gamma)./(wdisp+0.01);
      dE=dE/2.355;	% fwhm->sigma
      s=amp/(sqrt(2*pi)*dE)*s.*exp(-(w-wdisp).^2/(2*dE^2));
      
   case {5}, % 'La2CuO4 dispersion includes anisotropic magnetic form factor'   
      % parameters=[A J Jp dE bkg]
      % nn exchange J(meV) and nnn exchange Jp (J,Jp>0 antiferromagnetic)
      % dE(meV) FWHM Gaussian energy broadening
      amp=parameters(1);
      J=parameters(2);
      Jp=parameters(3);
 	  dE=parameters(4);
      h=Q(:,1);
      k=Q(:,2);
 	  l=Q(:,3);
      gam=cos(pi*h).*cos(pi*l);
      gamp=(cos(2*pi*h)+cos(2*pi*l))/2;
      A=2*J-2*Jp*(1-gamp);
      B=2*J*gam;
      wdisp=sqrt(A.^2-B.^2);
      s=(A-B)./(wdisp+0.01*J);
      dE=dE/2.355;	% fwhm->sigma
      s=parameters(5)+amp/(sqrt(2*pi)*dE)*s.*exp(-(w-wdisp).^2/(2*dE^2)).*...
         (mff_cu3d(h*norm(ar),l*norm(cr),k*norm(br))).^2;      
      
     
   case {6},	% 'spin-waves on the cycloidal structure of Cs2CuCl4, include polarization factor'
      % parameters=[amp J Jp dE]
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy
      % thesis page 85
      amp=parameters(1);
      J=parameters(2);
      Jp=parameters(3);
      dE=parameters(4)/2.355;
      spin=1/2;
      q=acos(Jp/(2*J))/pi;	% cycloidal ordering wavevector (rlu)=0.472 for Jp/J=0.175
      h=Q(:,1);
      k=Q(:,2);
  	   l=Q(:,3);      
      [A,B,C,D]=cs2cucl4(h,k,l,J,Jp);
      w1=sqrt((A+B).^2-(C+D).^2);
      sxx=spin*(A+B+C+D)./(w1+0.001)./(sqrt(2*pi)*dE).*exp(-(w-w1).^2/(2*dE^2));
      [A,B,C,D]=cs2cucl4(h,k-q,l,J,Jp);
      w2m=sqrt((A-B).^2-(C-D).^2);
      seem=spin*(A-B-C+D)./(w2m+0.001)./(sqrt(2*pi)*dE).*exp(-(w-w2m).^2/(2*dE^2));
      [A,B,C,D]=cs2cucl4(h,k+q,l,J,Jp);
      w2p=sqrt((A-B).^2-(C-D).^2);
      seep=spin*(A-B-C+D)./(w2p+0.001)./(sqrt(2*pi)*dE).*exp(-(w-w2p).^2/(2*dE^2));
      Qx=h*norm(ar);
      Qy=k*norm(br);
      Qz=l*norm(cr);
      modQ=sqrt(Qx.^2+Qy.^2+Qz.^2);
      alpha=acos(Qx./modQ); % angle (rad) between Q and a*
      s=amp*((sin(alpha)).^2.*sxx+(1+cos(alpha).^2).*(seep+seem)/4);
      
   case {7},	% 'spin-waves on the cycloidal structure of Cs2CuCl4 wxx only, with polarization factor'
      % parameters=[amp J Jp dE]
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy
      % thesis page 85
      amp=parameters(1);
      J=parameters(2);
      Jp=parameters(3);
      dE=parameters(4)/2.355;
      spin=1/2;
      q=acos(Jp/(2*J))/pi;	% cycloidal ordering wavevector
      h=Q(:,1);
      k=Q(:,2);
  	   l=Q(:,3);
      [A,B,C,D]=cs2cucl4(h,k,l,J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      s=amp*spin*(A+B+C+D)./(wdisp+0.001)./(sqrt(2*pi)*dE).*exp(-(w-wdisp).^2/(2*dE^2));
   %   Qx=h*norm(ar);
   %   Qy=k*norm(br);
   %   Qz=l*norm(cr);
   %   modQ=sqrt(Qx.^2+Qy.^2+Qz.^2);
   %   alpha=acos(abs(Qx)./modQ);
   %   s=(sin(alpha).^2).*sxx;
      
   case {8},	% 'Acoustic phonons in La2CuO4'
      % parameters=[A sig T]
      % === initialize elastic constants [10^10 N/m^2]
      c11=17.47;
		c22=17.30;
		c33=26.62;
		c23=9.91;
		c13=9.28;
		c12=9.0;
		c44=6.53;
		c55=6.69;
		c66=9.92;
      rho=7026;   % for La2CuO4 [kg/m^3]      
      h=Q(:,1);
      k=Q(:,2);
  	   l=Q(:,3);
      A=parameters(1);			% overall amplitude 
      sig=parameters(2)/2.355;	% gaussian energy width - sigma (meV)
      T=parameters(3)/11.605;	% temperature (meV)
      s=zeros(size(Q(:,:,1)));	% final intensities
      wdisp=zeros(size(s));		% final dispersion relation = longitudinal modes 
      for i=1:size(Q,1);,
         for j=1:size(Q,2),
            hh=h(i,j)-2*round(h(i,j)/2);
            kk=k(i,j)-2*round(k(i,j)/2);
            ll=l(i,j)-round(l(i,j));
            q=hh*ar+kk*br+ll*cr;	%  small q (Å^{-1})
            n=q/norm(q);		% direction versor 
            % === construct elastic constants matrix Gamma
   			G11=n(1)^2*c11+n(2)^2*c66+n(3)^2*c55;
   			G22=n(1)^2*c66+n(2)^2*c22+n(3)^2*c44;
   			G33=n(1)^2*c55+n(2)^2*c44+n(3)^2*c33;
   			G23=n(2)*n(3)*(c23+c44);
   			G13=n(1)*n(3)*(c13+c55);
   			G12=n(1)*n(2)*(c12+c66);
   			G=[G11 G12 G13; G12 G22 G23; G13 G23 G33];
            [U,V]=eig(G);	% eigenvalues and eigenvectors 
            U(:,1)=U(:,1)/norm(U(:,1));	% normalise polarisation vectors
            U(:,2)=U(:,2)/norm(U(:,2));
				U(:,3)=U(:,3)/norm(U(:,3));
			   v=sqrt([V(1,1) V(2,2) V(3,3)]/rho)*10^5;	% extract velocities [m/sec]
            v=v*6.56*1e-3;	% (3,1) meV Å
            % === put in the velocities and polarizations by hand 
            %v=[40 20 20];
            %U(:,1)=n(:);	% longitudinal
            %U(:,2)=[0 0 1]';	% transverse out of plane
            %U(:,3)=cross(U(:,1),U(:,2));	% transverse in plane
            wd=v*norm(q);	% (3,1) excitation energies (meV)
            [wdisp(i,j),index]=max(wd(:)');
            wd=-sort(-wd);
            bigQ=h(i,j)*ar+k(i,j)*br+l(i,j)*cr;	% big Q in Å^{-1}
            gss1=1/sqrt(2*pi)/sig*(exp(-(w(i,j)-wd(1))^2)/2/sig^2);
				%gss2=1/sqrt(2*pi)/sig*(exp(-(w(i,j)-wd(2))^2)/2/sig^2);
				%gss3=1/sqrt(2*pi)/sig*(exp(-(w(i,j)-wd(3))^2)/2/sig^2);
            %s(i,j)=gss1+gss2+gss3;
            s(i,j)= A/(exp(wd(1)/T)-1)*dot(bigQ,n)^2/wd(1)*gss1;%+...
            %   	  A/(exp(wd(2)/T)-1)*dot(bigQ,U(:,2))^2/wd(2)*gss2+...
            %		  A/(exp(wd(3)/T)-1)*dot(bigQ,U(:,3))^2/wd(3)*gss3;
         end	
      end   
   otherwise    
	   disp(['Unknown cross-section model ' icross]);
  	 	s=[];
   	wdisp=[];
end