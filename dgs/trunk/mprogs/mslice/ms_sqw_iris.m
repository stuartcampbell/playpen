function [s,wdisp]=ms_sqw_iris(icross,p,Q,en,twotheta,ar,br,cr)

% function [s,wdisp]=ms_sqw_iris(icross,p,Q,en,twotheta,ar,br,cr)
% calculate S(Q,w) for a grid Q=[h(:)';k(:)';l(:)']' (n,3) and en(n,1), scattering angles twotheta(n,1)
% for a single crystal with reciprocal lattice vectors ar,br,cr (3,1)

eps=1e-5;		% numbers smaller than eps are considered zero 
spin=1/2;
t=[0.0232 34.969 0.4023 11.564 0.5882 3.843 -0.0137]; % tabled values to calculate Cu2+ form factor (isotropic 3d^9)

% === initialise parameters and calculate bose factor, form factor, polarisation factor 
A_sw=p(1);	  	% amplitude (intensity)   
J=abs(p(2));	% energy scale (meV), intrachain exchange inetraction in Cs2CuCl4 
dE=p(3)/2.355; % fwhm->variance of Gaussian broadening in energy (meV)
Jp=p(4)*J;		% interchain coupling (meV) 
A_inc=p(5);		% incoherent scattering amplitude
sig_inc=p(6)/2.355;	% fwhm -> variance of the incoherent quasielastic scattering (meV)
cen=p(7);		% centre of incoherent scattering (meV) 
temp=p(8)/11.605;	% <temp> is temperature (K -> meV)
q=acos(Jp/(2*J))/pi;	% incommensurate ordering wavevector (rlu along b* in Cs2CuCl4)
deltaq=0.5-q	; % incommensurate displacement (rlu b*)

% === calculate Bose factor
bose=ones(size(en));	% bose=n(en)+1=1 for en=0
index=(abs(en)>eps); % indexes finite energy points
bose(index)=1./(1-exp(-en(index)/temp));
      
% === calculate magnetic form factor for Cu2+ ions (spherical approximation)
modQ=Q(:,1)*ar+Q(:,2)*br+Q(:,3)*cr;	% (n,3) (i,1)=Qx (i,2)=Qy ... along a rectangular coordinate system
modQ=sqrt(modQ(:,1).^2+modQ(:,2).^2+modQ(:,3).^2);	% |Q|(n,1) (Angs^-1)
temp=modQ/(4*pi);
fQ=t(1)*exp(-t(2)*temp.^2)+t(3)*exp(-t(4)*temp.^2)+t(5)*exp(-t(6)*temp.^2)+t(7);

% === calculate polarization factor
alpha=acos(Q(:,1)*norm(ar)./modQ); % angle [radians] between Q and the a* axis 

switch icross,
	case{1},	
      % ==========================================================================
      % icross=1 % non-dispersive mode 
      % dE (meV) FWHM Gaussian broadening in energy 
      % parameters=
      % 1 A_sw      amplitude factor
      % 2 J (meV)   non-dispersive excitation energy 
      % 3 fwhm_sw   (meV) excitation intrinsic linewidth
      % 4 unused
      % 5 A_inc     incoherent quasielastic amplitude
      % 6 fwhm_inc  (meV) quasielastic width
      % 7 cen (meV) quasielastic peak centre
      % 8 T (K)     temperature to calculate Bose factor (n(w)+1)
      % 9 Const_bkg flat background 
      % 10 Amp_bkg amplitude in an exponetially decreasing background term A*e-(x/tau)
      % 11 Tau_bkg decay rate of the exponentially decreasing background units of xaxis
      % 12 unused
      % 13 unused
      % 14 unused
      % 15 =1 if the exponentially decaying bkg is a function of energy, and otherwise if a function of Qy
      
      temp=A_sw/sqrt(2*pi)/dE*exp(-(en-J-cen).^2/(2*dE^2));
      
   case {2},
      % thesis page 85
      % ==========================================================================
      % icross=2 % isotropic spin waves with absolute amplitudes 
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_sw  spin-wave amplitude factor
      % 2 J (meV) intra-chain antiferromagnetic 
      % 3 fwhm_sw (meV) spin-wave linewidth
      % 4 Jp/J (adimensional) interchain coupling if =0 then simple antiferromagnetic spin waves
      % 5 A_inc incoherent quasielastic amplitude
      % 6 fwhm_inc (meV) quasielastic width
      % 7 cen (meV) quasielastic peak centre
      % 8 T (K) temperature to calculate Bose factor (n(w)+1)
      % 9 Const_bkg flat background 
      % 10 Amp_bkg amplitude in an exponetially decreasing background term A*e-(x/tau)
      % 11 Tau_bkg decay rate of the exponentially decreasing background units of xaxis
      % 12 A_sw_m amplitude factor for w(Q-q) mode
      % 13 A_sw_p amplitude factor for w(Q+q) mode
      % 14 unused
      % 15 =1 if the exponentially decaying bkg is a function of energy, and otherwise if a function of Qy                     
      
      % === calculate dispersion relation 
      %gam=cos(2*pi*Q(:,2));
		%gamp=cos(pi*Q(:,2)).*cos(pi*Q(:,3));
		%gamp_kmq=cos(pi*(Q(:,2)-q)).*cos(pi*Q(:,3));
		%gamp_kpq=cos(pi*(Q(:,2)+q)).*cos(pi*Q(:,3));
		%theta=2*pi*q;
		%A=-spin*J*(2*cos(theta)-(cos(theta)+1)*gam)+4*spin*Jp*cos(theta/2);
		%B=2*spin*Jp*(gamp-(gamp_kpq+gamp_kmq)/2);
		%C=spin*J*(cos(theta)-1)*gam;
		%D=-2*spin*Jp*(gamp+(gamp_kpq+gamp_kmq)/2);
      
      % === calculate dispersion relation wQ_{xx} do not include polarization fcator
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      temp=A_sw*spin*(A+B+C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarization factor       
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         p(12)*spin*(A-B-C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarization factor       
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         p(13)*spin*(A-B-C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
            
   case {3},
      % ==========================================================================
      % icross=3 polarised spin-waves with relative amplitudes
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_sw 
      % 2 J (meV)
      % 3 fwhm_sw (meV)
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_sw_m (relative) amplitude factor for the w(Q-q) relative to w(Q) branch =1 in cycloidal model
      % 13 A_sw_p (relative) amplitude factor for the w(Q+q) relative to w(Q) branch =1 in cycloidal model
      % 14 unused
      % 15 =1 if bkg(energy), otherwise bkg(Qy)A_sw=p(1);	  	
            
      % === calculate dispersion relation for S_{xx}	: w1(Q) include polarisation factor (sin(alpha)^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      temp=A_sw*spin*(sin(alpha).^2).*(A+B+C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         A_sw*p(12)*spin/4*(1+cos(alpha).^2).*(A-B-C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         A_sw*p(13)*spin/4*(1+cos(alpha).^2).*(A-B-C+D)./(wdisp+dE/100)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
   case {4},	
      % =============================================================================================
      % icross=4 % 3 isotropic Muller ansatz continua with absolute amplitudes and mean upper bound 
      % lower boundaries given by cycloidal spin wave dispersions 
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the Q continuum in {xx} 
      % 2 J (meV)
      % 3 dE (meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_sw_m (absolute) amplitude factor for (Q-q) continuum in {yy} and {zz}
      % 13 A_sw_p (absolute) amplitude factor for (Q+q) continuum in {yy} and {zz}
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)      
      
		% === calculate mean zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      [A,B,C,D]=cs2cucl4(0,3/4,0,J,Jp);
      w2=sqrt((A+B)^2-(C+D)^2);   % lower zone boundary energy k=3/4
      w2=(w1+w2)*p(14)*abs(sin(pi*Q(:,2)));   % upper bound of the continuum assumed the same for all modes 
      % take as the continuum upper boundary the Muller boundary for the average zone boundary energy

		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      %heaviside_l=(atan((en-cen-wdisp)/(dE/10000))/(pi/2)+1)/2;	% =1 for en-wdisp > dE/100 and =0 otherwise
      %heaviside_u=(atan((w2-(en-cen))/(dE/10000))/(pi/2)+1)/2;	% =1 for w2-en > dE/100 and =0 otherwise
 		%s=((en-cen)>=(wdisp+dE/100)).*((en-cen)<=w2).*A_sw.*spin./sqrt(abs((en-cen).^2-wdisp.^2)+(dE/100)^2);
      %s=A_sw*spin./sqrt(abs((en-cen).^2-wdisp.^2)+(dE/1000)^2).*heaviside_l.*heaviside_u;
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+p(12)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+p(13)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {5},
      % ============================================================================================
      % icross=5 % 3 polarised Muller ansatz continua with relative amplitudes and mean upper bound 
      % lower boundaries given by cycloidal spin wave dispersions 
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_sw 
      % 2 J (meV)
      % 3 fwhm_sw (meV)
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg 
      % 12 A_sw_m (relative) amplitude factor for the (Q-q) continuum relative to {xx} continuum 
      % 13 A_sw_p (relative) amplitude factor for the (Q+q) continuum relative to {xx} continuum 
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
		% === calculate mean zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      [A,B,C,D]=cs2cucl4(0,3/4,0,J,Jp);
      w2=sqrt((A+B)^2-(C+D)^2);   % lower zone boundary energy k=3/4
      w2=(w1+w2)*p(14)*abs(sin(pi*Q(:,2)));   % upper bound of the continuum assumed the same for all modes 
      % take as the continuum upper boundary the Muller boundary for the average zone boundary energy

		% === calculate dispersion relation for S_{xx}	: w1(Q) and include polarization factor (sin(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin*(sin(alpha).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(12)*spin*(1+cos(alpha).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(13)*spin*(1+cos(alpha).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {6},
      % ==========================================================================
      % icross=6 % spin-wave modes at low energies {yy} and {zz} (includes polarization factor) 
      %     + isotropic Muller ansatz for a continuum  at higher energies with mean upper boundary
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_sw amplitude of the continuum scattering 
      % 2 J (meV)
      % 3 fwhm_sw (meV)
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_sw_m absolute amplitude factor for the w(Q-q)
      % 13 A_sw_p absolute amplitude factor for the w(Q+q)
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
		% === calculate mean zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      [A,B,C,D]=cs2cucl4(0,3/4,0,J,Jp);
      w2=sqrt((A+B)^2-(C+D)^2);   % lower zone boundary energy k=3/4
      w2=(w1+w2)*p(14)*abs(sin(pi*Q(:,2)));   % upper bound of the continuum assumed the same for all modes 
      % take as the continuum upper boundary the Muller boundary for the average zone boundary energy

		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         p(12)*spin/4*(1+cos(alpha).^2).*(A-B-C+D)./(wdisp+dE/1000)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      temp=temp+...
         p(13)*spin/4*(1+cos(alpha).^2).*(A-B-C+D)./(wdisp+dE/100)./...
         (sqrt(2*pi)*dE).*exp(-(en-wdisp-cen).^2/(2*dE^2));
      
   case {7},
      % ==================================================================================
      % icross=7 % 3 isotropic spinon continua with Muller ansatz and relative amplitudes
      % lower boundaries given by cycloidal spin wave dispersions
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % upper boundary of the continuum given by the mean zone boundary energy
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the continuum polarized along {xx} 
      % 2 J (meV)
      % 3 dE(meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_yy_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      % 13 A_yy_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
		% === calculate mean zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      [A,B,C,D]=cs2cucl4(0,3/4,0,J,Jp);
      w2=sqrt((A+B)^2-(C+D)^2);   % lower zone boundary energy k=3/4
      w2=(w1+w2)*p(14)*abs(sin(pi*Q(:,2)));   % upper bound of the continuum assumed the same for all modes 
      % take as the continuum upper boundary the Muller boundary for the average zone boundary energy

		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(12)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(13)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {8},
      % ===================================================================================================
      % icross=8 % 3 isotropic continuam with Muller ansatz lineshape and relative amplitudes 
      % lower boundaries given by cycloidal spin wave dispersions 
      % upper boundary of the continuum is at the Muller ansatz for the maximum zone boundary energy
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the continuum polarized along {xx} 
      % 2 J (meV)
      % 3 dE(meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_yy_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      % 13 A_yy_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the maximum zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
      % === calculate maximum zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      w2=2*w2*p(14)*abs(sin(pi*Q(:,2)));  % upper bound of the continuum assumed the same for all modes 
      % take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy 
      
		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(12)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(13)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {9},
      % ============================================================================================
      % icross=9 % 3 isotropic spinon continua with Muller ansatz lineshape and relative amplitudes 
      % lower boundaries given by cycloidal spin wave dispersion 
      % upper bound modulated to mirror the lower-boundary dispersion
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the continuum polarized along {xx} 
      % 2 J (meV)
      % 3 dE(meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_yy_m_r (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} relative to A_xx 
      % 13 A_yy_p_r (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} relative to A_xx
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated boundary
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
      % === calculate maximum zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      w2=2*w1*p(14)*abs(sin(pi*Q(:,2)));  % upper bound of the continuum  
      % take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy 
      
		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1-2*deltaq)); % <index> redefined to index points in interval [0,1-2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1-2*deltaq))); % shorter period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1+2*deltaq))); % longer period      
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(12)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1+2*deltaq)); % <index> redefined to index points in interval [0,1+2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1+2*deltaq))); % longer period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1-2*deltaq))); % shorter period
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(13)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {10},
      % =============================================================================================
      % icross=10 % 3 isotropic spinon continua with Muller ansatz lineshape and absolute amplitudes
      % lower boundaries given by cycloidal spin wave dispersions and modulated upper bound
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the continuum polarized along {xx} 
      % 2 J (meV)
      % 3 dE(meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_yy_m absolute amplitude factor for (Q-q) continuum in {yy} and {zz} 
      % 13 A_yy_p absolute amplitude factor for (Q+q) continuum in {yy} and {zz} 
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the Muller ansatz for the mean zb energy
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
      % === calculate maximum zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      w2=2*w1*p(14)*abs(sin(pi*Q(:,2)));  % upper bound of the continuum  
      % take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy 
      
		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1-2*deltaq)); % <index> redefined to index points in interval [0,1-2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1-2*deltaq))); % shorter period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1+2*deltaq))); % longer period      
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+p(12)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) do not include polarisation factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1+2*deltaq)); % <index> redefined to index points in interval [0,1+2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1+2*deltaq))); % longer period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1-2*deltaq))); % shorter period
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+p(13)*spin./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
   case {11},
      % ============================================================================================
      % icross=11 % 3 polarised spinon continua with Muller ansatz lineshape and relative amplitudes
      % lower boundaries given by cycloidal spin wave dispersions and modulated upper bound
      % intrachain J(meV) and interchain Jp(meV) exchanges (>0 antiferromagnetic)
      % dE (meV) FWHM Gaussian broadening in energy thesis page 85
      % parameters=
      % 1 A_xx amplitude for the continuum polarized along {xx} 
      % 2 J (meV)
      % 3 dE(meV) rounding of the low-boundary divergence
      % 4 Jp/J (adimensional)
      % 5 A_inc 
      % 6 fwhm_inc (meV)
      % 7 cen (meV)
      % 8 T (K)
      % 9 Const_bkg 
      % 10 Amp_bkg 
      % 11 Tau_bkg
      % 12 A_yy_m (relative) amplitude factor for (Q-q) continuum in {yy} and {zz} 
      % 13 A_yy_p (relative) amplitude factor for (Q+q) continuum in {yy} and {zz} 
      % 14 UBoundScale =1 if upper boundary of the continuum coincides with the modulated upper bound
      % 15 =1 if bkg(energy), otherwise bkg(Qy)
      
      % === calculate maximum zone boundary energy
      [A,B,C,D]=cs2cucl4(0,1/4,0,J,Jp);
      w1=sqrt((A+B)^2-(C+D)^2);   % upper zone boundary energy k=1/4 
      w2=2*w1*p(14)*abs(sin(pi*Q(:,2)));  % upper bound of the continuum  
      % take as the continuum upper boundary the Muller boundary for the maximum zone boundary energy 
      
		% === calculate dispersion relation for S_{xx}	: w1(Q) and do not include polarization factor
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2),Q(:,3),J,Jp);
      wdisp=sqrt((A+B).^2-(C+D).^2);
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% index points inside the continuum (between lower and upper boundaries)
      temp=zeros(size(wdisp));
      temp(index)=A_sw*spin*(sin(alpha(index)).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2);
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q-q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)-q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1-2*deltaq)); % <index> redefined to index points in interval [0,1-2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1-2*deltaq))); % shorter period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1+2*deltaq))); % longer period      
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(12)*spin*(1+cos(alpha(index)).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2)/2; 
      % 13May00 add final factor/2 to have same total intensity in each channel as per icross=9
      
      % === calculate dispersion relation for S_{yy} and S_{zz} : w2(Q+q) include polarisation factor (1+cos(alpha).^2)
      [A,B,C,D]=cs2cucl4(Q(:,1),Q(:,2)+q,Q(:,3),J,Jp);
      wdisp=sqrt((A-B).^2-(C-D).^2);
      w2=Q(:,2)-floor(Q(:,2)/2)*2;	% reduce k to interval [0,2)      
      index=(w2>0)&(w2<(1+2*deltaq)); % <index> redefined to index points in interval [0,1+2*deltaq)
      w2(index)=2*w1*p(14)*abs(sin(w2(index)*pi/(1+2*deltaq))); % longer period
      w2(~index)=2*w1*p(14)*abs(sin((2-w2(~index))*pi/(1-2*deltaq))); % shorter period
      index=((en-cen)>=wdisp)&((en-cen)<=w2);	% <index> redefined to index points inside the continuum (between lower and upper boundaries)
      temp(index)=temp(index)+A_sw*p(13)*spin*(1+cos(alpha(index)).^2)./...
         sqrt(abs((en(index)-cen).^2-wdisp(index).^2)+(dE*2.355)^2)/2;
      
   case {15}, 	% test |q| and energy resolution
      % parameters =
      % 1 Amplitude 
      % 2 q1_centre 	(Angs^-1)
      % 3 w1_centre 	(meV)
      % 4 FWHM_q1		(Angs^-1)
      % 5 FWHM_w1 	(meV)
      % 6 q2_centre 	(Angs^-1)
      % 7 w2_centre 	(meV)
      % 8 FWHM_q2		(Angs^-1)
      % 9 FWHM_w2 	(meV)
      %10 Flat Background
     
      q=Q(:,1)*ar(:)'+Q(:,2)*br(:)'+Q(:,3)*cr(:)';	% [QX QY QZ] (n,1) along a rectangular coordinate system
      q=sqrt(q(:,1).^2+q(:,2).^2+q(:,3).^2);	% wavevector transfer |q| in Angs^-1
      sig_q1=p(4)/2.355;	% fwhm -> variance of q1 in Angs^-1
      sig_w1=p(5)/2.355;	% fwhm -> variance of w1 in meV
      sig_q2=p(8)/2.355;	% fwhm -> variance of q2 in Angs^-1
      sig_w2=p(9)/2.355;	% fwhm -> variance of w2 in meV
      % cross section is a double normalised gaussian in |q| and energy 
      s=p(1)/(2*pi)/sig_q1/sig_w1*exp(-0.5*((q-p(2))/sig_q1).^2-0.5*((w-p(3))/sig_w1).^2)+...
        p(1)/(2*pi)/sig_q2/sig_w2*exp(-0.5*((q-p(6))/sig_q2).^2-0.5*((w-p(7))/sig_w2).^2)+p(10);
   otherwise      
      s=[];
      wdisp=[];
      disp('Unknown cross-section model. Return');
end

switch icross,
	case {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15},	
		% === multiply by the form factor squared, Bose factor 
		% === and add incoherent quasielastic scattering plus background model
      temp=(fQ.^2).*bose.*temp+...
         A_inc/sqrt(2.0d0*pi)/sig_inc*exp(-(en-cen).^2/(2*sig_inc^2))+p(9);
      if (p(15)==1),
     		s=temp+p(10)*exp(-en/p(11)); % for bkg(energy)      
   	else
   		s=temp+p(10)*exp(-Q(:,2)/p(11)); % for bkg(k)
      end
   otherwise,      
end