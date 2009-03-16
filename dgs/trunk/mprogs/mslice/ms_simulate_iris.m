function ms_simulate_iris(emin,emax,deltae,icross,parameters)

% function ms_simulate(emin,emax,deltae,icross,parameters)
% read spectrometer and single crystal/powder information from MSlice Control Window
% and simulate scattering, put data back into the ControlWindow
% is setup for single crystal samples (07-Jan-1999)
% icross= cross-section number = 1,2,..
% parameters=[p1 p2 ...] cross-section parameters 

% === return if MSlice ControlWindow not active
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp(['MSlice Control Window has to be opened first before simulation can begun.']);
   return
end

% === return if detector information not loaded yet
data=fromwindow;
if isempty(data),
   disp(['Need to load detector information in a .phx file first.']);
   return;
end
ms_calc_proj;
data=fromwindow;

% === generate energy grid
data.en=emin:deltae:emax;
data.en=data.en(1:(end-1))+deltae/2; % choose midpoints per each bin

% === simulate scattering in each detector
th_h=parameters(1)*pi/180;	 % horizontal fwhm [degrees -> radians] for the incident beam 
th_v=parameters(2)*pi/180;  % vertical fwhm [degrees -> radians] for the incident beam 
psi_h=parameters(3)*pi/180; % horizontal fwhm [degrees -> radians] for the scattered beam 
psi_v=parameters(4)*pi/180; % vertical fwhm [degrees -> radians] for the scattered beam 
dt=parameters(5)*1e-6;    % time resolution fwhm [\mu s -> s]
nmc=parameters(6);	% number of monte carlo points for each pixel
L1=36.54;	% incident flight path [m]
L2=1.47; 	% scattered flight path [m]
qo=0.941;	% analyser wavevector [Angs^-1]
wo=1.836;	% analyser energy [meV]
vo=592.75;	% velocity of analysed neutrons [m/s]
factor=2.355;	% conversion factor gaussian fwhm/variance 

de=dt*2*vo/L1*sqrt((data.en+wo).^3/wo);	% energy resulution fwhm [meV]
ndet=length(data.det_theta);
ne=length(data.en);
data.S=zeros(ndet,ne);
data.ERR=zeros(ndet,ne);
for i=1:ndet,
   for j=1:ne,
      % generate nmc energy points with a normal distribution centred around data.en(j) and fwhm de
      en=data.en(j)+de(j)/factor*randn(nmc,1); % array (nmc,1)	[meV]
      ki=sqrt((data.efixed+en)/2.07); % array (nmc,1)	[Angs^-1]
      kf=sqrt(data.efixed/2.07);	% scalar 		[Angs^-1]
      [ky,kz,kx]=spherical2d(th_h,th_v,nmc);
      kix=ki.*kx;	% [Angs^-1]
      kiy=ki.*ky;	% horizontal spread in ki values	[Angs^-1]
      kiz=ki.*kz;	% vertical spread in ki values
      [ky,kz,kx]=spherical2d(psi_h,psi_v,nmc);
      kx=kf*kx;
      ky=kf*ky;	% horizontal spread in kf values
      kz=kf*kz;	% vertical spread in kf values
      kfx=kx*cos(data.det_theta(i))+ky*sin(data.det_theta(i));
      kfy=-kx*sin(data.det_theta(i))+ky*cos(data.det_theta(i));
      kfz=kz;
      % calculate wavevector transfer 
      Qx=kix-kfx;
      Qy=kiy-kfy;
      Qz=kiz-kfz;
      Q=sqe2samp(cat(2,Qx(:),Qy(:),Qz(:)),data.psi_samp); %(nmc,3)
      Q=q2rlu(Q,data.ar,data.br,data.cr);
      theta=data.det_theta(i)*ones(nmc,1); % (nmc,1) 
      s=ms_sqw(icross,parameters(7:end),Q,en(:),theta(:),data.ar,data.br,data.cr);
      if isempty(data.S),
         disp(['Cross-section type ' crosssection ' not defined.']);
         return
      end
      data.S(i,j)=mean(s);
      data.ERR(i,j)=1/sqrt(nmc)*data.S(i,j);
   end
end
data.filename=['cross=' num2str(crossection) ', p=['];
for i=1:length(parameters),
   data.filename=[data.filename sprintf(' %g,',parameters(i))];
end
data.filename=[data.filename(1:(end-1)) ']'];
towindow(data);
ms_calc_proj;
