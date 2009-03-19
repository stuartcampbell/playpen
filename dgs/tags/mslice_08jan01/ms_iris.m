function [y,err]=ms_iris(p,fit_pars,spec_pars,npixels,pixels,efixed,psi_samp,ar,br,cr,ub)

% function [y,err]=ms_iris(p,fit_pars,spec_pars,npixels,pixels,efixed,psi_samp,ar,br,cr,ub)
% y, err arrays same size as npixels
% ar,br,cr (3,1) reciprocal lattice vectors [Angs^-1] ub(3,3)

% === extract fit parameters 
nmc=fit_pars(1);	% number of monto-carlo points
icross=fit_pars(2);	% cross-section number
% === extract spectrometer parameters
factor=2.355;	% conversion factor gaussian fwhm/variance 
th_h=spec_pars(1)/factor;	 % horizontal [fwhm -> variance] [radians] for the incident beam 
th_v=spec_pars(2)/factor;  % vertical [fwhm -> variance][ radians] for the incident beam 
psi_h=spec_pars(3)/factor; % horizontal [fwhm -> variance][ radians] for the scattered beam 
psi_v=spec_pars(4)/factor; % vertical [fwhm -> variance][radians] for the scattered beam 
dt=spec_pars(5);    % time resolution fwhm [s]
L1=36.54;	% incident flight path [m]
vo=sqrt(efixed/5.227)*1e3;	% velocity of analysed neutrons [m/s]

% === simulate scattering in each detector
nbins=length(npixels);	% total number of bins
npixels_total=sum(npixels);	% total number of pixels
y=zeros(size(npixels));	% will have the intensities per bin 
err=y;	% will have the error per bin
ypixel=0; % average of the monte-carlo dsitribution of intensities per each pixel
errpixel=0; % variance of the monte-carlo distribution of intensities per each pixel
energy=pixels(1,:);	% energy value (meV) of each pixel (centre)
energy_bin=pixels(2,:);	% energy bin (meV) per pixel
theta=pixels(3,:);	% total scattering angle  (radians) for each pixel
kix=zeros(nmc,1);	% will contain the x-component of ki for all nmc points for one pixel
kiy=kix;
kiz=kix;
kfx=kix;
kfy=kix;
kfz=kix;
Qx=kix;
Qy=kiy;
Qz=kiz;
thx=kix;	% will contain a mone-carlo distribution of nmc angles in the horizontal plane 
thy=kiy;
th=thx;

bin=1;	% will index the bins
pixels_in_bin=0;	% will count partial number of pixels in each bin
for i=1:npixels_total,
   % === generate a number of uniformly distributed energy sampling points over the energy_bin(i) centred at energy(i)
   energy_centre=energy(i)+(rand(nmc,1)-1/2)*energy_bin(i);	% array (nmc,1) [meV]
   % === calculate energy resulution fwhm [meV] for each of these points (~ constant over the small energy_bin(i) length) 
   de=dt*2*vo/L1*sqrt((energy_centre+efixed).^3/efixed);  % array (nmc,1) [meV]
   % === generate nmc energy points with a normal distribution centred around data.en(j) and fwhm de
   en=energy_centre+(de/factor).*randn(nmc,1); % array (nmc,1)	[meV]
   % === calculate ki and kf for this normal distribution
   ki=sqrt((efixed+en)/2.07); % array (nmc,1)	[Angs^-1]
   kf=sqrt(efixed/2.07);	% scalar 		[Angs^-1]
   % === generate a monte carlo angular distribution for ki
   thx=th_h*randn(nmc,1);
	thy=th_v*randn(nmc,1);
	th=atan(sqrt(tan(thx).^2+tan(thy).^2));
	kiy=ki.*tan(thx).*cos(th);
	kiz=ki.*tan(thy).*cos(th);
   kix=ki.*cos(th);
   % === generate a monte carlo angular distribution for kf
   thx=psi_h*randn(nmc,1);
	thy=psi_v*randn(nmc,1);
	th=atan(sqrt(tan(thx).^2+tan(thy).^2));
	Qy=kf*tan(thx).*cos(th);
	Qz=kf*tan(thy).*cos(th);
   Qx=kf*cos(th);
   % === rotate kf in spectrometer reference frame
   kfx=Qx*cos(theta(i))-Qy*sin(theta(i));
   kfy=Qx*sin(theta(i))+Qy*cos(theta(i));
   kfz=Qz;
   % === calculate wavevector transfer in spectrometer reference frame
   Qx=kix-kfx;
   Qy=kiy-kfy;
   Qz=kiz-kfz;
   % === transform wavevctor transfer in sample reference frame
   kfx=Qx*cos(psi_samp)+Qy*sin(psi_samp);
   kfy=-Qx*sin(psi_samp)+Qy*cos(psi_samp);
   kfz=Qz;
   % === transform in sample reference frame wavevector Q into hkl using ub matrix
   Qx=kfx*ub(1,1)+kfy*ub(1,2)+kfz*ub(1,3);
   Qy=kfx*ub(2,1)+kfy*ub(2,2)+kfz*ub(2,3);
   Qz=kfx*ub(3,1)+kfy*ub(3,2)+kfz*ub(3,3);
   % === calculate monte-carlo intensity distribution for this particular pixel 
   s=ms_sqw_iris(icross,p,[Qx(:)';Qy(:)';Qz(:)']',en,theta(i),ar,br,cr);
   % === calculate average intensity and variance of distribution
   ypixel=mean(s);
   if nmc==1,
      errpixel=0;
   else   % error of the mean = standard deviation in the point distribution /sqrt(nmc points)
      errpixel=sqrt(mean(s.^2)-ypixel^2)*sqrt(nmc/(nmc-1))/sqrt(nmc);
   end
   % === add pixel to the current bin
   pixels_in_bin=pixels_in_bin+1;   
   y(bin)=y(bin)+ypixel;
   err(bin)=err(bin)+errpixel^2;
   % === calculate avearage per bin and move on to the next bin 
   % === if maximum number of pixels in that bin was reached by this latest pixel
   if pixels_in_bin==npixels(bin),     
      y(bin)=y(bin)/pixels_in_bin;
      err(bin)=sqrt(err(bin))/pixels_in_bin;
      if bin<nbins,
	      bin=bin+1;
         pixels_in_bin=0;
      end
   end   
end

% === if convergence level less than a threshold value suggest increase of NMC points 
index=(abs(y)>eps);	% select non-zero y-values
if sum(index)~=0,
	err_rel=abs(err(index)./y(index));
	if max(err_rel)>0.02,
   	disp(sprintf('Convergence level achieved : %8.2g %%',100*max(err_rel)));
	end
end