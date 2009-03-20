function spurion(data)

% function spurion
% reads position of a spurion peak from a (detector,energy) plot on IRIS
% and calculates conditions that it corresponds to 
% strong Bragg scattering on the crystal and 
% incoherent quasielastic scattering on the analysercrystal, rather than
% inelastic scattering from the crystal and Bragg scattering on the analyser crystal
% R.C. 13-August-1998

% === return if no data structure passed or not present in the MSlice ControlWindow
if ~exist('data','var'),
   hcw=findobj('Tag','ms_ControlWindow');
   if isempty(hcw),
      disp(['No data set given and MSlice ControlWindow not opened. Spurion function not executed.']);
      return;
   end
   data=get(hcw,'UserData');
   if isempty(data),
      disp(['No data loaded in the MSlice ControlWindow. Return.']);
      return
   end
end

% === redefine fligth paths on IRIS 
l1=36.41;	% primary flight path (m)
l2=1.47;		% secondary flight path (m)

% === if 'disp_spe' figure does not exist, return
fig=findobj('Tag','disp_spe');
if isempty(fig),
   disp('Can only read spurion positions on a (detector_group,energy) plot. Return.');
   return
end

% === read spurion position and eliminate point if outside the plotting region
disp(['Read spurion position from a (detector group number, energy) 2d colour plot']);
figure(fig);
[det_group,en]=ginput(1);
det_group=round(det_group);
if (en<data.en(1))|(en>data.en(length(data.en)))|(det_group<1)|(det_group>max(data.det_group)),
   disp(['Point chosen lies outside the plotting region. Try again.']);
   return
end
index=find(data.det_group==det_group);
twotheta=data.det_theta(index(1));

% === transfirm back into raw data as a function of time-of-flight
tof=l1/sqrt((en+data.efixed)/5.227)+l2/sqrt(data.efixed/5.227);	% transform back into total tof in mili secs.
v=(l1+l2)/tof;	% speed in [km/s] for elastically scattered neutrons 
k=sqrt(5.227*v^2/2.072);	% wavevector in Å^{-1}
modQ=2*k*sin(twotheta/2);
disp(sprintf('IRIS Spurion position: Energy = %5g meV \n TwoTheta = %5g deg',en,twotheta*180/pi));
disp(sprintf('tof = %5g mu secs. \n ki = %5g Å^{-1} Ei= %5g meV \n |Q| = %5g Å^{-1}',tof*1000,k,5.227*v^2,modQ));

% === transform Q into sample reference frame (if crystal orientation information given) 
% === by rotating the coordinate system
if ~isfield(data,'psi_samp'),
   return;
end
ki=k*[1 0 0];
kf=k*[cos(twotheta) sin(twotheta) 0];
Q=ki-kf;
Q=sqe2samp(Q,data.psi_samp);

% === assuming orthogonal reciprocal lattice vectors, should rewrite more generally for monoclinic lattices 
h=dot(data.ar,Q)/dot(data.ar,data.ar);
k=dot(data.br,Q)/dot(data.br,data.br);
l=dot(data.cr,Q)/dot(data.cr,data.cr);
h=round(h);
k=round(k);
l=round(l);

% === Find where the nearest Bragg reflection would produce a spurion in the inelastic spectrum
Q=sqe2samp(h*data.ar+k*data.br+l*data.cr,-data.psi_samp);
kf=sqrt(data.efixed/2.072);

% in choosing twotheta there is an ambiguity because sin(x)=sin(pi-x)
% out of these two choose value closest to angle where spurion occurs 

if abs(kf)<abs(Q(2)),
   disp(['Reflection not possible']);
   return
end
ki=Q(1)+sign(cos(twotheta))*sqrt(kf^2-Q(2)^2);
twotheta=atan2(-Q(2)/kf,(ki-Q(1))/kf);
en=2.072*ki^2-data.efixed;
disp(sprintf('Nearest elastic reflection (h,k,l) [rlu] = ( %3d %3d %3d )',h,k,l));
disp(sprintf('would occur at Twotheta = %5.3f deg and Energy = %5.3f meV',twotheta*180/pi,en));
