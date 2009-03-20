function ms_simulate(emin,emax,de,crossection,parameters)

% function ms_simulate(emin,emax,de,crosssection,parameters)
% read spectrometer and single crystal/powder information from MSlice Control Window
% and simulate scattering, put data back into the ControlWindow
% is setup for single crystal samples (07-Jan-1999)
% crosssection number = 1,2,..
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
data.en=emin:de:emax;
data.en=data.en(1:(end-1))+de/2; % choose midpoints per each bin

% === simulate scattering in each detector
Q=spe2sqe(data);	% create wavevector Q (ndet,ne,3) in spectrometer reference frame 			
Q=sqe2samp(Q,data.psi_samp);	% transform (Q,E)->(Qx',Qy',Qz',E) into sample reference frame Q[Å^{-1}]
Q=q2rlu(Q,data.ar,data.br,data.cr);
[ndet,ne]=size(Q(:,:,1)); % ndet = number of detector groups, ne=numberof energy bins
en=ones(ndet,1)*data.en;	% table (ndet,ne)
theta=data.det_theta*ones(size(data.en)); % table(ndet,ne)
Qx=Q(:,:,1); % (ndet,ne)
Qy=Q(:,:,2);
Qz=Q(:,:,3);
Q=[Qx(:) Qy(:) Qz(:)]; % (ndet*ne,3)
data.S=ms_sqw(crossection,parameters,Q,en(:),theta(:),data.ar,data.br,data.cr);
data.S=reshape(data.S,ndet,ne);
if isempty(data.S),
   disp(['Cross-section type ' crosssection ' not available.']);
   return
end
data.ERR=0.1*data.S;	% put arbitrary errors of 10% intensity
data.filename=['cross=' num2str(crossection) ', p=['];
for i=1:length(parameters),
   data.filename=[data.filename sprintf(' %g,',parameters(i))];
end
data.filename=[data.filename(1:(end-1)) ']'];
towindow(data);
ms_calc_proj;
