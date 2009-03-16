function data_out=ms_iris_spe(emin,emax,de,p,fit_pars,spec_pars)

% function ms_iris_spe(emin,emax,de,p,fit_pars,spec_pars)
% return simulated scattering including resolution effects and also
% put data back into the Mslice ControlWindow and calculate projections
% data.efixed, psi_samp, ar(1,3), br(1,3), cr(1,3), det_theta(ndet,1) 
% p=[p1 p2 ...] cross-section parameters 
% fit_pars =[icross nmc] cross-section number {1,2,3,...} and number of monte carlo points for each pixel
% spec_pars=[theta_h(fwhm rad) theta_v(fwhm rad) psi_h(fwhm rad) psi_v(fwhm rad) 
% 				dt(fwhm sec) ] for IRIS

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
data_out=data;
data_out.en=emin:de:emax;
data_out.en=data_out.en(1:(end-1))+de/2; % choose midpoints per each bin

ne=length(data_out.en);
% === allocate space for the intensities
ndet=length(data_out.det_theta);
data_out.S=NaN*ones(ndet,ne);
data_out.ERR=data_out.S;
% === construct transformation matrix for hkl projections  (ub matrix)
[aaa,bbb,ccc]=basis_hkl(data_out.ar,data_out.br,data_out.cr);
ub=[aaa(:)';bbb(:)';ccc(:)'];    

% === calculate cross-section scattering for one detector at a time
for i=1:ndet,
   npixels=ones(1,ne);	% all pixels in a detector treated individually
   pixels=[data_out.en' de*ones(ne,1) data_out.det_theta(i)*ones(ne,1)]';	% [en de theta] for each detector
%   p,fit_pars,spec_pars,npixels,data.emode,data.efixed,data.psi_samp,data.ar,data.br,data.cr,ub,
%   pause;
   [data_out.S(i,:),data_out.ERR(i,:)]=ms_iris(p,fit_pars,spec_pars,npixels,pixels,...
      data.efixed,data.psi_samp,data.ar,data.br,data.cr,ub);
   disp(['Completed detector ' num2str(i)]);
   temp=isnan(data_out.S(i,:));
   if sum(temp)>=1,
      disp(sprintf('%d pixels with NaN intensity',sum(temp))); 
      disp(sprintf('en=%g',data_out.en(temp)));
   end
   temp=isnan(data_out.ERR(i,:));
   if sum(temp)>=1,
      disp(sprintf('%d pixels with NaN ebar',sum(temp))); 
      disp(sprintf('en= %g',data_out.en(temp)));
   end
end
data_out.title_label=['cross=' num2str(fit_pars(2)) ' nmc=' num2str(fit_pars(1)) ', p=['];
for i=1:min(length(p),4),
   data_out.title_label=[data_out.title_label sprintf(' %g,',p(i))];
end
data_out.title_label=[data_out.title_label(1:(end-1)) ']'];
towindow(data_out);
ms_calc_proj;
