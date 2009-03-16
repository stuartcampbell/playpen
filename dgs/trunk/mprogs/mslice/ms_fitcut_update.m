function ms_fitcut_update;

% function ms_fitcut_update
% update parameters in the ms_fitcut window

% === return if Ms_fitcut window not opened 
h_cw=findobj('Tag','ms_fitcut');
if isempty(h_cw),
   disp(['No Mslice FitCut widow opened, no .cut data to be loaded.']);
   return;
end

% === extract data with pixel information
cut=get(h_cw,'UserData'); 
% structure with fields x(n,1),y(n,1),e(n,1),x_label,y_label,npixels(n,1),pixels(npixels,6)

% === establish which variable names are to be read 
% === read fit parameters
vars=str2mat('nmc','icross');
% === read sample parameters (lattice parameters and crystal information for single crystal)
samp=get(findobj(h_cw,'Tag','ms_fitcut_sample'),'Value');
if samp==1 % sample is single crystal 
   vars=str2mat(vars,'as','bs','cs','aa','bb','cc','ux','uy','uz');
   vars=str2mat(vars,'vx','vy','vz','efixed','emode','psi_samp');   
end   
% === read spectrometer parameters to calculate resolution
spectrometer=get(findobj(h_cw,'Tag','ms_fitcut_spectrometer'),'Value');
if spectrometer==1,
% IRIS spectrometer
   vars=str2mat(vars,'theta_h','theta_v','psi_h','psi_v','dt');
end

% ===== read relevant parameters from ControlWindow
for i=1:size(vars,1),
  	name=vars(i,:);
  	name=name(~isspace(name));
  	h=findobj(h_cw,'Tag',['ms_fitcut_' name]);
  	if isempty(h),
     	disp(['Warning: Object ms_fitcut_' name 'not found;']);
  	end;   
  	if strcmp(get(h,'Style'),'popupmenu'),
     	value=num2str(get(h,'Value'));
  		eval([ name '=' value ';']);   
  	else
     	value=get(h,'String');
     	% transform string into number
      if isempty(value),
        	eval([name '=[];']);
      else
       	eval([name '=' value ';']);
      end            
   end
end

% === attach fit parameters
cut.fit_pars=[nmc icross];

if samp==1,	% sample is sigle crystal
   % === determine reciprocal lattice basis
   [ar,br,cr]=basis_r(as,bs,cs,aa*pi/180,bb*pi/180,cc*pi/180);
   u=ux*ar+uy*br+uz*cr;	
   v=vx*ar+vy*br+vz*cr;
   % now u and v are (1,3) vectors expressed in the same reference frame as ar, br and cr
	u=u/norm(u);
	v=v-dot(v,u)*u;
	v=v/norm(v);	% (u,v) define the principal scattering plane of the spectrometer (Horiz HET&IRIS, Vert MARI)
   w=cross(u,v);	% w is vertically up on the (u,v) plane 
   % now (u,v,w) is a right-handed cooordinate system with all vectors orthogonal to each other and of unit length
   
	% === Determine components of the reciprical basis in terms of the (u,v,w) frame
	cut.ar=[dot(ar,u) dot(ar,v) dot(ar,w)];
	cut.br=[dot(br,u) dot(br,v) dot(br,w)];
	cut.cr=[dot(cr,u) dot(cr,v) dot(cr,w)];

	% === Update crystal orientation in the data set 
   cut.efixed=efixed;
	cut.emode=emode;
	cut.psi_samp=psi_samp*pi/180;
   [aaa,bbb,ccc]=basis_hkl(cut.ar,cut.br,cut.cr);
   cut.ub=[aaa(:)';bbb(:)';ccc(:)'];    
end  

% === attach also spectrometer parameters
if spectrometer==1,
   cut.spec_pars=[theta_h*pi/180 theta_v*pi/180 psi_h*pi/180 psi_v*pi/180 dt*1e-6];
   % [theta_h(fwhm deg -> radians) theta_v(fwhm deg -> radians) psi_h(fwhm deg ->radians) ...
   %   psi_v(fwhm deg -> radians) dt(fwhm \mu sec -> sec)]
end

% === put data back into the ms_fitcut window
set(h_cw,'UserData',cut);


