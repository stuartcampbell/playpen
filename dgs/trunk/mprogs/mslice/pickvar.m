function [v,axislabel,unitname,unitlength,shortlabel,Q]=pickvar(data,x,Q)

% function [v,axislabel,unitname,unitlength,shortlabel,Q]=pickvar(data,x,Q)

% === return if errors in calling syntax
if ~exist('data','var')|isempty(data)|~isnumeric(x)|(prod(size(x))~=1),
   disp('Empty data set or non-numeric index x. Command not executed.');
   help pickvar;
   v=[];
   axislabel=[];
   unitname=[];
   unitlength=[];
   shortlabel=[];
   return
end

% === determine sample and detector types from the MSlice ControlWindow
% === if MSlice Control Window non-existent, return
fig=findobj('Tag','ms_ControlWindow');
if isempty(fig),
   disp('Control Window non-existent. Return.');
   v=[];
   axislabel=[];
   unitname=[];
   unitlength=[];
   shortlabel=[];
   Q=[];
   return;
end
samp=get(findobj(fig,'Tag','ms_sample'),'Value'); % =1 single crystal, =2 powder
if samp==2, % powder
	wavevectors=[];   
elseif samp==1,	% single crystal
   analmode=get(findobj(fig,'Tag','ms_analysis_mode'),'Value');	% =1 if analysed as single crystal, =2 if analysed as powder
   if analmode==1,	% single crystal smaple analysed as a single crystal
	   psd=get(findobj(fig,'Tag','ms_det_type'),'Value');	%=1 psd , =2 conventional
   	if psd==1,	% PSD detectors
      	wavevectors={4,5,6,7,8,9};	% H, K, L , u1, u2, u3
   	else
      	wavevectors={4,5,6,7,8};	% H, K, L, u1, u2
      end
   else % single crystal analysed as a powder 
      wavevectors={4,5,6};				% H, K, L
   end
end

if ~isempty(wavevectors)&(sum(x==[1:(3+length(wavevectors))])==1), 	% choose a wavevector direction
	switch x,
		case {1,2,3},	% Q_x, Q_y, Q_z 
         if isempty(Q),
            Q=spe2sqe(data);
         end
     		v=Q(:,:,x); 
      	unitname='Å^{-1}';
      	unitlength=1;
   		switch x,	
      		case 1, axislabel='Q || k_i in '; shortlabel='Q_x';         
      		case 2, axislabel='Q \perp k_i in principal scattering plane in '; shortlabel='Q_y';
         	case 3, axislabel='Q \perp k_i and \perp to principal scattering plane in '; shortlabel='Q_z';
      	end
      case wavevectors,	% H, K, L, u1, u2, u3 for single crystal and psd ; 
         					% H, K, L, u1, u2 for single crystal and concventional detectors 
         					% H, K, L for single crystal analysed in powder mode
         if sum(x==[4,5,6])==1, % H, K, L (rlu)
            x=x-3;
            u=[0 0 0 0];
            u(x)=1;
            U=basis_u(u,data.ar,data.br,data.cr);
            if isempty(Q),
	            Q=spe2sqe(data);
            end
            try
	         	[aa,bb,cc]=basis_hkl(data.ar,data.br,data.cr);
         		hkl=zeros(length(data.det_group),length(data.en),3);
         		[hkl(:,:,1),hkl(:,:,2),hkl(:,:,3)]=...
            	   spe2proj_df(data.emode,data.efixed,data.en,...
      					data.det_theta,data.det_psi,data.psi_samp,...
      					[aa 0],[bb 0],[cc 0]);
      		   %disp('Using spe2proj_df for pickvar');         
            catch            
               hkl=q2rlu(sqe2samp(Q,data.psi_samp),data.ar,data.br,data.cr);
            end
            v=hkl(:,:,x);
            labels={'H','K','L'};
            shortlabel=labels{x};
            unitlength=norm(U);
            axislabel=[labels{x} ' (rlu) in ' num2str(unitlength,'%7.4g') ' '] ;
            unitname='Å^{-1}';
         else 	% u1, u2 (u3) for single crystal analysis
            x=x-6;
            if ~isfield(data,'v'),
         		disp('Calculate projections first, then plot detector trajectories.');
         		v=[];
         		axislabel=[];
         		unitname=[];
         		unitlength=[];
         		shortlabel=[];
         		return;
				end
            v=data.v(:,:,x);
      		u=data.u(x,:);
      		U=basis_u(u,data.ar,data.br,data.cr);
      		if all(U(1:3)==0)&(U(4)~=0),% Energy axis
         		axislabel='Energy ';
         		unitname='(meV)';
         		unitlength=1; 
         		shortlabel='E';
      		elseif any(U(1:3)~=0)&(U(4)==0),	% Wavevector direction
   				unitlength=norm(U);
   				unitname='Å^{-1}';   
            	axislabel=[combil(deblank(data.axis_label(x,:)),u) ' in ' num2str(unitlength,'%7.4g') ' '];
         		shortlabel=deblank(data.axis_label(x,:));  
      		else	% mixed direction
         		unitlength=NaN;
         		unitname='wavevector and energy';
         		axislabel='Mixed direction ';
         		shortlabel='Mixed';
         		disp(sprintf('Mixed direction in vector U=[%7.4g,%7.4g,%7.4g,%7.4g]',U(1:4)));
            end
         end
      otherwise,
   end % switch {x}
 else	% either single crystal or powder, but variable is Energy, |Q|, 2Theta, Azimuth or Det Group Number (or 'none')
   if ~isempty(wavevectors),	% single crystal sample and Energy,|Q| ...as variable
      x=x-3-length(wavevectors);
   end
   switch x,
   	case {1},			% Energy
      	v=ones(size(data.det_theta))*data.en;
      	axislabel='Energy ';
      	unitname='(meV)';
      	unitlength=1;
      	shortlabel='E';
   	case {2}, 			% mod Q 
      	Q=spe2sqe(data);
      	v=sqrt(Q(:,:,1).^2+Q(:,:,2).^2+Q(:,:,3).^2);
      	axislabel='|Q| in ';
      	unitname='Å^{-1}';
      	unitlength=1;
      	shortlabel='|Q|';
   	case {3},			% TwoTheta
      	v=(data.det_theta*180/pi)*ones(size(data.en));
      	axislabel='TwoTheta ';
      	unitname='(deg)';
      	unitlength=1; 
      	shortlabel='2\Theta';
   	case {4},			% Azimuthal angle
      	if isfield(data,'det_psi'),
         	v=(data.det_psi*180/pi)*ones(size(data.en));
      	else
         	v=NaN*ones(size(data.det_theta,1),size(data.en,2));
      	end
      	axislabel='Azimuthal Angle ';
      	unitname='(deg)';
      	unitlength=1;
      	shortlabel='Az';
		case {5},			% Det. Group No.
      	v=(data.det_group)*ones(size(data.en));
      	axislabel='Detector Group Number';
      	unitname=[];
      	unitlength=1;
      	shortlabel='Det';
   	case {6},			% 'none'
      	v=[];
      	axislabel=[];
      	unitname=[];
      	unitlength=[];
      	shortlabel=[];   
   	otherwise,
      	v=[],
      	axislabel=[];
      	unitname=[];
      	unitlength=[];
      	shortlabel=[];
      	disp(['Wrong parameter number ' num2str(x)]);
      	return;
	end
end