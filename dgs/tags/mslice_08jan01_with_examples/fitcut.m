function [y, name, pnames, pin]=fitcut(x,p, flag)

% fitcut 
% [y, {name, pnames, pin}]=fitcut(x,p, {flag}) 
% MFIT  fitting function
% Author:  R.Coldea 18-April-1999
% Fit cut data (single crystal) from IRIS with angular and energy resolution convolution

% === locate ms_fitcut window and extract data with pixel information and sample orientation
h=findobj('Tag','ms_fitcut');
if isempty(h)|isempty(get(h,'UserData')),
   disp(['Could not locate data with pixel information. Return']);
   return;
end
data=get(h,'UserData');

if nargin==2,
   %y=p(8)*ones(size(data.y));
   %clear mex ms_iris 
   [y,error]=ms_iris(p(:)',data.fit_pars,data.spec_pars,data.npixels,transpose(data.pixels(:,[1 2 3])),...
      data.efixed,data.psi_samp,data.ar,data.br,data.cr,data.ub);
   % data.pixels = [en(meV) enbin(meV) theta(radians)]
   % data.fitpars =[nmc icross]
   % === interpolate if x values are not the ones of the data set
   if (length(x)~=length(data.x))|any(abs(x(:)-data.x(:))>eps),
      y=interp1(data.x(:),y(:),x(:));
      err=interp1(data.x(:),err(:),x(:));
   end
else
   y=[];
   icross=data.fit_pars(2);	% cross-section number
   switch icross,
      case {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}	% dispersion relation in Cs2CuCl4
         name=['FitCut IRIS Cs2CuCl4 icross= ' num2str(icross)]; 
         if icross==1,
            % === non-dispersive mode
            pnames=str2mat('Amplitude','Energy','FWHM','unused','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'unused','unused','unused','Bkg_en?');
         elseif icross==2,
            % === isotropic cycloidal spin waves with absolute amplitudes 
            pnames=str2mat('Amp_sw','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'A_sw_m','A_sw_p','unused','Bkg_en?');            
         elseif (icross==3)|(icross==19),
            % === polarised cycloidal spin waves with relative amplitudes  
            pnames=str2mat('Amp_sw','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'A_sw_m_r','A_sw_p_r','unused','Bkg_en?');            
         elseif icross==4,            
            % === 3 isotropic Muller continua with absolute amplitudes and mean upper bound
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'A_yy_m','A_yy_p','UBoundScale','Bkg_en?');
         elseif icross==5,
            % === 3 polarised Muller continua with relative amplitudes and mean upper bound           
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'A_yy_m_r','A_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==6,
            % === 1 isotropic Muller continua for {xx} and polarised cycloidal spin waves for {yy} and {zz}
            % === with absolute amplitudes 
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'A_sw_m','A_sw_p','UBoundScale','Bkg_en?');            
         elseif icross==7,            
            % === same as icross=4 but amplitudes are relative to A_xx
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==8,            
            % === same as icross=7 but upper boundary corresponds to the 2-spinon upper bond for the maximum zone boundary energy 
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==9,            
            % === same as icross=8, but upper boundary corresponds has the modulation of the lower boundary  
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==10,            
            % === same as icross=4 (absolute amplitudes) but with upper boundaries as icross=9 
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'Ar_yy_m','Ar_yy_p','UBoundScale','Bkg_en?');         
         elseif icross==11,            
            % === polarised Muller ansatz continua with relative amplitudes and modulated upper bound 
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'Ar_yy_m','Ar_yy_p','UBoundScale','Bkg_en?');         
         elseif icross==12,            
            % === polarised Muller ansatz continua with relative amplitudes and modulated upper bound 
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'Ar_yy_m','Ar_yy_p','UBoundScale','Bkg_en?');         
         elseif icross==13,            
            % === Schultz's field theory for the temperature dependence of the continuum 
            pnames=str2mat('Amp_xx','J','unused','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'unused','unused','unused','Bkg_en?');         
         elseif icross==14,            
            % === Schultz's field theory for the temperature dependence of the continuum (only around the af zoner centre q~=0.5)
            pnames=str2mat('Amp_xx','J','unused','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
				pnames=str2mat(pnames,'unused','unused','unused','Bkg_en?');         
         elseif icross==15,            
            % === same as icross=9, but upper boundary has the modulation of the lower boundary is is approx=sum of min+max zb energies  
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==16,            
            % === same as icross=11, but upper boundary has the modulation of the lower boundary is is approx=sum of min+max zb energies  
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==17,            
            % === 1+2magnon cross-section from table with monte-carlo events 
            pnames=str2mat('Amp','J','FWHM','<uv>','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'A1m_in','A2m_r','DeltaS','Bkg_en?');
         elseif icross==18,            
            % === same as icross=9, but power of (w^2-wk^2) is adjustable  
            pnames=str2mat('Amp_xx','J','nu','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'Ar_yy_m_r','Ar_yy_p_r','UBoundScale','Bkg_en?');
         elseif icross==20,            
            % === isotropic Muller ansatz, modified lower boundary  
            pnames=str2mat('Amp_xx','J','FWHM','J/Jp','Amp_inc','FWHM_inc','Cen_inc','Temp(K)','Flat_Bkg','Amp_exp_bkg','Decay_bkg');
            pnames=str2mat(pnames,'Ar_yy_r','UBoundQ','UBoundAmp','Bkg_en?');            
         end   
         if flag==1, 
            pin=[0.48068 0.64 0.25 0.175 0 0.01  0 0.1 1.38 1.896 0.17781]; 
            if (icross==1)|(icross==2),
               pin=[pin 0 0 0 1];
            elseif (icross==3)|(icross==19),
               pin=[pin 1 1 0 1];
            elseif (icross==4),
               pin=[pin 0.33 0.33 1 1];pin(1)=0.33;
            elseif ~isempty(find(icross==[5:16])),
               pin=[pin 1 1 1 1];
            elseif (icross==17),
               pin=[pin 0 1 0.125 1];  
               pin(4)=0;
            elseif (icross==18),
               pin=[pin 1 1 1 1]; 
               pin(3)=1/2;
            elseif (icross==20),
               pin=[pin 1 0.3 1.01 1];                
            end
   		else
            mf_msg('Click on a point on the flat background portion');            
            [x,bg]=ginput(1); 
            mf_msg('Click on first point of the decaying background');
            [x1,y1]=ginput(1);
            mf_msg('Click on second point of the decaying background');
            [x2,y2]=ginput(1); 
            y1=y1-bg;
            y2=y2-bg;
            tau_bg=(x2-x1)/log(y1/y2);
            amp_bg=y1/exp(-x1/tau_bg);
            mf_msg('Sorry, the rest of the parameters do not support guess');
            pin=[0.48 0.64 0.01 0.175 0 0.01 0 0.1 bg amp_bg tau_bg];
            if (icross==1)|(icross==2),
               pin=[pin 0 0 0 1];
            elseif (icross==3)|(icross==19),
               pin=[pin 1 1 0 1];
            elseif (icross==4),
               pin=[pin 0.33 0.33 1 1];pin(1)=0.33;
            elseif ~isempty(find(icross==[5:16])),
               pin=[pin 1 1 1 1];
            elseif (icross==17),
               pin=[pin 0 1 0.125 1];
               pin(4)=0;
            elseif (icross==18),
               pin=[pin 1 1 1 1]; 
               pin(3)=1/2;
            elseif (icross==20),
               pin=[pin 1 0.3 1.01 1];                
            end
         end
      otherwise,
         y=[];
         name=[];
         pnames=[];
         pin=[];
         disp('Unknown cross-section number. Return');
   end
end