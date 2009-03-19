function v=pickvar_hkl(data,hkl,x)

% function v=pickvar_hkl(data,hkl,x)
% hkl = matrix (n,3)
% x = 1-8 (9 if PSD),  v=Q_x,Q_y,Q_z,H,K,L,U1,U1,(U3)

if ~exist('data','var')|isempty(data)|isempty(hkl)|~isnumeric(hkl)|...
      (size(hkl,2)~=3)|isempty(x)|~isnumeric(x),
   disp('Empty data set, hkl or wrong index x. Command not executed.');
   help pickvar;
   v=[];
   return
end

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

% === determine indexes of wavevector directions
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

switch x,
	case {1,2,3},	% Q_x, Q_y, Q_z 
      Q=hkl(:,1)*data.ar+hkl(:,2)*data.br+hkl(:,3)*data.cr;
      Q=sqe2samp(Q,-data.psi_samp);
      v=Q(:,x); 
   case wavevectors,	% H, K, L, u1, u2 (u3)
      if sum(x==[4,5,6])==1, % H, K, L
         x=x-3;
         v=hkl(:,x);
      else
        	U=basis_u(data.u(x-6,:),data.ar,data.br,data.cr);
        	if (U(4)==0)&any(U(1:3)~=0),	% true wavevector direction 
    	     Q=hkl(:,1)*data.ar+hkl(:,2)*data.br+hkl(:,3)*data.cr;
      	   v=(Q(:,1)*U(1)+Q(:,2)*U(2)+Q(:,3)*U(3))/norm(U(1:3)).^2;
      	else	% energy or mixed wavevector-energy direction
      		v=[];   
      	end
      end
   otherwise,
      v=[];
      disp(['Wrong parameter number for (h,k,l) label ' num2str(x)]);
      return
end
      
