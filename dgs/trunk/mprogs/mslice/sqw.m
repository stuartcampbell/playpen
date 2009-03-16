function [s,wdisp]=sqw(Q,w,model)

% dispersion relation 
% Q=(q_h q_k q_l) rlu (in sample reference frame)
% w (meV), same size as Qx,Qy,Qz

% 1d dispersion along [0 1 0]
if ndims(Q)==3,
   Q=Q(:,:,2);
elseif ndism(Q)==2,
   Q=Q(:,2);
end 

if strcmp(model,'1DAF spin-waves'),
	% simple 1D AF magnons 
	J=0.6;
	wdisp=J*abs(sin(2*pi*Q));
	s=(1-cos(2*pi*Q))./(abs(sin(2*pi*Q))+0.001);
	sig=J/10;	% artificial Gaussian broadening in energy 
	s=1/(sqrt(2*pi)*sig)*s.*exp(-(w-wdisp).^2/(2*sig^2));
elseif strcmp(model,'1D spinons')
	% spinon model
	J=0.41;
	w1=pi/2*J*abs(sin(2*pi*Q));
	wdisp=w1;
	w2=pi*J*abs(sin(pi*Q));
	s=(w>(w1-0.00001)).*(w<=w2).*1./sqrt(w.^2-w1.^2+0.001);
else
   disp(['Unknown model ' model]);
   disp(['Try ''1DAF spin-waves''  or ''1D spinons'' '])
   s=[];
   wdisp=[];
end