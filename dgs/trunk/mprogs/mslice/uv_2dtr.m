function data=uv_2dtr(data_in,J,Jp)

% function data=uv_2dtr(data_in,J,Jp)
% calculate analytical value of S(k,w) given data_in.v=cat(3,k,l,e); 

tic;
data=data_in;
k=data.v(:,:,1);
l=data.v(:,:,2);
e=data.v(:,:,3);
sig=(data.en(2)-data.en(1))*3;

if (2*J)>Jp,
   Q=1-1/pi*acos(Jp/(2*J));
else
   Q=1;
end

Jk=J*cos(2*pi*k)+2*Jp*cos(pi*k).*cos(pi*l);
JQ=J*cos(2*pi*Q)+2*Jp*cos(pi*Q);
JkpQ=J*cos(2*pi*(k+Q))+2*Jp*cos(pi*(k+Q)).*cos(pi*l);
JkmQ=J*cos(2*pi*(k-Q))+2*Jp*cos(pi*(k-Q)).*cos(pi*l);

a=-JQ+Jk;
b=(JkpQ+JkmQ)/2-JQ;
Ak=(a+b)/2;
Bk=(a-b)/2;
wk=sqrt(Ak.^2-Bk.^2);
thk=atanh(Bk./Ak)/2;
u=cosh(thk);
v=sinh(thk);
index=(isnan(u)|isnan(v)|isinf(u)|isinf(v));
deltaS=mean(v(~index).^2);
uv=mean(u(~index).*v(~index));
data.S=0.5/(1+2*deltaS-2*uv)*((u+v).^2)./(sqrt(2*pi)*sig).*exp(-0.5*(e-wk).^2/sig^2);
data.S(index)=NaN;
disp(sprintf('DeltaS=<v^2>=%g, <uv>=%g',deltaS,uv));
disp(sprintf('Completed in %g secs',toc));
