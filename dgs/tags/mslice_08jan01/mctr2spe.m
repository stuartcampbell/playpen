function data=mctr2spe(filename,normpernbz)

% function data=mc2spe(filename,normpernbz)
% produces a data structure of type spe
% from a monte-carlo simulation of the 2D S=1/2 HAF on a triangular lattice 
% S(x,y,e) stored in an ASCII file produced by 
% x1,x2,dx,y1,y2,dy,e1,e2,de, S(x,y,e)(:) nx*ny*ne values

if ~exist('filename','var'),
   help mc2spe;
   return
end

fid=fopen(filename,'r');
if fid==-1,
   disp(['Error opening file ' filename ' . Data not read.']);
   data=[];
   return
end

% === read grid boundary parameters
d=fscanf(fid,'%f',12); 
x1=d(1);
x2=d(2);
dx=d(3);
nx=d(4);
y1=d(5);
y2=d(6);
dy=d(7);
ny=d(8);
e1=d(9);
e2=d(10);
de=d(11);
ne=d(12);
%nx=floor((x2-x1)/dx);
%ny=floor((y2-y1)/dy);
%ne=floor((e2-e1)/de);
disp(sprintf('x1=%g, x2=%g',x1,x2));
disp(sprintf('y1=%g, y2=%g',y1,y2));
disp(sprintf('e1=%g, e2=%g',e1,e2));
disp(sprintf('nx=%d, ny=%d, ne=%d',nx,ny,ne));
disp(sprintf('dx=%g, dy=%g, de=%g',dx,dy,de));
ndet=nx*ny;
S=fscanf(fid,'%f',ny*nx*ne); 
fclose(fid);

% === prepare energy and x,y grid
x=(x1+dx/2):dx:(x2-dx/2);
y=(y1+dy/2):dy:(y2-dy/2);
en=((e1+de/2):de:(e2-de/2));
[Qk,Ql,En]=meshgrid(x,y,en);
data.filename=stripath(filename);
data.en=en; 	% energy scale converted to meV
data.emode=1;
ndet=nx*ny;	% virtual detector trajectories
data.S=reshape(S,ndet,ne);

% =================================
% === alternative algorithm which will be later implemented in fortran 
% ===================================
%data.S=NaN*ones(ndet,ne);
%data.v=NaN*ones(ndet,ne,3);


%for ih=1:nh,
%   for il=1:nl,
%      for ie=1:ne,
%         idet=il+nl*(ih-1);
%         itot=il+nl*(ih-1)+nl*nh*(ie-1);
%         data.S(idet,ie)=S(itot);
%         data.v(idet,ie,1)=qh(ih);
%			 data.v(idet,ie,2)=ql(il);
%         data.v(idet,ie,3)=en(ie);
%       end
%    end
%end   

data.det_group=[1:ndet]';
data.det_theta=zeros(size(data.det_group));
Nevents=sum(S(:));
data.title_label=['MC simulation Nevents=' num2str(Nevents)];
data.efixed=[];
data.psi_samp=[];
data.ERR=sqrt(data.S);	
%deltaS=0.196;
%deltaS=0.153;
%S=0.5;
%absnorm=deltaS*(deltaS+1)	% for 2-magnon
%absnorm=(S-deltaS)*(2*deltaS+1)	% for 1-magnon
disp(sprintf('Nevents=%10d',sum(data.S(:))));
disp(sprintf('Normalizing intensities to an integrated sum of %g \n per nuclear Brillouin zone',normpernbz));
if ne==1, % one energy bin
   % integration along energy already performed
   absnorm=2*2*normpernbz/(Nevents*dx*dy);	% see calculations notebook 24 page 45-46
else
   absnorm=2*2*normpernbz/(Nevents*dx*dy*(de*1e-3));	% see calculations notebook 24 page 45-46   
end   
% first factor of 2 from change in the lattice parameter orthorhombic notation b=a * sqrt(2), where a square lattice spacing 
% second factor of 2 because Qh=-2:0 Ql=-1:1 cover two nuclear Brillouin zones
data.S=data.S*absnorm; 
data.ERR=data.ERR*absnorm;

set=2;
if set==1,
	v1=reshape(Qk,ndet,ne);
	v2=reshape(Ql,ndet,ne);
	v3=reshape(En,ndet,ne);
	b=1;
	c=b/sqrt(3);
	qk=(v1-v2);
	ql=(v1+v2-2);
	data.v=cat(3,qk,ql,v3);
	u1=[0 1 0 0];
	u2=[0 0 1 0];
	u3=[0 0 0 1];
	data.u=[u1;u2;u3];
	data.axis_label=str2mat('Qk','Ql','E');
	data.axis_unitlabel=str2mat('in 1 Å^{-1}','in 1 Å^{-1}', ' (meV)','Intensity (arb. units)');
   data.axis_unitlength=[1 1 1];
elseif set==2,
	v1=reshape(Qk,ndet,ne);
	v2=reshape(Ql,ndet,ne);
	v3=reshape(En,ndet,ne);
	b=1;
   %c=b;
   c=b/sqrt(3);
	qk=(v1-v2)*b;
	ql=(v1+v2-2)*c;
	data.v=cat(3,qk,v3,ql);
	u1=[0 1 0 0];
	u2=[0 0 0 1];
	u3=[0 0 1 0];
	data.u=[u1;u2;u3];
	data.axis_label=str2mat('Qk','E','Ql');
	data.axis_unitlabel=str2mat('in 1 Å^{-1}',' (meV)','in 1 Å^{-1}','Intensity (arb. units)');
   data.axis_unitlength=[1 1 1];
end   

