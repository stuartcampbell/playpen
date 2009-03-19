function data=mc2spe(filename,normpernbz)

% function data=mc2spe(filename,normpernbz)
% produces a data structure of type spe
% from a monte-carlo simulation of the 2D S=1/2 HAF 
% S(qh,ql,e) stored in an ASCII file produced by init_all.m 
% qh1,qh2,dqh,ql1,ql2,dql,e1,e2,de, S(qh,ql,e)(:) nh*nl*ne values
% updated R.Coldea, 10 September 1999

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

d=fscanf(fid,'%f',9);
% === read grid boundary parameters
qh1=d(1);
qh2=d(2);
dqh=d(3);
ql1=d(4);
ql2=d(5);
dql=d(6);
e1=d(7);
e2=d(8);
de=d(9);
nh=floor((qh2-qh1)/dqh);
nl=floor((ql2-ql1)/dql);
ne=floor((e2-e1)/de);
disp(sprintf('nh=%12d, nl=%12d, ne=%12d',nh,nl,ne));
ndet=nh*nl;
S=fscanf(fid,'%f',nl*nh*ne); 
fclose(fid);

% === prepare energy and qh,ql grid
qh=(qh1+dqh/2):dqh:(qh2-dqh/2);
ql=(ql1+dql/2):dql:(ql2-dql/2);
en=((e1+de/2):de:(e2-de/2));
[Qh,Ql,En]=meshgrid(qh,ql,en);
data.filename=stripath(filename);
data.en=en; 	% energy scale converted to meV
data.emode=1;
ndet=nh*nl;	% virtual detector trajectories
data.S=reshape(S,ndet,ne);

v1=reshape(Qh,ndet,ne);
v2=reshape(Ql,ndet,ne);
v3=reshape(En,ndet,ne);
data.v=cat(3,v1,v2,v3);

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
%        	data.v(idet,ie,1)=qh(ih);
%			data.v(idet,ie,2)=ql(il);
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
disp(sprintf('Normalizing intensities to an integrated sum of %5.3g per nuclear Brillouin zone',normpernbz));
if ne==1,
   % integration along energy already performed
   absnorm=2*2*normpernbz/(Nevents*dqh*dql);	% see calculations notebook 24 page 45-46
else
   absnorm=2*2*normpernbz/(Nevents*dqh*dql*(de*1e-3));	% see calculations notebook 24 page 45-46   
end   
% first factor of 2 from change in the lattice parameter orthorhombic notation b=a * sqrt(2), where a square lattice spacing 
% second factor of 2 because Qh=-2:0 Ql=-1:1 cover two nuclear Brillouin zones
data.S=data.S*absnorm;
data.ERR=data.ERR*absnorm;
u1=[1 0 0 0];
u2=[0 0 1 0];
u3=[0 0 0 1];
data.u=[u1;u2;u3];
data.axis_label=str2mat('Q_h','Q_l','E');
data.axis_unitlabel=str2mat(' in 1.16 Å^{-1}',' in 1.16 Å^{-1}', ' (meV)','Intensity (arb. units)');
data.axis_unitlength=[1.16 1.16 1];