function datout=readmesh2avg (filename)
%function to read mesh files put out by the Data analysis group

%GEG 3.5.2009
%ORNL/NSSD
if nargin <1
    filename=uigetfile('*.in');
end
%generate format string
istr=repmat(' %g',1,27);
istr=strcat('%d',istr,'%g\n');
% open file
ttst=cputime;
fid=fopen(filename,'r');
%read all data in it is in one long column
data=fscanf(fid,istr);
fclose(fid);
cputime-ttst
%reshape data so it is in 29 rows and a give number of columns
B=reshape(data,29,[]);   
% reshape Q data for averaging
verts=reshape(B(6:29,:),3,8,[]);
%average Q points 
datout.Q=sum(verts,2)./8;
datout.Q=squeeze(datout.Q);
%Average E 
datout.E=sum(B(2:3,1))/2;
datout.I=B(4,:);
datout.err=B(5,:);
